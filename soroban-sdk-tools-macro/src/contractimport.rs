//! Implementation of the contractimport! macro.
//!
//! This macro wraps soroban_sdk::contractimport! and additionally generates:
//! - `SequentialError` and `ContractErrorSpec` implementations for imported error types,
//!   enabling them to be used as inner types in `#[scerr]` root enums with
//!   `#[transparent]` or `#[from_contract_client]` attributes
//! - AuthClient for simplified authorization testing (when testutils feature enabled)

use darling::FromMeta;
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote};
use sha2::{Digest, Sha256};
use soroban_spec::read::from_wasm;
use soroban_spec_rust::types::generate_type_ident;
use std::fs;
use stellar_xdr::curr::{ScSpecEntry, ScSpecTypeDef};
use syn::Error;

use crate::util::abs_from_rel_to_manifest;

#[derive(Debug, FromMeta)]
pub struct ContractImportArgs {
    file: String,
    #[darling(default)]
    sha256: Option<String>,
}

/// Information about an error enum extracted from WASM spec.
#[derive(Debug)]
struct ErrorEnumInfo {
    name: String,
    variants: Vec<ErrorVariantInfo>,
}

#[derive(Debug)]
struct ErrorVariantInfo {
    name: String,
    doc: String,
}

/// Extract error enum information from WASM spec entries.
fn extract_error_enums(specs: &[ScSpecEntry]) -> Vec<ErrorEnumInfo> {
    specs
        .iter()
        .filter_map(|entry| match entry {
            ScSpecEntry::UdtErrorEnumV0(e) => Some(ErrorEnumInfo {
                name: e.name.to_utf8_string_lossy(),
                variants: e
                    .cases
                    .iter()
                    .map(|c| ErrorVariantInfo {
                        name: c.name.to_utf8_string_lossy(),
                        doc: c.doc.to_utf8_string_lossy(),
                    })
                    .collect(),
            }),
            _ => None,
        })
        .collect()
}

/// Generate `ErrorSpecEntry` tokens for a list of variants.
///
/// Uses 1-based sequential codes (matching variant position) rather than
/// native error codes, consistent with `#[scerr]` basic mode.

// -----------------------------------------------------------------------------
// Function Extraction (for AuthClient generation)
// -----------------------------------------------------------------------------

/// Information about a contract function extracted from WASM spec.
#[derive(Debug)]
struct FunctionInfo {
    name: String,
    inputs: Vec<FunctionInputInfo>,
    outputs: Vec<ScSpecTypeDef>,
    doc: String,
}

/// Information about a function input parameter.
#[derive(Debug)]
struct FunctionInputInfo {
    name: String,
    type_def: ScSpecTypeDef,
}

/// Extract function information from WASM spec entries.
fn extract_functions(specs: &[ScSpecEntry]) -> Vec<FunctionInfo> {
    specs
        .iter()
        .filter_map(|entry| match entry {
            ScSpecEntry::FunctionV0(f) => Some(FunctionInfo {
                name: f.name.to_utf8_string_lossy(),
                inputs: f
                    .inputs
                    .iter()
                    .map(|i| FunctionInputInfo {
                        name: i.name.to_utf8_string_lossy(),
                        type_def: i.type_.clone(),
                    })
                    .collect(),
                outputs: f.outputs.iter().cloned().collect(),
                doc: f.doc.to_utf8_string_lossy(),
            }),
            _ => None,
        })
        .collect()
}

/// Unwrap a Result type to get the Ok type.
/// The standard Client methods return the Ok type (not the full Result).
fn unwrap_result_type(type_def: &ScSpecTypeDef) -> proc_macro2::TokenStream {
    match type_def {
        ScSpecTypeDef::Result(inner) => generate_type_ident(&inner.ok_type),
        _ => generate_type_ident(type_def),
    }
}

/// Generate the try return type for a function, matching soroban-sdk's try_ method convention.
///
/// The return type follows the pattern:
/// ```text
/// Result<
///     Result<T, <T as TryFromVal<Env, Val>>::Error>,
///     Result<E, soroban_sdk::InvokeError>
/// >
/// ```
fn generate_try_return_type(outputs: &[ScSpecTypeDef]) -> proc_macro2::TokenStream {
    let (ok_ty, err_ty) = if outputs.is_empty() {
        (quote! { () }, quote! { soroban_sdk::Error })
    } else {
        match &outputs[0] {
            ScSpecTypeDef::Result(inner) => {
                let ok = generate_type_ident(&inner.ok_type);
                let err = generate_type_ident(&inner.error_type);
                (quote! { #ok }, quote! { #err })
            }
            other => {
                let ty = generate_type_ident(other);
                (quote! { #ty }, quote! { soroban_sdk::Error })
            }
        }
    };

    quote! {
        Result<
            Result<#ok_ty, <#ok_ty as soroban_sdk::TryFromVal<soroban_sdk::Env, soroban_sdk::Val>>::Error>,
            Result<#err_ty, soroban_sdk::InvokeError>
        >
    }
}

/// Generate the expression to pass an argument to mock_auth.
///
/// This handles the different ownership semantics for various types:
/// - Copy types (primitives): dereference with `*`
/// - Clone types (heap/complex): clone with `.clone()`
/// - Void: use unit `()`
fn generate_arg_expr(
    name: &proc_macro2::Ident,
    type_def: &ScSpecTypeDef,
) -> proc_macro2::TokenStream {
    match type_def {
        // Copy types - dereference
        ScSpecTypeDef::Bool
        | ScSpecTypeDef::U32
        | ScSpecTypeDef::I32
        | ScSpecTypeDef::U64
        | ScSpecTypeDef::I64
        | ScSpecTypeDef::U128
        | ScSpecTypeDef::I128 => quote! { *#name },

        // Void - use unit
        ScSpecTypeDef::Void => quote! { () },

        // All other types need to be cloned
        // This includes: Address, Bytes, String, Symbol, Vec, Map, BytesN,
        // Udt, Val, Error, Timepoint, Duration, U256, I256, Option, Result, Tuple
        _ => quote! { #name.clone() },
    }
}

/// Generate the AuthClient struct and implementation.
fn generate_auth_client(functions: &[FunctionInfo]) -> proc_macro2::TokenStream {
    // Filter out special functions that aren't regular client methods
    let methods = functions
        .iter()
        .filter(|f| !f.name.starts_with("__"))
        .map(generate_auth_client_method);

    quote! {
        #[cfg(not(target_family = "wasm"))]
        extern crate alloc as __alloc;

        /// Auth-testing wrapper client for simplified authorization testing.
        ///
        /// This client wraps the standard `Client` and provides methods that
        /// return a `CallBuilder` for fluent authorization setup.
        ///
        /// # Example
        ///
        /// ```ignore
        /// let client = AuthClient::new(&env, &contract_id);
        /// let user = Address::generate(&env);
        ///
        /// // Builder pattern: method -> authorize -> invoke
        /// client.my_method(&user, &args).authorize(&user).invoke();
        ///
        /// // Multiple authorizers
        /// client.withdraw(&signers, &recipient, &amount)
        ///     .authorize(&signer1)
        ///     .authorize(&signer2)
        ///     .invoke();
        /// ```
        #[cfg(not(target_family = "wasm"))]
        pub struct AuthClient<'a> {
            inner: Client<'a>,
        }

        #[cfg(not(target_family = "wasm"))]
        impl<'a> AuthClient<'a> {
            /// Create a new AuthClient wrapping a contract at the given address.
            pub fn new(env: &'a soroban_sdk::Env, address: &'a soroban_sdk::Address) -> Self {
                Self {
                    inner: Client::new(env, address),
                }
            }

            /// Get a reference to the inner client.
            pub fn inner(&self) -> &Client<'a> {
                &self.inner
            }

            /// Get the environment reference.
            pub fn env(&self) -> &soroban_sdk::Env {
                &self.inner.env
            }

            /// Get the contract address.
            pub fn address(&self) -> &soroban_sdk::Address {
                &self.inner.address
            }

            #(#methods)*
        }
    }
}

/// Generate a wrapper method for AuthClient that returns a CallBuilder.
fn generate_auth_client_method(func: &FunctionInfo) -> proc_macro2::TokenStream {
    let fn_name = format_ident!("{}", func.name);
    let try_fn_name = format_ident!("try_{}", func.name);
    let fn_name_str = &func.name;
    let doc = &func.doc;

    // Generate parameter list with explicit lifetime
    let params = func.inputs.iter().map(|input| {
        let name = format_ident!("{}", input.name);
        let ty = generate_type_ident(&input.type_def);
        quote! { #name: &'b #ty }
    });

    // Generate argument names for the inner call (cloned for the closure)
    let arg_clones = func.inputs.iter().map(|input| {
        let name = format_ident!("{}", input.name);
        let clone_name = format_ident!("{}_clone", input.name);
        generate_clone_stmt(&name, &clone_name, &input.type_def)
    });

    // Generate a second set of clones for the try_invoker closure
    let try_arg_clones = func.inputs.iter().map(|input| {
        let name = format_ident!("{}", input.name);
        let clone_name = format_ident!("{}_try_clone", input.name);
        generate_clone_stmt(&name, &clone_name, &input.type_def)
    });

    // Generate the cloned argument names for the closure call
    let clone_names = func
        .inputs
        .iter()
        .map(|input| format_ident!("{}_clone", input.name));

    // Generate the try-cloned argument names for the try_invoker closure
    let try_clone_names = func
        .inputs
        .iter()
        .map(|input| format_ident!("{}_try_clone", input.name));

    // Generate args tuple for mock auth
    // Note: Single-element tuples need a trailing comma: (a,) not (a)
    // Collected because we need .len() and indexed access for the tuple expression
    let args_tuple: Vec<_> = func
        .inputs
        .iter()
        .map(|input| {
            let name = format_ident!("{}", input.name);
            generate_arg_expr(&name, &input.type_def)
        })
        .collect();

    // Build the args tuple expression with proper trailing comma handling
    let args_tuple_expr = if args_tuple.is_empty() {
        quote! { () }
    } else if args_tuple.len() == 1 {
        let arg = &args_tuple[0];
        quote! { (#arg,) }
    } else {
        quote! { (#(#args_tuple),*) }
    };

    // Generate return type
    // Note: The standard Client unwraps Result types - if the output is Result<T, E>,
    // the method returns T (and panics on error). We follow the same convention.
    let return_ty = if func.outputs.is_empty() {
        quote! { () }
    } else if func.outputs.len() == 1 {
        unwrap_result_type(&func.outputs[0])
    } else {
        let types = func.outputs.iter().map(unwrap_result_type);
        quote! { (#(#types),*) }
    };

    // Generate try return type matching soroban-sdk's try_ method convention
    let try_return_ty = generate_try_return_type(&func.outputs);

    quote! {
        #[doc = #doc]
        pub fn #fn_name<'b>(&'b self, #(#params),*) -> soroban_sdk_tools::auth::CallBuilder<'b, #return_ty, #try_return_ty>
        where
            'a: 'b,
        {
            use soroban_sdk::IntoVal;

            // Convert args to Vec<Val> for mock auth
            let args: soroban_sdk::Vec<soroban_sdk::Val> = #args_tuple_expr.into_val(&self.inner.env);

            // Clone args for the invoker closure
            #(#arg_clones)*

            // Clone args for the try_invoker closure
            #(#try_arg_clones)*

            // Create the invoker closure
            let inner = &self.inner;
            let invoker = __alloc::boxed::Box::new(move || {
                inner.#fn_name(#(&#clone_names),*)
            });

            // Create the try_invoker closure
            let inner = &self.inner;
            let try_invoker = __alloc::boxed::Box::new(move || {
                inner.#try_fn_name(#(&#try_clone_names),*)
            });

            soroban_sdk_tools::auth::CallBuilder::new(
                &self.inner.env,
                &self.inner.address,
                #fn_name_str,
                args,
                invoker,
                Some(try_invoker),
            )
        }
    }
}

/// Generate a clone statement for a parameter.
fn generate_clone_stmt(
    name: &proc_macro2::Ident,
    clone_name: &proc_macro2::Ident,
    type_def: &ScSpecTypeDef,
) -> proc_macro2::TokenStream {
    match type_def {
        // Copy types - just copy
        ScSpecTypeDef::Bool
        | ScSpecTypeDef::U32
        | ScSpecTypeDef::I32
        | ScSpecTypeDef::U64
        | ScSpecTypeDef::I64
        | ScSpecTypeDef::U128
        | ScSpecTypeDef::I128 => quote! { let #clone_name = *#name; },

        // Void - use unit
        ScSpecTypeDef::Void => quote! { let #clone_name = (); },

        // All other types need to be cloned
        _ => quote! { let #clone_name = #name.clone(); },
    }
}

// -----------------------------------------------------------------------------
// Error Spec Generation
// -----------------------------------------------------------------------------
fn generate_spec_entries(
    variants: &[ErrorVariantInfo],
) -> impl Iterator<Item = proc_macro2::TokenStream> + '_ {
    variants.iter().enumerate().map(|(idx, v)| {
        let code = (idx + 1) as u32;
        let name = &v.name;
        let doc = &v.doc;
        quote! {
            soroban_sdk_tools::error::ErrorSpecEntry {
                code: #code, name: #name, description: #doc,
            }
        }
    })
}

/// Generate `SpecNode` leaf tokens for a list of variants.
///
/// Uses 1-based sequential codes (matching variant position) rather than
/// native error codes, so that the XDR builder produces correct flattened
/// codes when this type is wrapped by an outer `#[scerr]` enum.
fn generate_tree_nodes(
    variants: &[ErrorVariantInfo],
) -> impl Iterator<Item = proc_macro2::TokenStream> + '_ {
    variants.iter().enumerate().map(|(idx, v)| {
        let code = (idx + 1) as u32;
        let name = &v.name;
        let doc = &v.doc;
        quote! {
            soroban_sdk_tools::error::SpecNode {
                code: #code, name: #name, description: #doc,
                children: &[],
            }
        }
    })
}

/// Generate the `__SCERR_SPEC_{TypeName}` const for an error enum.
/// This provides programmatic access to error variant info.
fn generate_spec_const(info: &ErrorEnumInfo) -> proc_macro2::TokenStream {
    let const_name = format_ident!("__SCERR_SPEC_{}", info.name);
    let entries = generate_spec_entries(&info.variants);
    let doc = format!(
        "Spec metadata for `{}` error enum. Used by scerr for spec flattening.",
        info.name
    );

    quote! {
        #[doc = #doc]
        #[allow(non_upper_case_globals)]
        pub const #const_name: &[soroban_sdk_tools::error::ErrorSpecEntry] = &[
            #(#entries),*
        ];
    }
}

/// Generate `ContractErrorSpec` implementation for imported error types.
fn generate_error_spec_impl(info: &ErrorEnumInfo) -> proc_macro2::TokenStream {
    let type_name = format_ident!("{}", info.name);
    let entries = generate_spec_entries(&info.variants);
    let tree_nodes = generate_tree_nodes(&info.variants);

    quote! {
        impl soroban_sdk_tools::error::ContractErrorSpec for #type_name {
            const SPEC_ENTRIES: &'static [soroban_sdk_tools::error::ErrorSpecEntry] = &[
                #(#entries),*
            ];
            const SPEC_TREE: &'static [soroban_sdk_tools::error::SpecNode] = &[
                #(#tree_nodes),*
            ];
        }
    }
}

/// Generate SequentialError implementation for imported error types.
///
/// Maps each variant to/from a 0-based sequential index based on its
/// position in the enum, regardless of native error codes.
fn generate_sequential_error_impl(info: &ErrorEnumInfo) -> proc_macro2::TokenStream {
    let type_name = format_ident!("{}", info.name);

    let to_seq_arms = info.variants.iter().enumerate().map(|(idx, v)| {
        let variant_ident = format_ident!("{}", v.name);
        let idx = idx as u32;
        quote! { #type_name::#variant_ident => #idx }
    });

    let from_seq_arms = info.variants.iter().enumerate().map(|(idx, v)| {
        let variant_ident = format_ident!("{}", v.name);
        let idx = idx as u32;
        quote! { #idx => Some(#type_name::#variant_ident) }
    });

    quote! {
        impl soroban_sdk_tools::error::SequentialError for #type_name {
            fn to_seq(&self) -> u32 {
                match self {
                    #(#to_seq_arms),*
                }
            }

            fn from_seq(seq: u32) -> Option<Self> {
                match seq {
                    #(#from_seq_arms,)*
                    _ => None,
                }
            }
        }
    }
}

/// Main implementation of contractimport!
pub fn contractimport_impl(attr: TokenStream) -> TokenStream {
    let args = match darling::ast::NestedMeta::parse_meta_list(attr.into()) {
        Ok(v) => v,
        Err(e) => {
            return Error::new(Span::call_site(), e.to_string())
                .into_compile_error()
                .into()
        }
    };

    let args = match ContractImportArgs::from_list(&args) {
        Ok(v) => v,
        Err(e) => return e.write_errors().into(),
    };

    // Resolve file path relative to the manifest directory
    let file_abs = abs_from_rel_to_manifest(&args.file);

    // Read WASM file
    let wasm = match fs::read(&file_abs) {
        Ok(wasm) => wasm,
        Err(e) => {
            return Error::new(
                Span::call_site(),
                format!("Failed to read WASM file '{}': {}", file_abs.display(), e),
            )
            .into_compile_error()
            .into()
        }
    };

    // Verify SHA256 if provided
    let sha256_hex = {
        let hash = Sha256::digest(&wasm);
        format!("{hash:x}")
    };

    if let Some(expected) = &args.sha256 {
        if expected != &sha256_hex {
            return Error::new(
                Span::call_site(),
                format!(
                    "SHA256 mismatch for '{}': expected {}, got {}",
                    args.file, expected, sha256_hex
                ),
            )
            .into_compile_error()
            .into();
        }
    }

    // Parse WASM spec
    let specs = match from_wasm(&wasm) {
        Ok(specs) => specs,
        Err(e) => {
            return Error::new(
                Span::call_site(),
                format!("Failed to parse WASM spec: {e:?}"),
            )
            .into_compile_error()
            .into()
        }
    };

    // Extract error enums and functions from spec
    let error_enums = extract_error_enums(&specs);
    let functions = extract_functions(&specs);

    // Generate the standard contractimport! code using soroban-spec-rust
    let standard_import =
        match soroban_spec_rust::generate_from_wasm(&wasm, &args.file, args.sha256.as_deref()) {
            Ok(code) => code,
            Err(e) => {
                return Error::new(
                    Span::call_site(),
                    format!("Failed to generate import code: {e}"),
                )
                .into_compile_error()
                .into()
            }
        };

    // Generate spec consts, ContractErrorSpec, and SequentialError impls for each error enum
    let spec_consts = error_enums.iter().map(generate_spec_const);
    let spec_impls = error_enums.iter().map(generate_error_spec_impl);
    let seq_impls = error_enums.iter().map(generate_sequential_error_impl);

    // Generate AuthClient for simplified auth testing
    let auth_client = generate_auth_client(&functions);

    let output = quote! {
        #standard_import

        // Spec metadata for error enums (programmatic access)
        #(#spec_consts)*

        // ContractErrorSpec implementations for imported error types
        #(#spec_impls)*

        // SequentialError implementations for imported error types
        #(#seq_impls)*

        // AuthClient for simplified authorization testing
        #auth_client
    };

    output.into()
}
