//! Implementation of the contractimport_with_errors! macro.
//!
//! This macro wraps soroban_sdk::contractimport! and additionally generates
//! getter macros that enable scerr to flatten inner error types into the outer
//! contract's unified error spec.

use darling::FromMeta;
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote};
use sha2::{Digest, Sha256};
use soroban_spec::read::from_wasm;
use std::fs;
use stellar_xdr::curr::ScSpecEntry;
use syn::Error;

use crate::util::abs_from_rel_to_manifest;

/// Hash seed for namespace computation (must match scerr's HASH_SEED)
const NAMESPACE_HASH_SEED: u32 = 5381;

/// Number of bits for namespace (top bits of u32 error code)
/// Using 22 bits gives ~4 million possible namespaces, reducing collision risk
const NAMESPACE_BITS: u32 = 22;
const NAMESPACE_MAX: u32 = (1 << NAMESPACE_BITS) - 1; // 4194303

/// Number of bits for inner error codes (lower bits)
const INNER_BITS: u32 = 32 - NAMESPACE_BITS; // 10 bits = 1024 inner codes per namespace

/// Compute namespace from variant name using DJB2 hash.
/// Must match scerr's namespace computation exactly.
fn compute_namespace(variant_name: &str) -> u32 {
    let mut hash = NAMESPACE_HASH_SEED;
    for c in variant_name.bytes() {
        hash = ((hash << 5).wrapping_add(hash)).wrapping_add(c as u32);
    }
    // Use bits from the hash, ensure non-zero (1-1023)
    (hash % NAMESPACE_MAX) + 1
}

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
    code: u32,
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
                        code: c.value,
                        doc: c.doc.to_utf8_string_lossy(),
                    })
                    .collect(),
            }),
            _ => None,
        })
        .collect()
}

/// Generate ErrorSpecEntry tokens for a list of variants.
fn generate_spec_entries(variants: &[ErrorVariantInfo]) -> Vec<proc_macro2::TokenStream> {
    variants
        .iter()
        .map(|v| {
            let code = v.code;
            let name = &v.name;
            let doc = &v.doc;
            quote! {
                soroban_sdk_tools::error::ErrorSpecEntry {
                    code: #code,
                    name: #name,
                    description: #doc,
                }
            }
        })
        .collect()
}

/// Generate the __SCERR_SPEC_{TypeName} const for an error enum.
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

/// Generate ContractErrorSpec implementation for imported error types.
fn generate_error_spec_impl(info: &ErrorEnumInfo) -> proc_macro2::TokenStream {
    let type_name = format_ident!("{}", info.name);
    let entries = generate_spec_entries(&info.variants);

    quote! {
        impl soroban_sdk_tools::error::ContractErrorSpec for #type_name {
            const SPEC_ENTRIES: &'static [soroban_sdk_tools::error::ErrorSpecEntry] = &[
                #(#entries),*
            ];
        }
    }
}

/// Generate an auto-named getter macro based on the error TYPE name.
///
/// This is automatically called for each error enum in the WASM spec.
/// The getter macro is named `__scerr_{TypeName}_variants` (e.g., `__scerr_MathError_variants`).
/// The namespace is computed from the type name (e.g., hash("MathError")).
/// Flattened variant names use the type name as prefix (e.g., `MathError_DivisionByZero`).
fn generate_auto_getter_macro(info: &ErrorEnumInfo) -> proc_macro2::TokenStream {
    let type_name = &info.name; // e.g., "MathError"
    let namespace = compute_namespace(type_name);
    let getter_ident = format_ident!("__scerr_{}_variants", type_name);

    // Build prefixed variants with combined codes and doc comments
    // Format: { name: Ident, code: u32, doc: "..." }
    let variants: Vec<_> = info
        .variants
        .iter()
        .map(|v| {
            let combined_code = (namespace << INNER_BITS) | v.code;
            let prefixed_name = format_ident!("{}_{}", type_name, v.name);
            let doc = &v.doc;
            quote! { { name: #prefixed_name, code: #combined_code, doc: #doc } }
        })
        .collect();

    // Use proc_macro2 to generate the dollar sign token
    let dollar = proc_macro2::Punct::new('$', proc_macro2::Spacing::Alone);

    quote! {
        /// Auto-generated error variant getter macro for scerr.
        ///
        /// This macro is automatically used by scerr when it sees a
        /// `#[from_contract_client]` variant wrapping this error type.
        /// Macro name: `__scerr_{TypeName}_variants`
        /// Namespace computed from: "{TypeName}"
        #[doc(hidden)]
        #[macro_export]
        macro_rules! #getter_ident {
            (
                @name #dollar enum_name:ident
                @acc [ #dollar ( #dollar acc:tt ),* ]
                @unit [ #dollar ( #dollar unit:tt ),* ]
                @pending [ #dollar ( #dollar rest:path ),* ]
            ) => {
                soroban_sdk_tools::__build_unified_spec! {
                    @name #dollar enum_name
                    @acc [ #dollar ( #dollar acc ,)* #(#variants),* ]
                    @unit [ #dollar ( #dollar unit ),* ]
                    @pending [ #dollar ( #dollar rest ),* ]
                }
            };
        }

        // Re-export at crate root for visibility
        #[doc(hidden)]
        pub use #getter_ident;
    }
}

/// Main implementation of contractimport_with_errors!
pub fn contractimport_with_errors_impl(attr: TokenStream) -> TokenStream {
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
        format!("{:x}", hash)
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
                format!("Failed to parse WASM spec: {:?}", e),
            )
            .into_compile_error()
            .into()
        }
    };

    // Extract error enums
    let error_enums = extract_error_enums(&specs);

    // Generate the standard contractimport! code using soroban-spec-rust
    let standard_import =
        match soroban_spec_rust::generate_from_wasm(&wasm, &args.file, args.sha256.as_deref()) {
            Ok(code) => code,
            Err(e) => {
                return Error::new(
                    Span::call_site(),
                    format!("Failed to generate import code: {}", e),
                )
                .into_compile_error()
                .into()
            }
        };

    // Generate spec consts and ContractErrorSpec impls for each error enum
    // These provide programmatic access to error variant info
    let spec_consts: Vec<_> = error_enums.iter().map(generate_spec_const).collect();
    let spec_impls: Vec<_> = error_enums.iter().map(generate_error_spec_impl).collect();

    // Generate getter macros for all error enums
    // These are named __scerr_{TypeName}_variants and used automatically by scerr
    // to build the unified flattened error spec
    let auto_getter_macros: Vec<_> = error_enums.iter().map(generate_auto_getter_macro).collect();

    let output = quote! {
        #standard_import

        // Spec metadata for error enums (programmatic access)
        #(#spec_consts)*

        // ContractErrorSpec implementations for imported error types
        #(#spec_impls)*

        // Getter macros for scerr's unified spec generation
        // Named __scerr_{TypeName}_variants, used automatically by scerr
        #(#auto_getter_macros)*
    };

    output.into()
}
