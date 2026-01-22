//! Implementation of the contractimport_with_errors! macro.
//!
//! This macro wraps soroban_sdk::contractimport! and additionally generates
//! spec metadata constants that enable scerr to flatten inner error types
//! into the outer contract's spec.

use darling::FromMeta;
use proc_macro::TokenStream;
use proc_macro2::{Literal, Span};
use quote::{format_ident, quote};
use sha2::{Digest, Sha256};
use soroban_spec::read::from_wasm;
use std::fs;
use stellar_xdr::curr::{
    Limits, ScSpecEntry, ScSpecUdtErrorEnumCaseV0, ScSpecUdtErrorEnumV0, StringM, WriteXdr,
};
use syn::Error;

use crate::util::abs_from_rel_to_manifest;

/// Default XDR read/write limits (same as soroban-sdk)
const DEFAULT_XDR_RW_LIMITS: Limits = Limits {
    depth: 500,
    len: 0x1000000,
};

/// Hash seed for namespace computation (must match scerr's HASH_SEED)
const NAMESPACE_HASH_SEED: u32 = 5381;

/// Number of bits for namespace (top bits of u32 error code)
/// Using 10 bits gives 1024 possible namespaces, reducing collision risk
const NAMESPACE_BITS: u32 = 10;
const NAMESPACE_MAX: u32 = (1 << NAMESPACE_BITS) - 1; // 1023

/// Number of bits for inner error codes (lower bits)
const INNER_BITS: u32 = 32 - NAMESPACE_BITS; // 22 bits

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
    /// The variant name that will be used in scerr's #[from_contract_client].
    /// When provided, generates a fully flattened error spec with combined codes.
    #[darling(default)]
    scerr_variant: Option<String>,
    /// The outer error enum name. When provided along with scerr_variant,
    /// the flattened spec will use this name so entries merge with the outer spec.
    #[darling(default)]
    outer_error: Option<String>,
    /// Generate a getter macro that can be used with scerr's mixins configuration.
    /// The macro will be named as specified and export prefixed variants with combined codes.
    /// Example: `getter_macro = "__get_math_variants"` generates `__get_math_variants!`
    #[darling(default)]
    getter_macro: Option<String>,
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
        .filter_map(|entry| {
            if let ScSpecEntry::UdtErrorEnumV0(e) = entry {
                Some(parse_error_enum(e))
            } else {
                None
            }
        })
        .collect()
}

fn parse_error_enum(spec: &ScSpecUdtErrorEnumV0) -> ErrorEnumInfo {
    let name = spec.name.to_utf8_string_lossy();
    let variants = spec
        .cases
        .iter()
        .map(|c| ErrorVariantInfo {
            name: c.name.to_utf8_string_lossy(),
            code: c.value,
            doc: c.doc.to_utf8_string_lossy(),
        })
        .collect();

    ErrorEnumInfo { name, variants }
}

/// Generate the __SCERR_SPEC_{TypeName} const for an error enum.
fn generate_spec_const(info: &ErrorEnumInfo) -> proc_macro2::TokenStream {
    let const_name = format_ident!("__SCERR_SPEC_{}", info.name);
    let entries: Vec<_> = info
        .variants
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
        .collect();

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
    let entries: Vec<_> = info
        .variants
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
        .collect();

    quote! {
        impl soroban_sdk_tools::error::ContractErrorSpec for #type_name {
            const SPEC_ENTRIES: &'static [soroban_sdk_tools::error::ErrorSpecEntry] = &[
                #(#entries),*
            ];
        }
    }
}

/// Generate an auto-flattened spec static using the error type name.
/// This creates a spec entry where variant names are prefixed with the type name
/// and codes include the namespace computed from the type name.
///
/// The generated enum is named `{TypeName}_Flattened` (e.g., `MathError_Flattened`).
fn generate_auto_flattened_spec_static(info: &ErrorEnumInfo) -> proc_macro2::TokenStream {
    let type_name = &info.name;
    let namespace = compute_namespace(type_name);
    let static_name = format_ident!("__SPEC_XDR_AUTO_FLATTENED_{}", type_name.to_uppercase());

    // Build flattened cases with combined codes
    let cases: Vec<ScSpecUdtErrorEnumCaseV0> = info
        .variants
        .iter()
        .map(|v| {
            let combined_code = (namespace << INNER_BITS) | v.code;
            let flattened_name = format!("{}_{}", type_name, v.name);
            ScSpecUdtErrorEnumCaseV0 {
                doc: StringM::try_from(v.doc.as_str()).unwrap_or_default(),
                name: StringM::try_from(flattened_name.as_str()).unwrap_or_default(),
                value: combined_code,
            }
        })
        .collect();

    // Name the enum {TypeName}_Flattened
    let flattened_enum_name = format!("{}_Flattened", type_name);

    let doc = format!(
        "Auto-flattened {} error variants (namespace {}). \
         Use with outer error enum: {{...OuterError, ...{}_Flattened}}",
        type_name, namespace, type_name
    );

    let spec_entry = ScSpecEntry::UdtErrorEnumV0(ScSpecUdtErrorEnumV0 {
        doc: StringM::try_from(doc).unwrap_or_default(),
        lib: StringM::default(),
        name: StringM::try_from(flattened_enum_name.as_str()).unwrap_or_default(),
        cases: cases.try_into().unwrap_or_default(),
    });

    let spec_xdr = match spec_entry.to_xdr(DEFAULT_XDR_RW_LIMITS) {
        Ok(xdr) => xdr,
        Err(_) => return quote! {},
    };

    let spec_xdr_lit = Literal::byte_string(&spec_xdr);
    let spec_xdr_len = spec_xdr.len();

    quote! {
        #[cfg_attr(target_family = "wasm", link_section = "contractspecv0")]
        #[allow(non_upper_case_globals)]
        pub static #static_name: [u8; #spec_xdr_len] = *#spec_xdr_lit;
    }
}

/// Generate a flattened spec static with combined error codes.
/// This creates a spec entry where variant names are prefixed and codes include the namespace.
///
/// If `outer_error` is provided, the spec enum will use that name (allowing it to merge
/// with the outer error spec in TypeScript bindings). Otherwise, it uses `{variant}_Flattened`.
fn generate_flattened_spec_static(
    info: &ErrorEnumInfo,
    variant_name: &str,
    outer_error: Option<&str>,
) -> proc_macro2::TokenStream {
    let namespace = compute_namespace(variant_name);
    let static_name = format_ident!("__SPEC_XDR_FLATTENED_{}", variant_name.to_uppercase());

    // Build flattened cases with combined codes
    let cases: Vec<ScSpecUdtErrorEnumCaseV0> = info
        .variants
        .iter()
        .map(|v| {
            let combined_code = (namespace << INNER_BITS) | v.code;
            let flattened_name = format!("{}_{}", variant_name, v.name);
            ScSpecUdtErrorEnumCaseV0 {
                doc: StringM::try_from(v.doc.as_str()).unwrap_or_default(),
                name: StringM::try_from(flattened_name.as_str()).unwrap_or_default(),
                value: combined_code,
            }
        })
        .collect();

    // Use {outer_error}_{variant} if outer_error is provided, otherwise use {variant}_Flattened
    // This creates a clear naming relationship (e.g., ImportTestError_Math) that TypeScript
    // users can easily combine with the main error enum via spread.
    let flattened_enum_name = outer_error
        .map(|s| format!("{}_{}", s, variant_name))
        .unwrap_or_else(|| format!("{}_Flattened", variant_name));

    let doc = format!(
        "Flattened {} error variants (namespace {}). Combine with main error enum: {{...{}, ...{}}}",
        variant_name,
        namespace,
        outer_error.unwrap_or("ErrorEnum"),
        flattened_enum_name
    );

    let spec_entry = ScSpecEntry::UdtErrorEnumV0(ScSpecUdtErrorEnumV0 {
        doc: StringM::try_from(doc).unwrap_or_default(),
        lib: StringM::default(),
        name: StringM::try_from(flattened_enum_name.as_str()).unwrap_or_default(),
        cases: cases.try_into().unwrap_or_default(),
    });

    let spec_xdr = match spec_entry.to_xdr(DEFAULT_XDR_RW_LIMITS) {
        Ok(xdr) => xdr,
        Err(_) => return quote! {},
    };

    let spec_xdr_lit = Literal::byte_string(&spec_xdr);
    let spec_xdr_len = spec_xdr.len();

    quote! {
        #[cfg_attr(target_family = "wasm", link_section = "contractspecv0")]
        #[allow(non_upper_case_globals)]
        pub static #static_name: [u8; #spec_xdr_len] = *#spec_xdr_lit;
    }
}

/// Generate a getter macro for use with scerr's mixins configuration.
///
/// This directly generates a `macro_rules!` definition that can be used by
/// `__build_unified_spec!` to collect all flattened variants.
///
/// The generated macro accepts continuation state and appends its variants
/// to the accumulator before calling `__build_unified_spec!` to continue.
fn generate_getter_macro(
    info: &ErrorEnumInfo,
    variant_name: &str,
    getter_name: &str,
) -> proc_macro2::TokenStream {
    let namespace = compute_namespace(variant_name);
    let getter_ident = format_ident!("{}", getter_name);

    // Build prefixed variants with combined codes
    let variants: Vec<_> = info
        .variants
        .iter()
        .map(|v| {
            let combined_code = (namespace << INNER_BITS) | v.code;
            let prefixed_name = format_ident!("{}_{}", variant_name, v.name);
            quote! { #prefixed_name = #combined_code, }
        })
        .collect();

    // Use proc_macro2 to generate the dollar sign token
    let dollar = proc_macro2::Punct::new('$', proc_macro2::Spacing::Alone);

    quote! {
        /// Generated error variant getter macro for scerr mixins.
        ///
        /// This macro accepts continuation state from `__build_unified_spec!` and
        /// appends its flattened variants before continuing the chain.
        #[doc(hidden)]
        #[macro_export]
        macro_rules! #getter_ident {
            (
                @name #dollar enum_name:ident
                @acc [ #dollar ( #dollar acc:tt)* ]
                @unit [ #dollar ( #dollar unit:tt)* ]
                @pending [ #dollar ( #dollar rest:path),* ]
            ) => {
                soroban_sdk_tools::__build_unified_spec! {
                    @name #dollar enum_name
                    @acc [ #dollar ( #dollar acc)* #(#variants)* ]
                    @unit [ #dollar ( #dollar unit)* ]
                    @pending [ #dollar ( #dollar rest),* ]
                }
            };
        }

        // Re-export at crate root for visibility
        #[doc(hidden)]
        pub use #getter_ident;
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

/// Generate a spec static that gets linked into the WASM.
/// This ensures the inner error spec is available for TypeScript binding generation.
fn generate_spec_static(info: &ErrorEnumInfo) -> proc_macro2::TokenStream {
    let type_name = &info.name;
    let static_name = format_ident!("__SPEC_XDR_IMPORTED_{}", type_name.to_uppercase());

    // Build the spec entry
    let cases: Vec<ScSpecUdtErrorEnumCaseV0> = info
        .variants
        .iter()
        .map(|v| ScSpecUdtErrorEnumCaseV0 {
            doc: StringM::try_from(v.doc.as_str()).unwrap_or_default(),
            name: StringM::try_from(v.name.as_str()).unwrap_or_default(),
            value: v.code,
        })
        .collect();

    let spec_entry = ScSpecEntry::UdtErrorEnumV0(ScSpecUdtErrorEnumV0 {
        doc: StringM::try_from(format!("Imported error enum: {}", type_name)).unwrap_or_default(),
        lib: StringM::default(),
        name: StringM::try_from(type_name.as_str()).unwrap_or_default(),
        cases: cases.try_into().unwrap_or_default(),
    });

    let spec_xdr = match spec_entry.to_xdr(DEFAULT_XDR_RW_LIMITS) {
        Ok(xdr) => xdr,
        Err(_) => return quote! {},
    };

    let spec_xdr_lit = Literal::byte_string(&spec_xdr);
    let spec_xdr_len = spec_xdr.len();

    quote! {
        #[cfg_attr(target_family = "wasm", link_section = "contractspecv0")]
        #[allow(non_upper_case_globals)]
        pub static #static_name: [u8; #spec_xdr_len] = *#spec_xdr_lit;
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
    let spec_consts: Vec<_> = error_enums.iter().map(generate_spec_const).collect();
    let spec_impls: Vec<_> = error_enums.iter().map(generate_error_spec_impl).collect();
    let spec_statics: Vec<_> = error_enums.iter().map(generate_spec_static).collect();

    // Always auto-generate getter macros for all error enums
    // These are named __scerr_{TypeName}_variants and use the type name for namespace
    let auto_getter_macros: Vec<_> = error_enums.iter().map(generate_auto_getter_macro).collect();

    // Always auto-generate flattened spec statics for all error enums
    // These are named {TypeName}_Flattened and use the type name for namespace
    // This ensures TypeScript bindings include the combined error codes
    let auto_flattened_specs: Vec<_> = error_enums
        .iter()
        .map(generate_auto_flattened_spec_static)
        .collect();

    // Generate flattened spec if scerr_variant is provided (legacy support)
    let flattened_spec = args.scerr_variant.as_ref().and_then(|variant_name| {
        error_enums.first().map(|info| {
            generate_flattened_spec_static(info, variant_name, args.outer_error.as_deref())
        })
    });

    // Generate custom getter macro if getter_macro is provided (legacy support)
    // Requires scerr_variant to determine the prefix for variant names
    let custom_getter_macro = args
        .getter_macro
        .as_ref()
        .and_then(|getter_name| {
            args.scerr_variant.as_ref().and_then(|variant_name| {
                error_enums
                    .first()
                    .map(|info| generate_getter_macro(info, variant_name, getter_name))
            })
        })
        .or_else(|| {
            // If getter_macro is specified but scerr_variant is not, emit error
            if args.getter_macro.is_some() && args.scerr_variant.is_none() {
                Some(
                    Error::new(
                        Span::call_site(),
                        "getter_macro requires scerr_variant to be specified",
                    )
                    .into_compile_error(),
                )
            } else {
                None
            }
        });

    // Generate wasm path const for each error enum
    // This allows scerr to find the wasm file without explicit specification
    let wasm_path_str = args.file.clone();
    let wasm_path_consts: Vec<_> = error_enums
        .iter()
        .map(|info| {
            let type_name = &info.name;
            let const_name = format_ident!("__SCERR_WASM_PATH_{}", type_name);
            quote! {
                /// WASM file path for this imported contract.
                /// Used by scerr to find error variant info.
                #[doc(hidden)]
                pub const #const_name: &str = #wasm_path_str;
            }
        })
        .collect();

    let output = quote! {
        #standard_import

        // WASM path consts for scerr to use
        #(#wasm_path_consts)*

        // Spec metadata for error enums
        #(#spec_consts)*

        // ContractErrorSpec implementations for imported error types
        #(#spec_impls)*

        // Spec statics to embed inner error specs in outer WASM
        #(#spec_statics)*

        // Auto-generated getter macros for all error enums
        // Named __scerr_{TypeName}_variants, used automatically by scerr
        #(#auto_getter_macros)*

        // Auto-generated flattened spec statics for all error enums
        // Named {TypeName}_Flattened, includes combined codes for TypeScript bindings
        #(#auto_flattened_specs)*

        // Flattened spec with combined codes (when scerr_variant is specified - legacy)
        #flattened_spec

        // Custom getter macro (when getter_macro is specified - legacy)
        #custom_getter_macro
    };

    output.into()
}
