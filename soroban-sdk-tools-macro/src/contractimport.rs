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

    // Generate flattened spec if scerr_variant is provided
    let flattened_spec = args.scerr_variant.as_ref().and_then(|variant_name| {
        error_enums.first().map(|info| {
            generate_flattened_spec_static(info, variant_name, args.outer_error.as_deref())
        })
    });

    let output = quote! {
        #standard_import

        // Spec metadata for error enums
        #(#spec_consts)*

        // ContractErrorSpec implementations for imported error types
        #(#spec_impls)*

        // Spec statics to embed inner error specs in outer WASM
        #(#spec_statics)*

        // Flattened spec with combined codes (when scerr_variant is specified)
        #flattened_spec
    };

    output.into()
}
