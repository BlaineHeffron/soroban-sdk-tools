//! Implementation of the contractimport! macro.
//!
//! This macro wraps soroban_sdk::contractimport! and additionally generates
//! `ContractError` and `ContractErrorSpec` implementations for imported error types,
//! enabling them to be used as inner types in `#[scerr]` root enums with
//! `#[transparent]` or `#[from_contract_client]` attributes.

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

/// Generate `ErrorSpecEntry` tokens for a list of variants.
fn generate_spec_entries(variants: &[ErrorVariantInfo]) -> Vec<proc_macro2::TokenStream> {
    variants
        .iter()
        .map(|v| {
            let code = v.code;
            let name = &v.name;
            let doc = &v.doc;
            quote! {
                soroban_sdk_tools::error::ErrorSpecEntry {
                    code: #code, name: #name, description: #doc,
                }
            }
        })
        .collect()
}

/// Generate `SpecNode` leaf tokens for a list of variants.
fn generate_tree_nodes(variants: &[ErrorVariantInfo]) -> Vec<proc_macro2::TokenStream> {
    variants
        .iter()
        .map(|v| {
            let code = v.code;
            let name = &v.name;
            let doc = &v.doc;
            quote! {
                soroban_sdk_tools::error::SpecNode {
                    code: #code, name: #name, description: #doc,
                    children: &[],
                }
            }
        })
        .collect()
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

/// Generate ContractError implementation for imported error types.
///
/// Imported types are `#[contracterror]` `repr(u32)` enums, so:
/// - `into_code`: `self as u32`
/// - `from_code`: uses `TryFrom<InvokeError>` (provided by soroban-sdk's contracterror)
/// - `description`: matches each variant to its doc string from the WASM spec
fn generate_contract_error_impl(info: &ErrorEnumInfo) -> proc_macro2::TokenStream {
    let type_name = format_ident!("{}", info.name);

    let desc_arms: Vec<_> = info
        .variants
        .iter()
        .map(|v| {
            let variant_ident = format_ident!("{}", v.name);
            let doc = if v.doc.is_empty() {
                v.name.clone()
            } else {
                v.doc.clone()
            };
            quote! { #type_name::#variant_ident => #doc }
        })
        .collect();

    quote! {
        impl soroban_sdk_tools::error::ContractError for #type_name {
            fn into_code(self) -> u32 {
                self as u32
            }

            fn from_code(code: u32) -> Option<Self> {
                <Self as core::convert::TryFrom<soroban_sdk::InvokeError>>::try_from(
                    soroban_sdk::InvokeError::Contract(code)
                ).ok()
            }

            fn description(&self) -> &'static str {
                match self {
                    #(#desc_arms),*
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

    // Generate spec consts, ContractErrorSpec, and ContractError impls for each error enum
    let spec_consts: Vec<_> = error_enums.iter().map(generate_spec_const).collect();
    let spec_impls: Vec<_> = error_enums.iter().map(generate_error_spec_impl).collect();
    let error_impls: Vec<_> = error_enums.iter().map(generate_contract_error_impl).collect();

    let output = quote! {
        #standard_import

        // Spec metadata for error enums (programmatic access)
        #(#spec_consts)*

        // ContractErrorSpec implementations for imported error types
        #(#spec_impls)*

        // ContractError implementations for imported error types
        #(#error_impls)*
    };

    output.into()
}
