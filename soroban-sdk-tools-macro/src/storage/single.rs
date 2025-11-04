//! Single-struct processing for #[contractstorage]
//!
//! This module handles the attribute macro variant that processes
//! a single struct in isolation.

use std::collections::HashSet;

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, Error, Expr, ItemStruct, Lit, Meta, MetaNameValue,
    Token,
};

use super::common::expand_struct_with_keys;

/// Implementation of #[contractstorage] attribute macro for single structs
///
/// This macro transforms a single struct with storage field declarations into
/// a fully-functional storage structure with automatic key generation.
///
/// # Arguments
/// * `attr` - Macro attributes (auto_shorten, symbolic)
/// * `item` - The struct to process
///
/// # Example
/// ```ignore
/// #[contractstorage(auto_shorten)]
/// pub struct MyStorage {
///     pub balance: PersistentMap<Address, i128>,
///     pub total: PersistentItem<i128>,
/// }
/// ```
pub fn contractstorage_attr_impl(attr: TokenStream, item: TokenStream) -> TokenStream {
    // Parse attributes more idiomatically
    let mut auto_shorten = false;
    let mut symbolic = false;

    if !attr.is_empty() {
        let parser = Punctuated::<Meta, Token![,]>::parse_terminated;
        match syn::parse::Parser::parse(parser, attr.clone()) {
            Ok(args) => {
                for arg in args {
                    match arg {
                        Meta::NameValue(MetaNameValue { path, value, .. }) => {
                            if path.is_ident("auto_shorten") {
                                if let Expr::Lit(expr_lit) = value {
                                    if let Lit::Bool(b) = expr_lit.lit {
                                        auto_shorten = b.value;
                                    }
                                }
                            } else if path.is_ident("symbolic") {
                                if let Expr::Lit(expr_lit) = value {
                                    if let Lit::Bool(b) = expr_lit.lit {
                                        symbolic = b.value;
                                    }
                                }
                            }
                        }
                        Meta::Path(path) => {
                            // Handle flag-style attributes like #[contractstorage(auto_shorten)]
                            if path.is_ident("auto_shorten") {
                                auto_shorten = true;
                            } else if path.is_ident("symbolic") {
                                symbolic = true;
                            }
                        }
                        _ => {} // Ignore other meta types
                    }
                }
            }
            Err(err) => {
                return Error::new_spanned(
                    proc_macro2::TokenStream::from(attr),
                    format!("Failed to parse attribute arguments: {}", err),
                )
                .to_compile_error()
                .into();
            }
        }
    }

    let item_struct = parse_macro_input!(item as ItemStruct);

    // Process single struct (no cross-struct aggregation here)
    match expand_struct_with_keys(item_struct, auto_shorten, symbolic, &mut HashSet::new()) {
        Ok(items) => {
            let output = quote! {
                #(#items)*
            };
            output.into()
        }
        Err(err) => err.to_compile_error().into(),
    }
}
