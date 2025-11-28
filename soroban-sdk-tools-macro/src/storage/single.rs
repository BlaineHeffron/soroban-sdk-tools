//! Single-struct processing for #[contractstorage]
//!
//! This module handles the attribute macro variant that processes
//! a single struct in isolation.

use std::collections::HashSet;

use darling::{ast::NestedMeta, FromMeta};
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, punctuated::Punctuated, ItemStruct, Token};

use super::common::{expand_struct_with_keys, StorageArgs};

/// Implementation of #[contractstorage] attribute macro for single structs
///
/// This macro transforms a single struct with storage field declarations into
/// a fully-functional storage structure with automatic key generation.
///
/// # Arguments
/// * `attr` - Macro attributes (`auto_shorten`, symbolic)
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
    let item_struct = parse_macro_input!(item as ItemStruct);

    let args = if attr.is_empty() {
        StorageArgs::default()
    } else {
        let attr_stream = proc_macro2::TokenStream::from(attr);
        match syn::parse::Parser::parse2(
            |input: syn::parse::ParseStream| {
                Punctuated::<NestedMeta, Token![,]>::parse_terminated(input)
            },
            attr_stream,
        ) {
            Ok(nested) => match StorageArgs::from_list(&nested.into_iter().collect::<Vec<_>>()) {
                Ok(args) => args,
                Err(err) => return err.write_errors().into(),
            },
            Err(err) => return err.to_compile_error().into(),
        }
    };

    // Process single struct (no cross-struct aggregation here)
    match expand_struct_with_keys(
        item_struct,
        args.auto_shorten,
        args.symbolic,
        &mut HashSet::new(),
    ) {
        Ok(items) => {
            let output = quote! {
                #(#items)*
            };
            output.into()
        }
        Err(err) => err.to_compile_error().into(),
    }
}
