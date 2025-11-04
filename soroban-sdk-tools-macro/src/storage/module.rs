//! Module-level processing for #[contractstorage_module]
//!
//! This module handles the module-level attribute macro that processes
//! multiple structs together for global key management and collision detection.

use std::collections::HashSet;

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, Error, Expr, Item, ItemMod, ItemStruct, Lit, Meta,
    MetaNameValue, Token,
};

use super::common::expand_struct_with_keys;

/// Implementation of #[contractstorage_module] attribute macro
///
/// This macro processes an inline module containing multiple structs annotated
/// with #[contractstorage]. It enables global key management across all structs
/// to prevent collisions and optimize key space usage.
///
/// # Arguments
/// * `_attr` - Currently unused, reserved for future module-level options
/// * `item` - The inline module to process
///
/// # Example
/// ```ignore
/// #[contractstorage_module]
/// mod storage {
///     #[contractstorage(auto_shorten)]
///     pub struct BalanceStorage {
///         pub balances: PersistentMap<Address, i128>,
///     }
///     
///     #[contractstorage(auto_shorten)]
///     pub struct TokenStorage {
///         pub total_supply: PersistentItem<i128>,
///     }
/// }
/// ```
pub fn contractstorage_module_impl(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut module = parse_macro_input!(item as ItemMod);

    // Only works on inline modules (mod name { ... })
    let content = match &mut module.content {
        Some((_, items)) => items,
        None => {
            return Error::new_spanned(
                &module,
                "#[contractstorage_module] requires an inline module",
            )
            .to_compile_error()
            .into();
        }
    };

    // Collect all #[contractstorage(...)] structs and their settings
    let structs = collect_contractstorage_structs(content);

    // If none found, just return the module
    if structs.is_empty() {
        return quote!(#module).into();
    }

    // Rebuild structs with global key minimization within this module
    let mut reserved_short_names: HashSet<String> = HashSet::new();
    let mut rebuilt: Vec<Vec<Item>> = vec![];

    for (s, auto, sym) in structs.into_iter() {
        match expand_struct_with_keys(s, auto, sym, &mut reserved_short_names) {
            Ok(items) => rebuilt.push(items),
            Err(err) => return err.to_compile_error().into(),
        }
    }

    // Replace original structs: filter out original marked structs, then append rebuilt ones
    let new_items = rebuild_module_items(content, rebuilt);
    module.content = Some((Default::default(), new_items));

    quote!(#module).into()
}

/// Collect all structs with #[contractstorage] annotations from the module
fn collect_contractstorage_structs(items: &[Item]) -> Vec<(ItemStruct, bool, bool)> {
    let mut structs: Vec<(ItemStruct, bool, bool)> = vec![];

    for it in items.iter() {
        if let Item::Struct(s) = it {
            if let Some((auto, sym)) = parse_contractstorage_attrs(&s.attrs) {
                structs.push((s.clone(), auto, sym));
            }
        }
    }

    structs
}

/// Parse #[contractstorage] attributes to extract settings
fn parse_contractstorage_attrs(attrs: &[syn::Attribute]) -> Option<(bool, bool)> {
    let mut auto = false;
    let mut sym = false;
    let mut has_marker = false;

    for attr in attrs {
        if attr.path().is_ident("contractstorage") {
            has_marker = true;

            match &attr.meta {
                Meta::Path(_) => {
                    // No arguments, use defaults
                }
                Meta::List(meta_list) => {
                    // Parse arguments within the list
                    if let Ok(args) =
                        meta_list.parse_args_with(Punctuated::<Meta, Token![,]>::parse_terminated)
                    {
                        for arg in args {
                            match arg {
                                Meta::NameValue(MetaNameValue { path, value, .. }) => {
                                    if path.is_ident("auto_shorten") {
                                        if let Expr::Lit(expr_lit) = value {
                                            if let Lit::Bool(b) = expr_lit.lit {
                                                auto = b.value;
                                            }
                                        }
                                    } else if path.is_ident("symbolic") {
                                        if let Expr::Lit(expr_lit) = value {
                                            if let Lit::Bool(b) = expr_lit.lit {
                                                sym = b.value;
                                            }
                                        }
                                    }
                                }
                                Meta::Path(path) => {
                                    // Handle flag-style attributes
                                    if path.is_ident("auto_shorten") {
                                        auto = true;
                                    } else if path.is_ident("symbolic") {
                                        sym = true;
                                    }
                                }
                                _ => {} // Ignore other meta types
                            }
                        }
                    }
                }
                Meta::NameValue(_) => {
                    // Unexpected format, skip
                }
            }
        }
    }

    if has_marker {
        Some((auto, sym))
    } else {
        None
    }
}

/// Rebuild module items by removing original marked structs and adding rebuilt ones
fn rebuild_module_items(content: &mut Vec<Item>, rebuilt: Vec<Vec<Item>>) -> Vec<Item> {
    let mut new_items: Vec<Item> = vec![];

    // Keep all items except the original marked structs
    for it in content.drain(..) {
        match &it {
            Item::Struct(s) if s.attrs.iter().any(|a| a.path().is_ident("contractstorage")) => {
                // skip original marked struct
            }
            _ => new_items.push(it),
        }
    }

    // Add all rebuilt items
    for group in rebuilt {
        for item in group {
            new_items.push(item);
        }
    }

    new_items
}
