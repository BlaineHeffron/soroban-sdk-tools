//! Module-level processing for #[`contractstorage_module`]
//!
//! This module handles the module-level attribute macro that processes
//! multiple structs together for global key management and collision detection.

use std::collections::HashSet;

use darling::{ast::NestedMeta, FromMeta};
use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, token::Brace, Error, Item, ItemMod, ItemStruct,
    Token,
};

use super::common::{expand_struct_with_keys, StorageArgs};

/// Implementation of #[`contractstorage_module`] attribute macro
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
    let Some((_, content)) = &mut module.content else {
        return Error::new_spanned(
            &module,
            "#[contractstorage_module] requires an inline module",
        )
        .to_compile_error()
        .into();
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

    for (s, args) in structs {
        match expand_struct_with_keys(
            s,
            args.auto_shorten,
            args.symbolic,
            &mut reserved_short_names,
        ) {
            Ok(items) => rebuilt.push(items),
            Err(err) => return err.to_compile_error().into(),
        }
    }

    // Replace original structs: filter out original marked structs, then append rebuilt ones
    let new_items = rebuild_module_items(content, rebuilt);
    module.content = Some((Brace::default(), new_items));

    quote!(#module).into()
}

/// Collect all structs with #[contractstorage] annotations from the module
fn collect_contractstorage_structs(items: &[Item]) -> Vec<(ItemStruct, StorageArgs)> {
    items
        .iter()
        .filter_map(|it| match it {
            Item::Struct(s) => parse_contractstorage_attrs(&s.attrs).map(|args| (s.clone(), args)),
            _ => None,
        })
        .collect()
}

/// Parse #[contractstorage] attributes to extract settings
fn parse_contractstorage_attrs(attrs: &[syn::Attribute]) -> Option<StorageArgs> {
    let mut nested = vec![];
    let mut has_marker = false;

    for attr in attrs {
        if attr.path().is_ident("contractstorage") {
            has_marker = true;
            if let Ok(metas) =
                attr.parse_args_with(Punctuated::<NestedMeta, Token![,]>::parse_terminated)
            {
                nested.extend(metas);
            }
        }
    }

    if has_marker {
        StorageArgs::from_list(&nested).ok()
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

#[cfg(test)]
mod tests {
    use super::*;
    use syn::parse_quote;

    #[test]
    fn test_parse_contractstorage_attrs_flags() {
        let attrs: Vec<syn::Attribute> = parse_quote! {
            #[contractstorage(auto_shorten, symbolic)]
        };

        let args = parse_contractstorage_attrs(&attrs).unwrap();
        assert!(args.auto_shorten);
        assert!(args.symbolic);
    }

    #[test]
    fn test_parse_contractstorage_attrs_name_value() {
        let attrs: Vec<syn::Attribute> = parse_quote! {
            #[contractstorage(auto_shorten = true, symbolic = false)]
        };

        let args = parse_contractstorage_attrs(&attrs).unwrap();
        assert!(args.auto_shorten);
        assert!(!args.symbolic);
    }

    #[test]
    fn test_parse_contractstorage_attrs_no_args() {
        let attrs: Vec<syn::Attribute> = parse_quote! {
            #[contractstorage]
        };

        let args = parse_contractstorage_attrs(&attrs).unwrap();
        assert!(!args.auto_shorten);
        assert!(!args.symbolic);
    }

    #[test]
    fn test_parse_contractstorage_attrs_none() {
        let attrs: Vec<syn::Attribute> = parse_quote! {
            #[other]
        };

        assert!(parse_contractstorage_attrs(&attrs).is_none());
    }
}
