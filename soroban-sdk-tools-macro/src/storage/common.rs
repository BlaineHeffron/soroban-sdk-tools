//! Common utilities shared between single-struct and module-level processing

use std::collections::HashSet;

use crate::util::TypeExt;
use darling::FromMeta;
use heck::{ToSnakeCase, ToUpperCamelCase};
use quote::quote;
use syn::{
    punctuated::Punctuated, spanned::Spanned, Error, Fields, Ident, Item, ItemStruct, Lit, LitStr,
    Meta, MetaNameValue, Result, Type, Visibility,
};

/// Information about a field in the storage struct
#[derive(Debug)]
pub struct FieldInfo {
    pub name: Ident,
    pub ty: Type,
    pub vis: Visibility,
    pub short_key: Option<String>,
    pub symbolic: bool,
    pub span: proc_macro2::Span,
}

/// Finalized field with assigned short key
/// Tuple of (name, type, visibility, assigned_key, is_symbolic)
pub type FinalizedField = (Ident, Type, Visibility, String, bool);

/// Arguments parsed from #[contractstorage(...)]
#[derive(Debug, Default, FromMeta)]
pub struct StorageArgs {
    #[darling(default)]
    pub auto_shorten: bool,
    #[darling(default)]
    pub symbolic: bool,
}

/// Generate a key wrapper struct for a map type
pub fn generate_map_key_wrapper(
    wrapper_name: &Ident,
    key_ty: &Type,
    prefix_lit: &Lit,
) -> proc_macro2::TokenStream {
    quote! {
        #[derive(Clone, Debug)]
        pub struct #wrapper_name(#key_ty);
        impl From<#key_ty> for #wrapper_name {
            fn from(k: #key_ty) -> Self { Self(k) }
        }
        impl ::soroban_sdk_tools::key::StorageKey for #wrapper_name {
            fn to_key(&self, env: &::soroban_sdk::Env) -> ::soroban_sdk::Val {
                let prefix = ::soroban_sdk::Symbol::new(env, #prefix_lit);
                let mut v: ::soroban_sdk::Vec<::soroban_sdk::Val> = ::soroban_sdk::Vec::new(env);
                v.push_back(prefix.into_val(env));
                v.push_back(self.0.into_val(env));
                v.into_val(env)
            }
        }
    }
}

/// Generate a key wrapper struct for an item type
pub fn generate_item_key_wrapper(
    wrapper_name: &Ident,
    prefix_lit: &Lit,
) -> proc_macro2::TokenStream {
    quote! {
        #[derive(Clone, Default, Debug)]
        pub struct #wrapper_name;
        impl ::soroban_sdk_tools::key::StorageKey for #wrapper_name {
            fn to_key(&self, env: &::soroban_sdk::Env) -> ::soroban_sdk::Val {
                ::soroban_sdk::Symbol::new(env, #prefix_lit).into_val(env)
            }
        }
    }
}

/// Combine multiple syn::Error instances into a single error
pub fn combine_errors(errors: Vec<Error>) -> Error {
    errors
        .into_iter()
        .reduce(|mut a, b| {
            a.combine(b);
            a
        })
        .expect("At least one error expected")
}

/// Parse fields from the struct, stripping attributes and collecting info
pub fn parse_fields(item_struct: &mut ItemStruct) -> Result<Vec<FieldInfo>> {
    // Strip the #[contractstorage(...)] marker from the struct
    item_struct
        .attrs
        .retain(|a| !a.path().is_ident("contractstorage"));

    // Collect errors instead of returning early
    let mut errors = Vec::new();

    // Process fields
    let mut field_infos: Vec<FieldInfo> = vec![];
    if let Fields::Named(fields_named) = &mut item_struct.fields {
        let mut cleaned_named = Punctuated::new();

        for mut field in fields_named.named.clone().into_iter() {
            let mut short_key: Option<String> = None;
            let mut field_symbolic = false;
            let mut cleaned_field_attrs = vec![];

            for attr in field.attrs.into_iter() {
                if attr.path().is_ident("short_key") {
                    if let Meta::NameValue(MetaNameValue {
                        value: syn::Expr::Lit(expr_lit),
                        ..
                    }) = &attr.meta
                    {
                        if let Lit::Str(lit_str) = &expr_lit.lit {
                            short_key = Some(lit_str.value());
                        }
                    }
                } else if attr.path().is_ident("symbolic") {
                    field_symbolic = true;
                } else {
                    cleaned_field_attrs.push(attr);
                }
            }
            field.attrs = cleaned_field_attrs;

            let name = match field.ident.as_ref() {
                Some(n) => n.clone(),
                None => {
                    errors.push(Error::new(field.span(), "Unnamed fields not supported"));
                    continue;
                }
            };
            let ty = field.ty.clone();
            let vis = field.vis.clone();

            // Validate that the type is a supported storage type
            if let Err(e) = ty.validate_storage_type() {
                errors.push(e);
                continue;
            }

            field_infos.push(FieldInfo {
                name,
                ty,
                vis,
                short_key,
                symbolic: field_symbolic,
                span: field.span(),
            });

            cleaned_named.push(field);
        }
        fields_named.named = cleaned_named;
    } else {
        errors.push(Error::new_spanned(
            &item_struct,
            "#[contractstorage] requires named fields",
        ));
    }

    // Return early if we have errors from field processing
    if !errors.is_empty() {
        return Err(combine_errors(errors));
    }

    Ok(field_infos)
}

/// Assign short keys to fields, handling auto-shortening and reservations
pub fn assign_short_keys(
    field_infos: Vec<FieldInfo>,
    auto_shorten: bool,
    struct_symbolic: bool,
    reserved_short_names: &mut HashSet<String>,
) -> Result<Vec<FinalizedField>> {
    let mut errors = Vec::new();
    let mut reserved_camel_names: HashSet<String> = HashSet::new();

    // Reserve explicit short keys and check duplicates (global via `reserved_short_names`)
    for field in &field_infos {
        if let Some(key) = &field.short_key {
            if reserved_short_names.contains(key) {
                errors.push(Error::new(
                    field.span,
                    format!("Duplicate explicit short key '{}'", key),
                ));
            }
            let camel = key.to_upper_camel_case();
            if reserved_camel_names.contains(&camel) {
                errors.push(Error::new(
                    field.span,
                    format!(
                        "Explicit short key '{}' leads to duplicate type name '{}' in keys module",
                        key, camel
                    ),
                ));
            }
            reserved_short_names.insert(key.clone());
            reserved_camel_names.insert(camel);
        }
    }

    // Return if we have duplicate key errors
    if !errors.is_empty() {
        return Err(combine_errors(errors));
    }

    // Assign keys
    let mut finalized_fields: Vec<(Ident, Type, Visibility, String, bool)> = vec![];
    for mut field in field_infos.into_iter() {
        let base_name = field.name.to_string();
        if field.short_key.is_none() {
            if auto_shorten {
                let camel_base = base_name.to_upper_camel_case();
                let mut len: usize = 1;
                let mut found = false;
                let mut short = String::new();
                const MAX_SUFFIX: usize = 99;

                'find: loop {
                    let candidate: String = camel_base.chars().take(len).collect();

                    if !reserved_short_names.contains(&candidate)
                        && !reserved_camel_names.contains(&candidate)
                    {
                        short = candidate;
                        found = true;
                        break;
                    }

                    len += 1;
                    if len > camel_base.chars().count() {
                        // Full name conflicts, append counter
                        for i in 0..=MAX_SUFFIX {
                            let cand_str = format!("{}{}", camel_base, i);
                            if !reserved_short_names.contains(&cand_str)
                                && !reserved_camel_names.contains(&cand_str)
                            {
                                short = cand_str;
                                found = true;
                                break 'find;
                            }
                        }
                        errors.push(Error::new(
                            field.span,
                            "Cannot find unique short key after exhausting candidates",
                        ));
                        break 'find;
                    }
                }

                if !found {
                    continue;
                }

                let key = short.clone();
                let key_camel = key.to_upper_camel_case();
                field.short_key = Some(key.clone());
                reserved_short_names.insert(key);
                reserved_camel_names.insert(key_camel);
            } else {
                let proposed = base_name;
                if reserved_short_names.contains(&proposed) {
                    errors.push(Error::new(field.span, format!("Collision with field name '{}'. Use #[short_key] or enable auto_shorten.", proposed)));
                    continue;
                }
                let proposed_camel = proposed.to_upper_camel_case();
                if reserved_camel_names.contains(&proposed_camel) {
                    errors.push(Error::new(field.span, format!("Field name '{}' leads to duplicate type name '{}' in keys module. Use #[short_key] or enable auto_shorten.", proposed, proposed_camel)));
                    continue;
                }
                let short = proposed;
                let key = short.clone();
                let key_camel = key.to_upper_camel_case();
                field.short_key = Some(key.clone());
                reserved_short_names.insert(key);
                reserved_camel_names.insert(key_camel);
            }
        }

        let key = field.short_key.as_ref().unwrap().clone();
        let field_is_symbolic = field.symbolic || struct_symbolic || !auto_shorten;

        // Validate the symbol string used.
        if field_is_symbolic {
            let sym_key = key.to_upper_camel_case();
            if sym_key.len() > 32 {
                errors.push(Error::new(
                    field.span,
                    "Symbolic key too long (>32 characters) when converted to Symbol",
                ));
            }
        }

        finalized_fields.push((field.name, field.ty, field.vis, key, field_is_symbolic));
    }

    // Return combined errors if any occurred
    if !errors.is_empty() {
        return Err(combine_errors(errors));
    }

    Ok(finalized_fields)
}

/// Generate the output items from finalized fields
pub fn generate_items(
    item_struct: &ItemStruct,
    finalized_fields: &[FinalizedField],
    auto_shorten: bool,
) -> Result<Vec<Item>> {
    let struct_name = &item_struct.ident;
    let generics = &item_struct.generics;
    let module_name = syn::Ident::new(
        &format!("{}_keys__", struct_name.to_string().to_snake_case()),
        struct_name.span(),
    );

    let mut additional_items: Vec<Item> = vec![];
    let mut rewritten_fields: Vec<proc_macro2::TokenStream> = vec![];
    let mut field_inits: Vec<proc_macro2::TokenStream> = vec![];
    let mut accessor_methods: Vec<proc_macro2::TokenStream> = vec![];

    // Collect symbolic fields
    let symbolic_fields: Vec<_> = finalized_fields
        .iter()
        .filter(|(_, _, _, _, is_sym)| *is_sym)
        .cloned()
        .collect();

    let has_symbolic = !symbolic_fields.is_empty() || !auto_shorten;

    let mut errors = vec![];

    if has_symbolic {
        let mut per_field_items: Vec<proc_macro2::TokenStream> = vec![];

        for (name, ty, _, short, _) in &symbolic_fields {
            let sym_key = short.to_upper_camel_case();
            // Validate wrapper ident to avoid panics
            if syn::parse_str::<Ident>(&sym_key).is_err() {
                errors.push(Error::new(
                    name.span(),
                    format!(
                        "Derived key wrapper name '{}' is not a valid Rust identifier",
                        sym_key
                    ),
                ));
                continue;
            }
            let prefix_lit = Lit::Str(LitStr::new(&sym_key, name.span()));
            let wrapper_name = syn::Ident::new(&sym_key, name.span());

            if ty.is_storage_map_type() {
                let (key_ty, _, _) = match ty.extract_map_generics() {
                    Ok(tuple) => tuple,
                    Err(e) => {
                        errors.push(e);
                        continue;
                    }
                };

                per_field_items.push(generate_map_key_wrapper(
                    &wrapper_name,
                    &key_ty,
                    &prefix_lit,
                ));
            } else if ty.is_storage_item_type() {
                per_field_items.push(generate_item_key_wrapper(&wrapper_name, &prefix_lit));
            } else {
                let type_name = ty
                    .get_type_name()
                    .unwrap_or_else(|| "<unknown>".to_string());
                errors.push(Error::new_spanned(
                    ty,
                    format!(
                        "Unsupported storage type '{type_name}' for symbolic key.\n\
                         \n\
                         Supported types:\n\
                         - Map types: PersistentMap<K, V>, InstanceMap<K, V>, TemporaryMap<K, V>\n\
                         - Item types: PersistentItem<V>, InstanceItem<V>, TemporaryItem<V>.",
                    ),
                ));
            }
        }

        // Return if we had errors generating wrappers
        if !errors.is_empty() {
            return Err(combine_errors(errors));
        }
        additional_items.push(syn::parse2(quote! {
                mod #module_name {
                    use super::*;
                    use ::soroban_sdk::IntoVal;
                    #(#per_field_items)*
            }
        })?)
    }

    // Now generate field decls and inits for all fields
    for (name, ty, vis, short, is_sym) in finalized_fields.iter() {
        let field_name = name;
        let prefix_lit = Lit::Str(LitStr::new(short, name.span()));
        let case_name_str = short.to_upper_camel_case();
        if syn::parse_str::<Ident>(&case_name_str).is_err() {
            errors.push(Error::new(
                name.span(),
                format!(
                    "Derived key wrapper name '{case_name_str}' is not a valid Rust identifier",
                ),
            ));
            continue;
        }
        let case_name = syn::Ident::new(&case_name_str, name.span());
        let wrapper_name = case_name;

        if *is_sym {
            // Symbolic (enum mode)
            if ty.is_storage_map_type() {
                let (key_ty, val_ty, map_ident) = match ty.extract_map_generics() {
                    Ok(tuple) => tuple,
                    Err(e) => {
                        errors.push(e);
                        continue;
                    }
                };
                rewritten_fields.push(quote! {
                    #vis #field_name: #map_ident<#key_ty, #val_ty, #module_name::#wrapper_name>
                });
                field_inits.push(quote! {
                    #field_name: <#map_ident<#key_ty, #val_ty, #module_name::#wrapper_name>>::new_raw(&env)
                });
            } else {
                rewritten_fields.push(quote! {
                    #vis #field_name: #ty
                });
                field_inits.push(quote! {
                    #field_name: <#ty>::new_raw(&env,
                        ::soroban_sdk_tools::key::StorageKey::to_key(
                            & #module_name :: #wrapper_name ::default(),
                            &env
                        )
                    )
                });
            }
        } else {
            // Hashed
            rewritten_fields.push(quote! {
                #vis #field_name: #ty
            });
            field_inits.push(quote! {
                #field_name: <#ty>::new_hashed(&env, #prefix_lit)
            });
        }

        if auto_shorten {
            let return_ty = if *is_sym && ty.is_storage_map_type() {
                let (key_ty, val_ty, map_ident) = match ty.extract_map_generics() {
                    Ok(tuple) => tuple,
                    Err(e) => {
                        errors.push(e);
                        continue;
                    }
                };
                quote! { #map_ident<#key_ty, #val_ty, #module_name::#wrapper_name> }
            } else {
                quote! { #ty }
            };
            accessor_methods.push(quote! { #vis fn #name(&self) -> &#return_ty { &self.#name } });
        }
    }

    // Final error check
    if !errors.is_empty() {
        return Err(combine_errors(errors));
    }

    // Build rebuilt_struct if needed (if has_symbolic or any maps)
    let needs_rebuilt = has_symbolic
        || finalized_fields
            .iter()
            .any(|(_, ty, _, _, is_sym)| *is_sym && ty.is_storage_map_type());
    let struct_item = if needs_rebuilt {
        syn::parse2::<Item>(quote! {
            #[derive(Clone)]
            pub struct #struct_name #generics {
                #(#rewritten_fields,)*
            }
        })?
    } else {
        Item::Struct(item_struct.clone())
    };

    let impl_new = syn::parse2(quote! {
        impl #generics #struct_name #generics {
            pub fn new(env: &::soroban_sdk::Env) -> Self {
                Self {
                    #(#field_inits,)*
                }
            }
            #(#accessor_methods)*
        }
    })?;

    let impl_default = syn::parse2(quote! {
        impl #generics ::core::default::Default for #struct_name #generics {
            fn default() -> Self { Self::new(&::soroban_sdk::Env::default()) }
        }
    })?;

    let mut items = vec![struct_item];
    items.extend(additional_items);
    items.push(Item::Impl(impl_new));
    items.push(Item::Impl(impl_default));

    Ok(items)
}

/// Expand a struct with storage keys
///
/// This function is the core transformation logic used by both single-struct
/// and module-level processing. It generates storage wrappers and key management
/// code for a struct annotated with #[contractstorage].
pub fn expand_struct_with_keys(
    mut item_struct: ItemStruct,
    auto_shorten: bool,
    struct_symbolic: bool,
    reserved_short_names: &mut HashSet<String>,
) -> Result<Vec<Item>> {
    let field_infos = parse_fields(&mut item_struct)?;
    let finalized_fields = assign_short_keys(
        field_infos,
        auto_shorten,
        struct_symbolic,
        reserved_short_names,
    )?;
    generate_items(&item_struct, &finalized_fields, auto_shorten)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;
    use syn::parse_quote;

    #[test]
    fn test_parse_fields_success() {
        let mut item_struct: ItemStruct = parse_quote! {
            #[contractstorage]
            pub struct TestStruct {
                pub field1: PersistentItem<u64>,
                #[short_key = "key2"]
                #[symbolic]
                field2: PersistentMap<Address, u64>,
            }
        };

        let fields = parse_fields(&mut item_struct).unwrap();

        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0].name.to_string(), "field1");
        assert!(fields[0].short_key.is_none());
        assert!(!fields[0].symbolic);

        assert_eq!(fields[1].name.to_string(), "field2");
        assert_eq!(fields[1].short_key, Some("key2".to_string()));
        assert!(fields[1].symbolic);

        // Check attributes are stripped
        assert!(item_struct.attrs.is_empty());
    }

    #[test]
    fn test_parse_fields_unnamed_fields() {
        let mut item_struct: ItemStruct = parse_quote! {
            #[contractstorage]
            pub struct TestStruct(u64);
        };

        let err = parse_fields(&mut item_struct).unwrap_err();
        assert!(err.to_string().contains("requires named fields"));
    }

    #[test]
    fn test_parse_fields_invalid_type() {
        let mut item_struct: ItemStruct = parse_quote! {
            #[contractstorage]
            pub struct TestStruct {
                invalid: u64,
            }
        };

        let err = parse_fields(&mut item_struct).unwrap_err();
        assert!(err.to_string().contains("not a valid storage type"));
    }

    #[test]
    fn test_assign_short_keys_auto_shorten() {
        let field_infos = vec![
            FieldInfo {
                name: parse_quote!(balance),
                ty: parse_quote!(PersistentMap<Address, u64>),
                vis: Visibility::Public(parse_quote!(pub)),
                short_key: None,
                symbolic: false,
                span: proc_macro2::Span::call_site(),
            },
            FieldInfo {
                name: parse_quote!(total),
                ty: parse_quote!(PersistentItem<u64>),
                vis: Visibility::Public(parse_quote!(pub)),
                short_key: None,
                symbolic: false,
                span: proc_macro2::Span::call_site(),
            },
        ];

        let mut reserved = HashSet::new();
        let finalized = assign_short_keys(field_infos, true, false, &mut reserved).unwrap();

        assert_eq!(finalized.len(), 2);
        assert_eq!(finalized[0].3, "B"); // Shortened from "balance"
        assert_eq!(finalized[1].3, "T"); // Shortened from "total"
        assert!(!finalized[0].4); // not symbolic
        assert!(!finalized[1].4);
    }

    #[test]
    fn test_assign_short_keys_explicit() {
        let field_infos = vec![FieldInfo {
            name: parse_quote!(balance),
            ty: parse_quote!(PersistentMap<Address, u64>),
            vis: Visibility::Public(parse_quote!(pub)),
            short_key: Some("bal".to_string()),
            symbolic: false,
            span: proc_macro2::Span::call_site(),
        }];

        let mut reserved = HashSet::new();
        let finalized = assign_short_keys(field_infos, false, false, &mut reserved).unwrap();

        assert_eq!(finalized[0].3, "bal");
    }

    #[test]
    fn test_assign_short_keys_duplicate_explicit() {
        let field_infos = vec![
            FieldInfo {
                name: parse_quote!(field1),
                ty: parse_quote!(PersistentItem<u64>),
                vis: Visibility::Public(parse_quote!(pub)),
                short_key: Some("key".to_string()),
                symbolic: false,
                span: proc_macro2::Span::call_site(),
            },
            FieldInfo {
                name: parse_quote!(field2),
                ty: parse_quote!(PersistentItem<u64>),
                vis: Visibility::Public(parse_quote!(pub)),
                short_key: Some("key".to_string()),
                symbolic: false,
                span: proc_macro2::Span::call_site(),
            },
        ];

        let mut reserved = HashSet::new();
        let err = assign_short_keys(field_infos, false, false, &mut reserved).unwrap_err();
        assert!(err.to_string().contains("Duplicate explicit short key"));
    }

    #[test]
    fn test_assign_short_keys_symbolic_too_long() {
        let field_infos = vec![FieldInfo {
            name: parse_quote!(long_field_name_that_is_too_long_for_symbol),
            ty: parse_quote!(PersistentItem<u64>),
            vis: Visibility::Public(parse_quote!(pub)),
            short_key: None,
            symbolic: true,
            span: proc_macro2::Span::call_site(),
        }];

        let mut reserved = HashSet::new();
        let err = assign_short_keys(field_infos, false, true, &mut reserved).unwrap_err();
        assert!(err.to_string().contains("Symbolic key too long"));
    }

    #[test]
    fn test_generate_items_basic() {
        let item_struct: ItemStruct = parse_quote! {
            pub struct TestStruct {
                pub field: PersistentItem<u64>,
            }
        };

        let finalized = vec![(
            parse_quote!(field),
            parse_quote!(PersistentItem<u64>),
            Visibility::Public(parse_quote!(pub)),
            "F".to_string(),
            false,
        )];

        let items = generate_items(&item_struct, &finalized, true).unwrap();

        assert_eq!(items.len(), 3); // struct, impl new, impl default
    }

    #[test]
    fn test_generate_items_with_symbolic() {
        let item_struct: ItemStruct = parse_quote! {
            pub struct TestStruct {
                pub field: PersistentMap<Address, u64>,
            }
        };

        let finalized = vec![(
            parse_quote!(field),
            parse_quote!(PersistentMap<Address, u64>),
            Visibility::Public(parse_quote!(pub)),
            "F".to_string(),
            true,
        )];

        let items = generate_items(&item_struct, &finalized, false).unwrap();

        // Should generate keys module
        assert_eq!(items.len(), 4); // struct, module, impl new, impl default
    }

    #[test]
    fn test_generate_items_invalid_map() {
        let item_struct: ItemStruct = parse_quote! {
            pub struct TestStruct {
                pub field: InvalidMap<Address, u64>,
            }
        };

        let finalized = vec![(
            parse_quote!(field),
            parse_quote!(InvalidMap<Address, u64>),
            Visibility::Public(parse_quote!(pub)),
            "F".to_string(),
            true,
        )];

        let err = generate_items(&item_struct, &finalized, false).unwrap_err();
        println!("{}", err.to_string());
        assert!(err.to_string().contains("Unsupported storage type"));
    }
}
