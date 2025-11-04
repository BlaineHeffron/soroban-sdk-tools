//! Common utilities shared between single-struct and module-level processing

use std::collections::HashSet;

use crate::util::{get_type_name, is_storage_item_type, is_storage_map_type, is_storage_type};
use heck::{ToSnakeCase, ToUpperCamelCase};
use quote::quote;
use syn::{
    punctuated::Punctuated, spanned::Spanned, Error, Fields, Ident, Item, ItemStruct, Lit, LitStr,
    Meta, MetaNameValue, Result, Type, Visibility,
};

/// Information about a field in the storage struct
pub struct FieldInfo {
    pub name: Ident,
    pub ty: Type,
    pub vis: Visibility,
    pub short_key: Option<String>,
    pub symbolic: bool,
    pub span: proc_macro2::Span,
}

/// Generate a key wrapper struct for a map type
pub fn generate_map_key_wrapper(
    wrapper_name: &Ident,
    key_ty: &Type,
    prefix_lit: &Lit,
) -> proc_macro2::TokenStream {
    quote! {
        #[derive(Clone)]
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
        #[derive(Clone, Default)]
        pub struct #wrapper_name;
        impl ::soroban_sdk_tools::key::StorageKey for #wrapper_name {
            fn to_key(&self, env: &::soroban_sdk::Env) -> ::soroban_sdk::Val {
                ::soroban_sdk::Symbol::new(env, #prefix_lit).into_val(env)
            }
        }
    }
}

/// Extract key and value types from a map type
pub fn extract_map_generics(ty: &Type) -> Result<(Type, Type, Ident)> {
    if let Type::Path(type_path) = ty {
        if let Some(segment) = type_path.path.segments.last() {
            let ident = segment.ident.clone();
            if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                if args.args.len() == 2 {
                    if let (syn::GenericArgument::Type(k), syn::GenericArgument::Type(v)) =
                        (&args.args[0], &args.args[1])
                    {
                        return Ok((k.clone(), v.clone(), ident));
                    }
                } else if args.args.len() == 3 {
                    // For when W is present
                    if let (
                        syn::GenericArgument::Type(k),
                        syn::GenericArgument::Type(v),
                        syn::GenericArgument::Type(_w),
                    ) = (&args.args[0], &args.args[1], &args.args[2])
                    {
                        return Ok((k.clone(), v.clone(), ident));
                    }
                }
            }
        }
    }
    Err(Error::new_spanned(
        ty,
        "Invalid map type: expected a type like PersistentMap<K, V>",
    ))
}

/// Combine multiple syn::Error instances into a single error
pub fn combine_errors(errors: Vec<Error>) -> Error {
    let mut iter = errors.into_iter();
    let mut combined = iter.next().expect("combine_errors called with empty vec");
    for err in iter {
        combined.combine(err);
    }
    combined
}

/// Validate that a type is one of the supported storage types
pub fn validate_storage_type(ty: &Type) -> Result<()> {
    if !is_storage_type(ty) {
        let type_name = get_type_name(ty).unwrap_or_else(|| "<unknown>".to_string());
        return Err(Error::new_spanned(
            ty,
            format!(
                "Field type '{}' is not a valid storage type.\n\
                 \n\
                 Supported types:\n\
                 - Map types: PersistentMap<K, V>, InstanceMap<K, V>, TemporaryMap<K, V>\n\
                 - Item types: PersistentItem<V>, InstanceItem<V>, TemporaryItem<V>.",
                type_name
            ),
        ));
    }

    Ok(())
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
            if let Err(e) = validate_storage_type(&ty) {
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
                // Pre-compute camel case version once
                let camel_base = base_name.to_upper_camel_case();
                let mut len: usize = 1;
                let mut found = false;
                let mut short = String::new();

                // Progressively lengthen prefix until unique, falling back to appending numbers if full name conflicts.`
                'find: loop {
                    let candidate = &camel_base[0..len.min(camel_base.len())];

                    if !reserved_short_names.contains(candidate)
                        && !reserved_camel_names.contains(candidate)
                    {
                        short = candidate.to_string();
                        found = true;
                        break;
                    }
                    len += 1;
                    if len > camel_base.len() {
                        // Full name conflicts, append counter
                        let mut i = 0;
                        loop {
                            let cand_str = format!("{}{}", camel_base, i);
                            if !reserved_short_names.contains(&cand_str)
                                && !reserved_camel_names.contains(&cand_str)
                            {
                                short = cand_str;
                                found = true;
                                break 'find;
                            }
                            i += 1;
                            if i > 99 {
                                errors.push(Error::new(field.span, "Cannot find unique short key"));
                                break 'find;
                            }
                        }
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

        if field_is_symbolic && key.len() > 32 {
            errors.push(Error::new(
                field.span,
                "Symbolic key too long (>32 characters) for Symbol",
            ));
        }

        finalized_fields.push((field.name, field.ty, field.vis, key, field_is_symbolic));
    }

    // Return combined errors if any occurred
    if !errors.is_empty() {
        return Err(combine_errors(errors));
    }

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
        .collect();

    let has_symbolic = !symbolic_fields.is_empty() || !auto_shorten;

    if has_symbolic {
        let mut per_field_items: Vec<proc_macro2::TokenStream> = vec![];

        for &(name, ty, _, short, _) in &symbolic_fields {
            let sym_key = short.to_upper_camel_case();
            let prefix_lit = Lit::Str(LitStr::new(&sym_key, name.span()));
            let wrapper_name = syn::Ident::new(&sym_key, name.span());

            if is_storage_map_type(ty) {
                let (key_ty, _, _) = match extract_map_generics(ty) {
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
            } else if is_storage_item_type(ty) {
                per_field_items.push(generate_item_key_wrapper(&wrapper_name, &prefix_lit));
            } else {
                let type_name = get_type_name(ty).unwrap_or_else(|| "<unknown>".to_string());
                errors.push(Error::new_spanned(
                    ty,
                    format!(
                        "Unsupported storage type '{}' for symbolic key.\n\
                         \n\
                         Supported types:\n\
                         - Map types: PersistentMap<K, V>, InstanceMap<K, V>, TemporaryMap<K, V>\n\
                         - Item types: PersistentItem<V>, InstanceItem<V>, TemporaryItem<V>.",
                        type_name
                    ),
                ));
            }
        }

        // Return if we had errors generating wrappers
        if !errors.is_empty() {
            return Err(combine_errors(errors));
        }
        let module: Item = syn::parse2(quote! {
                mod #module_name {
                    use super::*;
                    use ::soroban_sdk::IntoVal;
                    #(#per_field_items)*
            }
        })?;
        additional_items.push(module);
    }

    // Now generate field decls and inits for all fields
    for (name, ty, vis, short, is_sym) in finalized_fields.iter() {
        let field_name = name;
        let prefix_lit = Lit::Str(LitStr::new(short, name.span()));
        let case_name = syn::Ident::new(&short.to_upper_camel_case(), name.span());
        let wrapper_name = case_name;

        if *is_sym {
            // Symbolic (enum mode)
            if is_storage_map_type(ty) {
                let (key_ty, val_ty, map_ident) = match extract_map_generics(ty) {
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
            let return_ty = if *is_sym && is_storage_map_type(ty) {
                let (key_ty, val_ty, map_ident) = match extract_map_generics(ty) {
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
            .any(|(_, ty, _, _, is_sym)| *is_sym && is_storage_map_type(ty));
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
