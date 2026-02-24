//! Common utilities shared between single-struct and module-level processing

use std::collections::HashSet;

use crate::util::{collect_results, combine_errors, TypeExt};
use darling::FromMeta;
use heck::{ToSnakeCase, ToUpperCamelCase};
use quote::{format_ident, quote};
use syn::{
    spanned::Spanned, Error, Fields, Ident, Item, ItemStruct, Lit, LitStr, Meta, MetaNameValue,
    Result, Type, Visibility,
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
#[derive(Debug, Clone)]
pub struct FinalizedField {
    pub name: Ident,
    pub ty: Type,
    pub vis: Visibility,
    pub assigned_key: String,
    pub is_symbolic: bool,
}

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

// Derive wrapper name once
fn wrapper_ident(field: &FinalizedField) -> Ident {
    let sym_key = field.assigned_key.to_upper_camel_case();
    Ident::new(&sym_key, field.name.span())
}

/// Parse a single field into `FieldInfo`, stripping attributes
fn parse_field(field: &mut syn::Field) -> Result<FieldInfo> {
    let mut short_key: Option<String> = None;
    let mut symbolic = false;

    // Process and strip storage-related attributes
    let mut retained_attrs = Vec::new();
    for attr in std::mem::take(&mut field.attrs) {
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
            symbolic = true;
        } else {
            retained_attrs.push(attr);
        }
    }
    field.attrs = retained_attrs;

    // Build FieldInfo and preserve parsed attributes
    let mut info = FieldInfo::try_from(&*field)?;
    info.short_key = short_key;
    info.symbolic = symbolic;
    Ok(info)
}

/// Parse fields from the struct, stripping attributes and collecting info
pub fn parse_fields(item_struct: &mut ItemStruct) -> Result<Vec<FieldInfo>> {
    // Strip the #[contractstorage(...)] marker from the struct
    item_struct
        .attrs
        .retain(|a| !a.path().is_ident("contractstorage"));

    if let Fields::Named(fields_named) = &mut item_struct.fields {
        collect_results(fields_named.named.iter_mut().map(parse_field))
    } else {
        Err(Error::new_spanned(
            item_struct,
            "#[contractstorage] requires named fields",
        ))
    }
}

impl TryFrom<&syn::Field> for FieldInfo {
    type Error = syn::Error;

    fn try_from(field: &syn::Field) -> Result<Self> {
        let name = field
            .ident
            .clone()
            .ok_or_else(|| Error::new(field.span(), "Unnamed fields not supported"))?;
        let ty = field.ty.clone();
        let vis = field.vis.clone();

        // Validate that the type is a supported storage type
        ty.validate_storage_type()?;

        Ok(FieldInfo {
            name,
            ty,
            vis,
            short_key: None,
            symbolic: false,
            span: field.span(),
        })
    }
}

/// Reserve explicit short keys and check for duplicates
fn reserve_explicit_keys(
    field_infos: &[FieldInfo],
    reserved_short_names: &mut HashSet<String>,
    reserved_camel_names: &mut HashSet<String>,
) -> Result<()> {
    let mut errors = Vec::new();

    for field in field_infos {
        if let Some(key) = &field.short_key {
            if reserved_short_names.contains(key) {
                errors.push(Error::new(
                    field.span,
                    format!("Duplicate explicit short key '{key}'"),
                ));
            }
            let camel = key.to_upper_camel_case();
            if reserved_camel_names.contains(&camel) {
                errors.push(Error::new(
                    field.span,
                    format!(
                        "Explicit short key '{key}' leads to duplicate type name '{camel}' in keys module"
                    ),
                ));
            }
            reserved_short_names.insert(key.clone());
            reserved_camel_names.insert(camel);
        }
    }

    if !errors.is_empty() {
        return Err(combine_errors(errors));
    }

    Ok(())
}

/// Generate a unique short key for a field
fn generate_short_key(
    base_name: &str,
    auto_shorten: bool,
    reserved_short_names: &mut HashSet<String>,
    reserved_camel_names: &mut HashSet<String>,
    span: proc_macro2::Span,
) -> Result<String> {
    const MAX_SUFFIX: usize = 99;
    if !auto_shorten {
        let proposed = base_name.to_string();
        let proposed_camel = proposed.to_upper_camel_case();
        if reserved_short_names.contains(&proposed) {
            return Err(Error::new(
                span,
                format!(
                    "Collision with field name '{proposed}'. Use #[short_key] or enable auto_shorten."
                ),
            ));
        }
        if reserved_camel_names.contains(&proposed_camel) {
            return Err(Error::new(span, format!("Field name '{proposed}' leads to duplicate type name '{proposed_camel}' in keys module. Use #[short_key] or enable auto_shorten.")));
        }
        reserved_short_names.insert(proposed.clone());
        reserved_camel_names.insert(proposed_camel);
        return Ok(proposed);
    }

    let camel_base = base_name.to_upper_camel_case();
    let mut len: usize = 1;

    loop {
        let candidate: String = camel_base.chars().take(len).collect();

        if !reserved_short_names.contains(&candidate) && !reserved_camel_names.contains(&candidate)
        {
            reserved_short_names.insert(candidate.clone());
            reserved_camel_names.insert(candidate.clone());
            return Ok(candidate);
        }

        len += 1;
        if len > camel_base.chars().count() {
            // Full name conflicts, append counter
            for i in 0..=MAX_SUFFIX {
                let cand_str = format!("{camel_base}{i}");
                if !reserved_short_names.contains(&cand_str)
                    && !reserved_camel_names.contains(&cand_str)
                {
                    reserved_short_names.insert(cand_str.clone());
                    reserved_camel_names.insert(cand_str.clone());
                    return Ok(cand_str);
                }
            }
            return Err(Error::new(
                span,
                "Cannot find unique short key after exhausting candidates",
            ));
        }
    }
}

/// Validate a symbolic key
fn validate_symbolic_key(key: &str, span: proc_macro2::Span) -> Result<()> {
    let sym_key = key.to_upper_camel_case();
    if sym_key.len() > 32 {
        return Err(Error::new(
            span,
            "Symbolic key too long (>32 characters) when converted to Symbol",
        ));
    }
    Ok(())
}

/// Assign short keys to fields, handling auto-shortening and reservations
pub fn assign_short_keys(
    field_infos: Vec<FieldInfo>,
    auto_shorten: bool,
    struct_symbolic: bool,
    reserved_short_names: &mut HashSet<String>,
) -> Result<Vec<FinalizedField>> {
    let mut reserved_camel_names: HashSet<String> = HashSet::new();
    reserve_explicit_keys(
        &field_infos,
        reserved_short_names,
        &mut reserved_camel_names,
    )?;

    collect_results(field_infos.into_iter().map(|mut info| {
        if info.short_key.is_none() {
            let base_name = info.name.to_string();
            info.short_key = Some(generate_short_key(
                &base_name,
                auto_shorten,
                reserved_short_names,
                &mut reserved_camel_names,
                info.span,
            )?);
        }

        let key = info.short_key.as_deref().unwrap();
        let is_symbolic = info.symbolic || struct_symbolic || !auto_shorten;

        if is_symbolic {
            validate_symbolic_key(key, info.span)?;
        }

        // Propagate computed symbolic mode into FinalizedField
        info.symbolic = is_symbolic;

        FinalizedField::try_from(info)
    }))
}

impl TryFrom<FieldInfo> for FinalizedField {
    type Error = syn::Error;

    fn try_from(info: FieldInfo) -> Result<Self> {
        let assigned_key = info
            .short_key
            .ok_or_else(|| Error::new(info.span, "Missing short key"))?;
        Ok(FinalizedField {
            name: info.name,
            ty: info.ty,
            vis: info.vis,
            assigned_key,
            is_symbolic: info.symbolic,
        })
    }
}

/// Generate per-field key wrapper if symbolic
fn generate_symbolic_wrapper(field: &FinalizedField) -> Result<proc_macro2::TokenStream> {
    let sym_key = field.assigned_key.to_upper_camel_case();
    let wrapper_name = Ident::new(&sym_key, field.name.span());
    let prefix_lit = Lit::Str(LitStr::new(&sym_key, field.name.span()));

    // Validate wrapper ident
    if syn::parse_str::<Ident>(&sym_key).is_err() {
        return Err(Error::new(
            field.name.span(),
            format!("Derived key wrapper name '{sym_key}' is not a valid Rust identifier"),
        ));
    }

    if field.ty.is_storage_map_type() {
        let (key_ty, _, _) = field.ty.extract_map_generics()?;
        Ok(generate_map_key_wrapper(
            &wrapper_name,
            &key_ty,
            &prefix_lit,
        ))
    } else if field.ty.is_storage_item_type() {
        Ok(generate_item_key_wrapper(&wrapper_name, &prefix_lit))
    } else {
        let type_name = field
            .ty
            .get_type_name()
            .unwrap_or_else(|| "<unknown>".to_string());
        Err(Error::new_spanned(
            &field.ty,
            format!(
                "Unsupported storage type '{type_name}' for symbolic key.\n\
                 \n\
                 Supported types:\n\
                 - Map types: PersistentMap<K, V>, InstanceMap<K, V>, TemporaryMap<K, V>\n\
                 - Item types: PersistentItem<V>, InstanceItem<V>, TemporaryItem<V>.",
            ),
        ))
    }
}

/// Generate the keys module for symbolic fields
fn generate_keys_module(symbolic_fields: &[FinalizedField], module_name: &Ident) -> Result<Item> {
    let wrappers: Vec<proc_macro2::TokenStream> = symbolic_fields
        .iter()
        .map(generate_symbolic_wrapper)
        .collect::<Result<Vec<_>>>()?;

    syn::parse2(quote! {
        mod #module_name {
            use super::*;
            use ::soroban_sdk::IntoVal;
            #(#wrappers)*
        }
    })
}

/// Generate field declaration for the rebuilt struct
fn generate_field_decl(
    field: &FinalizedField,
    module_name: &Ident,
) -> Result<proc_macro2::TokenStream> {
    let vis = &field.vis;
    let name = &field.name;
    let ty = &field.ty;

    if field.is_symbolic && ty.is_storage_map_type() {
        let (key_ty, val_ty, map_ident) = ty.extract_map_generics()?;
        let wrapper_name = wrapper_ident(field);
        Ok(quote! {
            #vis #name: #map_ident<#key_ty, #val_ty, #module_name::#wrapper_name>
        })
    } else {
        Ok(quote! {
            #vis #name: #ty
        })
    }
}

/// Generate the bare storage handle construction expression for a field.
/// Used by both `generate_field_init` (for struct construction) and
/// `generate_static_convenience_methods` (for inline one-liners).
fn generate_storage_handle_expr(
    field: &FinalizedField,
    module_name: &Ident,
) -> Result<proc_macro2::TokenStream> {
    let ty = &field.ty;
    let prefix_lit = Lit::Str(LitStr::new(&field.assigned_key, field.name.span()));

    if field.is_symbolic {
        let wrapper_name = wrapper_ident(field);

        if ty.is_storage_map_type() {
            let (key_ty, val_ty, map_ident) = ty.extract_map_generics()?;
            Ok(quote! {
                <#map_ident<#key_ty, #val_ty, #module_name::#wrapper_name>>::new_raw(&env)
            })
        } else {
            Ok(quote! {
                <#ty>::new_raw(&env,
                    ::soroban_sdk_tools::key::StorageKey::to_key(
                        & #module_name :: #wrapper_name ::default(),
                        &env
                    )
                )
            })
        }
    } else {
        Ok(quote! {
            <#ty>::new_hashed(&env, #prefix_lit)
        })
    }
}

/// Generate field initialization in the `new` method
fn generate_field_init(
    field: &FinalizedField,
    module_name: &Ident,
) -> Result<proc_macro2::TokenStream> {
    let name = &field.name;
    let expr = generate_storage_handle_expr(field, module_name)?;
    Ok(quote! { #name: #expr })
}

/// Generate accessor method if `auto_shorten` is enabled
fn generate_accessor_method(
    field: &FinalizedField,
    module_name: &Ident,
    auto_shorten: bool,
) -> Result<Option<proc_macro2::TokenStream>> {
    if !auto_shorten {
        return Ok(None);
    }

    let vis = &field.vis;
    let name = &field.name;

    let return_ty = if field.is_symbolic && field.ty.is_storage_map_type() {
        let wrapper_name = wrapper_ident(field);
        let (key_ty, val_ty, map_ident) = field.ty.extract_map_generics()?;
        quote! { #map_ident<#key_ty, #val_ty, #module_name::#wrapper_name> }
    } else {
        let ty = &field.ty;
        quote! { #ty }
    };

    Ok(Some(
        quote! { #vis fn #name(&self) -> &#return_ty { &self.#name } },
    ))
}

fn generate_key_method(
    field: &FinalizedField,
    struct_name: &Ident,
    auto_shorten: bool,
) -> Result<proc_macro2::TokenStream> {
    let field_name = &field.name;
    let method_name = format_ident!(
        "get_{}_{}_key",
        struct_name.to_string().to_snake_case(),
        field_name
    );

    let accessor = if auto_shorten {
        quote! { self.#field_name() }
    } else {
        quote! { self.#field_name } // Changed: removed the & here
    };

    if field.ty.is_storage_map_type() {
        let (key_ty, _, _) = field.ty.extract_map_generics()?;
        Ok(quote! {
            pub fn #method_name(&self, key: #key_ty) -> ::soroban_sdk::Val {
                #accessor.get_storage_key(&key)
            }
        })
    } else if field.ty.is_storage_item_type() {
        Ok(quote! {
            pub fn #method_name(&self) -> ::soroban_sdk::Val {
                #accessor.get_storage_key()
            }
        })
    } else {
        let type_name = field
            .ty
            .get_type_name()
            .unwrap_or_else(|| "<unknown>".to_string());
        Err(Error::new_spanned(
            &field.ty,
            format!("Unsupported storage type '{type_name}' for key method."),
        ))
    }
}

/// Generate static convenience methods for a single field.
/// These are always generated regardless of `auto_shorten`, since they provide
/// a different ergonomic benefit (one-liner access) than the accessor methods
/// (which are only needed with `auto_shorten` to access shortened field names).
fn generate_static_convenience_methods(
    field: &FinalizedField,
    module_name: &Ident,
) -> Result<proc_macro2::TokenStream> {
    let vis = &field.vis;
    let name = &field.name;
    let ty = &field.ty;
    let init_expr = generate_storage_handle_expr(field, module_name)?;

    let get_name = format_ident!("get_{}", name);
    let set_name = format_ident!("set_{}", name);
    let has_name = format_ident!("has_{}", name);
    let remove_name = format_ident!("remove_{}", name);
    let update_name = format_ident!("update_{}", name);
    let extend_name = format_ident!("extend_{}_ttl", name);

    if ty.is_storage_item_type() {
        let val_ty = ty.extract_item_value_type()?;
        Ok(quote! {
            #vis fn #get_name(env: &::soroban_sdk::Env) -> Option<#val_ty> {
                #init_expr.get()
            }
            #vis fn #set_name(env: &::soroban_sdk::Env, value: &#val_ty) {
                #init_expr.set(value);
            }
            #vis fn #has_name(env: &::soroban_sdk::Env) -> bool {
                #init_expr.has()
            }
            #vis fn #remove_name(env: &::soroban_sdk::Env) {
                #init_expr.remove();
            }
            #vis fn #update_name(env: &::soroban_sdk::Env, f: impl FnOnce(Option<#val_ty>) -> #val_ty) -> #val_ty {
                #init_expr.update(f)
            }
            #vis fn #extend_name(env: &::soroban_sdk::Env, threshold: u32, extend_to: u32) {
                #init_expr.extend_ttl(threshold, extend_to);
            }
        })
    } else if ty.is_storage_map_type() {
        let (key_ty, val_ty, _) = ty.extract_map_generics()?;
        Ok(quote! {
            #vis fn #get_name(env: &::soroban_sdk::Env, key: &#key_ty) -> Option<#val_ty> {
                #init_expr.get(key)
            }
            #vis fn #set_name(env: &::soroban_sdk::Env, key: &#key_ty, value: &#val_ty) {
                #init_expr.set(key, value);
            }
            #vis fn #has_name(env: &::soroban_sdk::Env, key: &#key_ty) -> bool {
                #init_expr.has(key)
            }
            #vis fn #remove_name(env: &::soroban_sdk::Env, key: &#key_ty) {
                #init_expr.remove(key);
            }
            #vis fn #update_name(env: &::soroban_sdk::Env, key: &#key_ty, f: impl FnOnce(Option<#val_ty>) -> #val_ty) -> #val_ty {
                #init_expr.update(key, f)
            }
            #vis fn #extend_name(env: &::soroban_sdk::Env, key: &#key_ty, threshold: u32, extend_to: u32) {
                #init_expr.extend_ttl(key, threshold, extend_to);
            }
        })
    } else {
        let type_name = ty
            .get_type_name()
            .unwrap_or_else(|| "<unknown>".to_string());
        Err(Error::new_spanned(
            ty,
            format!("Unsupported storage type '{type_name}' for convenience methods."),
        ))
    }
}

/// Generate the output items from finalized fields
pub fn generate_items(
    item_struct: &ItemStruct,
    finalized_fields: &[FinalizedField],
    auto_shorten: bool,
) -> Result<Vec<Item>> {
    let struct_name = &item_struct.ident;
    let generics = &item_struct.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let module_name = syn::Ident::new(
        &format!("{}_keys__", struct_name.to_string().to_snake_case()),
        struct_name.span(),
    );

    // Collect symbolic fields
    let symbolic_fields: Vec<FinalizedField> = finalized_fields
        .iter()
        .filter(|f| f.is_symbolic)
        .cloned()
        .collect();

    let has_symbolic = !symbolic_fields.is_empty();

    let mut additional_items: Vec<Item> = vec![];
    if has_symbolic {
        additional_items.push(generate_keys_module(&symbolic_fields, &module_name)?);
    }
    // Generate rewritten fields, inits, and accessors
    let rewritten_fields: Vec<_> = finalized_fields
        .iter()
        .map(|f| generate_field_decl(f, &module_name))
        .collect::<Result<Vec<_>>>()?;

    let field_inits: Vec<_> = finalized_fields
        .iter()
        .map(|f| generate_field_init(f, &module_name))
        .collect::<Result<Vec<_>>>()?;

    let accessor_methods: Vec<proc_macro2::TokenStream> = finalized_fields
        .iter()
        .map(|f| generate_accessor_method(f, &module_name, auto_shorten))
        .collect::<Result<Vec<_>>>()?
        .into_iter()
        .flatten()
        .collect();

    let key_methods: Vec<proc_macro2::TokenStream> = finalized_fields
        .iter()
        .map(|f| generate_key_method(f, struct_name, auto_shorten))
        .collect::<Result<Vec<_>>>()?;

    let convenience_methods: Vec<proc_macro2::TokenStream> = finalized_fields
        .iter()
        .map(|f| generate_static_convenience_methods(f, &module_name))
        .collect::<Result<Vec<_>>>()?;

    // Only rebuild when symbolic maps require type changes
    let needs_rebuilt = finalized_fields
        .iter()
        .any(|f| f.is_symbolic && f.ty.is_storage_map_type());

    let attrs = &item_struct.attrs;
    let vis = &item_struct.vis;

    let struct_item = if needs_rebuilt {
        syn::parse2::<Item>(quote! {
            #(#attrs)*
            #vis struct #struct_name #generics #where_clause {
                #(#rewritten_fields,)*
            }
        })?
    } else {
        Item::Struct(item_struct.clone())
    };

    let impl_new = syn::parse2(quote! {
        impl #impl_generics #struct_name #ty_generics #where_clause {
            pub fn new(env: &::soroban_sdk::Env) -> Self {
                Self {
                    #(#field_inits,)*
                }
            }
            #(#accessor_methods)*
            #(#key_methods)*
            #(#convenience_methods)*
        }
    })?;

    let impl_default = syn::parse2(quote! {
        impl #impl_generics ::core::default::Default for #struct_name #ty_generics #where_clause {
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
        assert_eq!(finalized[0].assigned_key, "B"); // Shortened from "balance"
        assert_eq!(finalized[1].assigned_key, "T"); // Shortened from "total"
        assert!(!finalized[0].is_symbolic);
        assert!(!finalized[1].is_symbolic);
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

        assert_eq!(finalized[0].assigned_key, "bal");
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

        let finalized = vec![FinalizedField {
            name: parse_quote!(field),
            ty: parse_quote!(PersistentItem<u64>),
            vis: Visibility::Public(parse_quote!(pub)),
            assigned_key: "F".to_string(),
            is_symbolic: false,
        }];

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

        let finalized = vec![FinalizedField {
            name: parse_quote!(field),
            ty: parse_quote!(PersistentMap<Address, u64>),
            vis: Visibility::Public(parse_quote!(pub)),
            assigned_key: "F".to_string(),
            is_symbolic: true,
        }];

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

        let finalized = vec![FinalizedField {
            name: parse_quote!(field),
            ty: parse_quote!(InvalidMap<Address, u64>),
            vis: Visibility::Public(parse_quote!(pub)),
            assigned_key: "F".to_string(),
            is_symbolic: true,
        }];

        let err = generate_items(&item_struct, &finalized, false).unwrap_err();
        assert!(err.to_string().contains("Unsupported storage type"));
    }
}
