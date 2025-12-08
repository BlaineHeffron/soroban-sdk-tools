//! Implementation of the #[scerr] macro for generating Soroban contract error enums.
//!
//! This module provides the procedural macro logic for #[scerr] and #[scerr(root)],
//! which generate error enums with unique u32 codes, conversion traits, and composable
//! error handling.
//!
//! Architecture:
//! - 24/8 Split: Root enums use the top 8 bits (indices 1..255) to namespace sub-modules.
//!   Inner errors use the lower 24 bits.
//! - Code 0 is reserved. Hashes ensure generated codes are never 0.

use darling::{ast::NestedMeta, FromMeta};
use proc_macro::TokenStream;
use quote::quote;
use std::collections::{HashMap, HashSet};
use syn::{
    parse_macro_input, spanned::Spanned, Attribute, Data, DataEnum, DeriveInput, Error, Expr,
    ExprLit, Fields, Ident, Lit, Meta, Type, Variant,
};

use crate::util::{collect_results, combine_errors};

// -----------------------------------------------------------------------------
// Constants
// -----------------------------------------------------------------------------

const ROOT_BITS: u32 = 8;
const ROOT_MAX: u32 = (1 << ROOT_BITS) - 1;
const INNER_BITS: u32 = 24;
const INNER_MASK: u32 = (1 << INNER_BITS) - 1; // 0x00FFFFFF
// Ensure we don't return 0 from hashes, as 0 is reserved for Abort/System
const HASH_SEED: u32 = 5381;

// -----------------------------------------------------------------------------
// Parsing & Configuration
// -----------------------------------------------------------------------------

#[derive(Debug, Default, PartialEq, Copy, Clone)]
enum ScerrMode {
    #[default]
    Basic,
    Root,
}

#[derive(Debug, Default)]
struct ScerrModeDarling(ScerrMode);

impl FromMeta for ScerrModeDarling {
    fn from_word() -> darling::Result<Self> {
        Ok(Self(ScerrMode::Basic))
    }

    fn from_value(value: &Lit) -> darling::Result<Self> {
        match value {
            Lit::Str(s) => match s.value().as_str() {
                "root" => Ok(Self(ScerrMode::Root)),
                "basic" => Ok(Self(ScerrMode::Basic)),
                other => Err(darling::Error::unknown_value(other)),
            },
            _ => Err(darling::Error::unexpected_lit_type(value)),
        }
    }

    fn from_list(items: &[NestedMeta]) -> darling::Result<Self> {
        let mut mode = ScerrMode::Basic;
        for item in items {
            match item {
                NestedMeta::Meta(Meta::Path(p)) if p.is_ident("root") => {
                    mode = ScerrMode::Root;
                }
                NestedMeta::Meta(Meta::NameValue(nv)) if nv.path.is_ident("mode") => {
                    mode = match &nv.value {
                        Expr::Lit(ExprLit {
                            lit: Lit::Str(s), ..
                        }) => match s.value().as_str() {
                            "root" => ScerrMode::Root,
                            "basic" => ScerrMode::Basic,
                            other => return Err(darling::Error::unknown_value(other)),
                        },
                        _ => {
                            return Err(darling::Error::custom("mode must be a string literal")
                                .with_span(&nv.value))
                        }
                    }
                }
                other => {
                    return Err(darling::Error::custom("Unsupported #[scerr(...)] argument")
                        .with_span(other))
                }
            }
        }
        Ok(Self(mode))
    }
}

/// Parse the #[scerr(...)] attribute to determine the mode.
fn parse_scerr_attr(attr: TokenStream) -> syn::Result<ScerrMode> {
    let ts2 = proc_macro2::TokenStream::from(attr);
    if ts2.is_empty() {
        return Ok(ScerrMode::Basic);
    }
    let meta_items = darling::ast::NestedMeta::parse_meta_list(ts2)
        .map_err(|e| Error::new(proc_macro2::Span::call_site(), e.to_string()))?;
    let mode_darling = ScerrModeDarling::from_list(&meta_items)
        .map_err(|e| Error::new(e.span(), e.to_string()))?;
    Ok(mode_darling.0)
}

/// Parse the #[description(...)] attribute from a list of attributes.
fn parse_description_attr(attrs: &[Attribute]) -> Option<String> {
    for attr in attrs {
        if attr.path().is_ident("description") {
            if let Ok(meta_nv) = attr.meta.require_name_value() {
                if let Expr::Lit(ExprLit {
                    lit: Lit::Str(s), ..
                }) = &meta_nv.value
                {
                    return Some(s.value());
                }
            } else if let Ok(tokens) = attr.parse_args::<syn::LitStr>() {
                return Some(tokens.value());
            }
        }
    }
    None
}

// -----------------------------------------------------------------------------
// Variant Processing & Hashing
// -----------------------------------------------------------------------------

/// DJB2 hash function generating 24-bit codes. Ensures result is never 0.
fn hash_variant(s: &str) -> u32 {
    let mut hash = HASH_SEED;
    for c in s.bytes() {
        hash = ((hash << 5).wrapping_add(hash)).wrapping_add(c as u32);
    }
    let result = hash & INNER_MASK;
    if result == 0 {
        1
    } else {
        result
    }
}

/// The kind of variant behavior.
#[derive(Debug)]
enum VariantKind {
    /// Plain unit variant, optionally handles abort errors.
    Unit { from_abort: bool },
    /// Non-unit variant that transparently wraps another error type.
    Transparent {
        field_ty: Type,
        field_ident: Option<Ident>,
        has_from: bool,
    },
    /// Non-unit variant that handles cross-contract errors.
    FromContractClient {
        field_ty: Type,
        field_ident: Option<Ident>,
        has_from: bool,
    },
}

/// Information about a variant in the error enum.
#[derive(Debug)]
struct VariantInfo<'a> {
    ident: &'a Ident,
    code: u32,
    variant: &'a Variant,
    description: String,
    kind: VariantKind,
}

impl<'a> VariantInfo<'a> {
    /// Returns the field type if this is a non-unit variant.
    fn field_ty(&self) -> Option<&Type> {
        match &self.kind {
            VariantKind::Unit { .. } => None,
            VariantKind::Transparent { field_ty, .. }
            | VariantKind::FromContractClient { field_ty, .. } => Some(field_ty),
        }
    }

    /// Returns the field identifier if this is a named-field variant.
    fn field_ident(&self) -> Option<&Ident> {
        match &self.kind {
            VariantKind::Unit { .. } => None,
            VariantKind::Transparent { field_ident, .. }
            | VariantKind::FromContractClient { field_ident, .. } => field_ident.as_ref(),
        }
    }

    /// Returns true if this variant has a #[from] attribute on its field.
    fn has_from(&self) -> bool {
        match &self.kind {
            VariantKind::Unit { .. } => false,
            VariantKind::Transparent { has_from, .. }
            | VariantKind::FromContractClient { has_from, .. } => *has_from,
        }
    }

    /// Returns true if this is a from_contract_client variant.
    fn is_fcc(&self) -> bool {
        matches!(&self.kind, VariantKind::FromContractClient { .. })
    }

    /// Returns true if this is a unit variant that handles abort errors.
    fn is_from_abort(&self) -> bool {
        matches!(&self.kind, VariantKind::Unit { from_abort: true })
    }
}

/// Darling-based attribute parsing for variant-level attributes.
#[derive(Debug, Default, FromMeta)]
struct VariantAttrs {
    #[darling(default)]
    transparent: bool,
    #[darling(default)]
    from_contract_client: bool,
    #[darling(default)]
    from_abort_error: bool,
}

/// Parse field-level #[from] attribute.
fn has_from_attr(field: &syn::Field) -> bool {
    field.attrs.iter().any(|a| a.path().is_ident("from"))
}

/// Parse attributes for a single variant.
fn parse_variant_attrs(variant: &Variant) -> syn::Result<VariantAttrs> {
    let mut attrs = VariantAttrs::default();

    for attr in &variant.attrs {
        if attr.path().is_ident("transparent") {
            attrs.transparent = true;
        } else if attr.path().is_ident("from_contract_client") {
            attrs.from_contract_client = true;
        } else if attr.path().is_ident("from_abort_error") {
            attrs.from_abort_error = true;
        }
    }

    Ok(attrs)
}

/// Parse a single variant into `VariantInfo`.
fn parse_variant_info<'a>(variant: &'a Variant, code: u32) -> syn::Result<VariantInfo<'a>> {
    let ident = &variant.ident;
    let description = parse_description_attr(&variant.attrs).unwrap_or_else(|| ident.to_string());

    let attrs = parse_variant_attrs(variant)?;

    // Check mutual exclusivity
    if attrs.transparent && attrs.from_contract_client {
        return Err(Error::new(
            variant.span(),
            "Variant cannot have both #[transparent] and #[from_contract_client]",
        ));
    }

    let kind = match &variant.fields {
        Fields::Unit => {
            if attrs.transparent || attrs.from_contract_client {
                return Err(Error::new(
                    variant.span(),
                    "Unit variants cannot have #[transparent] or #[from_contract_client]",
                ));
            }
            VariantKind::Unit {
                from_abort: attrs.from_abort_error,
            }
        }
        Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
            if attrs.from_abort_error {
                return Err(Error::new(
                    variant.span(),
                    "#[from_abort_error] only allowed on unit variants",
                ));
            }

            let field = &fields.unnamed[0];
            let field_ty = field.ty.clone();
            let has_from = has_from_attr(field);

            if attrs.transparent {
                VariantKind::Transparent {
                    field_ty,
                    field_ident: None,
                    has_from,
                }
            } else if attrs.from_contract_client {
                VariantKind::FromContractClient {
                    field_ty,
                    field_ident: None,
                    has_from,
                }
            } else {
                return Err(Error::new(
                    variant.span(),
                    "Non-unit variants must have #[transparent] or #[from_contract_client]",
                ));
            }
        }
        Fields::Named(fields) if fields.named.len() == 1 => {
            if attrs.from_abort_error {
                return Err(Error::new(
                    variant.span(),
                    "#[from_abort_error] only allowed on unit variants",
                ));
            }

            let field = fields.named.iter().next().unwrap();
            let field_ty = field.ty.clone();
            let field_ident = field.ident.clone();
            let has_from = has_from_attr(field);

            if attrs.transparent {
                VariantKind::Transparent {
                    field_ty,
                    field_ident,
                    has_from,
                }
            } else if attrs.from_contract_client {
                VariantKind::FromContractClient {
                    field_ty,
                    field_ident,
                    has_from,
                }
            } else {
                return Err(Error::new(
                    variant.span(),
                    "Non-unit variants must have #[transparent] or #[from_contract_client]",
                ));
            }
        }
        _ => {
            return Err(Error::new(
                variant.span(),
                "Unsupported variant type: must be unit or single-field",
            ))
        }
    };

    Ok(VariantInfo {
        ident,
        code,
        variant,
        description,
        kind,
    })
}

/// Assign codes to variants and detect collisions.
fn assign_codes(data: &DataEnum, enum_ident: &Ident, mode: ScerrMode) -> syn::Result<Vec<u32>> {
    let mut next_code: u32 = match mode {
        ScerrMode::Root => 1, // Start at 1 in root mode (0 is reserved)
        ScerrMode::Basic => 0,
    };
    let mut used_codes: HashMap<u32, String> = HashMap::new();

    data.variants.iter().map(|variant| {
        let code = if let Some((_, expr)) = &variant.discriminant {
            if let Expr::Lit(ExprLit { lit: Lit::Int(li), .. }) = expr {
                li.base10_parse::<u32>()?
            } else {
                return Err(Error::new(expr.span(), "Only integer literal discriminants supported"));
            }
        } else {
            match mode {
                ScerrMode::Root => {
                    let c = next_code;
                    next_code = next_code.checked_add(1).ok_or_else(|| {
                        Error::new(variant.span(), "Too many error variants")
                    })?;
                    c
                }
                ScerrMode::Basic => {
                    let full_name = format!("{}::{}", enum_ident, variant.ident);
                    hash_variant(&full_name)
                }
            }
        };

        // Validate root mode limits
        if mode == ScerrMode::Root {
            if code == 0 {
                return Err(Error::new(
                    variant.span(),
                    "Error code 0 is reserved for system errors. Root mode variants must use codes 1-255."
                ));
            }
            if code > ROOT_MAX {
                return Err(Error::new(
                    variant.span(),
                    format!(
                        "Root mode error variant index {} exceeds maximum of {} ({}-bit limit). \
                        Consider splitting into multiple error enums or reducing variant count.",
                        code, ROOT_MAX, ROOT_BITS
                    )
                ));
            }
        }

        // Collision Check
        if let Some(existing_variant) = used_codes.get(&code) {
            let msg = format!(
                "Error code collision: Variant '{}' (code {}) conflicts with '{}'. \
                Please manually assign a unique discriminant (e.g., `{} = {}`) to resolve this.",
                variant.ident, code, existing_variant, variant.ident, code + 1
            );
            return Err(Error::new(variant.span(), msg));
        }

        used_codes.insert(code, variant.ident.to_string());
        Ok(code)
    }).collect()
}

/// Collect `VariantInfo` for all variants in the enum.
fn collect_variant_infos<'a>(
    data: &'a DataEnum,
    enum_ident: &'a Ident,
    mode: ScerrMode,
) -> syn::Result<Vec<VariantInfo<'a>>> {
    let codes = assign_codes(data, enum_ident, mode)?;
    collect_results(
        data.variants
            .iter()
            .zip(codes)
            .map(|(v, code)| parse_variant_info(v, code)),
    )
}

/// Validate all variants based on the mode.
fn validate_variants(infos: &[VariantInfo], mode: ScerrMode) -> syn::Result<()> {
    let errors: Vec<Error> = infos
        .iter()
        .filter_map(|info| match (mode, &info.kind) {
            (ScerrMode::Basic, VariantKind::Unit { .. }) => None,
            (ScerrMode::Basic, _) => Some(Error::new(
                info.variant.span(),
                "Non-unit variants not supported in basic mode. Use #[scerr(root)]",
            )),
            (ScerrMode::Root, _) => None,
        })
        .collect();

    if !errors.is_empty() {
        return Err(combine_errors(errors));
    }

    Ok(())
}

// -----------------------------------------------------------------------------
// Code Generation Helpers
// -----------------------------------------------------------------------------

/// Generate the enum definition.
fn generate_enum_def(
    name: &Ident,
    infos: &[VariantInfo],
    has_data: bool,
) -> proc_macro2::TokenStream {
    let repr = if !has_data {
        quote! { #[repr(u32)] }
    } else {
        quote! {}
    };
    let variants: Vec<_> = infos
        .iter()
        .map(|info| {
            let ident = info.ident;
            if let Some(ty) = info.field_ty() {
                if let Some(field) = info.field_ident() {
                    quote! { #ident { #field: #ty } }
                } else {
                    quote! { #ident(#ty) }
                }
            } else {
                quote! { #ident }
            }
        })
        .collect();

    quote! {
        #repr
        #[derive(Copy, Clone, Debug, Eq, PartialEq)]
        pub enum #name {
            #(#variants),*
        }
    }
}

/// Generate match arms for `into_code` method in root mode.
fn generate_into_arms(infos: &[VariantInfo]) -> Vec<proc_macro2::TokenStream> {
    let shift = INNER_BITS;
    infos.iter().map(|info| {
        let ident = info.ident;
        let root_idx = info.code;
        if let Some(field) = info.field_ident() {
            quote! { Self::#ident { #field: inner } => (#root_idx << #shift) | inner.into_code() }
        } else if info.field_ty().is_some() {
            quote! { Self::#ident(inner) => (#root_idx << #shift) | inner.into_code() }
        } else {
            quote! { Self::#ident => #root_idx }
        }
    }).collect()
}

/// Generate match arms for `from_code` unit variants in root mode.
fn generate_from_arms_unit(infos: &[VariantInfo]) -> Vec<proc_macro2::TokenStream> {
    infos
        .iter()
        .filter(|info| info.field_ty().is_none())
        .map(|info| {
            let ident = info.ident;
            let root_idx = info.code;
            quote! { #root_idx => Some(Self::#ident) }
        })
        .collect()
}

/// Generate match arms for `from_code` wrapped variants in root mode.
fn generate_from_arms_wrapped(infos: &[VariantInfo]) -> Vec<proc_macro2::TokenStream> {
    infos.iter().filter_map(|info| {
        if let Some(ty) = info.field_ty() {
            let ident = info.ident;
            let root_idx = info.code;
            let construct = if let Some(field) = info.field_ident() {
                quote! { <#ty as soroban_sdk_tools::error::ContractError>::from_code(low).map(|inner| Self::#ident { #field: inner }) }
            } else {
                quote! { <#ty as soroban_sdk_tools::error::ContractError>::from_code(low).map(Self::#ident) }
            };
            Some(quote! { #root_idx => #construct })
        } else {
            None
        }
    }).collect()
}

/// Generate match arms for `description` method.
fn generate_desc_arms(infos: &[VariantInfo]) -> Vec<proc_macro2::TokenStream> {
    infos
        .iter()
        .map(|info| {
            let ident = info.ident;
            let d = &info.description;
            if let Some(field) = info.field_ident() {
                quote! { Self::#ident { #field } => #field.description() }
            } else if info.field_ty().is_some() {
                quote! { Self::#ident(inner) => inner.description() }
            } else {
                quote! { Self::#ident => #d }
            }
        })
        .collect()
}

/// Generate the ContractError impl for root mode.
fn generate_contract_error_impl_root(
    name: &Ident,
    infos: &[VariantInfo],
) -> proc_macro2::TokenStream {
    let shift = INNER_BITS;
    let mask = INNER_MASK;
    let into_arms = generate_into_arms(infos);
    let from_arms_unit = generate_from_arms_unit(infos);
    let from_arms_wrapped = generate_from_arms_wrapped(infos);
    let desc_arms = generate_desc_arms(infos);

    quote! {
        impl soroban_sdk_tools::error::ContractError for #name {
            fn into_code(self) -> u32 {
                match self {
                    #(#into_arms,)*
                }
            }

            fn from_code(code: u32) -> Option<Self> {
                if code == 0 { return None; }

                let high = code >> #shift;
                match high {
                    0 => {
                        // Unit variants only - match by full code
                        let low = code;
                        match low {
                            #(#from_arms_unit,)*
                            _ => None,
                        }
                    },
                    _ => {
                        // Wrapped variants only - match by high bits (root index)
                        let low = code & #mask;
                        match high {
                            #(#from_arms_wrapped,)*
                            _ => None,
                        }
                    }
                }
            }

            fn description(&self) -> &'static str {
                match self {
                    #(#desc_arms),*
                }
            }
        }
    }
}

/// Generate From<T> impls for variants with #[from].
fn generate_from_trait_impls(
    name: &Ident,
    infos: &[VariantInfo],
) -> syn::Result<proc_macro2::TokenStream> {
    let mut impls = Vec::new();
    let mut seen = HashSet::new();

    for info in infos.iter().filter(|i| i.has_from()) {
        let ty = info
            .field_ty()
            .ok_or_else(|| Error::new(info.variant.span(), "#[from] on non-unit"))?;
        let ty_s = quote!(#ty).to_string();
        if seen.contains(&ty_s) {
            return Err(Error::new(
                info.variant.span(),
                format!("Duplicate From impl for {}", ty_s),
            ));
        }
        seen.insert(ty_s);

        let ident = info.ident;
        let body = if let Some(field) = info.field_ident() {
            quote! { Self::#ident { #field: inner } }
        } else {
            quote! { Self::#ident(inner) }
        };

        impls.push(quote! {
            impl From<#ty> for #name {
                fn from(inner: #ty) -> Self { #body }
            }
        });
    }
    Ok(quote! { #(#impls)* })
}

/// Generate match arms for From<InvokeError> impl.
fn generate_try_arms(infos: &[VariantInfo]) -> Vec<proc_macro2::TokenStream> {
    infos.iter().filter(|i| i.is_fcc()).map(|info| {
        let ident = info.ident;
        let ty = info.field_ty().unwrap();
        let construct = if let Some(field) = info.field_ident() {
            quote! { Self::#ident { #field: inner } }
        } else {
            quote! { Self::#ident(inner) }
        };
        quote! {
            if let Some(inner) = <#ty as soroban_sdk_tools::error::ContractError>::from_code(code) {
                return #construct;
            }
        }
    }).collect()
}

/// Generate the From<InvokeError> impl.
fn generate_invoke_error_impl(name: &Ident, infos: &[VariantInfo]) -> proc_macro2::TokenStream {
    let try_arms = generate_try_arms(infos);

    let abort_handler = if let Some(abort_var) = infos.iter().find(|i| i.is_from_abort()) {
        let ident = abort_var.ident;
        quote! { Self::#ident }
    } else {
        quote! { panic!("Cross-contract call aborted (error code 0)") }
    };

    quote! {
        impl From<soroban_sdk::InvokeError> for #name {
            fn from(e: soroban_sdk::InvokeError) -> Self {
                match e {
                    soroban_sdk::InvokeError::Contract(code) => {
                        // 1. Try exact match (root codes and unit variants)
                        if let Some(decoded) = Self::from_code(code) {
                            decoded
                        } else {
                            // 2. Try decoding as inner errors (from_contract_client only)
                            #(#try_arms)*

                            // 3. If we are here, we have a contract error code that
                            // matches nothing we know.
                            // Standard behavior: panic to be safe/loud.
                            panic!("Unknown contract error code: {}", code)
                        }
                    }
                    soroban_sdk::InvokeError::Abort => {
                        #abort_handler
                    }
                }
            }
        }
    }
}

/// Generate Soroban conversion traits.
fn generate_soroban_conversions(name: &Ident) -> proc_macro2::TokenStream {
    quote! {
        impl From<soroban_sdk::Error> for #name {
            fn from(e: soroban_sdk::Error) -> Self {
                Self::from(soroban_sdk::InvokeError::from(e))
            }
        }
        impl soroban_sdk::TryFromVal<soroban_sdk::Env, soroban_sdk::Val> for #name {
            type Error = soroban_sdk::Error;
            fn try_from_val(env: &soroban_sdk::Env, v: &soroban_sdk::Val) -> Result<Self, Self::Error> {
                let e = soroban_sdk::Error::try_from_val(env, v)?;
                Ok(Self::from(e))
            }
        }
        impl soroban_sdk::IntoVal<soroban_sdk::Env, soroban_sdk::Val> for #name {
            fn into_val(&self, env: &soroban_sdk::Env) -> soroban_sdk::Val {
                use soroban_sdk::IntoVal;
                let code: u32 = (*self).into_code();
                soroban_sdk::Error::from_contract_error(code).into_val(env)
            }
        }
        impl soroban_sdk::TryIntoVal<soroban_sdk::Env, soroban_sdk::Val> for #name {
            type Error = soroban_sdk::ConversionError;
            fn try_into_val(&self, env: &soroban_sdk::Env) -> Result<soroban_sdk::Val, Self::Error> {
                 use soroban_sdk::IntoVal;
                 Ok(self.into_val(env))
            }
        }
        impl From<soroban_sdk::ConversionError> for #name {
             fn from(_: soroban_sdk::ConversionError) -> Self {
                 panic!("Conversion error in error enum")
             }
        }
        impl From<#name> for soroban_sdk::Error {
            fn from(err: #name) -> soroban_sdk::Error {
                soroban_sdk::Error::from_contract_error(err.into_code())
            }
        }
        impl From<&#name> for soroban_sdk::Error {
            fn from(err: &#name) -> soroban_sdk::Error {
                soroban_sdk::Error::from_contract_error((*err).into_code())
            }
        }
    }
}

/// Generate impls for ?? operator support on Result<T, InvokeError>.
fn generate_double_q_impls(name: &Ident, infos: &[VariantInfo]) -> proc_macro2::TokenStream {
    let impls: Vec<_> = infos
        .iter()
        .filter(|i| i.is_fcc())
        .map(|info| {
            let ident = info.ident;
            let ty = info.field_ty().unwrap();
            let ok_construct = if let Some(field) = info.field_ident() {
                quote! { Self::#ident { #field: inner } }
            } else {
                quote! { Self::#ident(inner) }
            };
            quote! {
                impl From<Result<#ty, soroban_sdk::InvokeError>> for #name {
                    fn from(res: Result<#ty, soroban_sdk::InvokeError>) -> Self {
                        match res {
                            Ok(inner) => #ok_construct,
                            Err(e) => Self::from(e),
                        }
                    }
                }
            }
        })
        .collect();
    quote! { #(#impls)* }
}

// -----------------------------------------------------------------------------
// Expansion Logic
// -----------------------------------------------------------------------------

/// Expand the macro for root mode.
fn expand_scerr_root(
    input: &DeriveInput,
    infos: &[VariantInfo],
) -> syn::Result<proc_macro2::TokenStream> {
    let name = &input.ident;
    let has_data = infos.iter().any(|i| i.field_ty().is_some());

    let enum_def = generate_enum_def(name, infos, has_data);
    let contract_error_impl = generate_contract_error_impl_root(name, infos);
    let from_impls = generate_from_trait_impls(name, infos)?;
    let invoke_impl = generate_invoke_error_impl(name, infos);
    let soroban_impls = generate_soroban_conversions(name);
    let double_q_impls = generate_double_q_impls(name, infos);

    Ok(quote! {
        #enum_def
        #contract_error_impl
        #from_impls
        #invoke_impl
        #soroban_impls
        #double_q_impls
    })
}

/// Expand the macro for basic mode.
fn expand_scerr_basic(
    input: &DeriveInput,
    infos: &[VariantInfo],
) -> syn::Result<proc_macro2::TokenStream> {
    let name = &input.ident;
    let variants: Vec<_> = infos
        .iter()
        .map(|i| {
            let id = i.ident;
            let c = i.code;
            quote! { #id = #c }
        })
        .collect();

    let from_arms: Vec<_> = infos
        .iter()
        .map(|i| {
            let c = i.code;
            let ident = i.ident;
            quote! { #c => Some(#name::#ident) }
        })
        .collect();

    let desc_arms: Vec<_> = infos
        .iter()
        .map(|i| {
            let ident = i.ident;
            let d = &i.description;
            quote! { #name::#ident => #d }
        })
        .collect();

    Ok(quote! {
        #[soroban_sdk::contracterror]
        #[repr(u32)]
        #[derive(Copy, Clone, Debug, Eq, PartialEq)]
        pub enum #name {
            #(#variants),*
        }

        impl soroban_sdk_tools::error::ContractError for #name {
            fn into_code(self) -> u32 { self as u32 }
            fn from_code(code: u32) -> Option<Self> {
                match code {
                    #(#from_arms,)*
                    _ => None,
                }
            }
            fn description(&self) -> &'static str {
                match *self {
                    #(#desc_arms,)*
                }
            }
        }
    })
}

/// Main entry point for the #[scerr] macro.
pub fn scerr_impl(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mode = match parse_scerr_attr(attr) {
        Ok(m) => m,
        Err(e) => return e.to_compile_error().into(),
    };

    let input = parse_macro_input!(item as DeriveInput);
    let data_enum = match &input.data {
        Data::Enum(e) => e,
        _ => {
            return Error::new(input.ident.span(), "#[scerr] only supported on enums")
                .to_compile_error()
                .into()
        }
    };

    let infos = match collect_variant_infos(data_enum, &input.ident, mode) {
        Ok(i) => i,
        Err(e) => return e.to_compile_error().into(),
    };

    if let Err(e) = validate_variants(&infos, mode) {
        return e.to_compile_error().into();
    }

    let expanded = match mode {
        ScerrMode::Basic => expand_scerr_basic(&input, &infos),
        ScerrMode::Root => expand_scerr_root(&input, &infos),
    };

    match expanded {
        Ok(ts) => TokenStream::from(ts),
        Err(e) => e.to_compile_error().into(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use syn::parse_quote;

    #[test]
    fn test_hash_variant_non_zero() {
        assert_ne!(hash_variant("test"), 0);
    }

    #[test]
    fn test_parse_variant_info_unit() {
        let variant: Variant = parse_quote! { Test };
        let info = parse_variant_info(&variant, 1).unwrap();
        assert!(matches!(info.kind, VariantKind::Unit { from_abort: false }));
    }

    #[test]
    fn test_validate_variants_basic_non_unit() {
        let info = VariantInfo {
            ident: &parse_quote!(Test),
            code: 1,
            variant: &parse_quote!(Test(u32)),
            description: "test".to_string(),
            kind: VariantKind::Transparent {
                field_ty: parse_quote!(u32),
                field_ident: None,
                has_from: false,
            },
        };
        let err = validate_variants(&[info], ScerrMode::Basic).unwrap_err();
        assert!(err.to_string().contains("Non-unit variants not supported"));
    }
}
