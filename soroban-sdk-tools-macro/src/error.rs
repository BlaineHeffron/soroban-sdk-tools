//! Implementation of the #[scerr] macro for generating Soroban contract error enums.
//!
//! This module provides the procedural macro logic for #[scerr] and #[scerr(root)],
//! which generate error enums with unique u32 codes, conversion traits, and composable
//! error handling.
//!
//! ## Usage
//!
//! ### Basic Mode
//!
//! ```rust,ignore
//! use soroban_sdk_tools::scerr;
//!
//! #[scerr]
//! pub enum TokenError {
//!     #[description = "insufficient balance for transfer"]
//!     InsufficientBalance,
//!     Unauthorized,
//!     InvalidAmount,
//! }
//! ```
//!
//! ### Root Mode
//!
//! ```rust,ignore
//! #[scerr(mode="root")]
//! pub enum AppError {
//!     Unauthorized,
//!     #[transparent]
//!     Math(#[from] MathError),
//!     #[from_contract_client]
//!     ExternalMath(MathError),
//!     #[abort]
//!     Aborted,
//!     #[sentinel]
//!     Unknown,
//! }
//! ```
//!
//! ### Options
//!
//! - `handle_abort = "auto" | "panic"`: Auto-add Aborted variant or panic on abort.
//! - `handle_unknown = "auto" | "panic"`: Auto-add UnknownError variant or panic on unknown codes.
//! - `log_unknown_errors = true`: Log unknown error codes (requires sentinel).
//!
//! Architecture:
//! - 24/8 Split: Root enums use the top 8 bits (indices 1..255) to namespace sub-modules.
//!   Inner errors use the lower 24 bits.
//! - Code 0 is reserved. Hashes ensure generated codes are never 0.

use darling::FromMeta;
use proc_macro::TokenStream;
use quote::quote;
use std::collections::{HashMap, HashSet};
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, Attribute, Data, DataEnum, DeriveInput,
    Error, Expr, ExprLit, Fields, Ident, Lit, Type, Variant,
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

#[derive(Debug, Default, FromMeta)]
struct ScerrConfig {
    #[darling(default)]
    mode: Option<String>,

    #[darling(default)]
    handle_abort: Option<String>,

    #[darling(default)]
    handle_unknown: Option<String>,

    #[darling(default)]
    log_unknown_errors: bool,
}

/// Parse the #[scerr(...)] attribute to determine the config.
fn parse_scerr_attr(attr: TokenStream) -> syn::Result<(ScerrMode, ScerrConfig)> {
    let ts2 = proc_macro2::TokenStream::from(attr);
    if ts2.is_empty() {
        return Ok((ScerrMode::Basic, ScerrConfig::default()));
    }
    let meta_items = darling::ast::NestedMeta::parse_meta_list(ts2)
        .map_err(|e| Error::new(proc_macro2::Span::call_site(), e.to_string()))?;
    let config =
        ScerrConfig::from_list(&meta_items).map_err(|e| Error::new(e.span(), e.to_string()))?;
    let mode = match config.mode.as_deref() {
        None | Some("basic") => ScerrMode::Basic,
        Some("root") => ScerrMode::Root,
        Some(other) => {
            return Err(Error::new(
                proc_macro2::Span::call_site(),
                format!("unknown mode: {}", other),
            ))
        }
    };
    Ok((mode, config))
}

/// Parse the #[description(...)] attribute from a list of attributes.
fn parse_description_attr(attrs: &[Attribute]) -> Option<String> {
    attrs
        .iter()
        .find(|attr| attr.path().is_ident("description"))
        .and_then(|attr| {
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
            None
        })
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
///
/// # Variant Types
///
/// - `PlainUnit`: Simple unit variant for contract-specific errors
/// - `AbortUnit`: Unit variant marked with `#[abort]`, catches InvokeError::Abort
/// - `Sentinel`: Variant marked with `#[sentinel]`, catches unknown error codes
///   - Can optionally store the unknown code: `Sentinel { stores_code: true }`
/// - `Transparent`: Wraps another error type for in-process propagation via `?`
/// - `FromContractClient`: Wraps another error type for cross-contract error decoding
#[derive(Debug)]
enum VariantKind {
    PlainUnit,
    AbortUnit,
    Sentinel {
        stores_code: bool,
    },
    Transparent {
        field_ty: Type,
        field_ident: Option<Ident>,
        has_from: bool,
    },
    FromContractClient {
        field_ty: Type,
        field_ident: Option<Ident>,
        has_from: bool,
    },
}

/// Information about a variant in the error enum.
#[derive(Debug)]
struct VariantInfo {
    ident: Ident,
    code: u32,
    description: String,
    kind: VariantKind,
}

impl VariantInfo {
    fn is_from_abort(&self) -> bool {
        matches!(&self.kind, VariantKind::AbortUnit)
    }

    fn is_sentinel(&self) -> bool {
        matches!(&self.kind, VariantKind::Sentinel { .. })
    }

    fn stores_code(&self) -> bool {
        matches!(&self.kind, VariantKind::Sentinel { stores_code: true })
    }

    /// Returns the field type if this is a non-unit variant.
    fn field_ty(&self) -> Option<Type> {
        match &self.kind {
            VariantKind::Transparent { field_ty, .. }
            | VariantKind::FromContractClient { field_ty, .. } => Some(field_ty.clone()),
            VariantKind::Sentinel { stores_code: true } => Some(parse_quote!(u32)),
            _ => None,
        }
    }

    /// Returns the field identifier if this is a named-field variant.
    fn field_ident(&self) -> Option<Ident> {
        match &self.kind {
            VariantKind::Transparent { field_ident, .. }
            | VariantKind::FromContractClient { field_ident, .. } => field_ident.clone(),
            _ => None,
        }
    }

    /// Returns true if this variant has a #[from] attribute on its field.
    fn has_from(&self) -> bool {
        match &self.kind {
            VariantKind::Transparent { has_from, .. }
            | VariantKind::FromContractClient { has_from, .. } => *has_from,
            _ => false,
        }
    }

    /// Returns true if this is a from_contract_client variant.
    fn is_fcc(&self) -> bool {
        matches!(&self.kind, VariantKind::FromContractClient { .. })
    }
}

#[derive(Debug, Default, FromMeta)]
struct VariantAttrs {
    #[darling(default)]
    transparent: bool,
    #[darling(default)]
    from_contract_client: bool,
    #[darling(default)]
    abort: bool,
    #[darling(default)]
    sentinel: bool,
}

/// Parse field-level #[from] attribute.
fn has_from_attr(field: &syn::Field) -> bool {
    field.attrs.iter().any(|a| a.path().is_ident("from"))
}

/// Parse attributes for a single variant into VariantAttrs.
fn parse_variant_attrs(variant: &Variant) -> syn::Result<VariantAttrs> {
    let mut attrs = VariantAttrs::default();

    for attr in &variant.attrs {
        if attr.path().is_ident("transparent") {
            attrs.transparent = true;
        } else if attr.path().is_ident("from_contract_client") {
            attrs.from_contract_client = true;
        } else if attr.path().is_ident("abort") {
            attrs.abort = true;
        } else if attr.path().is_ident("sentinel") {
            attrs.sentinel = true;
        }
    }

    Ok(attrs)
}

/// Validate that variant attributes are mutually exclusive.
fn validate_variant_attrs(attrs: &VariantAttrs, variant: &Variant) -> syn::Result<()> {
    let count = [
        attrs.transparent,
        attrs.from_contract_client,
        attrs.abort,
        attrs.sentinel,
    ]
    .iter()
    .filter(|&&b| b)
    .count();

    if count > 1 {
        Err(Error::new(
            variant.span(),
            "Variant attributes are mutually exclusive",
        ))
    } else {
        Ok(())
    }
}

/// Determine the VariantKind for unit variants.
fn determine_unit_kind(attrs: &VariantAttrs) -> VariantKind {
    if attrs.abort {
        VariantKind::AbortUnit
    } else if attrs.sentinel {
        VariantKind::Sentinel { stores_code: false }
    } else {
        VariantKind::PlainUnit
    }
}

/// Determine the VariantKind for single-field variants.
fn determine_single_field_kind(
    attrs: &VariantAttrs,
    field: &syn::Field,
    variant: &Variant,
) -> syn::Result<VariantKind> {
    if attrs.abort || attrs.sentinel {
        return Err(Error::new(
            variant.span(),
            "#[abort] or #[sentinel] with fields - see rules",
        ));
    }

    let field_ty = field.ty.clone();
    let field_ident = field.ident.clone();
    let has_from = has_from_attr(field);

    if attrs.transparent {
        Ok(VariantKind::Transparent {
            field_ty,
            field_ident,
            has_from,
        })
    } else if attrs.from_contract_client {
        Ok(VariantKind::FromContractClient {
            field_ty,
            field_ident,
            has_from,
        })
    } else {
        Err(Error::new(
            variant.span(),
            "Non-unit variants must have #[transparent] or #[from_contract_client]",
        ))
    }
}

/// Determine the VariantKind for sentinel variants with fields.
fn determine_sentinel_with_field_kind(
    fields: &Fields,
    variant: &Variant,
) -> syn::Result<VariantKind> {
    if let Fields::Unnamed(fields_unnamed) = fields {
        if fields_unnamed.unnamed.len() == 1 {
            let field = &fields_unnamed.unnamed[0];
            if quote!(#(&field.ty)).to_string() == "u32" {
                return Ok(VariantKind::Sentinel { stores_code: true });
            } else {
                return Err(Error::new(
                    field.span(),
                    "#[sentinel] field must be u32 for storing code",
                ));
            }
        }
    }
    Err(Error::new(
        variant.span(),
        "#[sentinel] must be unit or unnamed (u32)",
    ))
}

/// Determine the VariantKind based on fields and attributes.
fn determine_variant_kind(variant: &Variant, attrs: &VariantAttrs) -> syn::Result<VariantKind> {
    match &variant.fields {
        Fields::Unit => {
            if attrs.transparent || attrs.from_contract_client {
                return Err(Error::new(
                    variant.span(),
                    "Unit variants cannot have #[transparent] or #[from_contract_client]",
                ));
            }
            Ok(determine_unit_kind(attrs))
        }
        Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
            let field = &fields.unnamed[0];
            determine_single_field_kind(attrs, field, variant)
        }
        Fields::Named(fields) if fields.named.len() == 1 => {
            let field = fields.named.iter().next().unwrap();
            determine_single_field_kind(attrs, field, variant)
        }
        _ if attrs.sentinel => determine_sentinel_with_field_kind(&variant.fields, variant),
        _ => Err(Error::new(
            variant.span(),
            "Unsupported variant type: must be unit or single-field",
        )),
    }
}

/// Parse a single variant into `VariantInfo`.
fn parse_variant_info(variant: &Variant, code: u32) -> syn::Result<VariantInfo> {
    let ident = variant.ident.clone();
    let description = parse_description_attr(&variant.attrs).unwrap_or_else(|| ident.to_string());

    let attrs = parse_variant_attrs(variant)?;
    validate_variant_attrs(&attrs, variant)?;

    let kind = determine_variant_kind(variant, &attrs)?;

    Ok(VariantInfo {
        ident,
        code,
        description,
        kind,
    })
}

/// Generate a code for a variant in root mode (sequential).
fn generate_root_code(next_code: &mut u32, variant: &Variant) -> syn::Result<u32> {
    let code = *next_code;
    *next_code = next_code
        .checked_add(1)
        .ok_or_else(|| Error::new(variant.span(), "Too many error variants"))?;
    Ok(code)
}

/// Generate a code for a variant in basic mode (hashed).
fn generate_basic_code(enum_ident: &Ident, variant: &Variant) -> u32 {
    let full_name = format!("{}::{}", enum_ident, variant.ident);
    hash_variant(&full_name)
}

/// Parse an explicit discriminant if present.
fn parse_explicit_discriminant(variant: &Variant) -> syn::Result<Option<u32>> {
    if let Some((_, expr)) = &variant.discriminant {
        if let Expr::Lit(ExprLit {
            lit: Lit::Int(li), ..
        }) = expr
        {
            li.base10_parse::<u32>().map(Some)
        } else {
            Err(Error::new(
                expr.span(),
                "Only integer literal discriminants supported",
            ))
        }
    } else {
        Ok(None)
    }
}

/// Validate a code in root mode.
fn validate_root_code(code: u32, variant: &Variant) -> syn::Result<()> {
    if code == 0 {
        return Err(Error::new(
            variant.span(),
            "Error code 0 is reserved for system errors. Root mode variants must use codes 1-255.",
        ));
    }
    if code > ROOT_MAX {
        return Err(Error::new(
            variant.span(),
            format!(
                "Root mode error variant index {} exceeds maximum of {} ({}-bit limit). \
                Consider splitting into multiple error enums or reducing variant count.",
                code, ROOT_MAX, ROOT_BITS
            ),
        ));
    }
    Ok(())
}

/// Check for code collisions and track used codes.
fn check_code_collision(
    code: u32,
    variant: &Variant,
    used_codes: &mut HashMap<u32, String>,
) -> syn::Result<()> {
    if let Some(existing_variant) = used_codes.get(&code) {
        let msg = format!(
            "Error code collision: Variant '{}' (code {}) conflicts with '{}'. \
            Please manually assign a unique discriminant (e.g., `{} = {}`) to resolve this.",
            variant.ident,
            code,
            existing_variant,
            variant.ident,
            code + 1
        );
        return Err(Error::new(variant.span(), msg));
    }
    used_codes.insert(code, variant.ident.to_string());
    Ok(())
}

/// Assign codes to variants and detect collisions.
fn assign_codes(data: &DataEnum, enum_ident: &Ident, mode: ScerrMode) -> syn::Result<Vec<u32>> {
    let mut next_code: u32 = match mode {
        ScerrMode::Root => 1, // Start at 1 in root mode (0 is reserved)
        ScerrMode::Basic => 0,
    };
    let mut used_codes: HashMap<u32, String> = HashMap::new();

    data.variants
        .iter()
        .map(|variant| {
            let code = match parse_explicit_discriminant(variant)? {
                Some(c) => c,
                None => match mode {
                    ScerrMode::Root => generate_root_code(&mut next_code, variant)?,
                    ScerrMode::Basic => generate_basic_code(enum_ident, variant),
                },
            };

            if mode == ScerrMode::Root {
                validate_root_code(code, variant)?;
            }

            check_code_collision(code, variant, &mut used_codes)?;

            Ok(code)
        })
        .collect()
}

/// Collect `VariantInfo` for all variants in the enum.
fn collect_variant_infos(
    data: &DataEnum,
    enum_ident: &Ident,
    mode: ScerrMode,
) -> syn::Result<Vec<VariantInfo>> {
    let codes = assign_codes(data, enum_ident, mode)?;
    collect_results(
        data.variants
            .iter()
            .zip(codes)
            .map(|(v, code)| parse_variant_info(v, code)),
    )
}

/// Validate variants unique to root mode (at most one abort and one sentinel).
fn validate_root_variants(infos: &[VariantInfo]) -> syn::Result<()> {
    let abort_count = infos.iter().filter(|i| i.is_from_abort()).count();
    if abort_count > 1 {
        return Err(Error::new(
            proc_macro2::Span::call_site(),
            "Only one #[abort] variant allowed",
        ));
    }
    let sentinel_count = infos.iter().filter(|i| i.is_sentinel()).count();
    if sentinel_count > 1 {
        return Err(Error::new(
            proc_macro2::Span::call_site(),
            "Only one #[sentinel] variant allowed",
        ));
    }
    Ok(())
}

/// Validate all variants based on the mode.
fn validate_variants(infos: &[VariantInfo], mode: ScerrMode) -> syn::Result<()> {
    let errors: Vec<Error> = infos
        .iter()
        .filter_map(|info| match (mode, &info.kind) {
            (ScerrMode::Basic, VariantKind::PlainUnit) => None,
            (ScerrMode::Basic, _) => Some(Error::new(
                info.ident.span(),
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
            let ident = &info.ident;
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
    infos
        .iter()
        .map(|info| {
            let ident = &info.ident;
            let root_idx = info.code;
            let shift = INNER_BITS;
            if info.is_sentinel() {
                if info.stores_code() {
                    if let Some(field) = info.field_ident() {
                        quote! { Self::#ident { #field: _ } => #root_idx }
                    } else {
                        quote! { Self::#ident(_) => #root_idx }
                    }
                } else {
                    quote! { Self::#ident => #root_idx }
                }
            } else if info.field_ty().is_some() {
                if let Some(field) = info.field_ident() {
                    quote! { Self::#ident { #field: inner } => (#root_idx << #shift) | inner.into_code() }
                } else {
                    quote! { Self::#ident(inner) => (#root_idx << #shift) | inner.into_code() }
                }
            } else {
                quote! { Self::#ident => #root_idx }
            }
        })
        .collect()
}

/// Generate match arms for `from_code` unit variants in root mode.
fn generate_from_arms_unit(infos: &[VariantInfo]) -> Vec<proc_macro2::TokenStream> {
    infos
        .iter()
        .filter(|info| info.field_ty().is_none() || info.stores_code())
        .map(|info| {
            let ident = &info.ident;
            let root_idx = info.code;
            if info.stores_code() {
                if let Some(field) = info.field_ident() {
                    quote! { #root_idx => Some(Self::#ident { #field: 0 }) }
                } else {
                    quote! { #root_idx => Some(Self::#ident(0)) }
                }
            } else {
                quote! { #root_idx => Some(Self::#ident) }
            }
        })
        .collect()
}

/// Generate match arms for `from_code` wrapped variants in root mode.
fn generate_from_arms_wrapped(infos: &[VariantInfo]) -> Vec<proc_macro2::TokenStream> {
    infos
        .iter()
        .filter_map(|info| {
            info.field_ty().and_then(|ty| {
                if info.is_sentinel() {
                    return None;
                }
                let ident = &info.ident;
                let root_idx = info.code;
                let construct = if let Some(field) = info.field_ident() {
                    quote! { <#ty as soroban_sdk_tools::error::ContractError>::from_code(low).map(|inner| Self::#ident { #field: inner }) }
                } else {
                    quote! { <#ty as soroban_sdk_tools::error::ContractError>::from_code(low).map(Self::#ident) }
                };
                Some(quote! { #root_idx => #construct })
            })
        })
        .collect()
}

/// Generate match arms for `description` method.
fn generate_desc_arms(infos: &[VariantInfo]) -> Vec<proc_macro2::TokenStream> {
    infos
        .iter()
        .map(|info| {
            let ident = &info.ident;
            let d = &info.description;

            // Sentinel variants use their own description, even if they store a code
            if info.is_sentinel() {
                if info.stores_code() {
                    if let Some(field) = info.field_ident() {
                        quote! { Self::#ident { #field: _ } => #d }
                    } else {
                        quote! { Self::#ident(_) => #d }
                    }
                } else {
                    quote! { Self::#ident => #d }
                }
            } else if let Some(field) = info.field_ident() {
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
            .ok_or_else(|| Error::new(info.ident.span(), "#[from] on non-unit"))?;
        let ty_s = quote!(#ty).to_string();
        if seen.contains(&ty_s) {
            return Err(Error::new(
                info.ident.span(),
                format!("Duplicate From impl for {}", ty_s),
            ));
        }
        seen.insert(ty_s);

        let ident = &info.ident;
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
    infos
        .iter()
        .filter(|i| i.is_fcc())
        .map(|info| {
            let ident = &info.ident;
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
        })
        .collect()
}

/// Generate the abort handler for From<InvokeError>.
fn generate_abort_handler(infos: &[VariantInfo]) -> proc_macro2::TokenStream {
    if let Some(info) = infos.iter().find(|i| i.is_from_abort()) {
        let ident = &info.ident;
        quote! { Self::#ident }
    } else {
        quote! { panic!("Cross-contract call aborted (error code 0)") }
    }
}

/// Generate the unknown handler for From<InvokeError>.
fn generate_unknown_handler(infos: &[VariantInfo]) -> proc_macro2::TokenStream {
    if let Some(info) = infos.iter().find(|i| i.is_sentinel()) {
        let ident = &info.ident;
        if info.stores_code() {
            if let Some(field) = info.field_ident() {
                quote! { Self::#ident { #field: code } }
            } else {
                quote! { Self::#ident(code) }
            }
        } else {
            quote! { Self::#ident }
        }
    } else {
        quote! { panic!("Unknown contract error code: {}", code) }
    }
}

/// Generate the From<InvokeError> impl.
fn generate_invoke_error_impl(name: &Ident, infos: &[VariantInfo]) -> proc_macro2::TokenStream {
    let try_arms = generate_try_arms(infos);
    let abort_handler = generate_abort_handler(infos);
    let unknown_handler = generate_unknown_handler(infos);

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

                            // 3. Handle unknown
                            #unknown_handler
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
            let ident = &info.ident;
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

/// Add an auto-generated Aborted variant if configured.
fn add_auto_abort_variant(
    infos: &mut Vec<VariantInfo>,
    next_code: u32,
    input: &DeriveInput,
) -> syn::Result<()> {
    let ident = Ident::new("Aborted", input.span());
    infos.push(VariantInfo {
        ident,
        code: next_code,
        description: "Cross-contract call aborted".into(),
        kind: VariantKind::AbortUnit,
    });
    Ok(())
}

/// Add an auto-generated UnknownError variant if configured.
fn add_auto_unknown_variant(
    infos: &mut Vec<VariantInfo>,
    next_code: u32,
    stores_code: bool,
    input: &DeriveInput,
) -> syn::Result<()> {
    let ident = Ident::new("UnknownError", input.span());
    infos.push(VariantInfo {
        ident,
        code: next_code,
        description: "Unknown error from cross-contract call".into(),
        kind: VariantKind::Sentinel { stores_code },
    });
    Ok(())
}

/// Handle auto-addition of special variants and config validation.
fn handle_auto_variants(
    mut infos: Vec<VariantInfo>,
    config: &ScerrConfig,
    input: &DeriveInput,
) -> syn::Result<Vec<VariantInfo>> {
    let has_abort = infos.iter().any(|i| i.is_from_abort());
    let has_sentinel = infos.iter().any(|i| i.is_sentinel());

    let handle_abort = config.handle_abort.clone().unwrap_or("auto".to_string());
    if handle_abort != "auto" && handle_abort != "panic" {
        return Err(Error::new(
            input.span(),
            "handle_abort must be 'auto' or 'panic'",
        ));
    }
    let handle_unknown = config.handle_unknown.clone().unwrap_or("auto".to_string());
    if handle_unknown != "auto" && handle_unknown != "panic" {
        return Err(Error::new(
            input.span(),
            "handle_unknown must be 'auto' or 'panic'",
        ));
    }
    let log_unknown_errors = config.log_unknown_errors;

    if handle_abort == "panic" && has_abort {
        return Err(Error::new(
            input.span(),
            "handle_abort='panic' conflicts with #[abort] variant",
        ));
    }
    if handle_unknown == "panic" && has_sentinel {
        return Err(Error::new(
            input.span(),
            "handle_unknown='panic' conflicts with #[sentinel] variant",
        ));
    }

    let mut next_code = infos.iter().map(|i| i.code).max().unwrap_or(0) + 1;

    if handle_abort == "auto" && !has_abort {
        if next_code > ROOT_MAX {
            return Err(Error::new(
                input.span(),
                "Too many error variants for root mode",
            ));
        }
        add_auto_abort_variant(&mut infos, next_code, input)?;
        next_code += 1;
    }

    if handle_unknown == "auto" && !has_sentinel {
        if next_code > ROOT_MAX {
            return Err(Error::new(
                input.span(),
                "Too many error variants for root mode",
            ));
        }
        add_auto_unknown_variant(&mut infos, next_code, log_unknown_errors, input)?;
    }

    Ok(infos)
}

/// Generate logging impl if enabled.
fn generate_logging_impl(
    name: &Ident,
    infos: &[VariantInfo],
    log_unknown_errors: bool,
    input: &DeriveInput,
) -> syn::Result<proc_macro2::TokenStream> {
    if !log_unknown_errors {
        return Ok(quote! {});
    }

    let sentinel = infos.iter().find(|i| i.is_sentinel()).ok_or_else(|| {
        Error::new(
            input.span(),
            "log_unknown_errors requires a sentinel variant (use handle_unknown='auto' or add #[sentinel])",
        )
    })?;

    let sentinel_ident = &sentinel.ident;
    let log_stmt = if sentinel.stores_code() {
        let pat = if let Some(field) = sentinel.field_ident() {
            quote! { Self::#sentinel_ident { #field: code } }
        } else {
            quote! { Self::#sentinel_ident(code) }
        };
        quote! {
            if let #pat = res {
                use soroban_sdk::IntoVal;
                env.logs().add("Unknown error code", &[code.into_val(env)]);
            }
        }
    } else {
        quote! {
            if let Self::#sentinel_ident = res {
                env.logs().add("Unknown error", &[]);
            }
        }
    };

    Ok(quote! {
        impl #name {
            pub fn from_invoke_error(env: &soroban_sdk::Env, e: soroban_sdk::InvokeError) -> Self {
                let res = Self::from(e);
                #log_stmt
                res
            }
        }
    })
}

/// Expand the macro for root mode.
fn expand_scerr_root(
    input: &DeriveInput,
    infos: Vec<VariantInfo>,
    config: &ScerrConfig,
) -> syn::Result<proc_macro2::TokenStream> {
    let infos = handle_auto_variants(infos, config, input)?;

    let name = &input.ident;
    let has_data = infos.iter().any(|i| i.field_ty().is_some());
    let enum_def = generate_enum_def(name, &infos, has_data);
    let contract_error_impl = generate_contract_error_impl_root(name, &infos);
    let from_impls = generate_from_trait_impls(name, &infos)?;
    let invoke_impl = generate_invoke_error_impl(name, &infos);
    let soroban_impls = generate_soroban_conversions(name);
    let double_q_impls = generate_double_q_impls(name, &infos);
    let logging_impl = generate_logging_impl(name, &infos, config.log_unknown_errors, input)?;

    Ok(quote! {
        #enum_def
        #contract_error_impl
        #from_impls
        #invoke_impl
        #soroban_impls
        #double_q_impls
        #logging_impl
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
            let id = &i.ident;
            let c = i.code;
            quote! { #id = #c }
        })
        .collect();

    let from_arms: Vec<_> = infos
        .iter()
        .map(|i| {
            let c = i.code;
            let ident = &i.ident;
            quote! { #c => Some(#name::#ident) }
        })
        .collect();

    let desc_arms: Vec<_> = infos
        .iter()
        .map(|i| {
            let ident = &i.ident;
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
    let (mode, config) = match parse_scerr_attr(attr) {
        Ok((m, c)) => (m, c),
        Err(e) => return e.to_compile_error().into(),
    };

    let input = parse_macro_input!(item as DeriveInput);

    match expand_scerr(mode, input, &config) {
        Ok(ts) => TokenStream::from(ts),
        Err(e) => e.to_compile_error().into(),
    }
}

fn expand_scerr(
    mode: ScerrMode,
    input: DeriveInput,
    config: &ScerrConfig,
) -> syn::Result<proc_macro2::TokenStream> {
    let data_enum = match &input.data {
        Data::Enum(e) => e,
        _ => {
            return Err(Error::new(
                input.ident.span(),
                "#[scerr] only supported on enums",
            ))
        }
    };

    let infos = collect_variant_infos(data_enum, &input.ident, mode)?;

    validate_variants(&infos, mode)?;

    if mode == ScerrMode::Root {
        validate_root_variants(&infos)?;
        expand_scerr_root(&input, infos, config)
    } else {
        expand_scerr_basic(&input, &infos)
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
        assert!(matches!(info.kind, VariantKind::PlainUnit));
    }

    #[test]
    fn test_validate_variants_basic_non_unit() {
        let info = VariantInfo {
            ident: parse_quote!(Test),
            code: 1,
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
