//! Implementation of the #[scerr] macro for generating Soroban contract error enums.
//!
//! This module provides the procedural macro logic for #[scerr], which generates
//! error enums with unique u32 codes, conversion traits, and composable error handling.
//!
//! The macro **auto-detects** whether to use basic or root mode based on the enum's
//! variants. If any variant has special attributes (`#[transparent]`,
//! `#[from_contract_client]`) or carries data, root mode is used automatically.
//!
//! In root mode, two special variants are always auto-generated:
//! - `Aborted` (code 0): catches `InvokeError::Abort` from cross-contract calls
//! - `UnknownError` (code `UNKNOWN_ERROR_CODE` / `i32::MAX`): catches unknown error codes
//!
//! ## Usage
//!
//! ### Simple Errors (auto-detected as basic mode)
//!
//! ```rust,ignore
//! use soroban_sdk_tools::scerr;
//!
//! #[scerr]
//! pub enum TokenError {
//!     /// insufficient balance for transfer
//!     InsufficientBalance,
//!     /// unauthorized operation
//!     Unauthorized,
//!     InvalidAmount,  // Falls back to variant name if no doc comment
//! }
//! ```
//!
//! ### Composable Errors (auto-detected as root mode)
//!
//! ```rust,ignore
//! #[scerr]
//! pub enum AppError {
//!     /// unauthorized operation
//!     Unauthorized,
//!     #[transparent]
//!     Math(#[from] MathError),
//!     #[from_contract_client]
//!     ExternalMath(MathError),
//! }
//! // Auto-generates: Aborted (code 0), UnknownError (code UNKNOWN_ERROR_CODE)
//! ```
//!
//! In root mode, two special variants are always auto-generated at fixed codes:
//! - `Aborted` (code 0) and `UnknownError` (code `UNKNOWN_ERROR_CODE`).
//!
//! ## Architecture
//!
//! Error codes are assigned **sequentially** starting at 1. When a variant wraps another
//! error type (via `#[transparent]` or `#[from_contract_client]`), the inner type's variants
//! are flattened into the sequential range at their position using const-chaining.
//!
//! For example, if `MathError` has 2 variants:
//! - `Unauthorized = 1`
//! - `Math_DivisionByZero = 2` (offset into MathError)
//! - `Math_NegativeInput = 3`
//! - `Aborted = 4`

use darling::FromMeta;
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use std::collections::HashSet;
use syn::{
    parse_macro_input, spanned::Spanned, Attribute, Data, DataEnum, DeriveInput, Error, Expr,
    ExprLit, Fields, Ident, Lit, Type, Variant,
};

use crate::util::collect_results;

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
struct ScerrConfig {}

/// Parse the #[scerr(...)] attribute to extract config options.
fn parse_scerr_attr(attr: TokenStream) -> syn::Result<ScerrConfig> {
    let ts2 = proc_macro2::TokenStream::from(attr);
    if ts2.is_empty() {
        return Ok(ScerrConfig::default());
    }
    let meta_items = darling::ast::NestedMeta::parse_meta_list(ts2)
        .map_err(|e| Error::new(proc_macro2::Span::call_site(), e.to_string()))?;
    let config =
        ScerrConfig::from_list(&meta_items).map_err(|e| Error::new(e.span(), e.to_string()))?;
    Ok(config)
}

/// Detect if a variant requires root mode based on its attributes.
fn variant_requires_root(variant: &Variant) -> bool {
    // Check for root-mode-only attributes
    let has_root_attr = variant
        .attrs
        .iter()
        .any(|a| a.path().is_ident("transparent") || a.path().is_ident("from_contract_client"));

    // Check for non-unit variants (data-carrying)
    let has_fields = !matches!(variant.fields, Fields::Unit);

    has_root_attr || has_fields
}

/// Auto-detect mode based on enum variants.
/// Returns Root mode if any variant has root-mode attributes or carries data.
fn detect_mode(data: &DataEnum) -> ScerrMode {
    if data.variants.iter().any(variant_requires_root) {
        ScerrMode::Root
    } else {
        ScerrMode::Basic
    }
}

/// Extract doc comment attributes from a list of attributes.
/// Returns a TokenStream of #[doc = "..."] attributes that can be applied to generated items.
fn extract_doc_attrs(attrs: &[Attribute]) -> Vec<proc_macro2::TokenStream> {
    attrs
        .iter()
        .filter(|attr| attr.path().is_ident("doc"))
        .map(|attr| quote! { #attr })
        .collect()
}

/// Extract doc comment text from attributes as a single string.
/// Concatenates multiple `///` lines with spaces.
fn extract_doc_string(attrs: &[Attribute]) -> Option<String> {
    let docs: Vec<String> = attrs
        .iter()
        .filter(|attr| attr.path().is_ident("doc"))
        .filter_map(|attr| {
            if let Ok(meta_nv) = attr.meta.require_name_value() {
                if let Expr::Lit(ExprLit {
                    lit: Lit::Str(s), ..
                }) = &meta_nv.value
                {
                    return Some(s.value().trim().to_string());
                }
            }
            None
        })
        .filter(|s| !s.is_empty())
        .collect();

    if docs.is_empty() {
        None
    } else {
        Some(docs.join(" "))
    }
}

// -----------------------------------------------------------------------------
// Variant Processing
// -----------------------------------------------------------------------------

/// The kind of variant behavior.
///
/// # Variant Types
///
/// - `PlainUnit`: Simple unit variant for contract-specific errors
/// - `AbortUnit`: Auto-generated variant that catches InvokeError::Abort (always code 0)
/// - `Sentinel`: Auto-generated variant that catches unknown error codes (always code `UNKNOWN_ERROR_CODE`)
/// - `Transparent`: Wraps another error type for in-process propagation via `?`
/// - `FromContractClient`: Wraps another error type for cross-contract error decoding
#[derive(Debug)]
enum VariantKind {
    PlainUnit,
    AbortUnit,
    Sentinel,
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
///
/// In the new sequential system, `code` is only used for basic mode.
/// In root mode, codes are determined by const-chaining at compile time.
/// The `index` field tracks the variant's position for const-chain generation.
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
        matches!(&self.kind, VariantKind::Sentinel)
    }

    /// Returns the field type if this is a non-unit variant.
    fn field_ty(&self) -> Option<Type> {
        match &self.kind {
            VariantKind::Transparent { field_ty, .. }
            | VariantKind::FromContractClient { field_ty, .. } => Some(field_ty.clone()),
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

    /// Returns true if this is a transparent variant.
    fn is_transparent(&self) -> bool {
        matches!(&self.kind, VariantKind::Transparent { .. })
    }

    /// Returns true if this is a wrapped variant (transparent or FCC).
    fn is_wrapped(&self) -> bool {
        self.is_transparent() || self.is_fcc()
    }
}

#[derive(Debug, Default)]
struct VariantAttrs {
    transparent: bool,
    from_contract_client: bool,
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
        }
    }

    Ok(attrs)
}

/// Validate that variant attributes are mutually exclusive.
fn validate_variant_attrs(attrs: &VariantAttrs, variant: &Variant) -> syn::Result<()> {
    let count = [attrs.transparent, attrs.from_contract_client]
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
fn determine_unit_kind(_attrs: &VariantAttrs) -> VariantKind {
    VariantKind::PlainUnit
}

/// Determine the VariantKind for single-field variants.
fn determine_single_field_kind(
    attrs: &VariantAttrs,
    field: &syn::Field,
    variant: &Variant,
) -> syn::Result<VariantKind> {
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
        _ => Err(Error::new(
            variant.span(),
            "Unsupported variant type: must be unit or single-field",
        )),
    }
}

/// Parse a single variant into `VariantInfo`.
fn parse_variant_info(variant: &Variant, code: u32) -> syn::Result<VariantInfo> {
    let ident = variant.ident.clone();
    let description = extract_doc_string(&variant.attrs).unwrap_or_else(|| ident.to_string());

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

/// Assign sequential codes to variants (1, 2, 3, ...).
/// In root mode, wrapped variants get code 0 (placeholder — actual codes are const-chained).
/// In basic mode, all variants get sequential codes starting at 1.
fn assign_codes(data: &DataEnum, mode: ScerrMode) -> syn::Result<Vec<u32>> {
    let mut next_code: u32 = 1;

    data.variants
        .iter()
        .map(|variant| {
            // Check for explicit discriminant
            if let Some((_, expr)) = &variant.discriminant {
                if mode == ScerrMode::Root {
                    return Err(Error::new(
                        expr.span(),
                        "Explicit discriminants are not allowed in composable (root) mode. \
                         Remove the `= N` assignment.",
                    ));
                }
                if let Expr::Lit(ExprLit {
                    lit: Lit::Int(li), ..
                }) = expr
                {
                    return li.base10_parse::<u32>();
                } else {
                    return Err(Error::new(
                        expr.span(),
                        "Only integer literal discriminants supported",
                    ));
                }
            }

            match mode {
                ScerrMode::Root => {
                    // In root mode, wrapped variants use 0 as placeholder
                    // Their actual codes are computed via const-chaining
                    let is_wrapped = variant.attrs.iter().any(|a| {
                        a.path().is_ident("transparent")
                            || a.path().is_ident("from_contract_client")
                    });
                    if is_wrapped {
                        Ok(0) // placeholder for wrapped variants
                    } else {
                        let code = next_code;
                        next_code += 1;
                        Ok(code)
                    }
                }
                ScerrMode::Basic => {
                    let code = next_code;
                    next_code += 1;
                    Ok(code)
                }
            }
        })
        .collect()
}

/// Collect `VariantInfo` for all variants in the enum.
fn collect_variant_infos(data: &DataEnum, mode: ScerrMode) -> syn::Result<Vec<VariantInfo>> {
    let codes = assign_codes(data, mode)?;
    collect_results(
        data.variants
            .iter()
            .zip(codes)
            .map(|(v, code)| parse_variant_info(v, code)),
    )
}

// -----------------------------------------------------------------------------
// Code Generation Helpers
// -----------------------------------------------------------------------------

/// Generate the enum definition.
fn generate_enum_def(
    name: &Ident,
    infos: &[VariantInfo],
    has_data: bool,
    doc_attrs: &[proc_macro2::TokenStream],
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
            let desc = &info.description;
            if let Some(ty) = info.field_ty() {
                if let Some(field) = info.field_ident() {
                    quote! {
                        #[doc = #desc]
                        #ident { #field: #ty }
                    }
                } else {
                    quote! {
                        #[doc = #desc]
                        #ident(#ty)
                    }
                }
            } else {
                quote! {
                    #[doc = #desc]
                    #ident
                }
            }
        })
        .collect();

    quote! {
        #(#doc_attrs)*
        #repr
        #[derive(Copy, Clone, Debug, Eq, PartialEq)]
        pub enum #name {
            #(#variants),*
        }
    }
}

/// Generate the const-chain declarations for root mode.
///
/// For each variant in order:
/// - Unit: `const __SCERR_{ENUM}_{VARIANT}: u32 = <prev> + 1;`
/// - Wrapped: `const __SCERR_{ENUM}_{VARIANT}_OFFSET: u32 = <prev> + 1;`
///   `const __SCERR_{ENUM}_{VARIANT}_COUNT: u32 = <InnerType as ContractErrorSpec>::TOTAL_CODES;`
/// - Abort: `const __SCERR_{ENUM}_{VARIANT}: u32 = 0;` (fixed, doesn't advance chain)
/// - Sentinel: `const __SCERR_{ENUM}_{VARIANT}: u32 = UNKNOWN_ERROR_CODE as u32;` (fixed, doesn't advance chain)
///
/// The "prev" for the first variant is 0, so first regular variant gets code 1.
fn generate_const_chain(enum_name: &Ident, infos: &[VariantInfo]) -> proc_macro2::TokenStream {
    let upper_enum = enum_name.to_string().to_uppercase();
    let mut consts = Vec::new();

    // Track what the "previous end" expression is
    // Start at 0 so first variant gets code 1
    let mut prev_end: proc_macro2::TokenStream = quote! { 0u32 };

    for info in infos {
        let upper_variant = info.ident.to_string().to_uppercase();

        if info.is_from_abort() {
            // Abort always gets code 0, doesn't advance chain
            let const_name = format_ident!("__SCERR_{}_{}", upper_enum, upper_variant);
            consts.push(quote! {
                const #const_name: u32 = 0;
            });
        } else if info.is_sentinel() {
            // Sentinel always gets UNKNOWN_ERROR_CODE, doesn't advance chain
            let const_name = format_ident!("__SCERR_{}_{}", upper_enum, upper_variant);
            consts.push(quote! {
                const #const_name: u32 = soroban_sdk_tools::error::UNKNOWN_ERROR_CODE;
            });
        } else if info.is_wrapped() {
            let ty = info.field_ty().unwrap();
            let offset_name = format_ident!("__SCERR_{}_{}_OFFSET", upper_enum, upper_variant);
            let count_name = format_ident!("__SCERR_{}_{}_COUNT", upper_enum, upper_variant);
            let end_name = format_ident!("__SCERR_{}_{}_END", upper_enum, upper_variant);

            consts.push(quote! {
                const #offset_name: u32 = #prev_end + 1;
                const #count_name: u32 = <#ty as soroban_sdk_tools::error::ContractErrorSpec>::TOTAL_CODES;
                const #end_name: u32 = #offset_name + #count_name;
            });

            prev_end = quote! { #end_name - 1 };
        } else {
            // Plain unit variant
            let const_name = format_ident!("__SCERR_{}_{}", upper_enum, upper_variant);

            consts.push(quote! {
                const #const_name: u32 = #prev_end + 1;
            });

            prev_end = quote! { #const_name };
        }
    }

    quote! { #(#consts)* }
}

/// Generate match arms for `into_code` method in root mode.
fn generate_into_arms(enum_name: &Ident, infos: &[VariantInfo]) -> Vec<proc_macro2::TokenStream> {
    let upper_enum = enum_name.to_string().to_uppercase();

    infos
        .iter()
        .map(|info| {
            let ident = &info.ident;
            let upper_variant = info.ident.to_string().to_uppercase();

            if info.is_sentinel() || info.is_from_abort() {
                // Sentinel (UNKNOWN_ERROR_CODE) and Abort (0) use fixed const values
                let const_name = format_ident!("__SCERR_{}_{}", upper_enum, upper_variant);
                quote! { Self::#ident => #const_name }
            } else if info.is_wrapped() {
                // Wrapped variant: OFFSET + inner.to_seq()
                let offset_name = format_ident!("__SCERR_{}_{}_OFFSET", upper_enum, upper_variant);
                if let Some(field) = info.field_ident() {
                    quote! {
                        Self::#ident { #field: inner } => #offset_name + inner.to_seq()
                    }
                } else {
                    quote! {
                        Self::#ident(inner) => #offset_name + inner.to_seq()
                    }
                }
            } else {
                // Plain unit variant
                let const_name = format_ident!("__SCERR_{}_{}", upper_enum, upper_variant);
                quote! { Self::#ident => #const_name }
            }
        })
        .collect()
}

/// Generate the from_code implementation for root mode.
fn generate_from_code_body(enum_name: &Ident, infos: &[VariantInfo]) -> proc_macro2::TokenStream {
    let upper_enum = enum_name.to_string().to_uppercase();

    let mut arms = Vec::new();

    for info in infos {
        let ident = &info.ident;
        let upper_variant = info.ident.to_string().to_uppercase();

        if info.is_sentinel() || info.is_from_abort() {
            // Abort (0) and Sentinel (UNKNOWN_ERROR_CODE) use fixed const values
            let const_name = format_ident!("__SCERR_{}_{}", upper_enum, upper_variant);
            arms.push(quote! {
                #const_name => Some(Self::#ident)
            });
        } else if info.is_wrapped() {
            let ty = info.field_ty().unwrap();
            let offset_name = format_ident!("__SCERR_{}_{}_OFFSET", upper_enum, upper_variant);
            let end_name = format_ident!("__SCERR_{}_{}_END", upper_enum, upper_variant);

            let construct = if let Some(field) = info.field_ident() {
                quote! { |inner| Self::#ident { #field: inner } }
            } else {
                quote! { Self::#ident }
            };

            arms.push(quote! {
                c @ #offset_name..#end_name => {
                    #ty::from_seq(c - #offset_name).map(#construct)
                }
            });
        } else {
            // Plain unit variant
            let const_name = format_ident!("__SCERR_{}_{}", upper_enum, upper_variant);
            arms.push(quote! {
                #const_name => Some(Self::#ident)
            });
        }
    }

    // Default arm
    arms.push(quote! { _ => None });

    quote! {
        use soroban_sdk_tools::error::SequentialError;
        match code {
            #(#arms,)*
        }
    }
}

/// Generate the ContractError impl for root mode.
fn generate_contract_error_impl_root(
    name: &Ident,
    infos: &[VariantInfo],
) -> proc_macro2::TokenStream {
    let into_arms = generate_into_arms(name, infos);
    let from_code_body = generate_from_code_body(name, infos);

    quote! {
        impl soroban_sdk_tools::error::ContractError for #name {
            fn into_code(self) -> u32 {
                use soroban_sdk_tools::error::SequentialError;
                match &self {
                    #(#into_arms,)*
                }
            }

            fn from_code(code: u32) -> Option<Self> {
                #from_code_body
            }
        }
    }
}

/// Generate SequentialError impl for root mode.
fn generate_sequential_error_impl_root(name: &Ident) -> proc_macro2::TokenStream {
    quote! {
        impl soroban_sdk_tools::error::SequentialError for #name {
            fn to_seq(&self) -> u32 {
                <Self as soroban_sdk_tools::error::ContractError>::into_code(*self) - 1
            }
            fn from_seq(seq: u32) -> Option<Self> {
                <Self as soroban_sdk_tools::error::ContractError>::from_code(seq + 1)
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
                format!("Duplicate From impl for {ty_s}"),
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

/// Generate the abort handler for From<InvokeError>.
/// The abort variant is always auto-generated, so it will always be present.
fn generate_abort_handler(infos: &[VariantInfo]) -> proc_macro2::TokenStream {
    if let Some(info) = infos.iter().find(|i| i.is_from_abort()) {
        let ident = &info.ident;
        quote! { Self::#ident }
    } else {
        quote! { panic!("Cross-contract call aborted (error code 0)") }
    }
}

/// Generate the unknown handler for From<InvokeError>.
/// The sentinel is always auto-generated, so it will always be present.
fn generate_unknown_handler(infos: &[VariantInfo]) -> proc_macro2::TokenStream {
    if let Some(info) = infos.iter().find(|i| i.is_sentinel()) {
        let ident = &info.ident;
        quote! { Self::#ident }
    } else {
        quote! { panic!("Unknown contract error code: {}", code) }
    }
}

/// Generate the From<InvokeError> impl.
fn generate_invoke_error_impl(name: &Ident, infos: &[VariantInfo]) -> proc_macro2::TokenStream {
    let abort_handler = generate_abort_handler(infos);
    let unknown_handler = generate_unknown_handler(infos);

    quote! {
        impl From<soroban_sdk::InvokeError> for #name {
            fn from(e: soroban_sdk::InvokeError) -> Self {
                match e {
                    soroban_sdk::InvokeError::Contract(code) => {
                        // Try exact match against our sequential codes.
                        // from_code covers the entire contiguous range 1..=TOTAL_CODES,
                        // so any code outside that range is truly unknown.
                        if let Some(decoded) = Self::from_code(code) {
                            return decoded;
                        }
                        #unknown_handler
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
    let abort_handler = generate_abort_handler(infos);
    let unknown_handler = generate_unknown_handler(infos);

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
                            // SDK decoded the raw code as the expected inner type
                            Ok(inner) => #ok_construct,
                            // SDK could not decode the raw code. We handle directly
                            // instead of delegating to From<InvokeError>, which
                            // would incorrectly interpret the raw code in our own
                            // remapped code space via from_code().
                            Err(soroban_sdk::InvokeError::Contract(code)) => {
                                #unknown_handler
                            }
                            Err(soroban_sdk::InvokeError::Abort) => {
                                #abort_handler
                            }
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

/// Add an auto-generated UnknownError sentinel variant.
/// Always a unit variant with code UNKNOWN_ERROR_CODE.
fn add_auto_unknown_variant(infos: &mut Vec<VariantInfo>, input: &DeriveInput) -> syn::Result<()> {
    let ident = Ident::new("UnknownError", input.span());
    infos.push(VariantInfo {
        ident,
        code: 0, // placeholder; actual code is UNKNOWN_ERROR_CODE via const-chain
        description: "Unknown error from cross-contract call".into(),
        kind: VariantKind::Sentinel,
    });
    Ok(())
}

/// Handle auto-addition of special variants.
///
/// Always adds:
/// - `Aborted` (code 0): catches `InvokeError::Abort`
/// - `UnknownError` (code `UNKNOWN_ERROR_CODE`): catches unknown error codes
fn handle_auto_variants(
    mut infos: Vec<VariantInfo>,
    _config: &ScerrConfig,
    input: &DeriveInput,
) -> syn::Result<Vec<VariantInfo>> {
    add_auto_abort_variant(&mut infos, 0, input)?;
    add_auto_unknown_variant(&mut infos, input)?;
    Ok(infos)
}

/// Generate ContractErrorSpec implementation for an error enum (basic mode).
fn generate_error_spec_impl(name: &Ident, infos: &[VariantInfo]) -> proc_macro2::TokenStream {
    let unit_variants = infos.iter().filter(|i| i.field_ty().is_none());

    // Basic-mode entries and tree nodes share the same data; build both in one pass.
    let (spec_entries, tree_nodes): (Vec<_>, Vec<_>) = unit_variants
        .map(|i| {
            let code = i.code;
            let variant_name = i.ident.to_string();
            let desc = &i.description;
            (
                quote! {
                    soroban_sdk_tools::error::ErrorSpecEntry {
                        code: #code, name: #variant_name, description: #desc,
                    }
                },
                quote! {
                    soroban_sdk_tools::error::SpecNode {
                        code: #code, name: #variant_name, description: #desc,
                        children: &[],
                    }
                },
            )
        })
        .unzip();

    quote! {
        impl soroban_sdk_tools::error::ContractErrorSpec for #name {
            const SPEC_ENTRIES: &'static [soroban_sdk_tools::error::ErrorSpecEntry] = &[
                #(#spec_entries),*
            ];
            const SPEC_TREE: &'static [soroban_sdk_tools::error::SpecNode] = &[
                #(#tree_nodes),*
            ];
        }
    }
}

/// Generate the WASM contract spec XDR for root-mode error enums.
///
/// This produces a `link_section = "contractspecv0"` static containing XDR
/// bytes for the error enum.  The XDR is built entirely at compile time via
/// const-fn helpers that walk the private `__SPEC_FULL_TREE` (which includes
/// sentinels).  Inner types' variant names are flattened with prefixes
/// (e.g. `Deep_DeepFailureOne`), and codes are remapped to sequential
/// positions.
fn generate_wasm_spec_root(enum_name: &Ident, enum_doc: &str) -> proc_macro2::TokenStream {
    let upper_enum = enum_name.to_string().to_uppercase();
    let name_str = enum_name.to_string();
    let spec_ident = format_ident!("__SPEC_XDR_TYPE_{}", upper_enum);
    let len_ident = format_ident!("__SPEC_XDR_LEN_{}", upper_enum);

    let full_tree_ident = format_ident!("__SPEC_FULL_TREE_{}", upper_enum);

    quote! {
        const #len_ident: usize = soroban_sdk_tools::error::xdr_error_enum_size(
            #name_str,
            #enum_doc,
            #full_tree_ident,
        );

        #[cfg_attr(target_family = "wasm", link_section = "contractspecv0")]
        pub static #spec_ident: [u8; #len_ident] =
            soroban_sdk_tools::error::build_error_enum_xdr::<{ #len_ident }>(
                #name_str,
                #enum_doc,
                #full_tree_ident,
            );
    }
}

/// Generate ContractErrorSpec implementation for root-mode error enums.
///
/// Root-mode enums include wrapped inner types whose codes are computed via
/// const-chaining. `SPEC_ENTRIES` contains only the unit-like variants (PlainUnit,
/// AbortUnit, Sentinel) with their const-chain code references. `TOTAL_CODES` is
/// set to the last code value so that an outer enum can correctly reserve
/// the right number of sequential slots when wrapping this type.
///
/// `SPEC_TREE` contains leaf nodes and group nodes but **excludes** sentinel
/// variants (Aborted, UnknownError) so that outer types don't duplicate them
/// when nesting via `#[from_contract_client]`.  A separate private
/// `__SPEC_FULL_TREE` const includes sentinels for this type's own XDR spec.
fn generate_error_spec_impl_root(name: &Ident, infos: &[VariantInfo]) -> proc_macro2::TokenStream {
    let upper_enum = name.to_string().to_uppercase();

    // Build spec entries for unit-like variants only (using const-chain references)
    let spec_entries: Vec<_> = infos
        .iter()
        .filter(|i| !i.is_wrapped())
        .map(|i| {
            let upper_variant = i.ident.to_string().to_uppercase();
            let const_name = format_ident!("__SCERR_{}_{}", upper_enum, upper_variant);
            let variant_name = i.ident.to_string();
            let desc = &i.description;
            quote! {
                soroban_sdk_tools::error::ErrorSpecEntry {
                    code: #const_name,
                    name: #variant_name,
                    description: #desc,
                }
            }
        })
        .collect();

    // Helper to build a SpecNode token for a given VariantInfo
    let make_node = |i: &VariantInfo| {
        let upper_variant = i.ident.to_string().to_uppercase();
        let variant_name = i.ident.to_string();
        let desc = &i.description;

        if i.is_wrapped() {
            // Group node: references inner type's SPEC_TREE
            let ty = i.field_ty().unwrap();
            let offset_name = format_ident!("__SCERR_{}_{}_OFFSET", upper_enum, upper_variant);
            quote! {
                soroban_sdk_tools::error::SpecNode {
                    code: #offset_name,
                    name: #variant_name,
                    description: "",
                    children: <#ty as soroban_sdk_tools::error::ContractErrorSpec>::SPEC_TREE,
                }
            }
        } else {
            // Leaf node
            let const_name = format_ident!("__SCERR_{}_{}", upper_enum, upper_variant);
            quote! {
                soroban_sdk_tools::error::SpecNode {
                    code: #const_name,
                    name: #variant_name,
                    description: #desc,
                    children: &[],
                }
            }
        }
    };

    // Full tree includes all variants (for this type's own XDR spec)
    let full_tree_nodes: Vec<_> = infos.iter().map(&make_node).collect();

    // SPEC_TREE excludes sentinels (Aborted, UnknownError) so outer types
    // don't duplicate them when nesting via #[from_contract_client]
    let tree_nodes: Vec<_> = infos
        .iter()
        .filter(|i| !i.is_from_abort() && !i.is_sentinel())
        .map(&make_node)
        .collect();

    // Compute TOTAL_CODES: the code of the very last variant in the sequence.
    let total_codes_expr = compute_total_codes_expr(name, infos);

    let full_tree_ident = format_ident!("__SPEC_FULL_TREE_{}", upper_enum);

    quote! {
        /// Private full tree for this type's own XDR spec (includes sentinels).
        const #full_tree_ident: &[soroban_sdk_tools::error::SpecNode] = &[
            #(#full_tree_nodes),*
        ];

        impl soroban_sdk_tools::error::ContractErrorSpec for #name {
            const SPEC_ENTRIES: &'static [soroban_sdk_tools::error::ErrorSpecEntry] = &[
                #(#spec_entries),*
            ];
            const TOTAL_CODES: u32 = #total_codes_expr;
            const SPEC_TREE: &'static [soroban_sdk_tools::error::SpecNode] = &[
                #(#tree_nodes),*
            ];
        }
    }
}

/// Compute the expression for the total number of codes a root-mode enum occupies.
///
/// This mirrors the const-chain logic: the last sequential variant's end position
/// gives us the total count. Abort (code 0) and Sentinel (code UNKNOWN_ERROR_CODE) are
/// excluded since they occupy fixed positions outside the sequential range.
fn compute_total_codes_expr(enum_name: &Ident, infos: &[VariantInfo]) -> proc_macro2::TokenStream {
    let upper_enum = enum_name.to_string().to_uppercase();

    // Find the last variant that participates in sequential numbering
    let last = infos
        .iter()
        .rev()
        .find(|i| !i.is_from_abort() && !i.is_sentinel());

    let Some(last) = last else {
        return quote! { 0u32 };
    };

    let upper_variant = last.ident.to_string().to_uppercase();

    if last.is_wrapped() {
        let end_name = format_ident!("__SCERR_{}_{}_END", upper_enum, upper_variant);
        quote! { #end_name - 1 }
    } else {
        let const_name = format_ident!("__SCERR_{}_{}", upper_enum, upper_variant);
        quote! { #const_name }
    }
}

/// Expand the macro for root mode.
fn expand_scerr_root(
    input: &DeriveInput,
    infos: Vec<VariantInfo>,
    config: &ScerrConfig,
) -> syn::Result<proc_macro2::TokenStream> {
    let infos = handle_auto_variants(infos, config, input)?;

    let name = &input.ident;
    let enum_doc = extract_doc_string(&input.attrs).unwrap_or_default();
    let doc_attrs = extract_doc_attrs(&input.attrs);
    let has_data = infos.iter().any(|i| i.field_ty().is_some());
    let enum_def = generate_enum_def(name, &infos, has_data, &doc_attrs);
    let const_chain = generate_const_chain(name, &infos);
    let contract_error_impl = generate_contract_error_impl_root(name, &infos);
    let sequential_error_impl = generate_sequential_error_impl_root(name);
    let spec_impl = generate_error_spec_impl_root(name, &infos);
    let wasm_spec = generate_wasm_spec_root(name, &enum_doc);
    let from_impls = generate_from_trait_impls(name, &infos)?;
    let invoke_impl = generate_invoke_error_impl(name, &infos);
    let soroban_impls = generate_soroban_conversions(name);
    let double_q_impls = generate_double_q_impls(name, &infos);

    Ok(quote! {
        #enum_def
        #const_chain
        #contract_error_impl
        #sequential_error_impl
        #spec_impl
        #wasm_spec
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
    let doc_attrs = extract_doc_attrs(&input.attrs);

    let variants: Vec<_> = infos
        .iter()
        .map(|i| {
            let id = &i.ident;
            let c = i.code;
            let d = &i.description;
            quote! {
                #[doc = #d]
                #id = #c
            }
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

    // Generate ContractErrorSpec implementation
    let spec_impl = generate_error_spec_impl(name, infos);

    Ok(quote! {
        #(#doc_attrs)*
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
        }

        impl soroban_sdk_tools::error::SequentialError for #name {
            fn to_seq(&self) -> u32 {
                *self as u32 - 1
            }
            fn from_seq(seq: u32) -> Option<Self> {
                <Self as soroban_sdk_tools::error::ContractError>::from_code(seq + 1)
            }
        }

        #spec_impl
    })
}

/// Main entry point for the #[scerr] macro.
pub fn scerr_impl(attr: TokenStream, item: TokenStream) -> TokenStream {
    let config = match parse_scerr_attr(attr) {
        Ok(c) => c,
        Err(e) => return e.to_compile_error().into(),
    };

    let input = parse_macro_input!(item as DeriveInput);

    match expand_scerr(input, &config) {
        Ok(ts) => TokenStream::from(ts),
        Err(e) => e.to_compile_error().into(),
    }
}

fn expand_scerr(input: DeriveInput, config: &ScerrConfig) -> syn::Result<proc_macro2::TokenStream> {
    let data_enum = match &input.data {
        Data::Enum(e) => e,
        _ => {
            return Err(Error::new(
                input.ident.span(),
                "#[scerr] only supported on enums",
            ))
        }
    };

    // Auto-detect mode based on variant attributes and fields
    let mode = detect_mode(data_enum);

    let infos = collect_variant_infos(data_enum, mode)?;

    if mode == ScerrMode::Root {
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
    fn test_parse_variant_info_unit() {
        let variant: Variant = parse_quote! { Test };
        let info = parse_variant_info(&variant, 1).unwrap();
        assert!(matches!(info.kind, VariantKind::PlainUnit));
    }

    #[test]
    fn test_detect_mode_basic_for_unit_variants() {
        let item: syn::ItemEnum = parse_quote! {
            enum Test { A, B, C }
        };
        let data = DataEnum {
            enum_token: item.enum_token,
            brace_token: item.brace_token,
            variants: item.variants,
        };
        assert_eq!(detect_mode(&data), ScerrMode::Basic);
    }

    #[test]
    fn test_detect_mode_root_for_data_variant() {
        let item: syn::ItemEnum = parse_quote! {
            enum Test { A, B(u32) }
        };
        let data = DataEnum {
            enum_token: item.enum_token,
            brace_token: item.brace_token,
            variants: item.variants,
        };
        assert_eq!(detect_mode(&data), ScerrMode::Root);
    }

    #[test]
    fn test_detect_mode_root_for_transparent_attr() {
        let item: syn::ItemEnum = parse_quote! {
            enum Test { A, #[transparent] B(SomeError) }
        };
        let data = DataEnum {
            enum_token: item.enum_token,
            brace_token: item.brace_token,
            variants: item.variants,
        };
        assert_eq!(detect_mode(&data), ScerrMode::Root);
    }

    #[test]
    fn test_detect_mode_root_for_fcc_attr() {
        let item: syn::ItemEnum = parse_quote! {
            enum Test { A, #[from_contract_client] B(SomeError) }
        };
        let data = DataEnum {
            enum_token: item.enum_token,
            brace_token: item.brace_token,
            variants: item.variants,
        };
        assert_eq!(detect_mode(&data), ScerrMode::Root);
    }
}
