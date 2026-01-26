//! Implementation of the #[scerr] macro for generating Soroban contract error enums.
//!
//! This module provides the procedural macro logic for #[scerr], which generates
//! error enums with unique u32 codes, conversion traits, and composable error handling.
//!
//! The macro **auto-detects** whether to use basic or root mode based on the enum's
//! variants. If any variant has special attributes (`#[transparent]`, `#[from_contract_client]`,
//! `#[abort]`, `#[sentinel]`) or carries data, root mode is used automatically.
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
//!     #[abort]
//!     Aborted,
//!     #[sentinel]
//!     Unknown,
//! }
//! ```
//!
//! ### Options (for root mode)
//!
//! - `handle_abort = "auto" | "panic"`: Auto-add Aborted variant or panic on abort.
//! - `handle_unknown = "auto" | "panic"`: Auto-add UnknownError variant or panic on unknown codes.
//! - `log_unknown_errors = true`: Log unknown error codes (requires sentinel).
//!
//! ## Unified Flattened Error Specs (TypeScript Bindings)
//!
//! When using `#[from_contract_client]` with types from `contractimport_with_errors!`,
//! scerr automatically generates a **unified flattened error enum** in the contract spec.
//! This allows TypeScript developers to look up any error code directly without manual merging.
//!
//! ### Requirements for Unified Flattened Errors
//!
//! The unified enum is **ONLY** generated when **BOTH** conditions are met:
//!
//! 1. Inner contracts are imported via `contractimport_with_errors!` (not local Cargo dependencies)
//! 2. `#[from_contract_client]` variants reference types from those imports
//!
//! ### Example
//!
//! ```rust,ignore
//! // Step 1: Import using contractimport_with_errors!
//! mod math_imported {
//!     soroban_sdk_tools::contractimport_with_errors!(
//!         file = "path/to/math_inner.wasm"
//!     );
//! }
//!
//! // Step 2: Use #[from_contract_client] with imported types
//! #[scerr]
//! pub enum MyError {
//!     /// unauthorized operation
//!     Unauthorized,
//!
//!     #[from_contract_client]
//!     Math(math_imported::MathError),  // ✅ Type from import module
//! }
//! ```
//!
//! This generates a TypeScript binding like:
//!
//! ```typescript
//! export const MyError = {
//!   1: {message: "Unauthorized"},
//!   936: {message: "Aborted"},
//!   937: {message: "UnknownError"},
//!   3922378893: {message: "MathError_DivisionByZero"},
//!   3925386672: {message: "MathError_NegativeInput"},
//! };
//! ```
//!
//! ### What Does NOT Work
//!
//! **1. Using local Cargo dependencies** will NOT generate the unified enum:
//!
//! ```rust,ignore
//! use math_inner::MathError;  // From Cargo.toml dependency - NO unified enum
//!
//! #[scerr]
//! pub enum MyError {
//!     #[from_contract_client]
//!     Math(MathError),  // ❌ Only namespace markers in spec, not flattened variants
//! }
//! ```
//!
//! **2. Using `use` to import the type** - must use full path in the enum:
//!
//! ```rust,ignore
//! use math_imported::MathError;  // ❌ Don't import, use full path instead
//!
//! #[scerr]
//! pub enum MyError {
//!     #[from_contract_client]
//!     Math(MathError),  // ❌ scerr can't derive getter macro path from 1-segment type
//! }
//!
//! // ✅ Correct: use full path in enum definition
//! #[scerr]
//! pub enum MyError {
//!     #[from_contract_client]
//!     Math(math_imported::MathError),  // ✅ Full path allows getter macro derivation
//! }
//! ```
//!
//! ## Architecture
//!
//! - **22/10 Split**: Root enums use the top 10 bits for namespace (1024 possible values).
//!   Inner errors use the lower 22 bits.
//! - **Namespace computation**: Namespaces are computed from the error TYPE name (e.g., "MathError")
//!   using DJB2 hash for deterministic, collision-resistant assignment.
//! - **Code 0 is reserved**: Hashes ensure generated codes are never 0.

use darling::FromMeta;
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use std::collections::{HashMap, HashSet};
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, Attribute, Data, DataEnum, DeriveInput,
    Error, Expr, ExprLit, Fields, Ident, Lit, Type, Variant,
};

use crate::util::collect_results;

// -----------------------------------------------------------------------------
// Constants
// -----------------------------------------------------------------------------

/// Number of bits for namespace (top bits of u32 error code)
/// Using 10 bits gives 1024 possible namespaces, reducing collision risk
const ROOT_BITS: u32 = 10;
const ROOT_MAX: u32 = (1 << ROOT_BITS) - 1; // 1023

/// Number of bits for inner error codes (lower bits)
const INNER_BITS: u32 = 32 - ROOT_BITS; // 22 bits
const INNER_MASK: u32 = (1 << INNER_BITS) - 1; // 0x003FFFFF

// Ensure we don't return 0 from hashes, as 0 is reserved for Abort/System
const HASH_SEED: u32 = 5381;

/// Compute namespace from variant name using DJB2 hash.
/// Must match contractimport_with_errors's computation exactly.
fn compute_variant_namespace(variant_name: &str) -> u32 {
    let mut hash = HASH_SEED;
    for c in variant_name.bytes() {
        hash = ((hash << 5).wrapping_add(hash)).wrapping_add(c as u32);
    }
    // Use bits from the hash, ensure non-zero (1-1023)
    (hash % ROOT_MAX) + 1
}

// -----------------------------------------------------------------------------
// Parsing & Configuration
// -----------------------------------------------------------------------------

#[derive(Debug, Default, PartialEq, Copy, Clone)]
enum ScerrMode {
    #[default]
    Basic,
    Root,
}

/// A wrapper for parsing a list of paths from a parenthesized list.
/// Handles syntax like: `mixins(path1, path2, path3)`
#[derive(Debug, Default, Clone)]
struct PathList(Vec<syn::Path>);

impl darling::FromMeta for PathList {
    fn from_list(items: &[darling::ast::NestedMeta]) -> darling::Result<Self> {
        let mut paths = Vec::new();
        for item in items {
            match item {
                darling::ast::NestedMeta::Meta(syn::Meta::Path(p)) => {
                    paths.push(p.clone());
                }
                _ => {
                    return Err(darling::Error::custom(
                        "Expected a path like `module::__get_variants`",
                    ));
                }
            }
        }
        Ok(PathList(paths))
    }
}

#[derive(Debug, Default, FromMeta)]
struct ScerrConfig {
    #[darling(default)]
    handle_abort: Option<String>,

    #[darling(default)]
    handle_unknown: Option<String>,

    #[darling(default)]
    log_unknown_errors: bool,

    /// List of getter macro paths to include in the unified flattened spec.
    /// These macros are generated by `contractimport_with_errors!` with the
    /// `getter_macro` option. When specified, scerr generates a call to
    /// `__build_unified_spec!` that collects variants from all mixins.
    ///
    /// Example:
    /// ```ignore
    /// #[scerr(mixins(math_module::__get_math_variants))]
    /// ```
    #[darling(default)]
    mixins: PathList,
}

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
    let has_root_attr = variant.attrs.iter().any(|a| {
        a.path().is_ident("transparent")
            || a.path().is_ident("from_contract_client")
            || a.path().is_ident("abort")
            || a.path().is_ident("sentinel")
    });

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
// Variant Processing & Hashing
// -----------------------------------------------------------------------------

/// DJB2 hash function generating 22-bit codes. Ensures result is never 0.
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

    /// Get the type name of the wrapped error type (last segment of the path).
    /// For example, `math_imported::MathError` returns `Some("MathError")`.
    fn wrapped_type_name(&self) -> Option<String> {
        use crate::util::TypeExt;
        self.field_ty().and_then(|ty| ty.get_type_name())
    }

    /// Returns true if this is a transparent variant.
    fn is_transparent(&self) -> bool {
        matches!(&self.kind, VariantKind::Transparent { .. })
    }
}

#[derive(Debug, Default)]
struct VariantAttrs {
    transparent: bool,
    from_contract_client: bool,
    abort: bool,
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

/// Generate a code for a unit variant in root mode (sequential).
fn generate_root_unit_code(next_code: &mut u32, variant: &Variant) -> syn::Result<u32> {
    let code = *next_code;
    *next_code = next_code
        .checked_add(1)
        .ok_or_else(|| Error::new(variant.span(), "Too many error variants"))?;
    Ok(code)
}

/// Extract the type name from a variant's field.
/// Returns the last segment of the type path (e.g., "MathError" from `math_imported::MathError`).
fn extract_type_name_from_variant(variant: &Variant) -> Option<String> {
    use crate::util::TypeExt;
    match &variant.fields {
        Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
            let field = &fields.unnamed[0];
            field.ty.get_type_name()
        }
        Fields::Named(fields) if fields.named.len() == 1 => {
            let field = fields.named.iter().next()?;
            field.ty.get_type_name()
        }
        _ => None,
    }
}

/// Check if a variant has the #[from_contract_client] attribute.
fn has_fcc_attr(variant: &Variant) -> bool {
    variant
        .attrs
        .iter()
        .any(|attr| attr.path().is_ident("from_contract_client"))
}

/// Generate a namespace code for a wrapped variant in root mode (hash-based).
/// For FCC variants, uses the TYPE name (e.g., "MathError") instead of the variant name.
/// This ensures consistency with contractimport_with_errors! which also uses the type name.
fn generate_root_wrapped_namespace(variant: &Variant) -> u32 {
    // For from_contract_client variants, use the TYPE name for namespace
    if has_fcc_attr(variant) {
        if let Some(type_name) = extract_type_name_from_variant(variant) {
            return compute_variant_namespace(&type_name);
        }
    }
    // Fallback to variant name (for transparent variants or when type can't be extracted)
    compute_variant_namespace(&variant.ident.to_string())
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

/// Check if a variant is a wrapped type (transparent or from_contract_client).
fn is_wrapped_variant(variant: &Variant) -> bool {
    variant
        .attrs
        .iter()
        .any(|a| a.path().is_ident("transparent") || a.path().is_ident("from_contract_client"))
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
                    ScerrMode::Root => {
                        // Wrapped variants use hash-based namespace assignment
                        // Unit variants use sequential assignment
                        if is_wrapped_variant(variant) {
                            generate_root_wrapped_namespace(variant)
                        } else {
                            generate_root_unit_code(&mut next_code, variant)?
                        }
                    }
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
            } else if info.is_transparent() || info.is_fcc() {
                // Both transparent and FCC variants can wrap either scerr types OR standard #[contracterror] types
                // Use Error::get_code() which works for both:
                // - Both types implement From<T> for Error
                // - Error::get_code() returns the contract error code
                //
                // NESTED FCC SUPPORT:
                // If the inner code already has namespace bits set (from nested #[from_contract_client]),
                // we pass it through directly instead of re-namespacing. This prevents overflow
                // when wrapping errors that themselves wrap other errors.
                if let Some(field) = info.field_ident() {
                    quote! {
                        Self::#ident { #field: inner } => {
                            let inner_code = soroban_sdk::Error::from(inner).get_code();
                            // If inner code has namespace bits set, pass through directly
                            if (inner_code >> #shift) != 0 {
                                inner_code
                            } else {
                                (#root_idx << #shift) | inner_code
                            }
                        }
                    }
                } else {
                    quote! {
                        Self::#ident(inner) => {
                            let inner_code = soroban_sdk::Error::from(inner).get_code();
                            // If inner code has namespace bits set, pass through directly
                            if (inner_code >> #shift) != 0 {
                                inner_code
                            } else {
                                (#root_idx << #shift) | inner_code
                            }
                        }
                    }
                }
            } else if info.field_ty().is_some() {
                // Other wrapped variants (shouldn't reach here normally)
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
                    quote! { #root_idx => return Some(Self::#ident { #field: 0 }) }
                } else {
                    quote! { #root_idx => return Some(Self::#ident(0)) }
                }
            } else {
                quote! { #root_idx => return Some(Self::#ident) }
            }
        })
        .collect()
}

/// Generate match arms for `from_code` wrapped variants in root mode.
///
/// Uses `TryFrom<InvokeError>` instead of `ContractError::from_code` so that
/// both scerr types AND standard `#[contracterror]` types can be used with
/// `#[from_contract_client]`. Both generate `TryFrom<InvokeError>` implementations.
///
/// Returns early with `Some` if decoding succeeds, otherwise falls through
/// to allow passthrough decoding to be tried.
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
                // Use TryFrom<InvokeError> which is implemented by both:
                // - scerr types (via #[soroban_sdk::contracterror] that scerr generates)
                // - standard #[contracterror] types (via soroban-sdk macro)
                //
                // Returns early if decoding succeeds, otherwise falls through.
                // This allows passthrough to be tried even when namespace matches
                // but inner decoding fails (e.g., namespace collision scenario).
                let construct = if let Some(field) = info.field_ident() {
                    quote! {
                        #root_idx => {
                            if let Ok(inner) = <#ty as core::convert::TryFrom<soroban_sdk::InvokeError>>::try_from(soroban_sdk::InvokeError::Contract(low)) {
                                return Some(Self::#ident { #field: inner });
                            }
                        }
                    }
                } else {
                    quote! {
                        #root_idx => {
                            if let Ok(inner) = <#ty as core::convert::TryFrom<soroban_sdk::InvokeError>>::try_from(soroban_sdk::InvokeError::Contract(low)) {
                                return Some(Self::#ident(inner));
                            }
                        }
                    }
                };
                Some(construct)
            })
        })
        .collect()
}

/// Generate passthrough arms for `from_code` to handle nested FCC.
///
/// When a code doesn't match any known namespace, try decoding the full code
/// against each wrapped type. This handles cases where a nested FCC error
/// was passed through without re-namespacing.
fn generate_from_passthrough(infos: &[VariantInfo]) -> Vec<proc_macro2::TokenStream> {
    infos
        .iter()
        .filter_map(|info| {
            info.field_ty().and_then(|ty| {
                if info.is_sentinel() {
                    return None;
                }
                let ident = &info.ident;
                // Try to decode the full code (passthrough case for nested FCC)
                let construct = if let Some(field) = info.field_ident() {
                    quote! {
                        if let Ok(inner) = <#ty as core::convert::TryFrom<soroban_sdk::InvokeError>>::try_from(soroban_sdk::InvokeError::Contract(code)) {
                            return Some(Self::#ident { #field: inner });
                        }
                    }
                } else {
                    quote! {
                        if let Ok(inner) = <#ty as core::convert::TryFrom<soroban_sdk::InvokeError>>::try_from(soroban_sdk::InvokeError::Contract(code)) {
                            return Some(Self::#ident(inner));
                        }
                    }
                };
                Some(construct)
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
            } else if info.is_fcc() || info.is_transparent() {
                // Both FCC and transparent variants use the outer variant's description
                // because the inner type might be a standard #[contracterror] without description()
                if let Some(field) = info.field_ident() {
                    quote! { Self::#ident { #field: _ } => #d }
                } else if info.field_ty().is_some() {
                    quote! { Self::#ident(_) => #d }
                } else {
                    quote! { Self::#ident => #d }
                }
            } else if info.field_ty().is_some() {
                // Other wrapped variants (shouldn't reach here normally)
                if let Some(field) = info.field_ident() {
                    quote! { Self::#ident { #field } => #field.description() }
                } else {
                    quote! { Self::#ident(inner) => inner.description() }
                }
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
    let from_passthrough = generate_from_passthrough(infos);
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
                if high == 0 {
                    // Unit variants only - match by full code
                    let low = code;
                    match low {
                        #(#from_arms_unit,)*
                        _ => return None,
                    }
                }

                // Wrapped variants - try namespace-based decoding first
                let low = code & #mask;
                match high {
                    #(#from_arms_wrapped)*
                    _ => {}
                }

                // NESTED FCC PASSTHROUGH:
                // If namespace-based decoding didn't succeed (either no match or
                // inner type couldn't decode), try decoding the full code against
                // each wrapped type. This handles nested #[from_contract_client].
                #(#from_passthrough)*
                None
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
///
/// Uses `TryFrom<InvokeError>` instead of `ContractError::from_code` so that
/// both scerr types AND standard `#[contracterror]` types can be used with
/// `#[from_contract_client]`. Both generate `TryFrom<InvokeError>` implementations.
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
            // Use TryFrom<InvokeError> which is implemented by both:
            // - scerr types (via #[soroban_sdk::contracterror] that scerr generates)
            // - standard #[contracterror] types (via soroban-sdk macro)
            quote! {
                if let Ok(inner) = <#ty as core::convert::TryFrom<soroban_sdk::InvokeError>>::try_from(soroban_sdk::InvokeError::Contract(code)) {
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

/// Represents a processed variant for the spec companion enum.
struct SpecVariant {
    tokens: proc_macro2::TokenStream,
    /// For wrapped variants, the expected flattened enum name (e.g., "AppError_Math")
    merge_target: Option<String>,
}

/// Process a single variant info into a spec variant.
fn process_spec_variant(info: &VariantInfo, enum_name: &Ident) -> SpecVariant {
    let ident = &info.ident;
    let code = info.code;

    match info.field_ty() {
        None => {
            // Unit variant - include directly
            let doc = &info.description;
            SpecVariant {
                tokens: quote! { #[doc = #doc] #ident = #code },
                merge_target: None,
            }
        }
        Some(_) if info.is_sentinel() => {
            // Sentinel variant with stored code - include directly
            let doc = &info.description;
            SpecVariant {
                tokens: quote! { #[doc = #doc] #ident = #code },
                merge_target: None,
            }
        }
        Some(ty) => {
            // Wrapped variant - create namespace marker
            let ns_code = code << INNER_BITS;
            let ty_str = quote!(#ty).to_string();

            // For FCC variants, use the TYPE name for merge target (matches contractimport_with_errors!)
            // e.g., Math(MathError) -> MathError_Flattened
            // For other wrapped variants, use OuterEnum_VariantName
            let flattened_name = if info.is_fcc() {
                if let Some(type_name) = info.wrapped_type_name() {
                    format!("{}_Flattened", type_name)
                } else {
                    format!("{}_{}", enum_name, ident)
                }
            } else {
                format!("{}_{}", enum_name, ident)
            };

            let doc = format!(
                "Namespace for `{}` errors.\n\n\
                 **Merge Required:** Combine with `{}` for full error details.\n\
                 Inner type: `{}`. Codes: [{}, {}).",
                ident,
                flattened_name,
                ty_str,
                ns_code,
                ns_code + INNER_MASK + 1
            );

            let ns_ident = format_ident!("{}Namespace", ident);
            SpecVariant {
                tokens: quote! { #[doc = #doc] #ns_ident = #ns_code },
                merge_target: Some(flattened_name),
            }
        }
    }
}

/// Generate the TypeScript usage documentation for merging error specs.
fn generate_ts_merge_docs(enum_name: &Ident, merge_targets: &[String]) -> String {
    if merge_targets.is_empty() {
        return String::new();
    }

    let spreads: String = std::iter::once(format!("  ...{},", enum_name))
        .chain(merge_targets.iter().map(|t| format!("  ...{},", t)))
        .collect::<Vec<_>>()
        .join("\n");

    format!(
        "\n\n## TypeScript Usage\n\
         To handle all errors, merge this object with the generated flattened specs:\n\
         ```typescript\n\
         const All{}Errors = {{\n{}\n}};\n\
         ```",
        enum_name, spreads
    )
}

/// Generate a spec-only companion enum for root mode.
///
/// Since root mode enums have data-carrying variants, they can't use `#[contracterror]`
/// directly. This function generates a companion enum with unit variants that captures
/// the error code structure for spec generation.
///
/// The companion enum includes:
/// - All unit variants from the original enum (with their codes)
/// - Namespace marker variants for wrapped variants (indicating the namespace)
///
/// For wrapped variants (like `Math(MathError)`), the flattened error codes are
/// generated separately by `contractimport_with_errors!` with the `scerr_variant`
/// parameter. The generated docstring includes a TypeScript usage example showing
/// how to merge the specs.
fn generate_spec_companion(name: &Ident, infos: &[VariantInfo]) -> proc_macro2::TokenStream {
    // Use {Name}Spec as the actual enum name in XDR spec to avoid collision
    // with the unified spec which uses {Name}
    let spec_name = format_ident!("{}Spec", name);
    let mod_name = format_ident!("__scerr_spec_{}", name.to_string().to_lowercase());

    // Process all variants and collect merge targets
    let processed: Vec<_> = infos
        .iter()
        .map(|info| process_spec_variant(info, name))
        .collect();

    let spec_variants: Vec<_> = processed.iter().map(|v| &v.tokens).collect();
    let merge_targets: Vec<_> = processed
        .iter()
        .filter_map(|v| v.merge_target.as_ref())
        .cloned()
        .collect();

    let ts_docs = generate_ts_merge_docs(name, &merge_targets);
    let enum_doc = format!(
        "Spec-only enum for contract spec generation.\n\
         Provides TypeScript bindings with error code information for `{}`.{}",
        name, ts_docs
    );

    quote! {
        #[doc(hidden)]
        pub mod #mod_name {
            #[doc = #enum_doc]
            #[soroban_sdk::contracterror]
            #[repr(u32)]
            #[derive(Copy, Clone, Debug, Eq, PartialEq)]
            #[allow(dead_code)]
            pub enum #spec_name {
                #(#spec_variants),*
            }
        }

        #[doc(hidden)]
        pub use #mod_name::#spec_name;
    }
}

/// Generate the unified flattened spec using declarative macros.
///
/// This emits a `__build_unified_spec!` macro call that:
/// 1. Collects unit variants from the root enum
/// 2. Calls getter macros for each FCC variant's inner type
/// 3. Generates a `#[contracterror]` enum with all flattened variants
///
/// The getter macros are generated by `contractimport_with_errors!` and named
/// `__scerr_{TypeName}_variants` (e.g., `__scerr_MathError_variants`).
fn generate_unified_spec_call(
    name: &Ident,
    infos: &[VariantInfo],
    explicit_mixins: &PathList,
) -> syn::Result<proc_macro2::TokenStream> {
    // Collect getter macro paths from FCC variants
    // For `#[from_contract_client] Math(math_imported::MathError)`,
    // we derive `math_imported::__scerr_MathError_variants`
    let fcc_getters: Vec<proc_macro2::TokenStream> = infos
        .iter()
        .filter(|i| i.is_fcc())
        .filter_map(|i| {
            // Get the type path (e.g., math_imported::MathError)
            let syn::Type::Path(type_path) = i.field_ty()? else {
                return None;
            };
            // Extract module path and type name
            let segments = &type_path.path.segments;
            if segments.len() >= 2 {
                // Module is all but the last segment
                let module_segments: Vec<_> = segments.iter().take(segments.len() - 1).collect();
                // Type name is the last segment
                let type_name = &segments.last()?.ident;
                let getter_name = format_ident!("__scerr_{}_variants", type_name);

                // Build the full path: module::__scerr_TypeName_variants
                let module_path = module_segments.iter().map(|s| &s.ident);
                return Some(quote! { #(#module_path)::* :: #getter_name });
            }
            None
        })
        .collect();

    // Also include explicit mixins
    let explicit_getters: Vec<proc_macro2::TokenStream> =
        explicit_mixins.0.iter().map(|p| quote! { #p }).collect();

    // Combine FCC getters and explicit mixins
    let all_getters: Vec<_> = fcc_getters.into_iter().chain(explicit_getters).collect();

    // If no getters, no unified spec needed
    if all_getters.is_empty() {
        return Ok(quote! {});
    }

    // Build unit variants in the new format: { name: Ident, code: u32, doc: "..." }
    let unit_variants: Vec<_> = infos
        .iter()
        .filter(|i| i.field_ty().is_none() || i.is_sentinel())
        .map(|i| {
            let ident = &i.ident;
            let code = i.code;
            let doc = &i.description;
            quote! { { name: #ident, code: #code, doc: #doc } }
        })
        .collect();

    Ok(quote! {
        soroban_sdk_tools::__build_unified_spec! {
            @name #name
            @unit [ #(#unit_variants),* ]
            @mixins [ #(#all_getters),* ]
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
    let doc_attrs = extract_doc_attrs(&input.attrs);
    let has_data = infos.iter().any(|i| i.field_ty().is_some());
    let enum_def = generate_enum_def(name, &infos, has_data, &doc_attrs);
    let contract_error_impl = generate_contract_error_impl_root(name, &infos);
    let from_impls = generate_from_trait_impls(name, &infos)?;
    let invoke_impl = generate_invoke_error_impl(name, &infos);
    let soroban_impls = generate_soroban_conversions(name);
    let double_q_impls = generate_double_q_impls(name, &infos);
    let logging_impl = generate_logging_impl(name, &infos, config.log_unknown_errors, input)?;

    // Generate unified spec when FCC variants exist with getter macros, or mixins are specified
    let unified_spec = generate_unified_spec_call(name, &infos, &config.mixins)?;

    // Only generate spec companion (with namespace markers) if unified spec wasn't generated.
    // When unified spec exists, it contains all flattened variants and the spec companion
    // would be redundant.
    let spec_companion = if unified_spec.is_empty() {
        generate_spec_companion(name, &infos)
    } else {
        quote! {}
    };

    Ok(quote! {
        #enum_def
        #contract_error_impl
        #from_impls
        #invoke_impl
        #soroban_impls
        #double_q_impls
        #logging_impl
        #spec_companion
        #unified_spec
    })
}

/// Generate ContractErrorSpec implementation for an error enum.
fn generate_error_spec_impl(name: &Ident, infos: &[VariantInfo]) -> proc_macro2::TokenStream {
    let spec_entries: Vec<_> = infos
        .iter()
        .filter(|i| i.field_ty().is_none()) // Only unit variants for basic mode
        .map(|i| {
            let code = i.code;
            let variant_name = i.ident.to_string();
            let desc = &i.description;
            quote! {
                soroban_sdk_tools::error::ErrorSpecEntry {
                    code: #code,
                    name: #variant_name,
                    description: #desc,
                }
            }
        })
        .collect();

    quote! {
        impl soroban_sdk_tools::error::ContractErrorSpec for #name {
            const SPEC_ENTRIES: &'static [soroban_sdk_tools::error::ErrorSpecEntry] = &[
                #(#spec_entries),*
            ];
        }
    }
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

    let desc_arms: Vec<_> = infos
        .iter()
        .map(|i| {
            let ident = &i.ident;
            let d = &i.description;
            quote! { #name::#ident => #d }
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
            fn description(&self) -> &'static str {
                match *self {
                    #(#desc_arms,)*
                }
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

    let infos = collect_variant_infos(data_enum, &input.ident, mode)?;

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

    #[test]
    fn test_detect_mode_root_for_abort_attr() {
        let item: syn::ItemEnum = parse_quote! {
            enum Test { A, #[abort] Aborted }
        };
        let data = DataEnum {
            enum_token: item.enum_token,
            brace_token: item.brace_token,
            variants: item.variants,
        };
        assert_eq!(detect_mode(&data), ScerrMode::Root);
    }

    #[test]
    fn test_detect_mode_root_for_sentinel_attr() {
        let item: syn::ItemEnum = parse_quote! {
            enum Test { A, #[sentinel] Unknown }
        };
        let data = DataEnum {
            enum_token: item.enum_token,
            brace_token: item.brace_token,
            variants: item.variants,
        };
        assert_eq!(detect_mode(&data), ScerrMode::Root);
    }
}
