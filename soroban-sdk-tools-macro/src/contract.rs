//! Implementation of the `#[contracttrait]` macro.
//!
//! This macro generates a two-trait structure from a single trait definition:
//!
//! - **Internal trait** (`{Trait}Internal`): Pure business logic that developers implement
//! - **Outer trait** (`{Trait}`): Auth-enforced wrapper with `type Provider` for DI
//!
//! Methods annotated with `#[auth(Self::method)]` get structural auth enforcement:
//! the generated outer trait's default method calls `require_auth()` on the
//! resolved address before delegating to the inner implementation.
//!
//! Additionally generates:
//! - `{Trait}AuthClient` for simplified auth testing
//! - `impl_{trait_snake}!` helper macro for sealed (non-overridable) auth wiring
//!
//! # Security Model
//!
//! ## What is structurally enforced
//! - When using `impl_{trait_snake}!`, auth methods are generated as direct
//!   `#[contractimpl]` methods, NOT as trait defaults. They cannot be overridden.
//! - The `{Trait}Internal` trait has no auth logic, so implementing it cannot
//!   accidentally bypass auth checks.
//!
//! ## What is convention-based (not enforced)
//! - When using `#[contractimpl(contracttrait)]` directly, a developer CAN
//!   override the outer trait's default methods, potentially bypassing auth.
//! - A developer can call `{Trait}Internal` methods directly from any
//!   `#[contractimpl]` block, bypassing the auth wrapper.
//! - Storage key isolation between composed traits depends on using
//!   `#[contractstorage]` with proper key management.
//!
//! For maximum security, use `impl_{trait_snake}!` instead of manually writing
//! `#[contractimpl(contracttrait)]` blocks.

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{
    parse2, parse_quote, Attribute, Error, Expr, FnArg, Ident, ItemTrait, Pat, ReturnType,
    Signature, TraitItem, TraitItemFn,
};

// -----------------------------------------------------------------------------
// Auth attribute parsing
// -----------------------------------------------------------------------------

/// Parsed `#[auth(Self::method)]` or `#[auth(param_name)]` annotation.
#[derive(Debug, Clone)]
enum AuthSource {
    /// `#[auth(Self::method)]` -- call method on the provider to get the address
    ProviderMethod(Ident),
    /// `#[auth(param_name)]` -- use a function parameter directly
    Param(Ident),
}

/// Extract `#[auth(...)]` from a method's attributes, if present.
fn extract_auth_attr(attrs: &[Attribute]) -> syn::Result<Option<AuthSource>> {
    for attr in attrs {
        if attr.path().is_ident("auth") {
            let expr: Expr = attr.parse_args()?;
            match &expr {
                // Self::method
                Expr::Path(ep) if ep.path.segments.len() == 2 => {
                    let first = &ep.path.segments[0].ident;
                    let second = &ep.path.segments[1].ident;
                    if first != "Self" {
                        return Err(Error::new_spanned(
                            &expr,
                            "expected `Self::method_name` or a parameter name",
                        ));
                    }
                    return Ok(Some(AuthSource::ProviderMethod(second.clone())));
                }
                // param_name
                Expr::Path(ep) if ep.path.segments.len() == 1 => {
                    let name = &ep.path.segments[0].ident;
                    return Ok(Some(AuthSource::Param(name.clone())));
                }
                _ => {
                    return Err(Error::new_spanned(
                        &expr,
                        "expected `Self::method_name` or a parameter name",
                    ));
                }
            }
        }
    }
    Ok(None)
}

/// Strip `#[auth(...)]` attributes from a list of attributes.
fn strip_auth_attrs(attrs: &[Attribute]) -> Vec<Attribute> {
    attrs
        .iter()
        .filter(|a| !a.path().is_ident("auth"))
        .cloned()
        .collect()
}

// -----------------------------------------------------------------------------
// Trait method info extraction
// -----------------------------------------------------------------------------

/// Information about a trait method for code generation.
struct MethodInfo {
    /// Method name
    name: Ident,
    /// Full signature (without attrs)
    sig: Signature,
    /// Doc comments and other non-auth attributes
    attrs: Vec<Attribute>,
    /// Auth source if `#[auth]` was present
    auth: Option<AuthSource>,
    /// The function parameters (excluding `env`)
    params: Vec<ParamInfo>,
    /// Whether the env parameter is a reference (&Env vs Env)
    #[allow(dead_code)]
    env_is_ref: bool,
}

/// Information about a function parameter.
struct ParamInfo {
    name: Ident,
    ty: syn::Type,
    _is_ref: bool,
}

/// Check if a type is `Env` or `&Env`.
fn is_env_type(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Path(tp) => tp
            .path
            .segments
            .last()
            .map_or(false, |s| s.ident == "Env"),
        syn::Type::Reference(r) => is_env_type(&r.elem),
        _ => false,
    }
}

/// Extract method info from a trait function item.
fn extract_method_info(method: &TraitItemFn) -> syn::Result<MethodInfo> {
    let auth = extract_auth_attr(&method.attrs)?;
    let attrs = strip_auth_attrs(&method.attrs);
    let sig = method.sig.clone();
    let name = sig.ident.clone();

    let mut params = Vec::new();
    let mut env_is_ref = false;
    let mut first = true;

    for arg in &sig.inputs {
        if let FnArg::Typed(pat_type) = arg {
            if first && is_env_type(&pat_type.ty) {
                first = false;
                env_is_ref = matches!(*pat_type.ty, syn::Type::Reference(_));
                continue; // skip env parameter
            }
            first = false;

            let param_name = if let Pat::Ident(pi) = &*pat_type.pat {
                pi.ident.clone()
            } else {
                return Err(Error::new_spanned(
                    &pat_type.pat,
                    "expected a simple identifier pattern",
                ));
            };

            let (ty, is_ref) = match &*pat_type.ty {
                syn::Type::Reference(r) => (*r.elem.clone(), true),
                other => (other.clone(), false),
            };

            params.push(ParamInfo {
                name: param_name,
                ty,
                _is_ref: is_ref,
            });
        }
    }

    Ok(MethodInfo {
        name,
        sig,
        attrs,
        auth,
        params,
        env_is_ref,
    })
}

// -----------------------------------------------------------------------------
// Inner trait generation
// -----------------------------------------------------------------------------

/// Generate the inner `{Trait}Internal` trait with business-logic-only method signatures.
fn generate_internal_trait(
    trait_name: &Ident,
    methods: &[MethodInfo],
    supertraits: &TokenStream2,
    vis: &syn::Visibility,
    trait_attrs: &[Attribute],
) -> TokenStream2 {
    let internal_trait_name = format_ident!("{}Internal", trait_name);

    let method_sigs: Vec<_> = methods
        .iter()
        .map(|m| {
            let attrs = &m.attrs;
            let sig = &m.sig;
            quote! { #(#attrs)* #sig; }
        })
        .collect();

    // Filter doc attrs from the trait for the internal trait
    let doc_attrs: Vec<_> = trait_attrs
        .iter()
        .filter(|a| a.path().is_ident("doc"))
        .collect();

    quote! {
        #(#doc_attrs)*
        #vis trait #internal_trait_name #supertraits {
            #(#method_sigs)*
        }
    }
}

// -----------------------------------------------------------------------------
// Outer trait generation (auth-wrapped defaults)
// -----------------------------------------------------------------------------

/// Generate the outer `{Trait}` trait with auth-enforced default implementations.
fn generate_outer_trait(
    trait_name: &Ident,
    methods: &[MethodInfo],
    vis: &syn::Visibility,
    trait_attrs: &[Attribute],
    sdk_passthrough_attrs: &TokenStream2,
) -> TokenStream2 {
    let internal_trait_name = format_ident!("{}Internal", trait_name);

    let default_methods: Vec<_> = methods
        .iter()
        .map(|m| {
            let attrs = &m.attrs;
            let sig = &m.sig;
            let method_name = &m.name;

            // Build delegation args
            let delegate_args = build_delegate_args(m);

            let env_arg = quote! { env };

            // Build auth check (if #[auth] present)
            // Cache the auth address to avoid redundant storage reads
            let auth_check = match &m.auth {
                Some(AuthSource::ProviderMethod(resolver)) => {
                    quote! {
                        let __auth_addr = Self::Provider::#resolver(#env_arg);
                        __auth_addr.require_auth();
                    }
                }
                Some(AuthSource::Param(param)) => {
                    quote! {
                        #param.require_auth();
                    }
                }
                None => quote! {},
            };

            quote! {
                #(#attrs)*
                #sig {
                    #auth_check
                    Self::Provider::#method_name(#delegate_args)
                }
            }
        })
        .collect();

    // Filter attrs
    let non_doc_attrs: Vec<_> = trait_attrs
        .iter()
        .filter(|a| !a.path().is_ident("doc") && !a.path().is_ident("auth"))
        .collect();
    let doc_attrs: Vec<_> = trait_attrs
        .iter()
        .filter(|a| a.path().is_ident("doc"))
        .collect();

    quote! {
        #(#doc_attrs)*
        #(#non_doc_attrs)*
        #[soroban_sdk::contracttrait(#sdk_passthrough_attrs)]
        #vis trait #trait_name {
            type Provider: #internal_trait_name;

            #(#default_methods)*
        }
    }
}

/// Build the argument list for delegating to `Self::Provider::method(...)`.
fn build_delegate_args(method: &MethodInfo) -> TokenStream2 {
    let mut args = Vec::new();

    let env_ident: Ident = parse_quote!(env);
    args.push(quote! { #env_ident });

    for p in &method.params {
        let name = &p.name;
        args.push(quote! { #name });
    }

    quote! { #(#args),* }
}

// -----------------------------------------------------------------------------
// Sealed impl macro generation
// -----------------------------------------------------------------------------

/// Generate `impl_{trait_snake}!` helper macro for non-overridable auth wiring.
///
/// This produces a `macro_rules!` that generates a `#[contractimpl]` block
/// (NOT `#[contractimpl(contracttrait)]`) with auth methods baked in.
/// Since these are inherent methods, not trait defaults, they cannot be overridden.
fn generate_sealed_impl_macro(
    trait_name: &Ident,
    methods: &[MethodInfo],
) -> TokenStream2 {
    let internal_trait_name = format_ident!("{}Internal", trait_name);
    let trait_snake = to_snake_case(&trait_name.to_string());
    let macro_name = format_ident!("impl_{}", trait_snake);

    // Generate method bodies for each method
    let method_impls: Vec<_> = methods
        .iter()
        .map(|m| {
            let method_name = &m.name;
            let sig = &m.sig;
            let delegate_args = build_delegate_args(m);
            let attrs = &m.attrs;

            let auth_check = match &m.auth {
                Some(AuthSource::ProviderMethod(resolver)) => {
                    quote! {
                        let __auth_addr = <$provider as #internal_trait_name>::#resolver(env);
                        __auth_addr.require_auth();
                    }
                }
                Some(AuthSource::Param(param)) => {
                    quote! { #param.require_auth(); }
                }
                None => quote! {},
            };

            quote! {
                #(#attrs)*
                pub #sig {
                    #auth_check
                    <$provider as #internal_trait_name>::#method_name(#delegate_args)
                }
            }
        })
        .collect();

    quote! {
        /// Sealed implementation macro for non-overridable auth enforcement.
        ///
        /// Usage: `impl_#trait_snake!(MyContract, MyProvider);`
        ///
        /// This generates a `#[contractimpl]` block where auth methods are
        /// inherent methods (not trait defaults), making them impossible to override.
        #[macro_export]
        macro_rules! #macro_name {
            ($contract:ty, $provider:ty) => {
                #[soroban_sdk::contractimpl]
                impl $contract {
                    #(#method_impls)*
                }
            };
        }
    }
}

/// Convert PascalCase to snake_case.
fn to_snake_case(s: &str) -> String {
    let mut result = String::new();
    for (i, c) in s.chars().enumerate() {
        if c.is_uppercase() {
            if i > 0 {
                result.push('_');
            }
            result.push(c.to_lowercase().next().unwrap());
        } else {
            result.push(c);
        }
    }
    result
}

// -----------------------------------------------------------------------------
// AuthClient generation (for traits)
// -----------------------------------------------------------------------------

/// Generate `{Trait}AuthClient` from trait method signatures.
fn generate_auth_client(
    trait_name: &Ident,
    methods: &[MethodInfo],
) -> TokenStream2 {
    let auth_client_name = format_ident!("{}AuthClient", trait_name);
    let client_name = format_ident!("{}Client", trait_name);
    let alloc_alias = format_ident!("__alloc_{}", trait_name);

    let client_methods: Vec<_> = methods
        .iter()
        .filter(|m| !m.name.to_string().starts_with("__"))
        .map(|m| generate_auth_client_method(m, &alloc_alias))
        .collect();

    quote! {
        #[cfg(not(target_family = "wasm"))]
        extern crate alloc as #alloc_alias;

        /// Auth-testing wrapper client for simplified authorization testing.
        ///
        /// This client wraps the standard `Client` and provides methods that
        /// return a `CallBuilder` for fluent authorization setup.
        #[cfg(not(target_family = "wasm"))]
        pub struct #auth_client_name<'a> {
            inner: #client_name<'a>,
        }

        #[cfg(not(target_family = "wasm"))]
        impl<'a> #auth_client_name<'a> {
            /// Create a new AuthClient wrapping a contract at the given address.
            pub fn new(env: &'a soroban_sdk::Env, address: &'a soroban_sdk::Address) -> Self {
                Self {
                    inner: #client_name::new(env, address),
                }
            }

            /// Get a reference to the inner client.
            pub fn inner(&self) -> &#client_name<'a> {
                &self.inner
            }

            /// Get the environment reference.
            pub fn env(&self) -> &soroban_sdk::Env {
                &self.inner.env
            }

            /// Get the contract address.
            pub fn address(&self) -> &soroban_sdk::Address {
                &self.inner.address
            }

            #(#client_methods)*
        }
    }
}

/// Generate a single AuthClient method that returns a CallBuilder.
fn generate_auth_client_method(
    method: &MethodInfo,
    alloc_alias: &Ident,
) -> TokenStream2 {
    let fn_name = &method.name;
    let try_fn_name = format_ident!("try_{}", fn_name);
    let fn_name_str = fn_name.to_string();

    let params: Vec<_> = method
        .params
        .iter()
        .map(|p| {
            let name = &p.name;
            let ty = &p.ty;
            quote! { #name: &'b #ty }
        })
        .collect();

    let arg_clones: Vec<_> = method
        .params
        .iter()
        .map(|p| {
            let name = &p.name;
            let clone_name = format_ident!("{}_clone", name);
            quote! { let #clone_name = #name.clone(); }
        })
        .collect();

    let try_arg_clones: Vec<_> = method
        .params
        .iter()
        .map(|p| {
            let name = &p.name;
            let clone_name = format_ident!("{}_try_clone", name);
            quote! { let #clone_name = #name.clone(); }
        })
        .collect();

    let clone_names: Vec<_> = method
        .params
        .iter()
        .map(|p| format_ident!("{}_clone", p.name))
        .collect();

    let try_clone_names: Vec<_> = method
        .params
        .iter()
        .map(|p| format_ident!("{}_try_clone", p.name))
        .collect();

    let args_tuple: Vec<_> = method
        .params
        .iter()
        .map(|p| {
            let name = &p.name;
            quote! { #name.clone() }
        })
        .collect();

    let args_tuple_expr = if args_tuple.is_empty() {
        quote! { () }
    } else if args_tuple.len() == 1 {
        let arg = &args_tuple[0];
        quote! { (#arg,) }
    } else {
        quote! { (#(#args_tuple),*) }
    };

    let (return_ty, try_return_ty) = extract_return_types(&method.sig.output);

    let doc_attrs: Vec<_> = method
        .attrs
        .iter()
        .filter(|a| a.path().is_ident("doc"))
        .collect();

    quote! {
        #(#doc_attrs)*
        pub fn #fn_name<'b>(&'b self, #(#params),*) -> soroban_sdk_tools::auth::CallBuilder<'b, #return_ty, #try_return_ty>
        where
            'a: 'b,
        {
            use soroban_sdk::IntoVal;

            let args: soroban_sdk::Vec<soroban_sdk::Val> = #args_tuple_expr.into_val(&self.inner.env);

            #(#arg_clones)*
            #(#try_arg_clones)*

            let inner = &self.inner;
            let invoker = #alloc_alias::boxed::Box::new(move || {
                inner.#fn_name(#(&#clone_names),*)
            });
            let inner2 = &self.inner;
            let try_invoker = #alloc_alias::boxed::Box::new(move || {
                inner2.#try_fn_name(#(&#try_clone_names),*)
            });

            soroban_sdk_tools::auth::CallBuilder::new(
                &self.inner.env,
                &self.inner.address,
                #fn_name_str,
                args,
                invoker,
                Some(try_invoker),
            )
        }
    }
}

/// Extract return type and try-return type from a method's ReturnType.
fn extract_return_types(output: &ReturnType) -> (TokenStream2, TokenStream2) {
    match output {
        ReturnType::Default => {
            let ret = quote! { () };
            let try_ret = quote! {
                Result<
                    Result<(), <() as soroban_sdk::TryFromVal<soroban_sdk::Env, soroban_sdk::Val>>::Error>,
                    Result<soroban_sdk::Error, soroban_sdk::InvokeError>
                >
            };
            (ret, try_ret)
        }
        ReturnType::Type(_, ty) => {
            if let Some((ok_ty, err_ty)) = unpack_result_type(ty) {
                let try_ret = quote! {
                    Result<
                        Result<#ok_ty, <#ok_ty as soroban_sdk::TryFromVal<soroban_sdk::Env, soroban_sdk::Val>>::Error>,
                        Result<#err_ty, soroban_sdk::InvokeError>
                    >
                };
                (quote! { #ok_ty }, try_ret)
            } else {
                let try_ret = quote! {
                    Result<
                        Result<#ty, <#ty as soroban_sdk::TryFromVal<soroban_sdk::Env, soroban_sdk::Val>>::Error>,
                        Result<soroban_sdk::Error, soroban_sdk::InvokeError>
                    >
                };
                (quote! { #ty }, try_ret)
            }
        }
    }
}

/// Unpack `Result<T, E>` into `Some((T, E))`, or None if not a Result.
fn unpack_result_type(ty: &syn::Type) -> Option<(syn::Type, syn::Type)> {
    if let syn::Type::Path(type_path) = ty {
        if let Some(segment) = type_path.path.segments.last() {
            if segment.ident == "Result" {
                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                    if args.args.len() == 2 {
                        if let (
                            syn::GenericArgument::Type(ok_ty),
                            syn::GenericArgument::Type(err_ty),
                        ) = (&args.args[0], &args.args[1])
                        {
                            return Some((ok_ty.clone(), err_ty.clone()));
                        }
                    }
                }
            }
        }
    }
    None
}

// -----------------------------------------------------------------------------
// Supertrait handling
// -----------------------------------------------------------------------------

/// Extract supertraits and map to Internal trait names.
/// e.g., `Pausable: Ownable` becomes `PausableInternal: OwnableInternal`
fn map_supertraits_to_internal(trait_def: &ItemTrait) -> TokenStream2 {
    let bounds: Vec<_> = trait_def
        .supertraits
        .iter()
        .filter_map(|bound| {
            if let syn::TypeParamBound::Trait(tb) = bound {
                if let Some(seg) = tb.path.segments.last() {
                    let internal_name = format_ident!("{}Internal", seg.ident);
                    Some(quote! { #internal_name })
                } else {
                    None
                }
            } else {
                None
            }
        })
        .collect();

    if bounds.is_empty() {
        quote! {}
    } else {
        quote! { : #(#bounds)+* }
    }
}

// -----------------------------------------------------------------------------
// Main macro entry point
// -----------------------------------------------------------------------------

/// Main implementation of `#[contracttrait]`.
pub fn contracttrait_impl(attr: TokenStream, item: TokenStream) -> TokenStream {
    let attr2: TokenStream2 = attr.into();
    let item2: TokenStream2 = item.into();

    match contracttrait_inner(attr2, item2) {
        Ok(tokens) => tokens.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

fn contracttrait_inner(attr: TokenStream2, item: TokenStream2) -> syn::Result<TokenStream2> {
    let trait_def: ItemTrait = parse2(item)?;

    let trait_name = &trait_def.ident;
    let vis = &trait_def.vis;

    let trait_attrs: Vec<_> = trait_def
        .attrs
        .iter()
        .filter(|a| !a.path().is_ident("auth"))
        .cloned()
        .collect();

    let mut methods = Vec::new();

    for item in &trait_def.items {
        if let TraitItem::Fn(method) = item {
            methods.push(extract_method_info(method)?);
        }
    }

    let internal_supertraits = map_supertraits_to_internal(&trait_def);

    let internal_trait =
        generate_internal_trait(trait_name, &methods, &internal_supertraits, vis, &trait_attrs);

    let outer_trait = generate_outer_trait(trait_name, &methods, vis, &trait_attrs, &attr);

    let auth_client = generate_auth_client(trait_name, &methods);

    // Generate sealed impl macro only if there are auth methods
    let has_auth_methods = methods.iter().any(|m| m.auth.is_some());
    let sealed_macro = if has_auth_methods {
        generate_sealed_impl_macro(trait_name, &methods)
    } else {
        quote! {}
    };

    Ok(quote! {
        #internal_trait

        #outer_trait

        #auth_client

        #sealed_macro
    })
}
