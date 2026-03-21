# Programming Language Research Analysis: soroban-sdk-tools `#[contracttrait]`

**Author:** PL Research Perspective (Type Systems, Effect Systems, Capability Security, Macro Metaprogramming)
**Date:** 2026-03-21
**Scope:** Deep technical analysis of the two-trait split, `#[auth]` annotation, provider-based composition, and sealed macro pattern.

---

## Table of Contents

1. [Type-Theoretic Analysis of the Two-Trait Split](#1-type-theoretic-analysis-of-the-two-trait-split)
2. [Effect System Perspective on `#[auth]`](#2-effect-system-perspective-on-auth)
3. [Capability-Based Security Analysis](#3-capability-based-security-analysis)
4. [Macro Hygiene Concerns](#4-macro-hygiene-concerns)
5. [Formal Verification Opportunities](#5-formal-verification-opportunities)
6. [Specific Code Improvements](#6-specific-code-improvements)
7. [Research Directions](#7-research-directions)

---

## 1. Type-Theoretic Analysis of the Two-Trait Split

### 1.1. The Encoding as a Product in a Slice Category

The `#[contracttrait]` macro implements a transformation that, in type-theoretic terms, factors a trait $T$ into a product of two components in the category of Rust traits:

$$T \xrightarrow{\text{macro}} T_{\text{Internal}} \times_{\text{Provider}} T_{\text{Outer}}$$

where the fibered product is taken over the `Provider` associated type, which mediates the connection. This is a faithful encoding in the following precise sense:

**Definition (Faithfulness).** The two-trait encoding is *faithful* if for every well-typed implementation of $T_{\text{Internal}}$ by a provider $P$, and every contract $C$ binding $\text{Provider} = P$, the observable behavior of $C$ through the $T_{\text{Outer}}$ interface is identical to a hypothetical monolithic $T$ where the auth checks from `#[auth]` annotations are manually inlined.

**Theorem (Behavioral Equivalence, informal).** Under the assumptions that (a) the provider's auth-source methods are pure (deterministic, side-effect-free reads of contract state), and (b) `require_auth()` is a total function that either succeeds or traps, the two-trait encoding is faithful.

*Sketch.* For each method $m$ with `#[auth(Self::resolver)]`, the outer trait generates:
```
let addr = P::resolver(env);
addr.require_auth();
P::m(env, args)
```
This is observationally equivalent to inlining `P::resolver(env).require_auth()` before `P::m(env, args)` in a monolithic trait, given purity of `resolver`. QED (modulo formalization).

### 1.2. What the Encoding Guarantees

The two-trait split provides three type-level guarantees:

1. **Separation of concerns (syntactic guarantee):** Business logic (in `Internal`) is textually separated from auth enforcement (in the outer trait). This is enforced by the macro -- there is no way to write auth logic inside `Internal` through the macro pathway.

2. **Provider polymorphism (parametric guarantee):** The `type Provider: TInternal` associated type ensures that any conforming provider can be substituted without changing the outer trait's auth logic. This is parametric polymorphism at the trait level -- the outer trait is parametric in its implementation strategy.

3. **Auth presence (syntactic guarantee, sealed path):** When using `impl_trait!`, the auth check is present in the generated code as an inherent method. Rust's coherence rules guarantee that inherent methods cannot be overridden.

### 1.3. What the Encoding Does NOT Guarantee

1. **Auth correctness:** The type system does not verify that `Self::Provider::owner(env)` returns the "intended" authorizer. Semantic correctness of the provider is beyond the reach of the encoding.

2. **Encapsulation of Internal:** The `Internal` trait is public. Any code in scope can call `P::transfer_ownership(env, addr)` directly, bypassing the outer trait entirely. The encoding provides no information hiding at the type level.

3. **Composition consistency:** When `Pausable: Ownable` generates `PausableInternal: OwnableInternal`, the type system ensures that a single provider $P$ implements both. But it does not ensure that $P$'s `owner()` implementation is consistent across both -- e.g., it could use different storage keys.

4. **Totality of auth sources:** If `Self::Provider::owner(env)` panics (e.g., storage not initialized), the auth check never reaches `require_auth()`, and the method traps. The encoding does not distinguish "auth rejected" from "auth source unavailable."

### 1.4. Comparison with Dependent Types

In a language with dependent types (Idris, Agda, Lean), we could express the encoding more precisely:

$$\text{Guarded}(m, g) : \Pi (e : \text{Env}). (\text{auth\_ok}(g(e)) = \text{true}) \to \text{Result}(m(e))$$

where the `auth_ok` predicate is a proof obligation carried in the type. Rust cannot express this because it lacks dependent function types. The `require_auth()` call is a runtime check that substitutes for the missing compile-time proof. This is the fundamental limitation: the encoding approximates a dependent type with a runtime assertion.

### 1.5. The Associated Type as a Type-Level Function

The `type Provider: TInternal` associated type is, from a type-theoretic perspective, a type-level function:

$$\text{Provider} : \text{Contract} \to \text{ProviderType}$$

This is a defunctionalized representation of what would be a type-level lambda in System $F_\omega$. The defunctionalization is forced by Rust's trait system, which does not support higher-kinded types directly. The consequence is that each contract can have at most one provider per trait -- there is no way to express "this contract uses different providers for different invocations."

---

## 2. Effect System Perspective on `#[auth]`

### 2.1. Auth as an Algebraic Effect

The `#[auth(Self::owner)]` annotation can be understood as declaring an *effect* on the method. In the language of algebraic effects (as in Koka, Effekt, or Frank), the annotation declares:

$$\text{transfer\_ownership} : \text{Env} \times \text{Address} \xrightarrow{\text{Auth}(\text{owner})} ()$$

where $\text{Auth}(\text{owner})$ is an effect that must be handled before the computation can proceed. The "handler" for this effect is `require_auth()`, which either succeeds (resuming the computation) or traps (aborting).

### 2.2. What a Proper Effect Encoding Would Look Like in Rust

Rust does not have native effect support. However, we can approximate algebraic effects using the typestate pattern combined with phantom types:

```rust
// Effect declaration
struct AuthEffect<Source>(PhantomData<Source>);

// Unhandled effect -- computation cannot proceed
struct Unhandled<E>(PhantomData<E>);

// Handled effect -- computation can proceed
struct Handled<E>(PhantomData<E>);

// Method with declared effect
trait OwnableInternal {
    fn transfer_ownership(
        env: &Env,
        new_owner: Address,
        _auth: Handled<AuthEffect<OwnerSource>>, // proof of handling
    );
}
```

In this encoding, the caller must produce a `Handled<AuthEffect<OwnerSource>>` value before calling the method. The outer trait would be the effect handler:

```rust
fn transfer_ownership(env: &Env, new_owner: Address) {
    let addr = Self::Provider::owner(env);
    addr.require_auth(); // may trap
    let proof = Handled::<AuthEffect<OwnerSource>>(PhantomData);
    Self::Provider::transfer_ownership(env, new_owner, proof)
}
```

### 2.3. Advantages of the Effect Encoding

1. **The proof parameter is unforgeable.** Only the effect handler can construct `Handled<AuthEffect<_>>`. This closes the Internal-trait-bypass vulnerability: you cannot call the Internal method without producing the proof.

2. **Effects compose.** A method requiring multiple guards would take multiple proof parameters, each independently satisfiable.

3. **Effects are visible in the type.** The function signature documents what effects have been discharged.

### 2.4. Disadvantages / Impracticality in Current Rust

1. **Ergonomic cost.** Adding phantom parameters to every method signature is noisy. The macro would need to manage these parameters in both the Internal and outer traits.

2. **Soroban SDK compatibility.** The SDK's `contracttrait` macro expects specific method signatures (first parameter is `Env`). Adding effect witness parameters would break compatibility.

3. **No effect polymorphism.** Rust cannot express "this function has effect $E$ plus whatever effects $F$ has," which limits composability.

### 2.5. Recommendation

The current approach (runtime `require_auth()` as a de facto effect handler) is pragmatically correct for Rust. However, the documentation should frame `#[auth]` explicitly as an effect annotation. This would:

- Clarify the mental model for developers familiar with effect systems
- Position the pattern for a future where Rust may gain limited effect support (via keyword generics or similar)
- Connect to the broader PL literature on effects and handlers

---

## 3. Capability-Based Security Analysis

### 3.1. The Provider Pattern as Object-Capability Discipline

In the object-capability (ocap) model (Mark Miller's E language, Caja, early Wasm proposals), access to a resource is mediated by possession of a *capability* -- an unforgeable reference that combines designation (naming the resource) with authorization (permission to use it).

The Provider pattern maps onto ocap as follows:

| Ocap Concept | soroban-sdk-tools Equivalent |
|---|---|
| Capability | The `Provider` associated type |
| Designation | The `Internal` trait methods (naming what can be done) |
| Authority | The `require_auth()` call in the outer trait (gating access) |
| Attenuation | Supertrait composition (capabilities narrow as traits compose) |
| Revocation | Not supported (provider binding is compile-time) |

### 3.2. Where Ocap Discipline Holds

The Provider pattern satisfies two of the three ocap invariants:

1. **Connectivity (capability propagation through legitimate channels):** A contract obtains its capabilities through the `type Provider = P` binding. The binding is explicit and visible in the source code. This is the ocap analog of "capabilities are passed, not ambient."

2. **No ambient authority:** The outer trait methods do not access global state directly. They delegate through `Self::Provider`, which must be explicitly wired. There is no ambient `get_owner()` function available to arbitrary code -- the owner resolution goes through the provider.

### 3.3. Where Ocap Discipline Breaks

The `Internal` trait's public visibility violates ocap discipline:

**The Confused Deputy Problem.** In ocap, a confused deputy is a program that is tricked into misusing a capability it legitimately holds. In the current design, any code that has access to the `OwnableInternal` trait (which is public) can act as a confused deputy:

```rust
// A "utility" function that bypasses auth
fn helpful_reset(env: &Env, new_owner: Address) {
    <SingleOwner as OwnableInternal>::transfer_ownership(env, new_owner);
}
```

This function has the capability (access to `OwnableInternal`) but uses it without authorization. The sealed macro does not prevent this -- it only seals the *specific methods generated for the contract*, not all possible uses of the Internal trait.

### 3.4. Recommendation: Capability Attenuation via Module Visibility

The most practical fix within Rust's type system:

```rust
// Generate the Internal trait with restricted visibility
pub(crate) trait OwnableInternal { ... }
```

This restricts the "capability" to the defining crate. External crates can use the `Ownable` outer trait but cannot bypass it via `OwnableInternal`. For cross-crate providers, a `#[contracttrait(pub_internal)]` opt-in could re-enable public visibility with an explicit warning.

A more advanced approach would use Rust's sealed trait pattern (a private supertrait) to prevent external implementations entirely, but this conflicts with the provider extensibility goal.

### 3.5. Capability Composition and the Diamond Problem

When `Pausable: Ownable` and `FungibleToken: Ownable` both require an owner, and a contract implements both, there is a single `type Provider` that must satisfy all three traits. This is the capability equivalent of the "powerbox" pattern -- a single entity that bundles multiple capabilities.

The risk: the provider for `FungibleToken` has access to `OwnableInternal` methods, meaning it could alter the owner as a side effect of a token operation. This is a capability leak. In a proper ocap system, the token provider would receive an attenuated capability that allows reading the owner but not changing it.

**Recommendation:** Consider splitting `OwnableInternal` into `OwnableReadInternal` (read-only capabilities) and `OwnableWriteInternal` (mutation capabilities). The `#[auth]` annotation on `transfer_ownership` implies that writes require auth, but reads do not -- this asymmetry should be reflected in the capability structure.

---

## 4. Macro Hygiene Concerns

### 4.1. Critical: Hardcoded `env` Identifier

The `build_delegate_args` function (line 306-318 of `contract.rs`) unconditionally inserts `env` as the first argument:

```rust
let env_ident: Ident = parse_quote!(env);
args.push(quote! { #env_ident });
```

This violates macro hygiene because the generated code references a name that may not exist in the user's scope. The `extract_method_info` function already identifies the env parameter -- it just discards the name.

**Before:**
```rust
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
```

**After:**
```rust
fn build_delegate_args(method: &MethodInfo) -> TokenStream2 {
    let mut args = Vec::new();
    args.push(quote! { #method.env_name });
    for p in &method.params {
        let name = &p.name;
        args.push(quote! { #name });
    }
    quote! { #(#args),* }
}
```

This requires adding `env_name: Ident` to `MethodInfo` and storing the actual parameter name during extraction.

### 4.2. Call-Site Hygiene on `__auth_addr`

The `__auth_addr` variable is generated with `quote!`, which uses `Span::call_site()` by default. This means the variable is visible to user code in the same scope and could conflict with user-defined variables.

**Before:**
```rust
quote! {
    let __auth_addr = Self::Provider::#resolver(#env_arg);
    __auth_addr.require_auth();
}
```

**After:**
```rust
let auth_var = Ident::new("__contracttrait_auth_addr", Span::mixed_site());
quote! {
    let #auth_var = Self::Provider::#resolver(#env_arg);
    #auth_var.require_auth();
}
```

The use of `Span::mixed_site()` ensures the identifier is hygienic: visible within the generated code block but not interacting with user-defined names.

### 4.3. The `extern crate alloc` Aliasing Pattern

The current pattern generates one `extern crate alloc as __alloc_{TraitName}` per trait. This is fragile for several reasons:

1. Multiple traits in the same module produce multiple `extern crate` declarations
2. The alias naming convention (`__alloc_Ownable`) violates Rust conventions
3. Potential name conflicts with user identifiers

**Recommendation:** Use absolute paths `::alloc::boxed::Box` (which works in edition 2021 without `extern crate`) or generate a single shared `extern crate alloc` without aliasing.

### 4.4. Missing Span Propagation

Error messages from the generated code point to the `#[contracttrait]` attribute rather than to the specific method that caused the error. The `generate_outer_trait` function should use `quote_spanned!` with the span of each method's identifier:

```rust
let method_span = method.sig.ident.span();
quote_spanned! { method_span =>
    #auth_check
    Self::Provider::#method_name(#delegate_args)
}
```

This change dramatically improves diagnostic quality.

### 4.5. The `to_snake_case` Function

The current implementation handles only basic PascalCase. It produces incorrect results for:

- Acronyms: `HTTPServer` becomes `h_t_t_p_server` (should be `http_server`)
- Trailing acronyms: `MyHTTP` becomes `my_h_t_t_p` (should be `my_http`)
- Consecutive caps: `CBDCToken` becomes `c_b_d_c_token` (should be `cbdc_token`)

Since the output determines macro names (`impl_h_t_t_p_server!`), this produces unusable names.

**Recommendation:** Replace with `heck::ToSnakeCase`, the standard solution in the Rust ecosystem.

### 4.6. Missing Input Validation

The macro does not validate:
- Empty traits (no methods)
- `#[auth(param)]` referencing a non-existent parameter
- Methods with `&self` (incompatible with Soroban)
- Non-function trait items (associated types, constants)
- Self-referential auth (`#[auth(Self::foo)]` on method `foo`)

A validation pass between parsing and code generation would catch these early with clear error messages.

### 4.7. Missing `#[automatically_derived]`

Generated impl blocks should carry `#[automatically_derived]` to suppress irrelevant clippy and rustc warnings. This is a best practice for all proc macros that generate impl blocks.

### 4.8. Double Parameter Cloning in AuthClient

The `generate_auth_client_method` function clones each parameter twice (once for `invoker`, once for `try_invoker`). For types with expensive `Clone` implementations, this is wasteful.

**Recommendation:** Generate a single closure that returns `Result` and wrap it:
```rust
let shared_fn = move || -> Result<T, _> {
    inner.try_fn_name(#(&#shared_clones),*)
};
```

---

## 5. Formal Verification Opportunities

### 5.1. Properties That Could Be Machine-Checked

The following properties are amenable to formal verification, either in a proof assistant (Lean 4, Coq, Agda) or via model checking (TLA+, Alloy):

#### Property 1: Auth Presence (Syntactic)

**Statement:** For every method $m$ annotated with `#[auth(src)]` in the input trait, the generated outer trait's default implementation of $m$ contains a call to `require_auth()` on the value returned by `src`.

**Verification approach:** This can be verified by inspecting the AST of the macro output. A test that runs `cargo expand` and pattern-matches on the output would suffice. Alternatively, a Lean formalization of the macro's `generate_outer_trait` function could prove this structurally.

#### Property 2: Provider Isolation (Semantic)

**Statement:** If provider $P$ implements `OwnableInternal` and `PausableInternal`, and the `owner()` method returns the same value for both traits given identical state, then the composed contract's auth checks are consistent.

**Verification approach:** This is a semantic property that depends on the provider implementation. It could be expressed as a refinement type or as a property-based test (QuickCheck / proptest).

#### Property 3: Sealed Non-Circumventability (Structural)

**Statement:** When using `impl_trait!(C, P)`, no Rust code in the same crate can produce a WASM export for the trait's methods that does not include the auth check, without modifying the `impl_trait!` invocation.

**Verification approach:** This follows from Rust's coherence rules (inherent methods take priority over trait methods for WASM export) and the Soroban SDK's `#[contractimpl]` behavior. A mechanized proof would formalize these rules and show the property holds.

#### Property 4: Functor Preservation

**Statement:** The macro preserves supertrait relationships: if $\text{Pausable}: \text{Ownable}$ in the input, then $\text{PausableInternal}: \text{OwnableInternal}$ in the output.

**Verification approach:** Direct inspection of `map_supertraits_to_internal`. This is a syntactic property of the macro and can be verified by testing with various supertrait hierarchies.

### 5.2. Proof Obligations

Using the macro introduces implicit proof obligations that developers must discharge (informally or formally):

1. **Purity of auth sources:** Methods referenced by `#[auth(Self::method)]` must be pure functions of contract state. Side effects in auth sources can lead to TOCTOU vulnerabilities.

2. **Storage isolation:** Composed providers must use disjoint storage keys unless sharing is intentional and documented.

3. **Provider correctness:** The provider must faithfully implement the intended business logic. The macro guarantees the auth *wrapper* but not the auth *logic*.

4. **Termination of auth sources:** Auth-source methods must terminate (within the gas budget). Non-terminating auth sources cause denial of service.

### 5.3. Towards a Lean 4 Formalization

A minimal Lean 4 formalization would define:

```lean
-- The input: a trait with auth annotations
structure TraitDef where
  name : String
  methods : List MethodDef
  supertraits : List String

structure MethodDef where
  name : String
  authSource : Option AuthSource

inductive AuthSource
  | providerMethod : String -> AuthSource
  | param : String -> AuthSource

-- The output: a pair of traits
structure TraitPair where
  internal : TraitDef  -- no auth annotations
  outer : TraitDef     -- auth-wrapped defaults

-- The macro as a function
def expand (input : TraitDef) : TraitPair := ...

-- The correctness property
theorem auth_present (input : TraitDef) (m : MethodDef)
  (h : m ∈ input.methods) (hauth : m.authSource.isSome) :
  has_auth_check (expand input).outer m.name := by
  ...
```

This formalization would be approximately 200 lines and would provide mechanized confidence in the macro's core correctness property.

### 5.4. Model Checking with TLA+

For reasoning about the runtime behavior of contracts using the provider pattern, TLA+ specifications could model:

- State transitions guarded by auth checks
- Provider substitution preserving safety invariants
- Composition of multiple trait implementations
- Upgrade scenarios (provider replacement)

---

## 6. Specific Code Improvements

### 6.1. Self-Referential Auth Optimization

When `#[auth(Self::foo)]` is on method `foo`, the provider method is called twice:

**Before (generated code):**
```rust
fn owner(env: &Env) -> Address {
    let __auth_addr = Self::Provider::owner(env); // call 1
    __auth_addr.require_auth();
    Self::Provider::owner(env) // call 2
}
```

**After (optimized):**
```rust
fn owner(env: &Env) -> Address {
    let __auth_addr = Self::Provider::owner(env);
    __auth_addr.require_auth();
    __auth_addr // reuse cached value
}
```

Detection: compare `method.name` with the auth source method name in `generate_outer_trait`.

### 6.2. Address Type Validation for Auth Parameters

When `#[auth(param)]` references a parameter, validate at macro expansion time that the parameter's type is `Address` (or `&Address`):

```rust
fn validate_auth_param(method: &MethodInfo) -> syn::Result<()> {
    if let Some(AuthSource::Param(ref param_name)) = method.auth {
        let param = method.params.iter().find(|p| p.name == *param_name);
        if let Some(p) = param {
            // Check if type is Address or &Address
            if !is_address_type(&p.ty) {
                return Err(Error::new_spanned(
                    param_name,
                    format!(
                        "`#[auth({})]` requires the parameter to be of type `Address`, found `{}`",
                        param_name, quote!(#p.ty)
                    ),
                ));
            }
        }
    }
    Ok(())
}
```

### 6.3. Doc Comments on Generated `type Provider`

**Before:**
```rust
quote! {
    type Provider: #internal_trait_name;
}
```

**After:**
```rust
let doc = format!(
    "The provider type implementing `{}` business logic. Must implement `{}`.",
    trait_name, internal_trait_name
);
quote! {
    #[doc = #doc]
    type Provider: #internal_trait_name;
}
```

### 6.4. Storage Key Namespacing in Providers

The current design relies on convention for storage key isolation. Add a helper that namespaces keys by trait:

```rust
/// Storage key namespaced by trait.
/// Prevents collisions when multiple providers share a contract.
pub fn trait_key(trait_name: &str, key: &str) -> Symbol {
    // Concatenate: "Ownable::owner" -> storage key
    Symbol::new(env, &format!("{}::{}", trait_name, key))
}
```

This could be generated by the macro as a companion to each Internal trait.

### 6.5. Event Emission in Outer Trait

Add automatic event emission for auth-guarded methods:

```rust
fn transfer_ownership(env: &Env, new_owner: Address) {
    let __auth_addr = Self::Provider::owner(env);
    __auth_addr.require_auth();
    Self::Provider::transfer_ownership(env, new_owner);
    // Generated event emission
    env.events().publish(
        (symbol_short!("Ownable"), symbol_short!("xfer_own")),
        (__auth_addr, new_owner),
    );
}
```

This could be opt-in via `#[contracttrait(events)]`.

### 6.6. Removing Dead Code

The `env_is_ref` field in `MethodInfo` and `_is_ref` in `ParamInfo` are unused. Either implement the ref-handling logic or remove these fields entirely. Dead code in security-critical infrastructure is a maintenance liability and a source of confusion for contributors.

---

## 7. Research Directions

### 7.1. Indexed Monads for Multi-Phase Authorization

The current `#[auth]` model supports only single-phase authorization: check, then execute. Many real-world scenarios require multi-phase authorization:

1. **Propose** (any member)
2. **Approve** (quorum of signers)
3. **Execute** (after timelock)

An indexed monad $\text{Auth}_{i,j}$ could model state transitions:

$$\text{propose} : \text{Auth}_{\text{open}, \text{proposed}}$$
$$\text{approve} : \text{Auth}_{\text{proposed}, \text{approved}}$$
$$\text{execute} : \text{Auth}_{\text{approved}, \text{executed}}$$

This is related to Atkey's indexed monads and could be implemented as a sequence of `#[auth]` annotations with state tracking.

### 7.2. Row-Polymorphic Effects for Composable Guards

The composition of guards from multiple traits (e.g., `Pausable + Ownable + RateLimited`) resembles row-polymorphic effect systems. A method might require:

$$\text{transfer} : \text{Env} \to \text{Address} \to \text{i128} \xrightarrow{\text{Auth}(\text{from}) + \text{NotPaused} + \text{RateLimit}} ()$$

Row polymorphism would allow guards to be composed without knowing the full set at definition time. This connects to Leijen's work on row-polymorphic effects in Koka.

### 7.3. Gradual Typing for Provider Trust

The current system treats all providers as equally trusted (or equally untrusted). A gradual typing approach could introduce trust levels:

- **Audited providers:** Trusted, no additional runtime checks
- **Unaudited providers:** Wrapped with additional invariant checks
- **Untrusted providers:** Sandboxed execution with state rollback on violation

This maps to Siek and Taha's gradual typing with cast insertion at trust boundaries.

### 7.4. Session Types for Multi-Party Authorization

When multiple parties must coordinate authorization (e.g., multisig), session types could model the protocol:

$$\text{MultiSig}_n : !(\text{propose}). \prod_{i=1}^{n} ?(\text{approve}_i). !(\text{execute})$$

The provider pattern could be extended with session-typed interfaces, ensuring that the authorization protocol is followed correctly.

### 7.5. Quantitative Type Theory for Gas Analysis

The "zero overhead" claim could be made precise using quantitative type theory (QTT), where each type carries a usage annotation:

$$\text{owner} : \text{Env} \xrightarrow{1} \text{Address}$$

The superscript indicates that `owner` accesses the environment exactly once. QTT would allow the macro to verify at compile time that the generated code performs the minimum number of storage reads -- one per auth check.

### 7.6. The Guard Trait as a Language Feature

The `#[guard_trait]` pattern proposed in the generalization blog post raises the question: should this be a language feature rather than a macro? Specifically:

```rust
// Hypothetical Rust syntax
trait AdminApi {
    fn current_admin(ctx: &AppContext) -> UserId;

    // Language-level precondition
    requires Self::current_admin
    fn delete_user(ctx: &AppContext, user_id: UserId);
}
```

This would integrate with Rust's trait coherence, allowing the compiler to verify that all implementations satisfy the precondition. It connects to Eiffel's Design by Contract and to Rust RFC discussions around `requires`/`ensures` clauses.

### 7.7. Open Questions for the PL Community

1. **Can the purity assumption on auth sources be checked statically in Rust?** Current Rust cannot distinguish pure functions from impure ones. Is there a lightweight annotation system (short of full effect typing) that could enforce this?

2. **What is the correct formalization of "sealed" in the context of Rust's trait system?** The sealed macro relies on Rust-specific coherence rules. How do these rules interact with future Rust features (specialization, negative impls, trait aliases)?

3. **Can the provider pattern be generalized to support multiple providers per trait?** This would require something like type-class instance resolution in Haskell, but Rust's coherence rules (one impl per type) prevent this. Is there a safe relaxation?

4. **How do capability-based security guarantees degrade under contract upgrades?** Stellar contracts are upgradeable. An upgraded contract can replace its provider. What invariants survive upgrades, and how can they be specified?

5. **Is there a connection between the two-trait split and the splitting of a category into a "pure" and "effectful" subcategory, as in Moggi's computational lambda calculus?** The Internal trait is the pure subcategory; the Outer trait is the effectful one. Can this connection be made precise?

6. **Can the AuthClient pattern be generalized into a theory of "test duals" -- for every trait with preconditions, there exists a canonical test client that is the "dual" of the trait?** This would connect to testing theory and to the duality between processes and tests in concurrency theory.

---

## Summary of Priority Recommendations

| Priority | Recommendation | Impact |
|----------|---------------|--------|
| P0 | Fix hardcoded `env` parameter name | Correctness bug |
| P0 | Add input validation pass (auth sources, param types) | Error quality |
| P1 | Use `Span::mixed_site()` for generated identifiers | Macro hygiene |
| P1 | Use `quote_spanned!` for method-level error spans | Diagnostic quality |
| P1 | Replace `to_snake_case` with `heck` crate | Correctness for acronyms |
| P1 | Optimize self-referential auth (avoid double call) | Performance |
| P2 | Add `#[automatically_derived]` to generated impls | Clippy compatibility |
| P2 | Restrict Internal trait visibility to `pub(crate)` | Capability security |
| P2 | Add event emission for auth-guarded methods | Observability |
| P2 | Remove dead code (`env_is_ref`, `_is_ref`) | Maintainability |
| P3 | Use absolute paths instead of `extern crate` aliasing | Robustness |
| P3 | Add doc comments to generated `type Provider` | Developer experience |
| P3 | Implement storage key namespacing | Composition safety |
| P3 | Write Lean 4 specification of core properties | Formal confidence |
