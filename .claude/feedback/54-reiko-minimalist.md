---
persona: Reiko
age: 48
background: Author of "The Art of Less Code," advocate for minimalism in software, contributed to TinyCC and suckless tools
focus: What can be removed, essential vs accidental complexity, Occam's Razor applied to APIs
tone: Sparse, decisive, every word earns its place, allergic to unnecessary abstraction
---

# Review: soroban-sdk-tools -- What Can Be Removed?

## The Question

The `#[contracttrait]` macro takes a ~5 line trait and generates ~50 lines of
code across four artifacts (Internal trait, Outer trait, AuthClient, sealed
macro). Is each artifact necessary? Is each line within each artifact
necessary?

## Artifact 1: Internal Trait -- KEEP

```rust
pub trait OwnableInternal {
    fn owner(env: &Env) -> Address;
    fn transfer_ownership(env: &Env, new_owner: Address);
}
```

This is the pure business logic interface. Without it, providers would
implement the outer trait directly, mixing auth with logic. The separation
is essential. It cannot be removed.

**But**: The name `OwnableInternal` is redundant. In Rust, the convention
for an implementation-detail trait is to use a `Raw` or `Impl` suffix. Or
simply `OwnableLogic`. `Internal` implies visibility, which is misleading
because the trait is public.

Suggested alternative: `OwnableImpl`. Shorter, clearer.

## Artifact 2: Outer Trait -- KEEP (with simplification)

The outer trait wraps Internal with auth. Essential. Cannot be removed.

**But**: The `type Provider: OwnableInternal` associated type adds a layer
of indirection. The generated default methods look like:

```rust
fn transfer_ownership(env: &Env, new_owner: Address) {
    let __auth_addr = Self::Provider::owner(env);
    __auth_addr.require_auth();
    Self::Provider::transfer_ownership(env, new_owner)
}
```

Question: could the Provider be a generic parameter instead of an associated
type? Then the outer trait would not need `type Provider`:

```rust
pub trait Ownable<P: OwnableInternal> {
    fn transfer_ownership(env: &Env, new_owner: Address) {
        let __auth_addr = P::owner(env);
        __auth_addr.require_auth();
        P::transfer_ownership(env, new_owner)
    }
}
```

This removes one concept (associated type) at the cost of generic noise.
For soroban contracts, where `contracttrait` needs associated types for the
SDK's code generation, the current approach may be forced by the platform.
But the question is worth asking.

## Artifact 3: AuthClient -- QUESTION

The AuthClient is a convenience wrapper for testing. It generates a struct
with methods that return `CallBuilder`. Is it necessary?

Alternative: a single generic function:

```rust
fn auth_call<F, R>(env: &Env, addr: &Address, fn_name: &str, args: Vec<Val>, f: F) -> CallBuilder
where F: Fn() -> R
```

This generic approach would not require generating a per-trait struct. One
function serves all traits. The tradeoff: less type safety (method names are
strings instead of method calls) and less discoverability (no autocomplete
for trait-specific methods).

**Verdict**: The per-trait AuthClient is worth its code weight. Type safety
and discoverability matter in testing. KEEP.

**But**: The `extern crate alloc as #alloc_alias` pattern is pure noise. It
exists to avoid name collisions when multiple AuthClients are generated.

Can it be removed? Each AuthClient is in a different scope. The collisions
occur because `extern crate alloc` is a crate-level declaration. But the
macro is invoked at module scope, not crate scope.

Alternative: wrap the AuthClient in a module:

```rust
mod __ownable_auth {
    extern crate alloc;
    pub struct OwnableAuthClient { ... }
}
pub use __ownable_auth::OwnableAuthClient;
```

This eliminates the aliasing pattern entirely. The module provides natural
namespacing for `extern crate alloc`.

## Artifact 4: Sealed Impl Macro -- QUESTION

The `impl_ownable!` macro generates inherent methods. Is a macro necessary?
Could this be a function-like proc macro or an attribute?

```rust
// Current:
impl_ownable!(MyContract, SingleOwner);

// Alternative: attribute
#[seal_trait(Ownable, SingleOwner)]
pub struct MyContract;
```

The attribute approach eliminates one concept (declarative macros) and uses
the same paradigm as the rest of the framework (attribute macros). But
`macro_rules!` macros are simpler to debug and do not require proc-macro
crate boundaries.

**Verdict**: The `macro_rules!` approach is pragmatically simpler. KEEP.
But only generate it when there are auth methods (already implemented --
good).

## Line-by-Line Elimination

### In `generate_auth_client_method`:

Lines 484-523 generate argument clones for both `invoker` and `try_invoker`.
The `try_invoker` arguments are cloned eagerly even if `try_invoke()` is
never called. This is unnecessary work and unnecessary code.

**Remove**: The `try_invoker` closure. Instead, generate it lazily in the
`CallBuilder::try_invoke()` method. Or share parameters via `Rc`.

### In `extract_method_info`:

```rust
let (ty, is_ref) = match &*pat_type.ty {
    syn::Type::Reference(r) => (*r.elem.clone(), true),
    other => (other.clone(), false),
};
```

The `is_ref` field is stored in `ParamInfo` as `_is_ref` (prefixed with
underscore, indicating it is unused). If it is unused, remove it. Dead
fields are dead code.

**Remove**: `_is_ref` from `ParamInfo` and the corresponding match logic.

### In `generate_outer_trait`:

```rust
let non_doc_attrs: Vec<_> = trait_attrs
    .iter()
    .filter(|a| !a.path().is_ident("doc") && !a.path().is_ident("auth"))
    .collect();
let doc_attrs: Vec<_> = trait_attrs
    .iter()
    .filter(|a| a.path().is_ident("doc"))
    .collect();
```

Two passes over `trait_attrs` to partition into doc and non-doc. Use a
single pass with `partition`:

```rust
let (doc_attrs, non_doc_attrs): (Vec<_>, Vec<_>) = trait_attrs
    .iter()
    .partition(|a| a.path().is_ident("doc"));
```

This is not fewer lines but fewer iterations.

### In `map_supertraits_to_internal`:

```rust
.filter_map(|bound| {
    if let syn::TypeParamBound::Trait(tb) = bound {
        if let Some(seg) = tb.path.segments.last() {
```

Nested `if let` can be flattened with pattern matching or `let-else`. Minor
but the function is small enough that clarity matters.

## The Macro's Macro Problem

The `#[contracttrait]` macro generates a `macro_rules!` macro
(`impl_ownable!`). This is a macro that generates a macro. Two levels of
meta-programming. Each level adds cognitive overhead.

Question: is the second level necessary? Could the sealed pattern be achieved
without generating a macro?

Alternative: generate a trait with a blanket implementation:

```rust
pub trait OwnableSealed: OwnableInternal {
    // Same sealed methods, but as trait defaults on a sealed trait
}

// Blanket: any type that is OwnableInternal gets the sealed defaults
impl<T: OwnableInternal> OwnableSealed for T {}
```

But this does not work because we need `#[contractimpl]` on the methods to
make them WASM exports. Trait defaults are not WASM exports. The macro
approach is necessary.

**Verdict**: The macro-generating-macro pattern is justified by Soroban's
WASM export requirements. KEEP, but acknowledge the cognitive cost in
documentation.

## Final Accounting

| Item | Verdict | Lines saved if removed |
|------|---------|----------------------|
| Internal trait | KEEP | 0 |
| Outer trait | KEEP | 0 |
| AuthClient | KEEP | 0 |
| Sealed macro | KEEP | 0 |
| `_is_ref` field | REMOVE | 5 |
| Eager try_invoker clones | REFACTOR | 10 |
| `extern crate alloc` aliasing | REFACTOR (use module) | 3 per trait |
| Double iteration in filter | REFACTOR | 2 |

Net lines removable: ~20 out of ~727. The framework is already lean.

## Verdict

The `#[contracttrait]` macro generates four artifacts from one input. All
four are justified. The generated code is mostly essential, with minor dead
code (`_is_ref`) and an avoidable clone pattern (try_invoker). The `extern
crate alloc` aliasing is the ugliest pattern and could be eliminated with
module wrapping.

The framework passes Occam's Razor. Each piece earns its existence.

**Rating: 8/10 for minimalism** -- surprisingly lean for a composition
framework. Fix the dead code and the alloc pattern.
