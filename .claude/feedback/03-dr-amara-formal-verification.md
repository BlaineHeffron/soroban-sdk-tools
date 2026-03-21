---
agent: Dr. Amara Okafor-Schneider
background: PhD in formal verification from ETH Zurich, maintains a Coq formalization of ERC-20 semantics, currently formalizing Soroban's auth model
date: 2026-03-21
---

# Review by Dr. Amara Okafor-Schneider

## Overall Impression

The claims in the documentation are stronger than what the implementation can actually guarantee. The phrase "structural auth enforcement" is used repeatedly, but the guarantees are conditional on usage patterns that are not enforced by the type system. This is not a fatal flaw -- it is a common one in macro-based security tooling -- but the documentation should be precise about the boundary between what is proven and what is assumed.

## Strengths

1. **The two-trait decomposition is a correct factoring.** Separating the authorization-free business logic (`OwnableInternal`) from the authorization-wrapped interface (`Ownable`) is a sound architectural decision. In formal terms, the Internal trait defines a state transition system, and the outer trait defines a guarded transition system where preconditions (auth checks) gate the transitions. This separation makes it possible, in principle, to verify each layer independently.

2. **The sealed macro eliminates one class of override attacks.** The `impl_ownable!` macro generates inherent methods rather than trait defaults. Since Rust's coherence rules prevent inherent methods from being "overridden" (they shadow, but the WASM export point is fixed), this does provide a real guarantee: the auth check cannot be removed without modifying the macro invocation itself. This is a legitimate structural property.

3. **The `AuthSource` enum is a well-typed representation.** Having `ProviderMethod(Ident)` and `Param(Ident)` as distinct variants makes the auth resolution strategy explicit and exhaustible. A verifier could pattern-match on these two cases and prove properties about each independently.

4. **The auth address caching (`let __auth_addr = ...`) prevents TOCTOU within a single call.** By binding the address before calling `require_auth()`, the generated code ensures that the address checked is the same one that authorized. This is a subtle but important property.

## Concerns

1. **The "structural" claim has a critical caveat that is buried in code comments.** The module doc says: "A developer can call `{Trait}Internal` methods directly from any `#[contractimpl]` block, bypassing the auth wrapper." This is correct. But the blog post says "structural auth enforcement that cannot be accidentally bypassed." These statements are contradictory. If the Internal trait is in scope, any `#[contractimpl]` block can call `SingleOwner::transfer_ownership(env, attacker_address)` directly. The auth is only structural if the Internal trait's methods are not callable from unauthorized contexts -- and the macro does nothing to prevent this. In Coq terms: the theorem holds only under an assumption that is not discharged.

2. **No formalization of the provider composition model.** The supertrait relationship `Pausable: Ownable` generates `PausableInternal: OwnableInternal`. But there is no check that a single provider implementing `PausableInternal` provides a consistent `owner()` across both the Ownable and Pausable paths. If `SingleOwner` implements both traits but uses different storage keys for the owner in each, the system has an inconsistency that the types do not catch. The constraint is semantic, not syntactic, and the macro cannot enforce it.

3. **The `#[auth(Self::owner)]` resolution assumes `owner()` is pure (or at least idempotent).** The generated code calls `Self::Provider::owner(env)` to get the address, then calls `require_auth()`. But if `owner()` has side effects (e.g., it increments a counter, or its return value depends on mutable state that changes during the call), the cached address may not reflect the intended authorizer. The documentation should state the assumption: auth-source methods must be pure reads with no side effects.

4. **The `try_invoke` error handling conflates authorization failure with invocation failure.** The `extract_return_types` function generates a `Result<Result<T, ConversionError>, Result<ContractError, InvokeError>>` -- a nested result that encodes two failure modes. But there is no way for the caller to distinguish "authorization was rejected" from "the business logic panicked" from "the contract was not found." A formal specification of the error semantics would reveal this ambiguity.

5. **The `to_snake_case` function is not injective.** `FooBar` and `Foo_Bar` (if someone names a trait with underscores) would both map to `foo_bar`, creating a macro name collision. This is unlikely but the function's type signature (`String -> String`) gives no indication that it is a partial function with collision potential.

## Suggestions

1. **Weaken the claims or strengthen the guarantees.** Either:
   - (a) Make `{Trait}Internal` a private (non-pub) trait so it cannot be called from consumer code, turning the structural claim into an actual structural property; or
   - (b) Change the documentation from "cannot be bypassed" to "bypassing requires explicit, deliberate misuse of the Internal trait."
   Option (a) is stronger but may limit legitimate use cases. Option (b) is honest.

2. **Add a `#[doc(hidden)]` to the Internal trait.** Even if you cannot make it private (because providers in other crates need to implement it), hiding it from documentation reduces the chance of accidental misuse and makes the "convention-based" boundary more robust.

3. **State the purity assumption for auth-source methods explicitly.** Add a doc comment to the `#[auth]` attribute: "The referenced method must be a pure function of contract state. Side effects in auth-source methods lead to undefined authorization semantics."

4. **Consider generating a compile-time assertion that the auth-source method exists on the Internal trait.** Currently, a typo in `#[auth(Self::ownr)]` would produce a confusing error at monomorphization time. A `static_assert` or a where-clause in the outer trait could catch this earlier.

5. **Write a specification document.** Not code, not a blog post -- a precise, semi-formal specification of the security properties this macro provides, the assumptions it requires, and the threat model it addresses. This is the single highest-value document you could produce. I would be happy to help formalize it in Lean4.

## Unique Perspective

In formal verification, we distinguish between "verified" (proven correct with respect to a specification) and "validated" (tested and believed correct). This codebase is validated, not verified. The macro generates code that is very likely correct, but the correctness depends on assumptions (provider consistency, auth-source purity, Internal trait encapsulation) that are not enforced by the type system or checked by the macro.

This is not a criticism -- almost no production software is formally verified. But the documentation uses language ("structural," "cannot be bypassed," "enforced everywhere") that implies a level of guarantee closer to verification than validation. The Soroban ecosystem will rely on these guarantees to secure real assets. Precision in claims is not pedantry; it is a safety obligation.

The most dangerous bugs are not in code that is wrong. They are in code that is right under assumptions that are wrong.

## Would I Use This?

I would use it as a foundation and then add formal property checks on top. The architecture is correct and the macro is well-implemented. But I would not rely on the "structural enforcement" claims without either (a) verifying the assumptions hold in my specific deployment, or (b) seeing a formal proof that the generated code satisfies the intended security properties for all valid inputs.
