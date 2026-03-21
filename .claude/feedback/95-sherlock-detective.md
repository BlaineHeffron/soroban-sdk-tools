---
agent: Sherlock Chen
background: Former fraud investigator at Interpol's cybercrime unit, now an independent smart contract auditor, applies investigative methodology to code review
date: 2026-03-21
---

# Review by Sherlock Chen

## Overall Impression

When I investigate a case, I begin by establishing the facts, identifying inconsistencies, and following the evidence wherever it leads. I do not accept claims at face value -- I verify them. Applied to this codebase, I find a well-constructed system with several inconsistencies between what the documentation claims and what the code actually guarantees, and at least one piece of missing evidence that undermines a central claim.

## Strengths

1. **Exhibit A: The sealed macro delivers on its promise.** I verified this claim by tracing the generated code path. `impl_ownable!` generates inherent methods on the contract type via `#[soroban_sdk::contractimpl]`. Soroban's SDK registers inherent `#[contractimpl]` methods as WASM exports. These exports cannot be overridden by trait implementations because they are not trait methods -- they are direct methods on the type. The auth check in these methods (`require_auth()`) is executed for every external invocation. This claim checks out.

2. **Exhibit B: The `extract_auth_attr` function is correctly restrictive.** I tested the parsing logic against various inputs:
   - `#[auth(Self::owner)]` -> ProviderMethod(owner) -- correct
   - `#[auth(caller)]` -> Param(caller) -- correct
   - `#[auth(Self::owner::nested)]` -> three-segment path -> error -- correct
   - `#[auth(42)]` -> not a path -> error -- correct
   The function rejects everything except the two expected patterns. No injection vectors.

3. **Exhibit C: The supertrait mapping is consistent.** `map_supertraits_to_internal` correctly appends "Internal" to each supertrait name. I verified that `Pausable: Ownable` produces `PausableInternal: OwnableInternal`, which enforces the correct composition constraint at the type level.

4. **Exhibit D: The auth address caching is correctly placed.** In the generated outer trait, `let __auth_addr = Self::Provider::owner(env)` is evaluated BEFORE `__auth_addr.require_auth()`. The cached address is the same address that is checked. This prevents the "check one address, execute as another" class of bugs.

## Concerns

1. **INCONSISTENCY: The documentation claims "structural enforcement" but the Internal trait is public.** The blog post states: "structural auth enforcement that cannot be accidentally bypassed." The code comment in `contract.rs` states: "A developer can call `{Trait}Internal` methods directly from any `#[contractimpl]` block, bypassing the auth wrapper." These two statements are contradictory. The documentation makes the stronger claim; the code acknowledges the weaker reality. In an investigation, this would be classified as a "material inconsistency" -- a discrepancy between the presented evidence and the actual facts.

2. **MISSING EVIDENCE: No test demonstrates that the sealed macro actually prevents override.** The test suite has four tests, all of which use the flexible path (`#[contractimpl(contracttrait)]`). None of them use `impl_ownable!`. None of them demonstrate that overriding auth is prevented. The documentation makes a strong claim ("cannot be overridden"), but the evidence (tests) does not support it. In forensics, we say "absence of evidence is not evidence of absence" -- but absence of tests is evidence of insufficient verification.

3. **INCONSISTENCY: The blog post criticizes `mock_all_auths()` then the example uses it.** The blog post says: "mock_all_auths() (which tests nothing)." The example file uses `mock_all_auths()` in `test_ownership_with_auth_enforcement` and `test_pausable_supertrait_composition`. This is a credibility issue. If the tool's own example does not follow the tool's own recommendations, the recommendations are undermined.

4. **SUSPICIOUS PATTERN: The `#[allow(dead_code)]` on `env_is_ref`.** The `MethodInfo` struct has `#[allow(dead_code)]` on the `env_is_ref` field. This field is populated during method extraction but never used in code generation. Why is it extracted? Was it intended for a feature that was never implemented? In an investigation, unused-but-populated fields suggest either abandoned functionality or planned-but-unimplemented features. Either way, it is suspicious: the `env` parameter is always assumed to be `&Env` (reference), but `env_is_ref` tracks whether it actually is. If it is not used, the macro may generate incorrect code for `env: Env` (owned) parameters.

5. **UNVERIFIED CLAIM: "Zero overhead."** The blog post states: "WASM binary size: Zero overhead. ... The final WASM is identical to hand-written code." I found no test, benchmark, or CI step that verifies this claim. This is an assertion without evidence. In any audit, unverified performance claims are flagged as "not substantiated."

6. **LOGICAL GAP: The `AuthClient` proves correct authorization but not incorrect authorization.** The tests show that the authorized party CAN invoke methods. They do not show that unauthorized parties CANNOT invoke methods. `test_ownable_auth_client` tests that the owner can transfer ownership. It does not test that a non-owner cannot. A complete evidence chain requires both positive and negative tests.

7. **UNACCOUNTED EVIDENCE: The `_is_ref` field in `ParamInfo`.** Like `env_is_ref`, the `_is_ref` field in `ParamInfo` is populated but prefixed with an underscore (conventional "unused" marker). It tracks whether a method parameter is a reference type. But the generated delegation code does not distinguish between reference and owned parameters -- it passes them as-is. If a method takes `owner: &Address` but the generated code passes `owner` (without dereferencing or referencing), the code may fail to compile for certain parameter patterns.

## Suggestions

1. **Resolve the "structural enforcement" inconsistency.** Either:
   - (a) Change the documentation to say "structural enforcement when using the sealed macro; convention-based enforcement when using the flexible path"
   - (b) Make the Internal trait `pub(crate)` so it cannot be called from consumer crates
   Option (a) is honest. Option (b) is stronger but may break legitimate use cases.

2. **Add sealed macro tests.** Write a test that:
   - Uses `impl_ownable!` to wire a contract
   - Verifies that auth-guarded methods require authorization
   - Attempts to bypass auth (if possible) and demonstrates that it fails
   This provides the missing evidence for the sealed macro claim.

3. **Add negative auth tests.** For every positive auth test ("owner can transfer"), add a negative test ("non-owner cannot transfer"):
   ```rust
   #[test]
   #[should_panic(expected = "...")]
   fn test_non_owner_cannot_transfer() {
       let non_owner = Address::generate(&env);
       auth_client.transfer_ownership(&new_owner)
           .authorize(&non_owner)
           .invoke();
   }
   ```

4. **Resolve or remove the dead fields.** Either use `env_is_ref` and `_is_ref` in code generation (to correctly handle owned vs. reference parameters) or remove them with a comment explaining why they are not needed.

5. **Remove `mock_all_auths()` from the official example.** Replace all tests with AuthClient-based tests. The example is the primary evidence that the tool works as documented -- it must be consistent with the documentation.

6. **Add a WASM comparison test.** Write a hand-coded contract equivalent to the macro-generated output. Compile both. Compare WASM sizes. If they differ, document why. This provides evidence for the "zero overhead" claim.

## Unique Perspective

In criminal investigation, we follow a principle: "Every contact leaves a trace" (Locard's Exchange Principle). Applied to code review: every design decision leaves traces in the implementation, and every claim leaves (or should leave) traces in the test suite.

This codebase has claims that leave no traces in the tests:
- "Cannot be overridden" -- no test demonstrates this
- "Zero overhead" -- no benchmark proves this
- "mock_all_auths() tests nothing" -- but the example uses it

These are not necessarily false claims. But they are unsubstantiated claims. In a court of law, unsubstantiated claims are dismissed. In a security audit, they are flagged as findings.

The architecture is sound. The design decisions are well-reasoned. But the evidence chain is incomplete. A good detective does not just solve the case -- they build an evidence chain that convinces the jury. Your jury is the developer community. Give them evidence, not just arguments.

## Would I Use This?

After verifying the claims myself (by reading `cargo expand` output and running my own negative tests), yes. The architecture is correct and the macro implementation is well-done. But I would not trust the documentation claims without independent verification, and I would add the negative tests myself before deploying to production.
