---
persona: Chen
age: 44
background: Byzantine fault tolerance researcher, published on PBFT variants, now studying smart contract atomicity
focus: Consensus safety, atomicity guarantees, state consistency under concurrent access
tone: Precise, formal, always considers the adversarial case, references impossibility results
---

# Review: soroban-sdk-tools -- Atomicity and Safety Analysis

## Scope of Review

I am evaluating the `#[contracttrait]` macro and its generated code from the
perspective of atomicity, state consistency, and adversarial safety. I am NOT
evaluating the consensus layer (that is Stellar's concern), but rather whether
the composition patterns introduced by this framework preserve the safety
properties that the underlying platform provides.

## Theorem: Auth Separation Preserves Atomicity

**Claim**: The two-trait structure (Internal + Outer) does not introduce new
atomicity violations beyond what vanilla Soroban contracts can exhibit.

**Proof sketch**: In Soroban, a single contract invocation is atomic -- it either
completes fully or rolls back. The `#[contracttrait]` macro generates code that
executes within a single invocation:

```
outer::transfer_ownership(env, new_owner)
  -> require_auth()           // step 1
  -> Provider::transfer_ownership(env, new_owner)  // step 2
```

Steps 1 and 2 execute within the same transaction. If step 2 panics, the
entire transaction rolls back, including any state changes. The auth check in
step 1 is a pure assertion (no state mutation). Therefore, the generated code
preserves Soroban's atomic execution model. QED.

**Caveat**: This holds only if the provider's implementation does not invoke
external contracts. If `Provider::transfer_ownership` calls another contract
via `env.invoke_contract()`, the atomicity guarantee depends on Soroban's
cross-contract call semantics, which are atomic within the same transaction
but may fail independently.

## The Double-Read Consistency Problem

The outer trait caches the auth address:

```rust
let __auth_addr = Self::Provider::owner(env);
__auth_addr.require_auth();
Self::Provider::transfer_ownership(env, new_owner);
```

This reads `owner` once (into `__auth_addr`), then calls `transfer_ownership`.
If `transfer_ownership` internally reads `owner` again (to verify it before
overwriting), it reads from the same storage snapshot because Soroban uses
read-your-writes semantics within a transaction.

But consider this adversarial provider:

```rust
impl OwnableInternal for MaliciousProvider {
    fn owner(env: &Env) -> Address {
        // Returns different values on successive calls within the same tx
        let counter: u32 = env.storage().temporary().get(&"call_count").unwrap_or(0);
        env.storage().temporary().set(&"call_count", &(counter + 1));
        if counter == 0 { attacker_address } else { real_owner_address }
    }
}
```

On the first call (auth check), `owner()` returns the attacker's address. The
attacker authorizes. On the second call (inside `transfer_ownership`, if it
reads `owner()` again), it returns the real owner. The transfer proceeds
because the auth was "valid" -- but it was valid for the wrong principal.

This is a TOCTOU (time-of-check-time-of-use) vulnerability in the provider
layer. The framework cannot prevent it because provider implementations are
arbitrary code.

**Mitigation**: The framework should document this as a known limitation and
recommend that providers be pure functions of storage state (no hidden counters,
no side effects in `owner()`). A lint or static analysis pass that detects
mutations inside `owner()` methods would be ideal.

## Sealed vs. Flexible: A Formal Comparison

Let S = the set of all possible external call sequences a contract can receive.
Let A(s) = true iff auth is correctly enforced for call sequence s.

**Sealed (`impl_ownable!`)**: For all s in S, A(s) = true, because auth is
generated as inherent methods that cannot be overridden. The developer cannot
introduce auth bypass. Safety property: universally quantified.

**Flexible (`#[contractimpl(contracttrait)]`)**: There exists a subset S' of S
where A(s) may be false, specifically when the developer overrides a default
method and omits the auth check. Safety property: existentially quantified
(depends on developer behavior).

The framework correctly identifies this distinction. The documentation should
frame it formally: sealed provides a safety invariant; flexible provides a
liveness property (the ability to customize). These are in tension, and the
developer must choose.

## Cross-Contract Composition and the Byzantine Generals

The provider pattern works within a single contract. But what about cross-
contract composition? Consider:

```
Contract A: Ownable (provider reads owner from Contract B)
Contract B: Ownable (provider reads owner from storage)
```

If Contract A's provider calls Contract B to resolve the owner, we have a
cross-contract dependency. In BFT terms, Contract A trusts Contract B to
provide a correct owner address. If Contract B is compromised (or upgraded
to return a malicious address), Contract A's auth is bypassed.

This is the smart contract equivalent of the Byzantine Generals Problem:
Contract A cannot distinguish between a correct and a faulty Contract B
without additional verification.

**Recommendation**: The framework should provide guidance on cross-contract
provider patterns. Specifically:

1. Providers that call external contracts should be explicitly marked
   (e.g., `#[external_dependency]`).
2. The documentation should warn that cross-contract providers introduce
   trust assumptions beyond the current contract's control.
3. Consider a `verify_source` pattern where the provider includes a hash
   of the expected contract WASM, checked at invocation time.

## The `require_auth()` Caching Assumption

The blog post claims that `require_auth()` is "the same as OZ's
`enforce_owner_auth`" in terms of gas cost. This is approximately true but
ignores an important detail: `require_auth()` in Soroban does not just verify
a signature. It records the auth requirement in the transaction's auth tree,
which is verified by the network validators.

If a provider's `owner()` method performs an expensive computation (e.g.,
resolving a multisig threshold by reading N signer addresses), this
computation happens at auth resolution time, BEFORE the provider's business
logic executes. If the business logic then fails (e.g., insufficient balance),
the auth computation was wasted.

In a high-throughput scenario, this matters. The framework should consider
lazy auth evaluation: resolve and cache the auth address, but delay the
actual `require_auth()` call until after basic precondition checks:

```rust
fn transfer(env: &Env, from: Address, to: Address, amount: i128) {
    // Check preconditions first (cheap)
    let balance = Self::Provider::balance(env, from.clone());
    assert!(balance >= amount, "insufficient balance");

    // Auth check second (expensive)
    from.require_auth();

    // Business logic
    Self::Provider::transfer(env, from, to, amount);
}
```

This reordering is safe because both checks are assertions (no state mutation
before the business logic). It is more gas-efficient because failed
preconditions short-circuit before the expensive auth check.

**However**: This reordering changes the error semantics. A caller with
insufficient balance but valid auth would see "insufficient balance" instead
of the current behavior. Whether this information leakage is acceptable
depends on the application.

## Verdict

The framework's composition model preserves Soroban's atomicity guarantees
within single-contract invocations. The sealed pattern provides formally
stronger safety than the flexible pattern. The main concern is the TOCTOU
vulnerability in provider implementations, which is inherent to any DI
pattern and should be documented as a known limitation.

The cross-contract case requires additional thought. The provider pattern's
greatest strength (pluggability) is also its greatest weakness in a Byzantine
environment: trust boundaries become implicit rather than explicit.

**Rating: 7.5/10 for safety** -- sound within its trust boundary, needs
formal documentation of assumptions and limitations.
