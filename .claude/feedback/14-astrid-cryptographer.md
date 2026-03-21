# Review: soroban-sdk-tools `#[contracttrait]` Macro

**Reviewer:** Astrid -- Cryptographer who designed threshold signature schemes
**Focus:** Key management, multi-party computation, threshold auth patterns

---

## Overall Impression

I evaluate this project through the lens of cryptographic protocol design,
specifically the intersection of key management, multi-party authorization, and
threshold schemes with smart contract composability. The `#[contracttrait]`
macro addresses an important architectural concern -- structural auth
enforcement -- but its current design reveals several assumptions about
authorization models that deserve cryptographic scrutiny.

The project's strength is its clean separation between auth enforcement (the
outer trait) and auth resolution (the provider). This separation is
architecturally sound and maps well to how cryptographic protocols are designed:
the protocol specifies *what* must be authenticated, and the cryptographic
primitive specifies *how*. This is the correct separation of concerns.

However, the current implementation treats authorization as a binary,
single-party operation (`require_auth()` on a single `Address`). This model is
insufficient for the multi-party, threshold, and time-locked authorization
schemes that real-world financial systems require. The question is whether the
current abstraction can be extended to support these patterns, or whether
fundamental changes are needed.

After careful analysis, I believe the abstraction is sufficient -- threshold
authorization can be implemented through Soroban's account-level multisig and
through multisig contract addresses as providers. But the documentation and
examples need significant expansion to guide developers toward correct
threshold implementations.

---

## Strengths

### 1. Clean Auth/Logic Separation

The two-trait architecture (Internal for logic, Outer for auth) mirrors the
fundamental principle in cryptographic protocol design: separate the security
mechanism from the application logic. This separation enables formal
verification of each layer independently.

The `OwnableInternal` trait is purely functional -- given an environment, it
returns data or modifies state. The outer `Ownable` trait adds the auth
invariant. This is exactly how we design signature schemes: the message
processing is separate from the signature verification. Neither layer needs
to know the internal details of the other.

This separation also means that a formal verification effort can focus on
the macro's code generation (does it correctly generate `require_auth()` calls?)
independently from the provider's correctness (does it return the right
address?). Neither the macro nor the provider is the complete security story,
but each can be verified in isolation.

### 2. Provider Pattern Enables Algorithm Agility

In cryptography, "algorithm agility" means the ability to swap one primitive
for another without changing the protocol. The TLS 1.3 specification, for
example, defines a protocol that works with any AEAD cipher -- you can swap
AES-GCM for ChaCha20-Poly1305 without changing the handshake logic.

The provider pattern achieves this for authorization:

```rust
impl_ownable!(MyContract, SingleOwner);    // Ed25519 single-key
impl_ownable!(MyContract, MultisigOwner);  // threshold scheme
impl_ownable!(MyContract, TimelockOwner);  // time-locked auth
```

This is the correct abstraction level. The contract does not care *how*
ownership is determined -- it only cares *that* ownership is determined. The
provider encapsulates the cryptographic details. This means a contract can
transition from single-key to threshold authorization without any changes
to the contract's logic or interface.

### 3. Sealed Pattern Prevents Auth Downgrade Attacks

In cryptographic protocol design, a "downgrade attack" forces a system to use
a weaker security mechanism than intended. The POODLE attack on TLS exploited
the ability to negotiate SSL 3.0 instead of TLS 1.2. The DROWN attack
exploited SSLv2 support to break TLS connections.

The `impl_ownable!` sealed pattern prevents the smart contract equivalent:
overriding the default auth implementation with a weaker one. Because the
sealed macro generates inherent methods (not trait defaults), a developer
cannot accidentally (or maliciously) replace the auth check with a no-op.

This is an important security property. The blog post's framing of the
"override problem" is correct: the ability to override trait defaults is
functionally equivalent to the ability to downgrade cipher suites. Both
represent attack surfaces where a stronger mechanism can be replaced with
a weaker one.

### 4. Auth Address Caching is Cryptographically Prudent

The generated code caches the auth address:

```rust
let __auth_addr = Self::Provider::owner(env);
__auth_addr.require_auth();
Self::Provider::transfer_ownership(env, new_owner);
```

This cache-then-verify pattern prevents a class of bug where `owner()` returns
different values on sequential calls. In cryptography, we call this "binding"
-- the auth check is bound to a specific identity, and the subsequent operation
uses the same binding. If `owner()` were called twice (once for auth, once for
the operation), a TOCTOU vulnerability could arise if the ownership changed
between the two calls.

---

## Concerns

### 1. Single-Address Auth Model and Threshold Signatures

The `#[auth(Self::owner)]` annotation resolves to a single `Address` and calls
`require_auth()` on it. This assumes a 1-of-1 authorization model at the
contract level. But real-world authorization often requires k-of-n threshold
signatures.

The question is: can threshold auth be implemented within the current model?

**Yes, through two mechanisms:**

1. **Soroban account-level multisig:** A Stellar account can have multiple
   signers with weights, and the account threshold determines how many must
   sign. The `owner()` method returns this account's address, and
   `require_auth()` checks the account's multisig threshold. This works
   transparently -- no changes to the macro needed.

2. **Multisig contract address:** A separate contract can implement k-of-n
   voting logic. The `owner()` method returns this contract's address, and
   `require_auth()` triggers the multisig contract's validation logic.

Both approaches work, but they conflate two concerns:
- The *contract's* authorization model (who can do what at the application level)
- The *account's* or *multisig contract's* authorization model (how many signers
  are needed at the crypto level)

A more explicit separation would support contract-level multi-party authorization
independent of account-level key management. Consider:

```rust
#[auth(Self::publisher, Self::songwriter)]  // both must authorize
fn update_split(env: &Env, holder: Address, bps: u32);
```

This would generate:
```rust
Self::Provider::publisher(env).require_auth();
Self::Provider::songwriter(env).require_auth();
```

Two independent auth checks, each potentially backed by their own threshold
scheme. This is more expressive than the current single-address model and
maps to real-world scenarios in finance, governance, and rights management.

### 2. No Support for Authorization Quorums at the Contract Level

Consider a DAO treasury contract where different operations require different
quorum sizes:
- Transfer < 1000 tokens: 1-of-5 signers
- Transfer >= 1000 tokens: 3-of-5 signers
- Change signers: 4-of-5 signers

The current `#[auth(Self::owner)]` annotation cannot express this because it
resolves to a single address regardless of the operation's parameters. To
implement quorum-based auth, the developer must push the quorum logic entirely
into the provider:

```rust
impl TreasuryInternal for TreasuryProvider {
    fn transfer(env: &Env, to: Address, amount: i128) {
        // Quorum logic is here, not in the auth layer
        if amount >= 1000 {
            assert!(TreasuryStorage::has_quorum(env, 3));
        }
        // ...
    }
}
```

But then the quorum check is in the business logic layer, not the auth layer.
The structural guarantee is lost.

**Recommendation:** Support a more general `#[auth(Self::authorize_transfer)]`
pattern where the auth method can take parameters and implement complex logic:

```rust
#[auth(Self::authorize_transfer(amount))]
fn transfer(env: &Env, to: Address, amount: i128);
```

Where `authorize_transfer` is a method on the Internal trait that receives
the method arguments and performs the appropriate auth check (potentially
calling `require_auth()` on multiple addresses based on the amount).

### 3. No Temporal Auth Constraints

Cryptographic protocols commonly include temporal constraints:
- Nonces to prevent replay
- Timestamps to enforce time windows
- Sequence numbers for ordering

The `#[auth]` annotation has no temporal dimension. Soroban's `require_auth()`
handles replay protection at the protocol level (via nonces in the auth
entries), but application-level temporal constraints are the provider's
responsibility. The documentation should make this explicit.

Consider:
- "This operation requires auth after a mandatory 24-hour waiting period"
- "This operation requires auth with a nonce greater than the last used nonce"
- "This auth expires after 100 ledger sequences"

These constraints are common in financial systems and should be documented as
provider-level concerns with example implementations.

### 4. Key Rotation and Recovery Are Not Addressed

The `owner()` method returns a single address. If that address's key is
compromised, the contract's security is entirely broken. Key rotation and
recovery are critical concerns:

- **Key rotation:** Changing the owner address should have a grace period where
  *both* old and new keys are valid (to prevent lockout due to timing issues).
  OZ's two-step transfer (transfer + accept) handles this correctly.
- **Social recovery:** If the owner's key is lost, a recovery mechanism (e.g.,
  3-of-5 guardians) should be able to designate a new owner.
- **Key escrow:** Some regulatory environments require key escrow -- the
  ability for a designated party to recover access.

The provider pattern *can* support all of these, but there are no examples or
guidance. The blog post mentions that two-step transfers "can be added in
provider" but does not show how.

**Recommendation:** Provide reference providers for:
- `TwoStepOwner` -- transfer + accept pattern (matching OZ's approach)
- `RecoverableOwner` -- guardian-based key recovery
- `TimelockOwner` -- delayed ownership transfers
- `GracePeriodOwner` -- both old and new keys valid during transition

### 5. The AuthClient and Threshold Testing

The `CallBuilder::sign()` method accepts individual signers and creates one
`SorobanAuthorizationEntry` per signer. This is a multi-signer model, not a
threshold model:

```rust
for signer in signers.iter() {
    // Each signer produces an independent auth entry
    entries.push(SorobanAuthorizationEntry { ... });
}
```

In threshold cryptography, k parties cooperate to produce ONE signature that
verifies against ONE public key. The parties never independently produce valid
signatures -- they produce shares that are combined.

The current architecture cannot directly express threshold signing because each
signer gets its own nonce, its own credentials, and its own entry. The
signatures are independent, not aggregated.

This is correct for Soroban's current auth model. But for testing threshold
patterns, a `ThresholdSigner` adapter that combines shares internally would
work with the existing API:

```rust
struct ThresholdSigner {
    combined_address: Address,
    shares: Vec<PartialShare>,
}

impl Signer for ThresholdSigner {
    fn address(&self) -> Address { self.combined_address.clone() }
    fn sign_payload(&self, env: &Env, payload: &[u8; 32]) -> ScVal {
        // Combine shares, produce one signature
        combine_threshold_shares(&self.shares, payload)
    }
}
```

This pattern should be documented as a first-class example.

### 6. Nonce Generation in Tests

The `setup_real_auth` function in `builder.rs` uses a thread-local monotonic
counter for nonces:

```rust
static NONCE_COUNTER: Cell<i64> = const { Cell::new(0) };
```

This is fine for testing but should be explicitly documented as test-only.
Production nonce generation requires cryptographic randomness to prevent
pre-computation attacks. A comment or doc attribute on the counter clarifying
its test-only nature would prevent misuse.

---

## Suggestions

### 1. Support Multi-Address Auth Annotations

Extend `#[auth]` to accept multiple auth sources:

```rust
#[auth(Self::publisher, Self::songwriter)]  // both must authorize
fn update_split(env: &Env, holder: Address, bps: u32);
```

This would generate:
```rust
fn update_split(env: &Env, holder: Address, bps: u32) {
    Self::Provider::publisher(env).require_auth();
    Self::Provider::songwriter(env).require_auth();
    Self::Provider::update_split(env, holder, bps);
}
```

### 2. Provide Threshold Provider Templates

Create reference implementations:
- `ThresholdOwner<const K: usize>` -- k-of-n via multisig contract
- `TwoStepOwner` -- transfer + accept pattern
- `RecoverableOwner` -- guardian-based recovery
- `TimelockOwner` -- delayed transfers with configurable delay

### 3. Document the TOCTOU Property

Add a security note explaining:
- The auth address is cached per method call
- The auth check happens before the method body executes
- Modifications to the auth source within the method body do not affect the
  current call's auth check
- This is by design and prevents double-read inconsistencies

### 4. Document Temporal Auth Patterns

Provide example providers for:
- Timelock authorization (mandatory waiting period before execution)
- Expiring authorization (auth must be renewed periodically)
- Nonce-sequenced authorization (operations must happen in order)

### 5. Add a ThresholdSigner Example

Show how to implement a `ThresholdSigner` adapter for the `Signer` trait
that combines partial signature shares into a single valid signature.
Include the adapter in the examples directory with a complete test.

### 6. Add `.with_expiration()` to CallBuilder

For threshold signing ceremonies that happen over time (hours or days for
gathering signatures), the signature expiration should be configurable:

```rust
auth_client.transfer_ownership(&new_owner)
    .sign(&signer)
    .with_expiration(current_ledger + 1000)  // 1000 ledger sequences
    .invoke();
```

### 7. Create a Formal Security Model

Document the exact security properties:
- **Guaranteed:** Auth enforcement in the outer trait wrapper (sealed path)
- **Assumed:** Provider correctness (returns valid addresses, modifies state correctly)
- **Not addressed:** Key management, rotation, recovery, temporal constraints
- **Delegated to Soroban:** Replay protection, nonce management, signature verification

This model would be invaluable for security auditors.

---

## Unique Perspective: From Signatures to Smart Contracts

As a cryptographer, I see a deep parallel between this project and the
evolution of digital signature schemes:

1. **First generation (OZ):** Authentication is procedural. Each method call
   includes explicit auth checks, like how early protocols included signature
   verification inline with message processing.

2. **Second generation (soroban-sdk-tools):** Authentication is structural.
   The type system enforces that auth checks exist, like how modern protocols
   use typed wrappers to ensure signatures are verified before messages are
   processed.

3. **Third generation (not yet built):** Authentication is composable. Auth
   requirements can be combined, composed, and transformed using logical
   operators, like how modern threshold schemes combine partial signatures
   into aggregate signatures.

This project is firmly in the second generation and pushing toward the third.
The provider pattern is the right abstraction for reaching the third generation,
but the `#[auth]` annotation needs to grow beyond single-address resolution.

The most impactful improvement would be supporting `#[auth]` annotations that
resolve to *authorization policies* rather than *addresses*. An authorization
policy could be "3-of-5 signers" or "admin AND timelock" or "any member of
role X." The provider would implement the policy, and the outer trait would
enforce it. This would bridge the gap between the structural guarantees of
the current system and the expressiveness needed for real-world multi-party
authorization.

---

## Would I Use This?

For simple ownership models (single owner, basic multisig via Soroban account
thresholds), yes. The structural enforcement is a genuine improvement over
convention-based approaches, and the provider pattern provides adequate
algorithm agility for straightforward authorization models.

For complex multi-party authorization (contract-level threshold schemes,
weighted voting with parameter-dependent quorums, time-locked governance),
the tool works but requires significant provider-level complexity that is
not structurally enforced. The auth check layer is clean, but the auth
resolution layer (the provider's `owner()` method) bears all the complexity.

For threshold signing in tests, the existing `Signer` trait is adaptable via
a combiner adapter, which is acceptable.

**Verdict:** Excellent foundation with a clear architectural direction. The
auth/logic separation is cryptographically sound. The provider pattern's
algorithm agility is well-designed. To reach production readiness for complex
financial systems, the auth model needs two enhancements: multi-address auth
annotations and documented threshold provider patterns. Neither requires
fundamental changes to the architecture -- they are extensions of the existing
design.
