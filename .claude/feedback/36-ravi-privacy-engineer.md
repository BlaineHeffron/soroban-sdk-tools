# Review: soroban-sdk-tools -- Privacy & Zero-Knowledge Compatibility

**Reviewer:** Ravi -- Privacy engineer building zero-knowledge proof systems
**Date:** 2026-03-21
**Materials Reviewed:** oz-comparison.md, blog-post-composable-contracts.md, trait-test example, contract.rs macro

---

## Overall Impression

I build ZK proof systems for privacy-preserving applications. My primary
concern with any smart contract framework is: how does it interact with
privacy? Specifically:

1. Does the auth model leak information about who is authorizing?
2. Can the composability framework support confidential transactions?
3. Is the generated code ZK-compatible (can it run inside a proof circuit)?
4. Does the storage model expose relationships that should be private?

`soroban-sdk-tools` is not a privacy framework, and it does not claim to
be. But its design decisions have significant implications for privacy,
both positive and negative. This review analyzes those implications.

---

## Auth Model and Privacy

### The Transparency Problem

The `#[auth(Self::owner)]` pattern generates:

```rust
let __auth_addr = Self::Provider::owner(env);
__auth_addr.require_auth();
```

This reveals TWO pieces of information on-chain:

1. **Who the owner is** -- `Self::Provider::owner(env)` reads the owner
   from storage, making it publicly queryable
2. **That the owner authorized this specific action** -- `require_auth()`
   records the authorization in the transaction's auth entries

For many use cases, this transparency is desired. For privacy-sensitive
applications, it is a problem. Consider:

- A whistleblower contract where revealing the owner's identity is
  dangerous
- A voting contract where the voter's choice should not be linkable to
  their address
- A medical records contract where the patient's identity should not be
  associated with their health data

### Privacy-Preserving Auth Patterns

The provider pattern could support privacy-preserving auth if the framework
supported it:

```rust
pub struct ZKOwner;
impl OwnableInternal for ZKOwner {
    fn owner(env: &Env) -> Address {
        // Returns a commitment, not a real address
        // The actual owner proves knowledge of the preimage
        ZKStorage::get_owner_commitment(env)
    }

    fn transfer_ownership(env: &Env, new_owner: Address) {
        // Verifies a ZK proof that the caller knows the current owner's
        // secret, without revealing the current owner's address
        ZKStorage::update_commitment(env, &new_owner);
    }
}
```

The challenge is that `require_auth()` in Soroban is inherently
transparent -- it requires the caller to identify themselves. For ZK auth,
you would need a `require_proof()` equivalent that verifies a proof
without revealing the prover's identity.

### Recommendation: Auth Source Abstraction

The `AuthSource` enum in contract.rs (line 49-54) currently has two
variants:

```rust
enum AuthSource {
    ProviderMethod(Ident),
    Param(Ident),
}
```

Consider adding a third:

```rust
/// ZK auth -- verify a proof instead of requiring a signature
ZKProof { verifier: Ident, proof_param: Ident },
```

This would generate:

```rust
fn transfer_ownership(env: &Env, proof: ZKProof) {
    let verifier = Self::Provider::zk_verifier(env);
    assert!(verifier.verify(&proof), "invalid proof");
    Self::Provider::transfer_ownership(env, proof.new_owner_commitment);
}
```

The auth check becomes proof verification instead of signature verification.
The caller's identity is never revealed.

---

## Confidential Transactions

### The State Visibility Problem

Soroban's storage is transparent. Every value written to instance storage
is readable by anyone. The provider pattern does not change this -- a
`FungibleTokenInternal` provider that writes balances to storage exposes
all balances publicly.

For confidential transactions, you need:

1. **Encrypted balances** -- Store encrypted values, not plaintext
2. **Range proofs** -- Prove a transfer is valid without revealing amounts
3. **Nullifier sets** -- Prevent double-spending without revealing
   which specific output is being spent

### How the Provider Pattern Helps

The provider pattern is actually well-suited for confidential transactions
because the INTERFACE (the outer trait) remains the same -- `transfer(from,
to, amount)` -- while the IMPLEMENTATION (the provider) handles encryption,
proofs, and nullifiers.

```rust
pub struct ConfidentialToken;
impl FungibleTokenInternal for ConfidentialToken {
    fn transfer(env: &Env, from: Address, to: Address, amount: i128) {
        // 'amount' here is actually an encrypted amount + proof
        // The provider verifies the proof and updates encrypted balances
        let proof = PedersenCommitment::verify(env, &from, &to, amount);
        assert!(proof.is_valid());
        EncryptedStorage::update_balances(env, &from, &to, &proof.commitments);
    }

    fn balance(env: &Env, addr: Address) -> i128 {
        // Returns an encrypted balance -- only the owner can decrypt
        EncryptedStorage::get_balance(env, &addr)
    }
}
```

The swap is transparent to the caller:

```rust
// Before: public token
impl_fungible_token!(MyToken, BasicToken);

// After: confidential token
impl_fungible_token!(MyToken, ConfidentialToken);
```

This is a compelling use of the provider pattern. The blog post should
highlight this as a future direction.

### Limitations

The provider pattern helps with implementation swapping, but Soroban itself
has limitations for ZK/privacy:

1. **No native ZK verifier** -- Soroban does not have built-in ZK proof
   verification. Proof verification must be implemented in WASM, which
   is expensive. Until Soroban adds ZK precompiles, confidential
   transactions will be impractical due to gas costs.

2. **No homomorphic storage** -- Soroban's storage is key-value with
   transparent values. Homomorphic encryption would require custom
   serialization.

3. **Transaction graph analysis** -- Even with encrypted values, the
   transaction graph (who transacts with whom) is visible. The provider
   pattern cannot fix this without a mixing/shielding protocol.

---

## ZK Compatibility of Generated Code

### Can the Generated Code Run in a ZK Circuit?

Short answer: no, and this is not a meaningful question for the current
architecture. The `#[contracttrait]` macro generates Soroban contract code
that runs in the Soroban runtime. ZK circuits run in entirely different
execution environments (R1CS, Plonk, STARKs).

However, there is a meaningful question: **can the trait definitions serve
as specifications for ZK circuits?** If the `Ownable` trait defines:

```rust
fn transfer_ownership(env: &Env, new_owner: Address);
```

...then a ZK circuit could implement the same specification with privacy
guarantees. The trait becomes a shared interface between the on-chain
contract and the off-chain proof generator.

This is how ZK rollups work: the on-chain contract defines the interface,
and the off-chain prover implements the logic in a ZK-compatible way.
The `#[contracttrait]` macro could generate BOTH:

1. Soroban contract code (current functionality)
2. ZK circuit specifications (future functionality)

### Recommendation: Trait as Specification

Consider the trait definition as a formal specification that can be
consumed by multiple backends:

- **Soroban backend**: generates contract code (current)
- **ZK backend**: generates circuit specifications (future)
- **Formal verification backend**: generates proof obligations (future)
- **TypeScript backend**: generates client SDK (partially implemented)

The current macro architecture (parse trait -> generate code) is
extensible to support multiple backends. This should be part of the
long-term roadmap.

---

## Privacy-Preserving Auth Testing

### AuthClient and Privacy

The `AuthClient` pattern tests authorization by explicitly specifying who
authorizes:

```rust
auth_client.transfer_ownership(&new_owner)
    .authorize(&owner)
    .invoke();
```

For privacy-preserving auth, the test should verify that a proof is valid
without revealing the authorizer:

```rust
// ZK auth testing
zk_auth_client.transfer_ownership(&new_owner_commitment)
    .with_proof(&zk_proof)
    .invoke();

// Verify: the proof is valid
// Verify: the new owner commitment is recorded
// Verify: the old owner's identity is NOT revealed in the transaction
```

The AuthClient generation could be extended to support ZK auth testing if
the `AuthSource::ZKProof` variant is added.

---

## Storage and Data Minimization

### The GDPR Angle

Storage on a public blockchain violates data minimization principles by
default -- everything stored is public and permanent. The provider pattern
does not address this directly, but it does enable a data minimization
strategy:

```rust
pub struct MinimalStorageProvider;
impl OwnableInternal for MinimalStorageProvider {
    fn owner(env: &Env) -> Address {
        // Store only a hash of the owner's address
        // The owner proves knowledge of the preimage
        let hash = env.storage().instance().get::<_, BytesN<32>>(&KEY).unwrap();
        // ... reconstruct address from proof
    }
}
```

But this requires changes to the `require_auth()` flow that are not
currently supported by Soroban.

### Recommendation: Privacy Metadata

The `#[contracttrait]` macro could support privacy annotations:

```rust
#[contracttrait]
pub trait MedicalRecords {
    #[private]  // This value should not be publicly queryable
    fn patient_data(env: &Env, patient: Address) -> EncryptedData;

    #[auth(patient)]
    fn update_record(env: &Env, patient: Address, data: EncryptedData);
}
```

The `#[private]` annotation would:

1. Generate a warning if the return type is not encrypted
2. Exclude the method from public query clients
3. Generate access control in the outer trait (only the patient can query
   their own data)

This is convention-based (not structurally enforced at the storage level),
but it would encourage privacy-by-design patterns.

---

## Composable Privacy Patterns

### Shielded Pools

A shielded pool (like Tornado Cash or Zcash's shielded pool) could be
implemented as a provider:

```rust
pub struct ShieldedPool;
impl FungibleTokenInternal for ShieldedPool {
    fn transfer(env: &Env, _from: Address, _to: Address, _amount: i128) {
        // Actual implementation uses commitments and nullifiers
        // from/to/amount are not used -- replaced by ZK proof
        panic!("Use shielded_transfer instead");
    }
}
```

But this exposes a limitation: the trait interface (`transfer(from, to,
amount)`) assumes transparent parameters. A shielded pool needs:

```rust
fn shielded_transfer(env: &Env, proof: ShieldedProof, nullifier: BytesN<32>);
```

The current `#[contracttrait]` macro generates methods with the exact
signature defined in the trait. For privacy, you would need to transform
the signature:

```rust
#[contracttrait(privacy = "shielded")]
pub trait FungibleToken {
    #[shielded]  // Generates: fn transfer(env, proof, nullifier) instead
    fn transfer(env: &Env, from: Address, to: Address, amount: i128);
}
```

This is speculative and complex, but it illustrates the gap between the
current composability model (which assumes transparent parameters) and
privacy requirements (which need parameter transformation).

---

## Analysis of the Blog Post's Security Claims

### "Structural Auth Enforcement That Cannot Be Accidentally Bypassed"

This is true for the sealed pattern (`impl_ownable!`), but the blog post
should clarify what "structural" means in a privacy context. The auth is
structural in the sense that it is generated by the macro and cannot be
overridden. But it is NOT structural in the sense that it provides privacy
guarantees -- the auth is still transparent (who authorized is visible).

### "Provider-Based Dependency Injection for Swapping Implementations"

This is the most privacy-relevant feature. The ability to swap a
`TransparentToken` for a `ConfidentialToken` with a single type parameter
change is powerful. But the blog post should note that the trait interface
constrains what privacy-preserving implementations are possible. If the
trait requires `fn balance(env, addr) -> i128`, you cannot return an
encrypted balance without changing the return type.

### "AuthClient Generation for Ergonomic Authorization Testing"

The AuthClient tests authorization flow but does not test privacy
properties. For privacy-preserving contracts, you need to test:

1. The transaction does not reveal the authorizer's identity
2. The transferred amount is not visible in the transaction data
3. The sender and receiver are not linkable through transaction analysis

These are fundamentally different from auth testing and would require a
`PrivacyClient` or `ZKClient` with different testing primitives.

---

## Practical Privacy Recommendations

### Short-Term (Can Be Done Now)

1. **Document privacy implications** -- The blog post should include a
   section on privacy considerations. Which aspects of a transaction
   using `#[auth]` are public? What can an observer learn?

2. **Encrypted storage helpers** -- Provide utility functions for storing
   encrypted values. Even if the encryption key management is off-chain,
   having standard patterns for encrypted storage reduces errors.

3. **Commitment-based ownership** -- Provide a `CommitmentOwner` provider
   that stores a commitment (hash) of the owner's address instead of the
   address itself. The owner proves knowledge of the preimage during auth.

### Medium-Term (Needs Soroban Changes)

4. **ZK proof verification precompile** -- Work with the Stellar team to
   add ZK proof verification as a host function. Without this, ZK-based
   privacy on Soroban is impractical.

5. **Private storage** -- Advocate for a storage tier that is not publicly
   queryable. Validators can read it for execution, but it is not exposed
   through the public API.

### Long-Term (Research)

6. **Multi-backend trait compilation** -- Generate ZK circuit
   specifications from trait definitions alongside Soroban contract code.
   This would enable a single trait definition to power both transparent
   on-chain execution and private off-chain proving.

7. **Homomorphic computation** -- Support providers that operate on
   encrypted data using fully homomorphic encryption. The trait interface
   remains the same; the provider performs all operations on ciphertexts.

---

## Verdict

`soroban-sdk-tools` is not a privacy tool, and evaluating it primarily on
privacy would be unfair. However, the architectural decisions it makes have
significant privacy implications:

**Positive:**
- The provider pattern enables swapping transparent implementations for
  privacy-preserving ones without changing the trait interface
- The sealed pattern prevents auth bypasses that could leak information
- The Internal/Outer trait separation cleanly separates business logic
  from authorization, which is the same separation needed for ZK circuits

**Negative:**
- The `#[auth]` pattern is inherently transparent (identity-revealing)
- The trait interface constrains privacy-preserving implementations
  (transparent parameter types)
- No privacy annotations or data classification in the macro
- No ZK-compatible testing in the AuthClient

**Neutral:**
- Storage privacy is a Soroban platform limitation, not a framework
  limitation
- ZK proof verification cost is a runtime limitation, not a framework
  limitation

The framework's extensibility (providers, macro-generated code) provides
a clean path toward privacy features. The foundation does not preclude
privacy; it just does not prioritize it. For a v1 composability framework,
this is acceptable. For v2, privacy should be a first-class concern.

**Rating:** 6/10 -- Sound architecture that does not preclude privacy but
does not enable it either. The provider pattern is the strongest asset for
future privacy work.

---

*"Privacy is not a feature you add later. It is an architectural decision
you make at the beginning. The provider pattern makes this decision
reversible -- that is its greatest privacy contribution."*
