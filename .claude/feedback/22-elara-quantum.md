---
reviewer: Elara Christodoulou
role: Post-Quantum Cryptography Researcher
domain: Lattice-Based Signatures, NIST PQC Standards, Crypto-Agility
date: 2026-03-21
focus: Future-proofing against quantum attacks, crypto-agility
---

# Review: soroban-sdk-tools -- Post-Quantum Readiness Assessment

## Context

NIST finalized ML-DSA (formerly CRYSTALS-Dilithium) and SLH-DSA (formerly
SPHINCS+) as post-quantum signature standards in 2024. The migration
timeline for financial systems is measured in years, but the "harvest now,
decrypt later" threat means data signed today with classical algorithms
may be forged retroactively once cryptographically relevant quantum
computers exist.

I review `soroban-sdk-tools` for crypto-agility: can the auth architecture
accommodate a transition from Ed25519/secp256k1 to post-quantum signature
schemes without breaking existing contracts?

## Crypto-Agility Analysis

### The `Signer` Trait as an Abstraction Boundary

The `Signer` trait in `builder.rs` provides a clean abstraction:

```rust
pub trait Signer {
    fn address(&self) -> Address;
    fn sign_payload(&self, env: &Env, payload: &[u8; 32]) -> ScVal;
}
```

This is algorithm-agnostic. The trait does not specify Ed25519, secp256k1,
or any particular scheme. A post-quantum signer would implement the same
trait:

```rust
struct MlDsaSigner {
    address: Address,
    secret_key: ml_dsa::SecretKey,
}

impl Signer for MlDsaSigner {
    fn address(&self) -> Address { self.address.clone() }
    fn sign_payload(&self, _env: &Env, payload: &[u8; 32]) -> ScVal {
        let sig = self.secret_key.sign(payload);
        // Encode as ScVal -- but this is where problems start
        ScVal::Bytes(sig.to_bytes().try_into().unwrap())
    }
}
```

The `Signer` trait itself is quantum-ready. The problems lie deeper.

### Problem 1: Signature Size

Ed25519 signatures are 64 bytes. ML-DSA-65 signatures are 3,309 bytes.
SLH-DSA-128s signatures are 7,856 bytes.

The `ScVal` encoding can handle arbitrary byte arrays, but Soroban
transactions have size limits. A single ML-DSA signature consumes
significant transaction space. With multi-party auth (multiple signers),
the overhead multiplies.

The `setup_real_auth` function creates one `SorobanAuthorizationEntry`
per signer:

```rust
for signer in signers.iter() {
    entries.push(SorobanAuthorizationEntry {
        credentials: SorobanCredentials::Address(SorobanAddressCredentials {
            signature, // <-- 3,309+ bytes for PQ
            ...
        }),
        ...
    });
}
```

**Observation**: The current architecture handles PQ signatures transparently
at the signing layer, but transaction size limits may prevent practical use
with multiple PQ signers. This is a Soroban-level constraint, not a
soroban-sdk-tools one.

### Problem 2: Hash Payload is 32 Bytes

```rust
fn sign_payload(&self, env: &Env, payload: &[u8; 32]) -> ScVal;
```

The payload is SHA-256 output (32 bytes). Post-quantum hash functions like
SHA-3 also produce 32-byte digests, so the payload size is fine. However,
the hash algorithm itself matters:

The `setup_real_auth` function uses:

```rust
let payload_hash = {
    let bytes = soroban_sdk::Bytes::from_slice(env, &buf);
    env.crypto().sha256(&bytes).to_array()
};
```

SHA-256 is quantum-resistant for collision resistance (Grover's algorithm
only provides a quadratic speedup on preimage attacks, requiring 2^128
operations). But some organizations may require SHA-3 for compliance.
The hash algorithm is hardcoded, not configurable.

**Suggestion**: The hash function should be part of the configuration,
not hardcoded. Or at minimum, document that SHA-256 remains
quantum-resistant for signing purposes and that the hash algorithm is
dictated by Soroban's XDR spec, not by this library.

### Problem 3: The `SorobanCredentials` XDR Schema

The deeper issue is that `SorobanCredentials::Address` expects a specific
signature format defined in Soroban's XDR schema. Post-quantum algorithms
would require XDR schema changes at the protocol level. This is outside
the scope of `soroban-sdk-tools`, but the library's architecture determines
how easily it can adapt when those changes come.

The `Signer` trait's return type of `ScVal` is the right choice -- it is
the most flexible XDR type and can encode any byte sequence. A protocol
upgrade that adds PQ credential types would only require:

1. New `SorobanCredentials` variants in the XDR
2. New `Signer` implementations in the library
3. No changes to `#[contracttrait]`, `CallBuilder`, or user contracts

This is good crypto-agility.

### Problem 4: Hybrid Signatures (Classical + PQ)

During the transition period (which may last a decade), best practice is
to require BOTH a classical and a post-quantum signature. This ensures
security even if one scheme is broken.

The current `CallBuilder` supports multiple signers:

```rust
builder.sign(&ed25519_signer).sign(&ml_dsa_signer).invoke();
```

But each signer produces an independent `SorobanAuthorizationEntry`.
A hybrid signature should be a single entry with both signatures bundled.
This requires a composite signer:

```rust
struct HybridSigner {
    classical: Ed25519Signer,
    pq: MlDsaSigner,
}

impl Signer for HybridSigner {
    fn sign_payload(&self, env: &Env, payload: &[u8; 32]) -> ScVal {
        let classical_sig = self.classical.sign(payload);
        let pq_sig = self.pq.sign(payload);
        // Bundle both signatures as a single ScVal
        ScVal::Vec(Some(vec![
            ScVal::Bytes(classical_sig.into()),
            ScVal::Bytes(pq_sig.into()),
        ].try_into().unwrap()))
    }
}
```

This works with the current `Signer` trait. The abstraction is flexible
enough.

## Recommendations

### 1. Document the Crypto-Agility Story

The `Signer` trait is the key abstraction for PQ readiness. The
documentation should explicitly state:

- The `Signer` trait is algorithm-agnostic
- Post-quantum algorithms can be added as new `Signer` implementations
- Hybrid signing is supported via composite `Signer` implementations
- The hash function (SHA-256) is protocol-mandated, not library-chosen

### 2. Add a `SignatureScheme` Marker

For contracts that want to declare their signature requirements:

```rust
#[contracttrait]
pub trait Ownable {
    type SignatureScheme: SignatureVerifier;

    #[auth(Self::owner)]
    fn transfer_ownership(env: &Env, new_owner: Address);
}
```

This would allow compile-time verification that the signer matches the
contract's expectations.

### 3. Consider Signature Aggregation

For multi-party PQ auth, signature aggregation (e.g., via lattice-based
aggregate signatures) would be crucial to manage transaction size. The
`setup_real_auth` function could support an aggregation mode where multiple
signers produce one combined entry.

### 4. Test with Large Signatures

Add integration tests that simulate PQ-sized signatures (3,000+ bytes)
to verify that the `CallBuilder`, XDR serialization, and auth setup
handle large payloads correctly. Even if Soroban does not yet support
PQ natively, proving the library handles large `ScVal` signatures
is valuable.

### 5. Prepare for Key Size Growth

PQ public keys are also larger (ML-DSA-65: 1,952 bytes vs Ed25519: 32
bytes). If the `Address` type is extended to support PQ keys, the
`OwnableInternal::owner()` return value will carry larger data. Providers
should be tested with large address types.

## Summary

soroban-sdk-tools has good crypto-agility fundamentals. The `Signer` trait
abstraction is algorithm-agnostic, composite/hybrid signers work within
the current API, and the `ScVal` return type can encode arbitrary signature
formats. The main risks are transaction size limits with PQ signatures
(a Soroban-level constraint), hardcoded SHA-256 (protocol-mandated), and
lack of documentation about the PQ migration path. Adding explicit
crypto-agility documentation and large-signature tests would position
this library ahead of most blockchain tooling for the post-quantum
transition.
