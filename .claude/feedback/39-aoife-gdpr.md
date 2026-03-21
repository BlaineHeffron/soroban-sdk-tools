---
reviewer: Aoife O'Sullivan
role: Data Protection Officer & GDPR Specialist
domain: Privacy Regulation, Right to Erasure, Data Minimization
date: 2026-03-21
focus: GDPR compliance, right to be forgotten, immutability conflicts
---

# Review: soroban-sdk-tools -- GDPR Compliance Analysis

## Context

I advise organizations on GDPR compliance. The fundamental tension between
blockchain (append-only, immutable) and GDPR (right to erasure, data
minimization) is my daily work. I review `soroban-sdk-tools` for patterns
that help or hinder GDPR compliance in Soroban smart contracts.

Article 17 of the GDPR grants individuals the right to have their personal
data erased. Article 25 requires data protection by design and by default.
Article 5(1)(c) mandates data minimization. These requirements are not
suggestions -- they carry fines of up to 4% of global annual turnover.

## Analysis

### 1. Address Storage is Personal Data

Under GDPR guidance (and confirmed by the CJEU), blockchain addresses
that can be linked to natural persons constitute personal data. The
`SingleOwner` provider stores an address in contract storage:

```rust
env.storage().instance().set(&Symbol::new(env, "owner"), &owner);
```

This address is:
- Stored on-chain (persistent, globally replicated)
- Readable by anyone who queries the contract
- Linked to all transactions the owner has made

If this address can be linked to a natural person (which is often possible
through exchange KYC, ENS-like services, or social media posts), it is
personal data under GDPR.

**The `transfer_ownership` function does not erase the previous owner's
address.** It overwrites it with a new value. In Soroban's storage model,
the previous value is not necessarily erased from all nodes' state history.

### 2. The Sealed Macro and Data Protection by Design

Article 25 requires "data protection by design." The `impl_ownable!` sealed
macro is actually a positive GDPR pattern because:

- It prevents unauthorized access to ownership modification
- Auth checks cannot be bypassed, reducing breach risk
- The structural enforcement is auditable

If a Data Protection Impact Assessment (DPIA) is conducted, the sealed
macro provides evidence of "appropriate technical measures" under
Article 32.

### 3. Soroban's TTL Mechanism and Right to Erasure

Soroban's storage has a TTL mechanism: data that is not extended
eventually expires and is removed from the ledger. This is the closest
thing to "erasure" available in a blockchain context.

The OZ comparison document notes that OZ handles TTL management better.
For GDPR purposes, TTL management is not a nice-to-have -- it is the
primary mechanism for implementing data lifecycle policies.

**Suggestion**: The Provider pattern should include explicit TTL management:

```rust
pub trait OwnableInternal {
    fn owner(env: &Env) -> Address;
    fn transfer_ownership(env: &Env, new_owner: Address);

    // GDPR: configure data retention period
    fn data_retention_ledgers() -> u32 { 6_307_200 } // ~1 year at 5s ledgers
}
```

The generated outer trait would automatically extend TTL on access, and
data would expire after the retention period if not accessed.

### 4. Data Minimization in Trait Design

The example traits store minimal data, which is good. But the blog post
shows patterns that could lead to excessive data collection:

```rust
fn custody_history(env: &Env) -> Vec<(Address, Symbol, u64)>;
```

A custody history containing addresses and timestamps is a rich dataset
for tracking individuals. Under data minimization (Article 5(1)(c)),
contracts should store only what is strictly necessary.

**Suggestion**: Document a data minimization pattern using commitments:

```rust
// Instead of storing addresses in history:
fn custody_proof(env: &Env) -> BytesN<32>;  // Merkle root of custody chain

// The full history is stored off-chain; only the proof is on-chain
// Individuals can request deletion of off-chain records
```

### 5. The Right to Object (Article 21)

Data subjects have the right to object to processing. In contract terms,
this means an address should be able to opt out of being processed by a
contract. The current architecture has no opt-out mechanism.

**Suggestion**: Consider a `#[gdpr_aware]` attribute that generates:

```rust
fn has_objected(env: &Env, addr: Address) -> bool;

fn object_to_processing(env: &Env, addr: Address);
// Generated: checks objection status before processing
```

### 6. Event Emission and Data Subject Access Requests (DSARs)

If events are added to the generated outer trait (as recommended in the
OZ comparison), every ownership transfer, pause, and state change emits
a public event containing addresses. Under GDPR, a data subject can
request all data held about them (Article 15).

Events on Stellar are queryable via Horizon. A DSAR response would need
to include all events referencing the subject's address. This is
technically feasible but operationally complex.

**Suggestion**: Events containing personal data should use pseudonymized
identifiers:

```rust
// Instead of:
env.events().publish(("transfer", from, to, amount));

// Use:
let from_hash = env.crypto().sha256(&from.to_bytes());
env.events().publish(("transfer", from_hash, amount));
```

The mapping between hash and address is maintained off-chain and can be
deleted under Article 17.

### 7. Cross-Border Data Transfers

Stellar network nodes operate globally. Storing personal data (addresses
linked to EU residents) on a global blockchain constitutes cross-border
data transfer under Chapter V of GDPR. The legal basis for this transfer
is unclear.

This is a Soroban-level concern, not specific to `soroban-sdk-tools`.
But the documentation should acknowledge this for EU-based developers.

## Compliance Checklist for Contract Developers

Based on this review, contracts using `soroban-sdk-tools` should:

1. Conduct a DPIA before storing any address data on-chain
2. Use commitment schemes instead of storing addresses where possible
3. Configure TTL on all storage entries containing personal data
4. Document the legal basis for processing (Article 6)
5. Implement pseudonymization in events
6. Provide a mechanism for the data controller to respond to DSARs
7. Use the sealed macro (`impl_ownable!`) as evidence of Article 32 compliance
8. Avoid storing custody histories with identifiable addresses on-chain
9. Consider the Provider pattern for implementing data lifecycle policies
10. Document cross-border data transfer implications for EU deployments

## Recommendations for the Library

1. **Add a GDPR considerations section** to the documentation
2. **Implement TTL management** in the Provider interface or as a supertrait
3. **Provide commitment-based storage examples** as an alternative to plaintext
4. **Document the relationship** between Soroban TTL expiry and right to erasure
5. **Make event emission opt-in** with pseudonymization options
6. **Add a `DataLifecycle` trait** that providers can implement:

```rust
#[contracttrait]
pub trait DataLifecycle {
    fn retention_period_ledgers(env: &Env) -> u32;
    fn extend_retention(env: &Env);
    fn request_erasure(env: &Env, subject: Address);
}
```

## Summary

soroban-sdk-tools has both GDPR advantages and risks. The sealed macro
provides evidence of data protection by design. The Provider pattern can
implement data lifecycle policies. Soroban's TTL mechanism enables a form
of data erasure. However, the default patterns store personal data
(addresses) in persistent, globally-replicated storage without TTL
management, data minimization guidance, or pseudonymization. The most
impactful addition would be TTL management in the Provider interface and
documentation of GDPR-aware contract design patterns.
