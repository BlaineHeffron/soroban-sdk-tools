# Review: soroban-sdk-tools -- GDPR & Data Protection Analysis

**Reviewer:** Aoife -- Irish Data Protection Officer, GDPR specialist
**Date:** 2026-03-21
**Materials Reviewed:** oz-comparison.md, blog-post-composable-contracts.md, trait-test example, contract.rs macro

---

## Overall Impression

I have spent twelve years enforcing GDPR across EU organizations. When
blockchain projects come to me for compliance assessment, I ask five
questions:

1. Can personal data be deleted? (Right to erasure)
2. Is data collection minimized? (Data minimization)
3. Is there a lawful basis for processing? (Lawful processing)
4. Can data subjects exercise their rights? (Data subject rights)
5. Are there appropriate technical and organizational measures? (Security)

Smart contracts on public blockchains are fundamentally in tension with
GDPR. Data written to a blockchain is immutable, public, and permanent --
the opposite of what GDPR requires. However, the DESIGN PATTERNS used in
smart contracts can mitigate these tensions.

`soroban-sdk-tools` does not address GDPR directly. But its architectural
decisions have significant data protection implications, and the provider
pattern offers genuine opportunities for privacy-by-design.

---

## Article 17: Right to Erasure ("Right to Be Forgotten")

### The Fundamental Tension

GDPR Article 17 gives data subjects the right to have their personal data
erased. A blockchain makes this technically impossible -- data written to
the ledger is permanent.

The trait definitions in `soroban-sdk-tools` write data to Soroban's
storage:

```rust
fn transfer_ownership(env: &Env, new_owner: Address) {
    env.storage()
        .instance()
        .set(&soroban_sdk::Symbol::new(env, "owner"), &new_owner);
}
```

The `new_owner` Address is personal data (it identifies a natural person
or, through linkability, can be connected to a natural person). Once
written to instance storage, it is on the ledger permanently.

### Mitigating Patterns

The provider pattern enables GDPR-aware implementations:

```rust
pub struct GDPRCompliantOwner;
impl OwnableInternal for GDPRCompliantOwner {
    fn owner(env: &Env) -> Address {
        // Store only a hash of the owner's address
        // The real address is held off-chain in a GDPR-compliant database
        // When the data subject requests erasure, the off-chain record
        // is deleted, and the on-chain hash becomes meaningless
        let hash = env.storage().instance().get::<_, BytesN<32>>(&OWNER_HASH).unwrap();
        // ... resolve address through off-chain oracle
        todo!("Requires off-chain integration")
    }
}
```

This is an imperfect solution. The hash itself may constitute personal
data under GDPR if it can be linked back to the data subject. But it is
better than storing the raw address.

### Soroban's TTL Feature

Soroban's storage has a TTL (time-to-live) mechanism. Data that is not
refreshed eventually expires and is removed from the ledger. This is a
potential compliance mechanism:

```rust
pub struct ExpiringOwner;
impl OwnableInternal for ExpiringOwner {
    fn transfer_ownership(env: &Env, new_owner: Address) {
        env.storage().temporary().set(&OWNER_KEY, &new_owner);
        // Data expires after TTL -- natural erasure
        env.storage().temporary().extend_ttl(&OWNER_KEY, 100, 200);
    }
}
```

If ownership data is stored in temporary storage with a TTL, it naturally
expires. The data subject's right to erasure is satisfied by the passage of
time. This is a novel compliance approach.

The blog post and OZ comparison document mention that OZ handles TTL better.
For GDPR compliance, TTL management is not just a performance feature --
it is a legal compliance mechanism. This should be highlighted.

### Recommendation

The `#[contracttrait]` macro should support a `#[gdpr(ttl = N)]` annotation:

```rust
#[contracttrait]
pub trait UserProfile {
    #[gdpr(storage = "temporary", ttl = 365)]
    fn set_profile(env: &Env, user: Address, profile: ProfileData);
}
```

This would generate storage calls that automatically use temporary storage
with the specified TTL, and generate documentation noting the data retention
period.

---

## Article 5(1)(c): Data Minimization

### What Is Stored vs. What Is Needed

The trait-test example stores the owner's Address directly:

```rust
env.storage()
    .instance()
    .set(&soroban_sdk::Symbol::new(env, "owner"), &new_owner);
```

Data minimization asks: is the full Address necessary, or would a derived
identifier suffice?

For authorization purposes, a hash or commitment of the address might be
sufficient:

```rust
// Minimized: store only what is needed for auth
env.storage()
    .instance()
    .set(&Symbol::new(env, "owner_hash"), &hash_address(&new_owner));
```

But Soroban's `require_auth()` requires the actual Address. This is a
platform constraint that limits data minimization options.

### The Event Problem

The blog post mentions that OZ emits events for every state change. Events
are also on-chain data. If an `OwnershipTransferred` event contains the
old owner's Address, that is personal data that is permanently recorded.

Events should follow data minimization principles:

```rust
// BAD: emits both addresses (permanent personal data on-chain)
emit_event(env, OwnershipTransferred { old: owner, new: new_owner });

// BETTER: emits only that a transfer occurred (no personal data)
emit_event(env, OwnershipTransferred { timestamp: env.ledger().timestamp() });

// BEST: emit hashed identifiers (pseudonymization)
emit_event(env, OwnershipTransferred {
    old_hash: hash(&owner),
    new_hash: hash(&new_owner),
    timestamp: env.ledger().timestamp()
});
```

If the `#[contracttrait]` macro generates events (as recommended in the OZ
comparison), it should default to the minimized or pseudonymized form.

---

## Article 6: Lawful Basis for Processing

### Consent Management on Blockchain

GDPR requires a lawful basis for processing personal data. The most common
basis for blockchain applications is consent (Article 6(1)(a)). But
consent must be:

1. **Freely given** -- The data subject must have a real choice
2. **Specific** -- Consent must be for a specific purpose
3. **Informed** -- The data subject must understand what they are
   consenting to
4. **Unambiguous** -- Consent must be a clear affirmative action
5. **Withdrawable** -- Consent can be withdrawn at any time

The `#[auth]` pattern partially maps to consent:

```rust
#[auth(user)]
fn store_profile(env: &Env, user: Address, profile: ProfileData);
```

The `#[auth(user)]` means the user must sign the transaction -- this is
"unambiguous" (clear affirmative action) and "freely given" (the user
chooses to sign). But it is NOT:

- **Specific** -- The signature authorizes the function call, but does not
  specify what data processing is included
- **Informed** -- The user may not understand what the function does
- **Withdrawable** -- Once the function executes, the data is on-chain;
  consent withdrawal does not erase the data

### Consent Trait

A GDPR-compliant consent management trait:

```rust
#[contracttrait]
pub trait ConsentManagement {
    fn has_consent(env: &Env, user: Address, purpose: Symbol) -> bool;

    #[auth(user)]
    fn grant_consent(env: &Env, user: Address, purpose: Symbol, expiry: u64);

    #[auth(user)]
    fn withdraw_consent(env: &Env, user: Address, purpose: Symbol);

    fn consent_expiry(env: &Env, user: Address, purpose: Symbol) -> Option<u64>;
}
```

Methods that process personal data would check consent before proceeding:

```rust
impl UserProfileInternal for GDPRProvider {
    fn store_profile(env: &Env, user: Address, profile: ProfileData) {
        // Check consent before processing
        assert!(
            ConsentStorage::has_consent(env, &user, &Symbol::new(env, "profile")),
            "No consent for profile storage"
        );
        // Check consent is not expired
        let expiry = ConsentStorage::consent_expiry(env, &user, &Symbol::new(env, "profile"));
        if let Some(exp) = expiry {
            assert!(env.ledger().timestamp() < exp, "Consent expired");
        }
        // Process data
        ProfileStorage::set(env, &user, &profile);
    }
}
```

The provider pattern is well-suited for this: a `GDPRProvider` can
integrate consent checks into every data processing operation. Swap
`BasicProvider` for `GDPRProvider` to add compliance.

---

## Article 25: Data Protection by Design and by Default

### Privacy by Design in the Macro

Article 25 requires that data protection is integrated into system design,
not bolted on after the fact. The `#[contracttrait]` macro has an
opportunity to support privacy by design.

Currently, the macro generates:

1. Internal trait (business logic)
2. Outer trait (auth-wrapped)
3. AuthClient (testing)
4. Sealed macro (non-overridable)

It could additionally generate:

5. **Privacy impact annotation** -- A compile-time analysis of which methods
   store personal data:

   ```
   WARNING: store_profile() writes an Address to instance storage.
   This may constitute personal data under GDPR.
   Consider using temporary storage with TTL.
   ```

6. **Data flow documentation** -- Auto-generated data flow diagrams showing
   what data enters the contract, where it is stored, and who can access it.

7. **Consent verification stubs** -- If a method accepts an Address
   parameter and writes to storage, generate a consent check stub:

   ```rust
   // Generated: consent check before processing
   // TODO: Implement or remove if consent is not required
   assert!(has_consent(env, &user, "profile_storage"));
   ```

### The #[auth] Pattern as Article 25 Implementation

The `#[auth]` pattern IS a form of privacy by design. It ensures that
data processing (the function execution) only happens with the data
subject's authorization (their signature). This maps directly to
Article 25's requirement for appropriate technical measures.

But `#[auth]` alone is insufficient. Article 25 requires:

- Data minimization by default
- Purpose limitation
- Storage limitation
- Integrity and confidentiality

`#[auth]` addresses integrity (only authorized parties modify data) but
not the other requirements.

---

## Article 13/14: Information to the Data Subject

### Transparency Obligations

When personal data is collected, the data subject must be informed of:

- The identity of the controller
- The purposes of processing
- The legal basis for processing
- The retention period
- The data subject's rights

For smart contracts, the "controller" is ambiguous. Is it the contract
deployer? The contract itself? The DAO that governs the contract?

The provider pattern partially addresses this: the `Ownable` trait
identifies the controller (the owner). But the purposes of processing,
retention periods, and data subject rights are not encoded in the contract.

### Recommendation: Metadata Trait

```rust
#[contracttrait]
pub trait GDPRMetadata {
    fn data_controller(env: &Env) -> Address;
    fn processing_purposes(env: &Env) -> Vec<Symbol>;
    fn retention_period(env: &Env) -> u64;  // in seconds
    fn privacy_policy_url(env: &Env) -> Bytes;
}
```

This makes GDPR-required information queryable on-chain. Data subjects
(or their representatives) can verify the privacy terms directly from
the contract.

---

## Article 35: Data Protection Impact Assessment (DPIA)

### When Is a DPIA Required?

A DPIA is required when processing is "likely to result in a high risk to
the rights and freedoms of natural persons." Smart contracts that handle
personal data (addresses, balances, transaction history) are likely to
trigger this requirement.

The `#[contracttrait]` macro could generate DPIA-supporting documentation:

1. **Data inventory** -- List of all data types stored by the contract
2. **Access control matrix** -- Who can read/write each data type
3. **Retention schedule** -- How long each data type is retained
4. **Risk assessment inputs** -- What risks arise from the processing

### Example DPIA Output

```
DATA PROTECTION IMPACT ASSESSMENT
Contract: MyToken (FungibleToken + Ownable + Pausable)

DATA INVENTORY:
- owner: Address (instance storage, no TTL, public access)
  Risk: Owner identity linked to transaction history
  Mitigation: Use pseudonymous address, off-chain identity mapping

- balances: Map<Address, i128> (instance storage, no TTL, public access)
  Risk: Balance amounts reveal financial information
  Mitigation: Consider encrypted balances or private storage

AUTH MODEL:
- transfer_ownership: Only owner can execute (structural, sealed)
- transfer: Only sender can execute (structural, sealed)
- pause: Only owner can execute (structural, sealed)

DATA FLOWS:
- transfer(): reads sender balance, receiver balance; writes both
  Personal data involved: sender Address, receiver Address, amount
  Lawful basis required: consent (sender signs transaction)
```

This is the kind of documentation that a DPO needs to assess compliance.
The macro has all the information needed to generate it.

---

## Storage Implications

### Instance vs. Temporary vs. Persistent Storage

Soroban offers three storage tiers:

1. **Instance** -- Shared across the contract, no automatic expiry
2. **Temporary** -- Can expire (TTL-based)
3. **Persistent** -- Per-entry, can expire but is always recoverable

For GDPR:

- **Instance storage** is the worst for compliance: data is shared,
  permanent, and public
- **Temporary storage** is the best for compliance: data expires naturally
- **Persistent storage** is middling: data can expire but is recoverable

The trait-test example uses instance storage exclusively:

```rust
env.storage()
    .instance()
    .set(&soroban_sdk::Symbol::new(env, "owner"), &new_owner);
```

The provider pattern allows GDPR-aware providers to use appropriate
storage tiers:

```rust
pub struct GDPRStorageProvider;
impl OwnableInternal for GDPRStorageProvider {
    fn transfer_ownership(env: &Env, new_owner: Address) {
        // Use persistent storage with TTL instead of instance storage
        env.storage()
            .persistent()
            .set(&OWNER_KEY, &new_owner);
        env.storage()
            .persistent()
            .extend_ttl(&OWNER_KEY, 30 * 24 * 3600, 365 * 24 * 3600);
        // Data expires after 1 year if not refreshed
    }
}
```

### Cross-Border Data Transfers

Under GDPR, transferring personal data outside the EU/EEA requires
safeguards. A public blockchain is inherently cross-border -- validators
and nodes operate globally. This means ANY personal data stored on-chain
is subject to cross-border transfer rules.

The `#[contracttrait]` macro cannot solve this -- it is a platform-level
issue. But the documentation should acknowledge it and recommend:

1. Do not store raw personal data on-chain
2. Use pseudonymization (addresses are already pseudonymous)
3. Store the mapping between pseudonyms and real identities off-chain in
   an EU-hosted database
4. Use temporary storage with TTL for any data that must be on-chain

---

## Evaluation of Auth as a Privacy Measure

### The #[auth] Pattern

The `#[auth(Self::owner)]` pattern is a positive privacy measure because
it restricts who can access and modify data. Under GDPR:

- It implements **access control** (Article 5(1)(f))
- It provides **integrity protection** (Article 5(1)(f))
- It creates an **audit trail** (Article 5(2) accountability)

### The AuthClient as Audit Tool

The `AuthClient` generates precise records of who authorized what:

```rust
auth_client.transfer_ownership(&new_owner)
    .authorize(&owner)
    .invoke();
```

In a production system, these authorization records could serve as
accountability evidence under Article 5(2). The DPO can verify:

- Who authorized each data modification
- When the authorization occurred
- What data was modified

This is a genuine compliance advantage over OZ's `mock_all_auths()` testing
pattern, which does not test authorization at all.

---

## Recommendations for GDPR Compliance

### 1. Add Storage Tier Annotations to Traits

```rust
#[contracttrait]
pub trait UserProfile {
    #[storage(temporary, ttl = "365d")]
    fn set_profile(env: &Env, user: Address, profile: ProfileData);
}
```

### 2. Generate Privacy Documentation

The macro should generate a privacy data map alongside the code:

- What personal data is stored
- Where it is stored (which storage tier)
- Who can access it (the auth model)
- How long it is retained (TTL)

### 3. Provide Consent Management as a Standard Trait

Include a `ConsentManagement` trait in the standard library of providers.
This should be as easy to compose as `Ownable`:

```rust
impl_consent_management!(MyContract, ConsentProvider);
```

### 4. Support Data Subject Access Requests (DSAR)

Provide a trait for handling DSAR:

```rust
#[contracttrait]
pub trait DataSubjectRights: Ownable {
    #[auth(user)]
    fn export_data(env: &Env, user: Address) -> UserData;

    #[auth(user)]
    fn request_erasure(env: &Env, user: Address);
    // Marks data for erasure -- actual erasure via TTL expiry

    #[auth(user)]
    fn restrict_processing(env: &Env, user: Address);
    // Freezes the user's data -- no further processing allowed
}
```

### 5. Document the GDPR Limitations of Blockchain

Be honest about what blockchain CANNOT do for GDPR:

- True erasure is not possible on instance/persistent storage
- Cross-border transfers are inherent
- The right to object to processing is limited (once the transaction is
  submitted, processing occurs)
- Automated decision-making (Article 22) constraints may apply if contracts
  make decisions affecting individuals

---

## Verdict

`soroban-sdk-tools` is not a GDPR compliance tool, and it should not
pretend to be. Public blockchains are fundamentally in tension with GDPR's
core principles of data minimization, purpose limitation, and erasure
rights.

However, the framework's architectural decisions provide genuine
opportunities for privacy-by-design:

1. The provider pattern enables GDPR-aware implementations without changing
   the trait interface
2. The auth pattern provides access control and accountability
3. The sealed pattern prevents unauthorized data access
4. The composition model allows consent management and data subject rights
   to be added as composable traits

The biggest gap is documentation. The blog post and comparison document do
not mention data protection, privacy, or GDPR at all. For any European
deployment, this is a significant omission.

**Rating:** 5/10 -- The architecture enables GDPR-aware design through
providers, but the documentation ignores data protection entirely. Any EU
deployment will require significant additional compliance work.

---

*"GDPR does not prohibit blockchain. It prohibits thoughtless data
processing. The provider pattern gives you the tools to think carefully
about what data you store and how. Use them."*
