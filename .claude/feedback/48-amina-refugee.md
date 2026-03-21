---
reviewer: Amina Hassan
role: Humanitarian Aid Technologist
domain: Refugee Aid Distribution, Stateless Identity, Displacement Response
date: 2026-03-21
focus: Identity without documents, dignity-preserving aid, camp logistics
---

# Review: soroban-sdk-tools -- Refugee Aid Distribution

## Context

I build aid distribution systems for refugee camps. My users have no
government ID, no bank account, no permanent address, and often no
phone. They may have crossed borders with nothing but themselves. The
systems I build must work with biometric identity (iris scan, fingerprint),
shared devices (one phone per family group), and rotating staff from
multiple NGOs who each need different access levels.

The fundamental question: can `soroban-sdk-tools` support an identity and
authorization model that does not assume government-issued identity or
personal device ownership?

## The Identity Problem

### Addresses Assume Key Ownership

The entire `#[auth]` framework is built on `Address` and `require_auth()`.
An Address represents a cryptographic identity -- someone who controls a
private key. Refugees typically:

1. Do not have devices to store keys
2. Share devices with family/community members
3. Move between camps, losing access to previously used devices
4. May be children who cannot manage cryptographic keys

The current architecture requires that every authorized party has an
Address. For aid recipients, this means someone (an NGO, a camp
administrator) must create and manage keys on their behalf. This is
custodial by necessity.

### Provider Pattern for Custodial Identity

The Provider pattern can accommodate custodial models:

```rust
pub struct CustodialIdentity;
impl OwnableInternal for CustodialIdentity {
    fn owner(env: &Env) -> Address {
        // The "owner" is the camp administrator
        // who manages identities on behalf of refugees
        CampStorage::get_administrator(env)
    }
}
```

But this concentrates power in the administrator. History has shown
that concentrated power in refugee camps leads to abuse -- aid
diversion, favoritism, exploitation.

### A Better Pattern: Distributed Custody

```rust
pub struct DistributedCustody;
impl AidDistributionInternal for DistributedCustody {
    fn authorize_distribution(env: &Env, recipient_hash: BytesN<32>,
                               aid_type: Symbol, quantity: u32) {
        // Require BOTH a field worker AND a supervisor
        // Neither alone can authorize distribution
        // This prevents single-point-of-failure abuse
        let field_worker = DistributionStorage::get_field_worker(env);
        let supervisor = DistributionStorage::get_supervisor(env);
        field_worker.require_auth();
        supervisor.require_auth();

        // Record distribution against biometric hash
        // (not an address -- refugees do not have addresses)
        DistributionStorage::record(env, &recipient_hash, &aid_type, quantity);
    }
}
```

Note the use of `recipient_hash` (a biometric hash) instead of `Address`.
The recipient is identified by their biometric data, not by a cryptographic
key they control. The authorization comes from the aid workers, not the
recipient.

## Concerns

### 1. No Support for Non-Address Identity

The `#[auth]` attribute generates `require_auth()` which operates on
`Address`. There is no equivalent for biometric verification, token-based
identity, or other non-cryptographic identity systems.

For refugee contexts, the identity assertion is: "this person's iris scan
matches hash X in our database." This is not a signature -- it is a
biometric verification performed by a device at the distribution point.

**Suggestion**: Support alternative identity assertions in the trait:

```rust
#[contracttrait]
pub trait AidDistribution {
    #[auth(distributor)]  // the aid worker authorizes
    fn distribute(env: &Env, distributor: Address,
                  recipient_biometric: BytesN<32>,
                  aid_type: Symbol, quantity: u32);
}
```

The biometric verification happens off-chain (at the distribution device).
The on-chain record only stores the hash. The aid worker's signature
attests that they performed the biometric check.

This pattern works with the current `#[auth(param)]` syntax. The key
insight is that the recipient is NOT an auth parameter -- they are a
data parameter identified by biometric hash.

### 2. Staff Rotation and Key Management

NGO staff rotate frequently -- deployments are typically 3-6 months.
When a field worker's deployment ends, their authorization must be
revoked and the new worker's must be granted. This is a rapid key
rotation scenario.

The `transfer_ownership` pattern handles single-owner rotation. But
aid distribution needs multi-role rotation:

```rust
#[contracttrait]
pub trait StaffManagement: Ownable {
    #[auth(Self::owner)]  // country director
    fn add_field_worker(env: &Env, worker: Address, camp: Symbol,
                        expiry_ledger: u32);

    #[auth(Self::owner)]
    fn revoke_field_worker(env: &Env, worker: Address);

    fn is_authorized_worker(env: &Env, worker: Address, camp: Symbol) -> bool;
}
```

The Provider handles expiry checking:

```rust
impl StaffManagementInternal for RotatingStaffProvider {
    fn is_authorized_worker(env: &Env, worker: Address, camp: Symbol) -> bool {
        let entry = StaffStorage::get_worker(env, &worker);
        match entry {
            Some(e) => e.camp == camp && env.ledger().sequence() < e.expiry_ledger,
            None => false,
        }
    }
}
```

This works with the current architecture. The time-bounded authorization
is implemented in the provider. The sealed macro ensures staff management
functions cannot be bypassed.

### 3. Offline Distribution Recording

Many camps have intermittent connectivity. Aid distribution happens
continuously, but transactions can only be submitted when connectivity
is available. This means distributions must be recorded offline and
submitted in batches.

The current `CallBuilder` requires an active `Env`. For offline
recording, we need:

1. Record distributions locally (offline device)
2. Batch them when connectivity returns
3. Submit as a batch transaction with one authorization

**Suggestion**: Support batch submission patterns in documentation:

```rust
#[contracttrait]
pub trait AidDistribution {
    #[auth(distributor)]
    fn record_batch(env: &Env, distributor: Address,
                    distributions: Vec<Distribution>);
}
```

A single auth check for the entire batch. The distributor signs once
for all distributions performed during the offline period.

### 4. Duplicate Distribution Prevention

In camps, the same person might visit multiple distribution points on
the same day (intentionally or due to family members collecting on their
behalf). The contract must prevent duplicate distributions.

```rust
impl AidDistributionInternal for DeduplicatingProvider {
    fn distribute(env: &Env, distributor: Address,
                  recipient_biometric: BytesN<32>,
                  aid_type: Symbol, quantity: u32) {
        let today = env.ledger().timestamp() / 86400;  // day number
        let key = (recipient_biometric.clone(), aid_type.clone(), today);

        if DistributionStorage::has_received(env, &key) {
            panic_with_error!(env, AidError::AlreadyDistributed);
        }

        DistributionStorage::mark_received(env, &key);
        DistributionStorage::record(env, &recipient_biometric, &aid_type, quantity);
    }
}
```

This works with the current Provider pattern. The deduplication logic
is entirely in the provider.

### 5. Multi-NGO Coordination

A single camp may be served by multiple NGOs (UNHCR, WFP, MSF, local
organizations). Each NGO manages its own staff but must share the
distribution ledger to prevent duplicate aid.

This requires a shared contract with per-organization access:

```rust
#[contracttrait]
pub trait MultiOrgAid: Ownable {
    #[auth(Self::owner)]  // camp coordinator
    fn register_organization(env: &Env, org: Address, org_name: Symbol);

    #[auth(org_admin)]
    fn add_org_worker(env: &Env, org_admin: Address, worker: Address);

    // Worker must be registered under an authorized org
    #[auth(distributor)]
    fn distribute(env: &Env, distributor: Address,
                  recipient_biometric: BytesN<32>,
                  aid_type: Symbol, quantity: u32);
}
```

The Provider verifies that the distributor is a registered worker of an
authorized organization before allowing distribution. The structural
auth ensures the distributor personally authorizes the action. The
organizational check happens in the provider logic.

### 6. Privacy of Biometric Data

Biometric hashes stored on-chain can potentially be used to track
refugee movements across camps. This is a serious privacy concern with
life-safety implications (persecution, targeting).

The contract should store salted hashes:

```rust
fn distribute(env: &Env, distributor: Address,
              recipient_biometric: BytesN<32>,
              aid_type: Symbol, quantity: u32) {
    // Salt with camp-specific value to prevent cross-camp linking
    let camp_salt = CampStorage::get_salt(env);
    let salted_hash = env.crypto().sha256(
        &Bytes::from_slice(env, &[recipient_biometric.as_ref(), camp_salt.as_ref()].concat())
    );
    // Store salted_hash, not recipient_biometric
}
```

This is a provider-level concern, but it should be documented as a
required pattern for humanitarian use cases.

## Dignity-Preserving Design Principles

1. **Recipients should not need to manage keys**: Auth should be on
   the aid worker side, not the recipient side
2. **No single person should control access**: Distributed custody
   with multi-party auth prevents abuse
3. **Biometric data must be salted per-camp**: Prevents tracking
4. **Error messages must be actionable**: "Distribution denied" is
   not helpful. "Already received today -- next distribution tomorrow
   at 8am" preserves dignity
5. **Offline-first**: Connectivity cannot be assumed
6. **Time-bounded staff access**: Workers' authorization must expire
   automatically when their deployment ends

## Summary

soroban-sdk-tools can support refugee aid distribution through the
Provider pattern and parameter-level auth. The key architectural
insight is that aid recipients are NOT auth subjects -- they are
identified by biometric hashes, and authorization comes from aid
workers. The `#[auth(param)]` pattern correctly models this. The
Provider pattern handles deduplication, time-bounded staff access,
and multi-organization coordination. The gaps are batch submission
for offline periods, documentation of biometric privacy patterns,
and multi-party auth syntax for distributed custody. The most
impactful addition for humanitarian use would be first-class batch
operation support for offline-first environments.
