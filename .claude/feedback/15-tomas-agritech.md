---
reviewer: Tomas Ortega
role: Agricultural Technology Engineer
domain: Supply Chain Tracking, Rural Connectivity, Precision Agriculture
date: 2026-03-21
focus: Low-bandwidth environments, offline-first, rural deployment
---

# Review: soroban-sdk-tools -- Agricultural Supply Chain Perspective

## Context

I build supply chain tracking systems for coffee and cacao cooperatives in
Central America. Our users -- farmers, cooperative managers, transport drivers
-- operate in areas where cellular connectivity is intermittent, bandwidth
is measured in kilobits, and a contract interaction might need to be assembled
offline and submitted hours later when the truck reaches a town with signal.

## What the Architecture Gets Right

### 1. Provider Pattern for Offline-Compatible Ownership

The swap from `SingleOwner` to any custom provider is exactly what we need.
In our cooperatives, the "owner" of a shipment contract changes as goods
move through the chain: farmer -> cooperative -> transporter -> exporter.
Each handoff happens at a physical location that may lack connectivity.

```rust
pub struct CooperativeOwner;
impl OwnableInternal for CooperativeOwner {
    fn owner(env: &Env) -> Address {
        // Return current custodian based on shipment stage
        ShipmentStorage::get_current_custodian(env)
    }
    fn transfer_ownership(env: &Env, new_owner: Address) {
        // Record custody transfer with timestamp
        ShipmentStorage::transfer_custody(env, &new_owner);
    }
}
```

The structural auth via `#[auth(Self::owner)]` means the custody transfer
is always authorized by the current holder. This is the right primitive.

### 2. Sealed Auth for Regulatory Compliance

Coffee traceability regulations (EU Deforestation Regulation, EUDR) require
that custody chain integrity cannot be tampered with. The `impl_ownable!`
sealed pattern guarantees that the auth check on custody transfer cannot be
bypassed by a downstream contract modification. This is auditable and
defensible to regulators.

### 3. Small WASM Binary Size

The blog post claims zero overhead after monomorphization. For our deployments,
WASM binary size directly affects how long a contract takes to deploy over
EDGE or 2G connections. Every kilobyte matters when your upload might be
interrupted. The two-trait pattern producing code identical to hand-written
equivalents is a concrete advantage.

## Concerns

### 1. No Batch Operation Support

A farmer delivering 47 bags of coffee needs to register 47 custody transfers
in one transaction. The current `#[auth(Self::owner)]` pattern calls
`require_auth()` per method invocation. For batch operations, this means
47 separate auth checks for the same address.

Soroban supports `require_auth_for_args()` which can batch, but the macro
does not expose this. The generated code is always:

```rust
let __auth_addr = Self::Provider::owner(env);
__auth_addr.require_auth();
```

**Suggestion**: Support a `#[auth(Self::owner, batch)]` variant that generates
`require_auth_for_args()` with the method arguments, allowing the auth layer
to batch multiple calls under a single authorization.

### 2. No TTL Management in the Provider Pattern

In rural supply chains, contracts might not be accessed for weeks (between
harvest seasons). The OZ comparison acknowledges that OZ handles TTL
management better. Without automatic TTL extension, a cooperative's contract
storage could expire, losing the custody chain.

The Provider pattern should include lifecycle hooks:

```rust
pub trait OwnableInternal {
    fn owner(env: &Env) -> Address;
    fn transfer_ownership(env: &Env, new_owner: Address);

    // Optional: called on every interaction to extend TTL
    fn on_access(env: &Env) {
        env.storage().instance().extend_ttl(100, 1000);
    }
}
```

### 3. Error Messages Assume Connectivity for Debugging

When `.expect("not initialized")` panics in the example, the error surfaces
through the Soroban host. In our field testing scenarios, the farmer sees a
generic "transaction failed" on their phone. The error details are only
visible if someone connects to a Horizon node and queries the transaction.

**Suggestion**: The documentation should emphasize that providers should use
numeric error codes (`#[contracterror]` / `#[scerr]`) rather than string
panics, because error codes are included in the transaction result XDR and
can be decoded locally by the mobile app without additional network requests.

### 4. The AuthClient Requires an Active Soroban Environment

The `CallBuilder` and `AuthClient` require a live `Env` instance. For
offline-first workflows, we need to construct and sign transactions without
a running environment, then submit them later.

The current `setup_real_auth` function builds `SorobanAuthorizationEntry`
objects tied to a specific `env` and ledger state. An offline signing flow
would need:

1. Fetch current ledger sequence (when online)
2. Pre-build the authorization payload
3. Sign offline (possibly on a different device)
4. Submit when connectivity returns

The `Signer` trait and `setup_real_auth` could support this with a
`prepare_for_offline_signing()` method that returns the payload hash
without requiring immediate submission.

### 5. Symbol-Based Storage Keys are Bandwidth-Inefficient

```rust
Symbol::new(env, "owner")
```

Symbol keys are fine for readability but each one allocates in the
environment. For contracts that will be called thousands of times across
a growing season, using `#[contracttype]` enums as storage keys (as OZ
does) is more efficient in both CPU and storage size. The examples should
demonstrate this as the recommended pattern.

## What I Would Use This For

### Custody Chain Contract

```rust
#[contracttrait]
pub trait CustodyChain: Ownable {
    fn current_stage(env: &Env) -> Symbol;

    #[auth(Self::owner)]
    fn advance_stage(env: &Env, next_custodian: Address, stage: Symbol);

    fn custody_history(env: &Env) -> Vec<(Address, Symbol, u64)>;
}
```

The supertrait composition with Ownable is perfect for this. The custody
chain inherits ownership semantics, and `advance_stage` transfers both
custody and stage in one atomic operation.

### Batch Harvest Registration

```rust
#[contracttrait]
pub trait HarvestRegistry {
    #[auth(farmer)]
    fn register_batch(env: &Env, farmer: Address, lot_ids: Vec<u32>,
                      weights_kg: Vec<u32>, gps_lat: i64, gps_lon: i64);
}
```

One auth check for the entire batch. The farmer signs once at the weighing
station when they have signal, and the transaction covers all bags delivered.

## Summary

The Provider pattern and sealed auth are genuinely useful for agricultural
supply chains. The main gaps are batch operation support, TTL lifecycle
management, offline signing workflows, and documentation that guides
developers toward bandwidth-efficient patterns (enum keys, numeric errors).
The cooperative farmers I work with will never see this code, but the
contracts built with it will track their livelihoods. Getting the
low-bandwidth story right matters.
