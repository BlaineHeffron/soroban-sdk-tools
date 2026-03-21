---
reviewer: Tariq al-Rashid
role: Land Rights Technologist
domain: Property Deed Verification, Cadastral Systems, Dispute Resolution
date: 2026-03-21
focus: Property rights, deed transfer, boundary disputes, inheritance
---

# Review: soroban-sdk-tools -- Land Deed Verification Systems

## Context

I work on land deed digitization in regions where property records have been
destroyed by conflict, are maintained in corrupt registries, or simply never
existed in written form. In these contexts, proving ownership of land can be
a matter of life and death. Families are displaced because they cannot prove
their claim. Widows lose property because inheritance records do not exist.

The blockchain promise for land deeds is immutable, transparent, and
tamper-proof records. My review evaluates whether `soroban-sdk-tools` can
support the complex ownership models that real property rights require.

## Architectural Analysis

### The Ownable Pattern Maps to Property Ownership

The `Ownable` trait with `transfer_ownership` is a direct analog for
property deed transfer:

```rust
impl_ownable!(LandDeedContract, SingleOwner);
```

The sealed macro ensures that deed transfers require the current owner's
authorization. This is the digital equivalent of requiring a signature
on a transfer deed at the land registry office.

But property ownership is more complex than single-owner contracts.

### Property Ownership Models

Real property has several ownership types that exceed single-address models:

1. **Joint tenancy**: Multiple owners, right of survivorship
2. **Tenancy in common**: Multiple owners, no survivorship, unequal shares
3. **Community property**: Spousal co-ownership
4. **Trust ownership**: Legal entity owns on behalf of beneficiaries
5. **Customary tenure**: Community-based ownership recognized by tradition
6. **State leasehold**: Government owns, individual has usage rights

Each requires a different Provider:

```rust
pub struct JointTenancy;
impl OwnableInternal for JointTenancy {
    fn owner(env: &Env) -> Address {
        // Return a joint tenancy contract address
        // that requires ALL tenants to authorize transfers
        DeedStorage::get_joint_tenancy_contract(env)
    }
}

pub struct TenancyInCommon;
impl OwnableInternal for TenancyInCommon {
    fn owner(env: &Env) -> Address {
        // Return a DAO-like contract address
        // where majority share holders can authorize
        DeedStorage::get_tic_contract(env)
    }
}

pub struct CustomaryTenure;
impl OwnableInternal for CustomaryTenure {
    fn owner(env: &Env) -> Address {
        // Return the community council contract address
        // Traditional leaders authorize on behalf of community
        DeedStorage::get_council_address(env)
    }
}
```

The Provider pattern's ability to swap ownership models is exactly right
for land systems that must accommodate multiple legal frameworks within
the same country.

## Concerns

### 1. No Inheritance Support

Property ownership transfers at death. This is not a voluntary
`transfer_ownership` call -- it is a legal event that must be processed
without the deceased owner's authorization.

The current `#[auth(Self::owner)]` on `transfer_ownership` requires the
owner to sign. A deceased owner cannot sign.

**Suggestion**: Support inheritance override in the Provider:

```rust
pub struct InheritableOwnership;
impl OwnableInternal for InheritableOwnership {
    fn owner(env: &Env) -> Address {
        let primary = DeedStorage::get_owner(env);
        let death_registered = DeedStorage::is_death_registered(env, &primary);

        if death_registered {
            // Return the executor/heir address
            DeedStorage::get_heir(env)
        } else {
            primary
        }
    }
}
```

When a death is registered (by a court or registrar), `owner()` returns
the heir's address. The `#[auth(Self::owner)]` then requires the heir's
signature for transfers. The heir can complete the inheritance transfer.

This works with the current architecture. The Provider handles the
inheritance logic transparently.

### 2. Boundary Disputes and Encumbrances

Land deeds are not just about ownership -- they carry encumbrances:
- Mortgages (the bank has a lien)
- Easements (neighbor has right of way)
- Restrictive covenants (zoning, usage restrictions)
- Lis pendens (pending litigation)

A deed with a lis pendens should not be transferable. The `Pausable`
trait could represent this:

```rust
// Lis pendens = pause the deed
fn register_dispute(env: &Env, court_address: Address,
                    case_number: Symbol) {
    court_address.require_auth();  // only a court can freeze a deed
    PausableInternal::pause(env);
}
```

But `Pausable` is too coarse -- it freezes everything. A deed under
dispute should still allow the owner to grant easements or register
mortgage payments. Only transfers should be frozen.

**Suggestion**: Use the granular pause pattern:

```rust
#[contracttrait]
pub trait LandDeed: Ownable + GranularPausable {
    #[auth(Self::owner)]
    #[when_not_paused(scope = "transfer")]
    fn transfer_deed(env: &Env, buyer: Address, consideration: i128);

    // Easements are NOT affected by transfer freezes
    #[auth(Self::owner)]
    fn grant_easement(env: &Env, beneficiary: Address, easement_type: Symbol);
}
```

### 3. Two-Step Transfer is Essential for Property

The OZ comparison document notes that OZ's two-step transfer pattern
(transfer + accept) is a safety feature. For land deeds, this is not
just a safety feature -- it is a legal requirement in most jurisdictions.

A property transfer requires:
1. Seller signs the transfer deed
2. Buyer accepts and pays consideration
3. Transfer is recorded in the registry

The current `transfer_ownership` is a single step. The two-step pattern
should be a first-class Provider:

```rust
pub struct TwoStepDeedTransfer;
impl LandDeedInternal for TwoStepDeedTransfer {
    fn initiate_transfer(env: &Env, buyer: Address, consideration: i128) {
        DeedStorage::set_pending_transfer(env, &buyer, consideration);
    }

    fn accept_transfer(env: &Env) {
        let pending = DeedStorage::get_pending_transfer(env);
        let buyer = pending.buyer;
        buyer.require_auth();

        // Verify consideration was paid
        // Transfer deed
        DeedStorage::complete_transfer(env, &buyer);
    }
}
```

### 4. Registrar Role

In most land systems, a government registrar must approve transfers.
This is a third-party authorization that does not fit the current
two-party model (owner transfers, buyer accepts).

```rust
#[contracttrait]
pub trait LandRegistry: Ownable {
    fn registrar(env: &Env) -> Address;

    #[auth(Self::owner)]
    fn initiate_transfer(env: &Env, buyer: Address, parcel_id: BytesN<32>,
                         consideration: i128);

    #[auth(buyer)]
    fn accept_transfer(env: &Env, buyer: Address, parcel_id: BytesN<32>);

    #[auth(Self::registrar)]
    fn approve_transfer(env: &Env, parcel_id: BytesN<32>);
}
```

Three separate auth sources (owner, buyer, registrar) in one trait.
This works with the current `#[auth]` syntax using both `Self::method`
and parameter-level auth.

### 5. Historical Records and Immutability

Land records must be immutable -- once recorded, a deed cannot be
altered. But corrections must be possible (clerical errors, court
orders). The solution is an append-only correction chain:

```rust
fn correct_record(env: &Env, parcel_id: BytesN<32>,
                  correction_type: Symbol,
                  corrected_data: Bytes) {
    // Record correction as a new entry, not a modification
    // Original record remains intact
    CorrectionStorage::append(env, &parcel_id, &correction_type, &corrected_data);
}
```

The sealed macro ensures corrections can only be made by authorized
registrars. The append-only pattern preserves the historical record
while allowing corrections.

### 6. Community Land Rights

In many African and Asian contexts, land is owned communally. There is
no individual owner -- the community council makes decisions by consensus.

The Provider pattern supports this:

```rust
pub struct CommunalOwnership;
impl OwnableInternal for CommunalOwnership {
    fn owner(env: &Env) -> Address {
        // The community DAO contract
        CommunalStorage::get_council_contract(env)
    }
}
```

The council contract handles consensus internally. The `Ownable` trait
does not need to know whether the owner is an individual, a family, or
a village council. This abstraction is powerful.

## Land Deed Contract Architecture

```rust
#[contracttrait]
pub trait LandDeed: Ownable + GranularPausable {
    fn registrar(env: &Env) -> Address;
    fn parcel_info(env: &Env) -> ParcelInfo;
    fn encumbrances(env: &Env) -> Vec<Encumbrance>;
    fn transfer_history(env: &Env) -> Vec<TransferRecord>;

    // Three-party transfer
    #[auth(Self::owner)]
    fn initiate_transfer(env: &Env, buyer: Address, consideration: i128);

    #[auth(buyer)]
    fn accept_transfer(env: &Env, buyer: Address);

    #[auth(Self::registrar)]
    fn approve_transfer(env: &Env);

    // Encumbrances
    #[auth(Self::registrar)]
    fn register_encumbrance(env: &Env, encumbrance: Encumbrance);

    #[auth(Self::registrar)]
    fn release_encumbrance(env: &Env, encumbrance_id: u32);

    // Dispute
    #[auth(Self::registrar)]
    fn register_dispute(env: &Env, case_number: Symbol);

    #[auth(Self::registrar)]
    fn resolve_dispute(env: &Env, case_number: Symbol, resolution: Symbol);
}

impl_land_deed!(VillageDeed, InheritableOwnership);
```

The sealed macro ensures all registrar operations are protected.
The `InheritableOwnership` provider handles death-based transfers.
The registrar role provides government oversight. Disputes freeze
transfers but not other operations.

## Summary

soroban-sdk-tools provides a strong foundation for land deed systems.
The Provider pattern accommodates multiple ownership models (individual,
joint, communal, trust) through a single trait interface. The sealed
macro provides the immutability guarantees that land registries require.
The multi-role auth model (owner, buyer, registrar) works with existing
`#[auth]` syntax. The gaps are two-step transfer as a first-class
pattern, inheritance support, granular pause for encumbrance management,
and documentation of communal ownership models. The most impactful
addition would be a standard `TwoStepTransfer` provider, since every
land system requires multi-party transfer approval.
