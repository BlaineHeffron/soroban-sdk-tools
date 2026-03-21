---
reviewer: Aaliyah Washington
role: Music Technologist & Beat Producer
domain: Hip-Hop Beat Marketplace, Sample Licensing, Collaborative Production
date: 2026-03-21
focus: Collaborative ownership, beat licensing, producer splits
---

# Review: soroban-sdk-tools -- Hip-Hop Beat Marketplace

## Context

I produce beats and build tools for producers. The beat marketplace is
broken. A producer sells a beat for $30 on BeatStars, but if the rapper
using that beat gets a sync placement in a TV show, the producer sees
nothing from the sync fee. Exclusive vs non-exclusive licensing is tracked
in PDFs that no one reads. Splits between co-producers are verbal agreements
that fall apart when money arrives.

I want to build a beat marketplace where licensing terms are in the contract,
splits are enforced on-chain, and every usage triggers automatic royalty
distribution. My review focuses on whether `soroban-sdk-tools` can express
the collaborative ownership models that hip-hop production requires.

## What Works

### 1. Provider Pattern for License Types

Different beat licenses have different terms:

```rust
pub struct NonExclusiveLicense;
pub struct ExclusiveLicense;
pub struct SyncLicense;
pub struct StemLicense;

impl BeatLicenseInternal for NonExclusiveLicense {
    fn can_license(env: &Env, beat_id: BytesN<32>) -> bool {
        // Non-exclusive: always available (unlimited licenses)
        true
    }
    fn license_fee(env: &Env, beat_id: BytesN<32>) -> i128 {
        BeatStorage::get_non_exclusive_price(env, &beat_id)
    }
    fn on_license(env: &Env, beat_id: BytesN<32>, licensee: Address) {
        BeatStorage::add_licensee(env, &beat_id, &licensee);
        // Non-exclusive: do NOT remove from marketplace
    }
}

impl BeatLicenseInternal for ExclusiveLicense {
    fn can_license(env: &Env, beat_id: BytesN<32>) -> bool {
        // Exclusive: only if no exclusive license exists
        !BeatStorage::has_exclusive(env, &beat_id)
    }
    fn on_license(env: &Env, beat_id: BytesN<32>, licensee: Address) {
        BeatStorage::set_exclusive_licensee(env, &beat_id, &licensee);
        // Exclusive: remove from marketplace, notify existing licensees
    }
}
```

Swapping the license model with `impl_beat_license!(MyBeat, ExclusiveLicense)`
is clean. The producer chooses the licensing model at deployment.

### 2. Supertrait Composition for Full Beat Contract

```rust
#[contracttrait]
pub trait BeatMarketplace: Ownable + Pausable {
    fn beat_info(env: &Env, beat_id: BytesN<32>) -> BeatInfo;
    fn splits(env: &Env, beat_id: BytesN<32>) -> Vec<(Address, u32)>;

    #[auth(producer)]
    fn upload_beat(env: &Env, producer: Address, beat_hash: BytesN<32>,
                   metadata: BeatMetadata);

    #[auth(producer)]
    fn set_splits(env: &Env, producer: Address, beat_id: BytesN<32>,
                  splits: Vec<(Address, u32)>);

    #[auth(buyer)]
    fn purchase_license(env: &Env, buyer: Address, beat_id: BytesN<32>,
                        license_type: Symbol);

    // Permissionless -- anyone can trigger distribution
    fn distribute_royalties(env: &Env, beat_id: BytesN<32>);
}
```

The `#[auth(producer)]` parameter-level auth means any producer can upload
beats without being THE owner of the marketplace. The marketplace `owner`
(via Ownable) is the platform operator, while individual producers
authenticate themselves. This is the correct role separation.

### 3. The AuthClient for Testing License Flows

```rust
let auth = BeatMarketplaceAuthClient::new(&env, &marketplace_id);

// Producer uploads a beat
auth.upload_beat(&producer, &beat_hash, &metadata)
    .authorize(&producer)
    .invoke();

// Producer sets splits: 60% producer, 25% co-producer, 15% sample owner
auth.set_splits(&producer, &beat_id, &splits)
    .authorize(&producer)
    .invoke();

// Buyer purchases non-exclusive license
auth.purchase_license(&buyer, &beat_id, &symbol!("non_exclusive"))
    .authorize(&buyer)
    .invoke();
```

The fluent chain reads like the actual user flow. This is excellent for
testing complex marketplace interactions.

## Concerns

### 1. Co-Producer Authorization for Split Changes

When two producers make a beat together, split changes should require
BOTH producers to agree. The current `#[auth(producer)]` only requires
one signature.

```rust
// Current: only the uploading producer can set splits
#[auth(producer)]
fn set_splits(env: &Env, producer: Address, beat_id: BytesN<32>,
              splits: Vec<(Address, u32)>);
```

In hip-hop, the co-producer who added the 808 pattern should have veto
power over split changes. A beat with three co-producers needs 3-of-3
agreement for split modifications.

The Provider can enforce this:

```rust
impl BeatMarketplaceInternal for CollaborativeProvider {
    fn set_splits(env: &Env, producer: Address, beat_id: BytesN<32>,
                  splits: Vec<(Address, u32)>) {
        // Check that all existing split holders have approved
        let current_splits = BeatStorage::get_splits(env, &beat_id);
        for (holder, _bps) in current_splits.iter() {
            holder.require_auth();  // Each holder must sign
        }
        BeatStorage::set_splits(env, &beat_id, &splits);
    }
}
```

But this bypasses the `#[auth]` structural guarantee -- the additional
`require_auth()` calls are in the provider, not in the trait definition.
The sealed macro does not protect these inner auth calls.

**Suggestion**: Support multi-auth in trait definitions:

```rust
#[auth(all_of: Self::split_holders)]
fn set_splits(env: &Env, beat_id: BytesN<32>, splits: Vec<(Address, u32)>);
```

Where `split_holders` returns a `Vec<Address>` and the macro generates
`require_auth()` for each.

### 2. Sample Clearance Chain

Hip-hop is built on sampling. A beat that samples a James Brown record
needs clearance from the rights holders. The sample clearance creates a
dependency chain:

```
My Beat (100%)
  |- Me (60%)
  |- Co-producer (25%)
  |- Sample Clearance -> Original Track Contract (15%)
      |- Original Artist Estate (70% of 15%)
      |- Original Label (30% of 15%)
```

When `distribute_royalties` is called, 15% must flow to the original
track's contract, which then distributes internally. This is a
cross-contract call.

The `add_sub_invoke` on `CallBuilder` supports nested invocations, but
the trait-level `distribute_royalties` is permissionless (no auth).
Cross-contract calls from a permissionless function need careful design
to prevent reentrancy and ensure the nested contract can receive funds.

### 3. Exclusive License Reverts to Non-Exclusive After Term

An exclusive license might be time-limited: "exclusive for 2 years, then
reverts to non-exclusive." This is the same time-based ownership pattern
discussed in the music rights review.

The Provider handles this:

```rust
fn can_license(env: &Env, beat_id: BytesN<32>) -> bool {
    if let Some(exclusive) = BeatStorage::get_exclusive(env, &beat_id) {
        if env.ledger().timestamp() > exclusive.expiry {
            // Exclusive term expired -- anyone can license again
            BeatStorage::clear_exclusive(env, &beat_id);
            true
        } else {
            false  // Still under exclusive term
        }
    } else {
        true
    }
}
```

This works. The Provider pattern is flexible enough for time-based
license transitions.

### 4. Beat Packs and Bundle Pricing

Producers often sell beat packs (5 beats for $100 instead of $30 each).
The current trait models individual beat licensing. Bundle operations
need batch support:

```rust
#[auth(buyer)]
fn purchase_bundle(env: &Env, buyer: Address, beat_ids: Vec<BytesN<32>>,
                   bundle_price: i128);
```

The `#[auth(buyer)]` works for a single authorization covering the
entire bundle. The Provider validates the bundle pricing and processes
all licenses atomically.

### 5. Producer Reputation and Verified Accounts

In the beat marketplace, verified producer status matters. A provider
could integrate reputation:

```rust
pub struct VerifiedProducerMarketplace;
impl BeatMarketplaceInternal for VerifiedProducerMarketplace {
    fn upload_beat(env: &Env, producer: Address, beat_hash: BytesN<32>,
                   metadata: BeatMetadata) {
        assert!(
            ReputationStorage::is_verified(env, &producer),
            "producer not verified"
        );
        BeatStorage::store_beat(env, &producer, &beat_hash, &metadata);
    }
}
```

The verification check is in the provider, separate from the auth check
in the trait. This is the right layering -- auth confirms identity,
the provider confirms business rules.

## Beat Contract Architecture

```rust
#[contracttrait]
pub trait BeatMarketplace: Ownable + Pausable {
    // Read operations (no auth)
    fn beat_info(env: &Env, beat_id: BytesN<32>) -> BeatInfo;
    fn splits(env: &Env, beat_id: BytesN<32>) -> Vec<(Address, u32)>;
    fn license_price(env: &Env, beat_id: BytesN<32>, license_type: Symbol) -> i128;
    fn is_available(env: &Env, beat_id: BytesN<32>, license_type: Symbol) -> bool;

    // Producer operations
    #[auth(producer)]
    fn upload_beat(env: &Env, producer: Address, beat_hash: BytesN<32>,
                   metadata: BeatMetadata);

    #[auth(producer)]
    fn set_pricing(env: &Env, producer: Address, beat_id: BytesN<32>,
                   prices: Vec<(Symbol, i128)>);

    #[auth(producer)]
    fn set_splits(env: &Env, producer: Address, beat_id: BytesN<32>,
                  splits: Vec<(Address, u32)>);

    // Buyer operations
    #[auth(buyer)]
    fn purchase_license(env: &Env, buyer: Address, beat_id: BytesN<32>,
                        license_type: Symbol);

    // Platform operations
    #[auth(Self::owner)]
    fn set_platform_fee(env: &Env, fee_bps: u32);

    // Permissionless
    fn distribute_royalties(env: &Env, beat_id: BytesN<32>);
}

impl_beat_marketplace!(HipHopBeats, CollaborativeProvider);
```

The sealed macro ensures:
- Only producers can upload/modify their beats
- Only buyers can purchase licenses (preventing unauthorized charges)
- Only the platform can set fees
- Anyone can trigger royalty distribution
- Split changes require all co-producers to agree (in provider)

## Summary

soroban-sdk-tools provides a solid foundation for a hip-hop beat
marketplace. The Provider pattern cleanly separates license types
(exclusive, non-exclusive, sync, stems). Parameter-level auth correctly
distinguishes producer, buyer, and platform roles. The AuthClient
enables testing of complex marketplace flows. The main gaps are
multi-party auth for collaborative split management, cross-contract
royalty distribution patterns, and batch operations for beat packs.
The most impactful addition would be multi-auth support for the
co-producer approval pattern, since collaborative ownership is the
heart of hip-hop production culture.
