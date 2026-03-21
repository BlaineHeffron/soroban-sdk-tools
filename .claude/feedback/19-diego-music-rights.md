# Review: soroban-sdk-tools `#[contracttrait]` Macro

**Reviewer:** Diego -- Music rights management on blockchain, has built 3 royalty platforms
**Focus:** Complex ownership models (multiple rights holders), royalty splits, licensing

---

## Overall Impression

I build royalty distribution systems. A single song can have 15+ rights
holders: songwriter, co-writers, producer, publisher, sub-publishers per
territory, performers, sample clearance holders, and their estates. Each
holder has a percentage split that can change over time (reversion clauses,
death of an artist, catalog sales). The ownership model is not "one owner"
or even "k-of-n multisig" -- it is a directed graph of fractional claims
with temporal dynamics and geographic boundaries.

When I evaluate the `#[contracttrait]` macro, I ask: can this tool express
the complex ownership and authorization models that music rights require?
Can it handle multi-party splits, joint administration, time-based reversions,
and nested rights from sample clearances?

The answer is nuanced. The provider pattern is genuinely useful for music
rights -- more useful than OZ's fixed ownership model. But the single-address
auth annotation is a significant limitation, and the lack of event emission
is a deal-breaker for royalty audit trails.

---

## Strengths

### 1. Provider Pattern for Ownership Models

The ability to swap `SingleOwner` for a custom provider is exactly the
right abstraction level for music rights. Music rights need:

```rust
pub struct FractionalOwnership;
impl OwnableInternal for FractionalOwnership {
    fn owner(env: &Env) -> Address {
        // Return the "rights administrator" -- the entity
        // authorized to modify the split table
        RightsStorage::get_administrator(env)
    }
    fn transfer_ownership(env: &Env, new_owner: Address) {
        // Transfer administration rights (not the royalties themselves)
        RightsStorage::set_administrator(env, &new_owner);
    }
}
```

The distinction between "owner" (who administers the contract) and "rights
holders" (who receive royalties) maps cleanly to the `Ownable` trait being
about administrative control while a separate `RoyaltyDistribution` trait
handles payment splits. This separation of concerns is exactly right for
music rights.

In my previous platforms, we conflated administration and ownership, which
caused endless problems: every time a rights holder changed, we had to
update the admin address. With the provider pattern, these are independent
concerns.

### 2. Supertrait Composition for Rights Bundles

A music contract typically needs:
- Ownership (who administers the contract)
- Pausable (freeze during disputes -- and disputes are constant in music)
- Royalty splits (who gets paid and how much)
- Territory restrictions (geographic rights)

```rust
#[contracttrait]
pub trait RoyaltyDistribution: Ownable + Pausable {
    fn split_table(env: &Env) -> Vec<(Address, u32)>;  // basis points

    #[auth(Self::owner)]
    fn update_split(env: &Env, holder: Address, bps: u32);

    fn distribute(env: &Env, amount: i128);  // no auth -- anyone can trigger
}
```

The supertrait composition is elegant. `distribute` has no `#[auth]` because
anyone should be able to trigger a distribution (the money goes to the rights
holders, not the caller -- streaming platforms, distributors, or even fans can
trigger a payout). `update_split` requires admin auth because changing who gets
paid is a privileged operation. This distinction is expressible in the current
system and it maps directly to how music royalties work.

### 3. Sealed Pattern for Rights Integrity

Music rights are legally binding. A songwriter's 40% share is a legal claim.
If a software bug allows an unauthorized party to modify the split table, it
is not just a security incident -- it is a breach of contract that could result
in litigation.

The sealed pattern (`impl_royalty_distribution!`) ensures that split
modifications always require administrator auth. This is a structural guarantee
that I can present to lawyers, labels, and collecting societies as evidence
that the system enforces their agreements.

### 4. Time-Based Ownership via Providers

In music publishing, rights revert to the songwriter after a contract term
expires (typically 10-35 years). This means the "owner" of a rights contract
changes automatically based on time, not by explicit transfer.

The provider pattern handles this elegantly:

```rust
pub struct RevertingOwnership;
impl OwnableInternal for RevertingOwnership {
    fn owner(env: &Env) -> Address {
        let reversion_ledger = RightsStorage::get_reversion_ledger(env);
        if env.ledger().sequence() >= reversion_ledger {
            RightsStorage::get_original_songwriter(env)
        } else {
            RightsStorage::get_publisher(env)
        }
    }
}
```

The `owner()` return value changes based on ledger time, and
`#[auth(Self::owner)]` will require auth from the correct party at any given
time. This is actually elegant and deserves to be highlighted as a use case in
the documentation. No other framework I have used makes time-based ownership
transitions this clean.

### 5. The "No Auth" Pattern for Public Operations

Some operations in music rights should be callable by anyone:
- Distributing accumulated royalties
- Querying the split table
- Checking the contract's ISRC (International Standard Recording Code)

The fact that methods without `#[auth]` have no auth requirement is a clean
way to express public operations. In OZ's model, every method must explicitly
handle auth (even if the handling is "no auth required"), which adds noise.

---

## Concerns

### 1. Single `owner` Returns One Address -- Joint Administration

The `Ownable` trait's `owner()` returns a single `Address`. In music rights,
administrative control is often shared:

- Publisher and songwriter must BOTH agree to modify splits
- During a catalog sale, both buyer and seller must approve the transition
- Estate executors may have joint control
- Co-publishing agreements require both publishers to agree

The `#[auth(Self::owner)]` pattern calls `require_auth()` on ONE address.
For joint administration, the options are:

a) Use a multisig contract as the "owner" (works but adds deployment
   complexity -- now every rights contract needs a companion multisig contract)
b) Create a custom auth pattern in the flexible path (loses the structural
   guarantee)

**Suggestion**: Support multi-address auth annotations:

```rust
#[contracttrait]
pub trait JointAdmin {
    fn publisher(env: &Env) -> Address;
    fn songwriter(env: &Env) -> Address;

    #[auth(Self::publisher, Self::songwriter)]  // BOTH must authorize
    fn update_split(env: &Env, holder: Address, bps: u32);
}
```

Generated code:
```rust
fn update_split(env: &Env, holder: Address, bps: u32) {
    Self::Provider::publisher(env).require_auth();
    Self::Provider::songwriter(env).require_auth();
    Self::Provider::update_split(env, holder, bps);
}
```

This is the single most impactful improvement for music rights use cases.
Multi-party auth is not an edge case -- it is the norm in the music industry.

### 2. No Event Emission -- Audit Trail Gap

The OZ comparison notes that OZ emits events for state changes and this
should be adopted. For music royalties, events are not optional -- they are
the audit trail that proves distributions happened correctly. Collecting
societies (ASCAP, BMI, PRS, GEMA) require detailed distribution records.

Every `distribute()` call needs an event showing:
- Total amount distributed
- Per-holder amounts and their basis point shares
- Timestamp and ledger sequence
- Distribution trigger (manual, streaming threshold, schedule)

Every `update_split()` call needs an event showing:
- Previous split table
- New split table
- Who authorized the change
- Why (this is hard to encode on-chain, but the event should at least capture
  the authorization chain)

If events were generated in the outer trait defaults, every provider would
automatically get audit trails:

```rust
// Generated outer trait default:
fn distribute(env: &Env, amount: i128) {
    let result = Self::Provider::distribute(env, amount);
    env.events().publish(
        (symbol_short!("dist"), symbol_short!("royalty")),
        (amount,)
    );
    result
}
```

Without structural event emission, I must add event code to every provider
method. That is exactly the boilerplate the framework promises to eliminate.

### 3. No Percentage/Basis Point Validation

Music splits must always total exactly 10000 basis points (100%). If splits
sum to 9900, someone is losing 1% of their royalties. If splits sum to 10100,
the distribution function will over-pay and eventually run out of funds.

The Provider pattern allows a provider to validate this, but there is no
built-in support for invariant checking in the trait definition.

**Suggestion**: Consider a `#[post_condition]` or `#[invariant]` attribute:

```rust
#[contracttrait]
pub trait RoyaltyDistribution {
    fn split_table(env: &Env) -> Vec<(Address, u32)>;

    #[auth(Self::owner)]
    #[post_condition(Self::splits_valid)]  // called after provider method
    fn update_split(env: &Env, holder: Address, bps: u32);
}
```

Where `splits_valid` is a method on the Internal trait that panics if the
splits do not sum to 10000. This would be a structural guarantee that splits
are always valid, regardless of the provider implementation.

### 4. Sample Clearance and Nested Rights

When a song samples another song, the original rights holders get a percentage
of the new song's royalties. This creates a tree of rights:

```
New Song (100%)
  |- Songwriter A (40%)
  |- Producer B (20%)
  |- Publisher C (25%)
  |- Sample Clearance -> Original Song Contract (15%)
      |- Original Songwriter X (60% of 15% = 9% of total)
      |- Original Publisher Y (40% of 15% = 6% of total)
```

The `distribute()` function needs to make cross-contract calls to the sampled
song's distribution contract. The provider can implement this:

```rust
impl RoyaltyDistributionInternal for SampleAwareDistributor {
    fn distribute(env: &Env, amount: i128) {
        let splits = SplitStorage::get_splits(env);
        for (holder, bps) in splits {
            let share = amount * bps as i128 / 10000;
            if SplitStorage::is_sample_clearance(env, &holder) {
                // Cross-contract call to the sampled song's contract
                let sample_client = RoyaltyDistributionClient::new(env, &holder);
                sample_client.distribute(&share);
            } else {
                TokenClient::new(env, &token).transfer(
                    &env.current_contract_address(), &holder, &share
                );
            }
        }
    }
}
```

This works but raises auth questions: the cross-contract `distribute()` call
does not require auth (it is a public function), but it does require the
calling contract to have the funds to distribute. The auth model does not
address cross-contract fund management.

### 5. Territory-Based Rights Are Not Expressible

Music rights are territorial. A song might have different publishers in
different countries, with different split arrangements. The trait definition
has no concept of territory:

```rust
// NEEDED but not supported:
#[auth(Self::publisher_for_territory(territory))]
fn update_territory_split(env: &Env, territory: Symbol, holder: Address, bps: u32);
```

This would require parameterized auth sources, which the current system does
not support. The provider would need to accept the `territory` argument in
the auth method, which creates a dependency between the auth source and the
method parameters.

### 6. Dispute Resolution and Pausability

Music rights disputes are extremely common. A songwriter claims they were
not credited. An estate disputes a split change. A sample clearance is
contested. During disputes, the contract should be paused -- but only the
disputed operations, not the entire contract.

The current `Pausable` trait is binary: the contract is either paused or not.
For music rights, I need granular pausability:

```rust
#[contracttrait]
pub trait GranularPausable: Ownable {
    fn is_operation_paused(env: &Env, operation: Symbol) -> bool;

    #[auth(Self::owner)]
    fn pause_operation(env: &Env, operation: Symbol);

    #[auth(Self::owner)]
    fn unpause_operation(env: &Env, operation: Symbol);
}
```

Distributions can continue while split modifications are paused during a
dispute.

---

## Suggestions

### 1. Support Multi-Address Auth

This is the highest priority for music rights. Allow `#[auth(Self::a, Self::b)]`
to generate multiple `require_auth()` calls. This enables joint administration
without requiring multisig contract deployment.

### 2. Add Structural Event Emission

Either auto-generate events for all state-modifying methods, or provide an
`#[event]` annotation that generates event emission in the outer trait.
Audit trails are non-negotiable for royalty distribution.

### 3. Document the Reversion Pattern

The time-based ownership transition via providers is elegant and unique. Create
a dedicated example showing a `RevertingOwnership` provider with configurable
reversion date. This would attract music industry developers.

### 4. Add Invariant Checking

Support `#[post_condition]` or `#[invariant]` annotations that call validation
methods after the provider method executes. This enables structural enforcement
of business rules (splits sum to 10000, etc.).

### 5. Create a Music Rights Example

Build a complete example showing:
- Fractional ownership with a split table
- Joint administration (publisher + songwriter)
- Time-based reversion
- Distribution with sample clearance (cross-contract)
- Event emission for audit trail

This would demonstrate the framework's suitability for real-world creative
industry use cases.

### 6. Support Granular Pausability

Allow pausing specific operations rather than the entire contract. This is
essential for dispute resolution in music rights where distributions should
continue while administrative operations are frozen.

### 7. Provide a Royalty Split Provider Template

Create a reference `RoyaltySplitProvider` that handles:
- Split table management with basis point validation
- Distribution with proportional splits
- Sample clearance with cross-contract distribution
- Event emission for all state changes

---

## Unique Perspective: Music as the Hardest Ownership Problem

Music rights are, in my experience, the most complex ownership model in any
industry. A single song can have:
- 15+ rights holders
- Different splits in different territories
- Time-based reversions
- Nested rights from samples
- Joint administration requirements
- Active disputes with competing claims

If a framework can handle music rights, it can handle anything. The
`#[contracttrait]` macro comes closer than any Soroban framework I have
evaluated, primarily because of the provider pattern's flexibility. The
ability to implement `RevertingOwnership`, `FractionalOwnership`, and
`JointAdmin` as different providers for the same trait is exactly the
right abstraction.

But the single-address auth limitation is a real barrier. Music is
inherently multi-party. Every contract involves multiple stakeholders who
must jointly approve changes. Until `#[auth]` supports multiple addresses,
music rights developers must choose between structural auth guarantees
(sealed pattern with a multisig wrapper) and multi-party expressiveness
(flexible pattern with manual auth).

The ideal future state is a framework where I can write:

```rust
#[contracttrait]
pub trait MusicRights: Ownable + Pausable {
    fn isrc(env: &Env) -> Bytes;
    fn split_table(env: &Env) -> Vec<(Address, u32)>;

    #[auth(Self::publisher, Self::songwriter)]  // joint auth
    #[event]  // auto-emit audit event
    #[post_condition(Self::splits_valid)]  // invariant check
    fn update_split(env: &Env, holder: Address, bps: u32);

    #[event]  // auto-emit distribution event
    fn distribute(env: &Env, token: Address, amount: i128);
}

impl_music_rights!(SongContract, RevertingOwnership);
```

This single trait definition would express the complete authorization model,
audit trail, and business rules for a music rights contract. The framework
is 60% of the way there.

---

## Would I Use This?

For administrative control (who manages the contract): yes, today. The
provider pattern with time-based reversion is better than anything else
available for Soroban.

For royalty distribution logic: yes, with the caveat that I must implement
events, split validation, and multi-party auth manually in providers.

For a production music rights platform: not yet, because of the event and
multi-party auth gaps. I would use the framework for the ownership layer
and build the distribution layer with custom code on top.

**Verdict:** The most promising framework for music rights I have evaluated
on Soroban. The provider pattern's flexibility is a genuine breakthrough for
creative industry use cases. Multi-address auth and structural event emission
are the two features that would make this production-ready for royalty
distribution. The reversion pattern alone is worth highlighting in the
project's marketing -- it is a killer feature for the music industry that
no competitor offers as cleanly.
