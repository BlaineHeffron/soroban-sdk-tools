# Review: Mikaela -- Swedish Death Metal Vocalist Building a Fan Token Platform

**Reviewer Profile:** Vocalist for the Swedish death metal band "Gravblot." Building a fan token platform where token holders get concert access, exclusive merch drops, and governance over band decisions like setlists and tour destinations.

**Materials Reviewed:**
- `docs/oz-comparison.md`
- `docs/blog-post-composable-contracts.md`
- `examples/trait-test/src/lib.rs`
- `soroban-sdk-tools-macro/src/contract.rs`

---

## Overall Impression

Let me be blunt: most fan token platforms are cash grabs that give fans nothing meaningful. They mint tokens, hype the price, and disappear. I want something different. Gravblot's fan tokens should give real utility: vote on setlists, unlock backstage access, get first dibs on limited merch, and participate in band decisions.

soroban-sdk-tools is interesting because the provider pattern lets me build different engagement models (casual fans, superfans, lifetime supporters) using the same trait interface. The sealed macro means fans can trust that the rules will not change after they buy in. But the toolkit is missing the engagement primitives I need: timed drops, tiered access, community voting, and event-based unlocks.

**Rating: 3.5/5** -- Solid infrastructure for trustworthy token systems, but needs fan engagement features to be useful for a music platform.

---

## Strengths

### 1. Provider Pattern Supports Tiered Fan Engagement

Gravblot has different fan tiers:
- **Bloodpact** holders (1000 tokens): vote on setlists, early merch access
- **Deathsworn** holders (5000 tokens): backstage passes, signed merch
- **Eternal** holders (10000 tokens): band dinner, studio sessions, name in album credits

The provider pattern can model these tiers:

```rust
pub struct BloodpactAccess;
impl FanAccessInternal for BloodpactAccess {
    fn can_access(env: &Env, fan: Address, feature: Symbol) -> bool {
        let balance = get_token_balance(env, &fan);
        match feature.to_string().as_str() {
            "setlist_vote" => balance >= 1000,
            "early_merch" => balance >= 1000,
            "backstage" => balance >= 5000,
            "band_dinner" => balance >= 10000,
            _ => false,
        }
    }
}
```

Swapping providers lets us adjust tier thresholds without redeploying. When we go on a bigger tour, maybe backstage access drops to 3000 tokens. One line change, same contract:

```rust
impl_fan_access!(GravblotContract, TourSpecialAccess);
```

This is exactly the flexibility a touring band needs.

### 2. Sealed Macro Means "No Rug Pulls"

The biggest problem in fan tokens is rug pulls: the band changes the rules after fans buy in, or the developers drain the treasury. The sealed macro prevents this:

```rust
impl_fan_token!(GravblotContract, FairLaunchProvider);
// Treasury withdrawal requires fan vote
// Token supply cannot be inflated
// These rules are baked into the compiled contract
```

When I tell fans "the contract is sealed -- even I cannot change the rules," they can verify this by examining the WASM binary. This is the trust foundation that fan tokens need. No promises, no trust-me-bro -- just code that cannot be changed.

### 3. Supertrait Composition Models the Full Fan Experience

A fan token platform is not just a token -- it is a token plus access control plus governance plus merchandise plus events. The supertrait pattern composes all of these:

```rust
#[contracttrait]
pub trait FanToken: Ownable {
    fn balance(env: &Env, fan: Address) -> i128;
    fn total_supply(env: &Env) -> i128;

    #[auth(fan)]
    fn transfer(env: &Env, fan: Address, to: Address, amount: i128);
}

#[contracttrait]
pub trait FanGovernance: FanToken {
    #[auth(fan)]
    fn vote(env: &Env, fan: Address, proposal_id: u64, choice: bool);
    // Voting power = token balance

    fn proposal_result(env: &Env, proposal_id: u64) -> (i128, i128);
    // Returns (votes_for, votes_against)
}

#[contracttrait]
pub trait MerchDrop: FanToken {
    #[auth(Self::owner)]
    fn create_drop(env: &Env, drop_id: Symbol, supply: u32, price: i128, min_tokens: i128);

    #[auth(fan)]
    fn claim_merch(env: &Env, fan: Address, drop_id: Symbol);
}
```

Each trait builds on the previous. Governance requires tokens. Merch drops require tokens. The supertrait chain enforces this structurally. You cannot vote without tokens. You cannot claim merch without tokens. The hierarchy is correct.

### 4. AuthClient Tests the Fan Journey

The `AuthClient` lets me test the complete fan journey:

```rust
// Fan buys tokens
token_auth.transfer(&treasury, &fan_addr, &1000)
    .authorize(&treasury)
    .invoke();

// Fan votes on setlist
governance_auth.vote(&fan_addr, &proposal_id, &true)
    .authorize(&fan_addr)
    .invoke();

// Fan claims merch
merch_auth.claim_merch(&fan_addr, &tshirt_drop_id)
    .authorize(&fan_addr)
    .invoke();

// Non-holder tries to vote -- should fail
let result = governance_auth.vote(&non_holder, &proposal_id, &true)
    .authorize(&non_holder)
    .try_invoke();
assert!(result.is_err());
```

Every step of the fan journey is testable with real auth. No `mock_all_auths()` hiding bugs.

---

## Concerns

### 1. No Timed Event Mechanism (Merch Drops, Concert Access)

**Severity: High**

Fan engagement is time-based. A merch drop opens at 10 AM on Friday and closes when sold out. Concert access unlocks 24 hours before the show and expires when the show ends. The current trait system has no temporal primitives.

The `Ownable` and `Pausable` traits have no concept of "this function is only callable between timestamp X and timestamp Y." For a fan token platform, every engagement feature has a time window.

**Recommendation:** Add temporal gating to the `#[auth]` system:

```rust
#[contracttrait]
pub trait TimedMerchDrop: FanToken {
    #[auth(fan)]
    #[active_between(Self::drop_start, Self::drop_end)]
    fn claim_merch(env: &Env, fan: Address, drop_id: Symbol);

    fn drop_start(env: &Env) -> u64;
    fn drop_end(env: &Env) -> u64;
}
```

The macro would generate temporal validation alongside the auth check:

```rust
fn claim_merch(env: &Env, fan: Address, drop_id: Symbol) {
    fan.require_auth();
    let now = env.ledger().timestamp();
    assert!(now >= Self::Provider::drop_start(env), "drop not started");
    assert!(now <= Self::Provider::drop_end(env), "drop ended");
    Self::Provider::claim_merch(env, fan, drop_id);
}
```

### 2. No Supply-Limited Minting (Edition Drops)

**Severity: High**

Merch drops and NFT-style collectibles need supply limits. "Only 500 tour posters exist" is a key value driver for fan tokens. The current trait system has no concept of supply limits, edition sizes, or scarcity management.

The `FungibleToken` trait in the comparison has `total_supply` but no `max_supply`. There is no mechanism to enforce "this function can only be called N times."

**Recommendation:** Add a supply-limited pattern:

```rust
#[contracttrait]
pub trait EditionDrop {
    fn edition_size(env: &Env, drop_id: Symbol) -> u32;
    fn claimed_count(env: &Env, drop_id: Symbol) -> u32;

    #[auth(fan)]
    #[supply_limited(Self::edition_size, Self::claimed_count)]
    fn claim(env: &Env, fan: Address, drop_id: Symbol);
}
```

### 3. No Community Governance for Band Decisions

**Severity: High**

The whole point of Gravblot's fan tokens is governance: fans vote on setlists, tour destinations, album art, and merch designs. The current `Ownable` pattern puts the band (owner) in charge of everything. We want the opposite: fans in charge of certain decisions.

This echoes the cooperative governance concern (see Yael's review), but with a specific use case: how do token-weighted votes work?

The `#[auth(fan)]` pattern checks that the fan signed the transaction. But it does not check that the fan *holds enough tokens* to vote. Token-weighted governance requires:

1. Snapshot the token balances at a specific ledger
2. Open a voting period
3. Allow each fan to vote with weight proportional to their balance at snapshot time
4. Close voting and execute the winning proposal

None of this is supported by the current `#[auth]` pattern.

**Recommendation:** Token-weighted governance should be a standard provider pattern, not a custom implementation. Create a `GovernanceProvider` that handles snapshot-based voting:

```rust
pub struct TokenWeightedGovernance;
impl FanGovernanceInternal for TokenWeightedGovernance {
    fn vote(env: &Env, fan: Address, proposal_id: u64, choice: bool) {
        let snapshot_ledger = get_proposal_snapshot(env, proposal_id);
        let weight = get_balance_at_snapshot(env, &fan, snapshot_ledger);
        assert!(weight > 0, "no voting power at snapshot");
        record_vote(env, proposal_id, &fan, choice, weight);
    }
}
```

### 4. No Event-Based Unlocks (Proof of Attendance)

**Severity: Medium**

Fan engagement is tied to real-world events. Fans who attend a concert should get a "Proof of Attendance" token that unlocks exclusive content. Fans who attend 10 concerts should get a special status.

The current system has no mechanism for bridging real-world events to on-chain state. The auth pattern requires cryptographic signatures, but a fan at a concert might authenticate via:
- QR code scan at the venue
- NFC tap at the merch booth
- Geolocation check (proximity to venue)

These are off-chain authentication methods that need to be bridged to on-chain auth.

**Recommendation:** Add an oracle/attestation pattern for real-world event verification:

```rust
#[contracttrait]
pub trait EventAttendance {
    fn event_oracle(env: &Env) -> Address; // trusted venue system

    #[auth(Self::event_oracle)]
    fn record_attendance(env: &Env, fan: Address, event_id: Symbol);

    fn attendance_count(env: &Env, fan: Address) -> u32;
    fn attended_event(env: &Env, fan: Address, event_id: Symbol) -> bool;
}
```

The venue system (event oracle) is authorized to record attendance. The sealed macro ensures that only the authorized venue system can record attendance -- no self-attestation, no fake attendance.

### 5. No Royalty or Revenue Sharing

**Severity: Medium**

When Gravblot earns money (streaming royalties, merch sales, concert revenue), token holders should get a share. This is the "fan as stakeholder" model: not just governance, but economic participation.

The current system has no revenue distribution mechanism. The `FungibleToken` trait models transfers but not dividends. For a fan token platform, revenue sharing is the economic engine:

```rust
#[contracttrait]
pub trait RevenueShare: FanToken {
    fn accumulated_revenue(env: &Env) -> i128;

    #[auth(Self::owner)]
    fn deposit_revenue(env: &Env, amount: i128, source: Symbol);
    // source: "streaming", "merch", "concerts"

    #[auth(fan)]
    fn claim_share(env: &Env, fan: Address);
    // Share proportional to token balance / total_supply
}
```

### 6. No Merchandise State Machine

**Severity: Low**

A merch drop has states: announced, open, sold_out, shipped, delivered. The current trait system does not model state machines. Each merch item goes through:

1. Created (by band)
2. Listed (available for claim)
3. Claimed (fan has claimed, payment deducted)
4. Shipped (physical item sent)
5. Delivered (confirmed receipt)

The auth requirements change at each state: the band creates and ships, the fan claims, and the delivery oracle confirms.

**Recommendation:** Document a state machine pattern using providers:

```rust
pub struct MerchStateMachine;
impl MerchDropInternal for MerchStateMachine {
    fn claim_merch(env: &Env, fan: Address, drop_id: Symbol) {
        let state = get_merch_state(env, &drop_id);
        assert!(state == MerchState::Listed, "merch not available");
        assert!(get_claimed_count(env, &drop_id) < get_edition_size(env, &drop_id), "sold out");
        // Process claim...
        set_fan_claim_state(env, &fan, &drop_id, MerchState::Claimed);
    }
}
```

---

## Use Case Exploration: Gravblot Fan Token Platform

Here is the full system:

```rust
// Layer 1: Fan Token (fungible, transferable)
#[contracttrait]
pub trait GravblotToken: Ownable {
    fn balance(env: &Env, fan: Address) -> i128;
    fn total_supply(env: &Env) -> i128;

    #[auth(from)]
    fn transfer(env: &Env, from: Address, to: Address, amount: i128);

    #[auth(Self::owner)]
    fn mint(env: &Env, to: Address, amount: i128);
    // Only band can mint -- sealed macro prevents inflation hacks
}

// Layer 2: Fan Governance (vote on band decisions)
#[contracttrait]
pub trait GravblotGovernance: GravblotToken {
    #[auth(Self::owner)]
    fn create_proposal(env: &Env, title: Symbol, options: Vec<Symbol>, snapshot_ledger: u32);

    #[auth(voter)]
    fn cast_vote(env: &Env, voter: Address, proposal_id: u64, option_index: u32);

    fn get_results(env: &Env, proposal_id: u64) -> Vec<(Symbol, i128)>;
}

// Layer 3: Merch Drops (time-limited, supply-limited)
#[contracttrait]
pub trait GravblotMerch: GravblotToken {
    #[auth(Self::owner)]
    fn create_drop(env: &Env, drop_id: Symbol, price: i128, supply: u32,
                   min_tokens: i128, open_at: u64, close_at: u64);

    #[auth(fan)]
    fn claim(env: &Env, fan: Address, drop_id: Symbol);

    fn drop_info(env: &Env, drop_id: Symbol) -> DropInfo;
}

// Layer 4: Concert Access (event-based, oracle-verified)
#[contracttrait]
pub trait GravblotConcerts: GravblotToken {
    fn venue_oracle(env: &Env) -> Address;

    #[auth(Self::owner)]
    fn create_event(env: &Env, event_id: Symbol, venue: Symbol, date: u64,
                    capacity: u32, min_tokens: i128);

    #[auth(fan)]
    fn reserve_spot(env: &Env, fan: Address, event_id: Symbol);

    #[auth(Self::venue_oracle)]
    fn confirm_attendance(env: &Env, fan: Address, event_id: Symbol);
}
```

The sealed macro ensures:
- Only the band can mint tokens (no inflation)
- Only the band can create proposals (fans vote, band sets the agenda)
- Only authorized fans can claim merch (token gate enforced)
- Only the venue oracle can confirm attendance (no fake POAPs)

This is a fan token platform with real utility, real governance, and real trust guarantees.

---

## Documentation Feedback

### The Blog Post

The blog post talks to DeFi developers. For the music industry, the messaging should be:

- "Structural auth means fans can trust that the rules will not change"
- "Sealed macros mean the band cannot rug-pull, even if they wanted to"
- "Provider swapping means the engagement model can evolve with the fan base"
- "AuthClient means every interaction is testable before launch"

### The OZ Comparison

The comparison focuses on ownership and pausability. For fan tokens, the comparison should include:
- Token-gated access (neither OZ nor soroban-sdk-tools has this built-in)
- Governance (OZ has AccessControl, but not token-weighted voting)
- Event-based unlocks (neither has this)

### The Example Code

The example shows `Ownable` and `Pausable`. Consider adding a fan token example that demonstrates:
1. Token minting with supply cap
2. Token-gated function access
3. Simple governance (create proposal, vote, read results)

This would show that soroban-sdk-tools is not just for DeFi plumbing -- it is for building real fan experiences.

---

## The Death Metal Perspective

In death metal, the riff is everything. A great riff is simple, brutal, and unforgettable. The `#[contracttrait]` macro is a great riff: simple (one annotation), brutal (structural auth enforcement), and unforgettable (two-trait generation).

But a song is more than a riff. You need verses, choruses, bridges, breakdowns, and the occasional blast beat. Right now, soroban-sdk-tools gives you the riff but not the song structure. The temporal patterns (timed drops, voting periods, event windows) are the verses and choruses that make the riff into a complete song.

Give me the full song structure, and I will build a fan token platform that makes Gravblot's community the most engaged in metal. The riff is there. Now write the rest of the song.

SKOL.
