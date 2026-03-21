---
reviewer: Jin-soo Park
role: Esports Platform Engineer
domain: Tournament Infrastructure, Prize Pools, Competitive Integrity
date: 2026-03-21
focus: Real-time prize distribution, tournament lifecycle, anti-cheat
---

# Review: soroban-sdk-tools -- Esports Tournament Infrastructure

## Context

I build tournament infrastructure for competitive gaming. A major tournament
has 128 teams, group stages, double elimination brackets, and prize pools
that shift based on crowd-funding, sponsor contributions, and performance
bonuses. Prize distribution must happen within minutes of a match conclusion
-- players expect instant payouts. The system must handle disputes,
disqualifications, and redistribution without manual intervention.

## Architectural Analysis

### Tournament as a Composed Contract

A tournament contract needs multiple composed behaviors:

```rust
#[contracttrait]
pub trait Tournament: Ownable + Pausable {
    fn prize_pool(env: &Env) -> i128;
    fn stage(env: &Env) -> Symbol;  // registration, group, bracket, final, settled

    #[auth(Self::owner)]  // tournament organizer
    fn advance_stage(env: &Env, new_stage: Symbol);

    #[auth(Self::owner)]
    fn record_result(env: &Env, match_id: u32, winner: Address, loser: Address);

    #[auth(Self::owner)]
    fn disqualify(env: &Env, team: Address, reason: Symbol);

    // Permissionless -- anyone can fund the prize pool
    fn contribute(env: &Env, funder: Address, amount: i128);

    #[auth(Self::owner)]
    fn distribute_prizes(env: &Env);
}
```

The supertrait composition works well here. `Pausable` handles dispute
freezes. `Ownable` identifies the tournament organizer. The `#[auth]`
pattern correctly distinguishes organizer-only actions from permissionless
actions.

### Real-Time Requirements

The `#[auth(Self::owner)]` on `record_result` means the tournament organizer
must sign every match result. In a 128-team tournament with 127 matches, that
is 127 separate authorized transactions. Each must confirm within one
ledger close (approximately 5 seconds on Stellar).

The `CallBuilder` supports this:

```rust
for result in match_results {
    auth_client.record_result(&result.match_id, &result.winner, &result.loser)
        .authorize(&organizer)
        .invoke();
}
```

But this is sequential. Each `invoke()` waits for confirmation.

**Concern**: There is no batch invocation support. Recording 127 match
results requires 127 separate transactions. At 5 seconds each, that is
over 10 minutes -- unacceptable for live tournament broadcasts.

**Suggestion**: Support batch invocations in `CallBuilder`:

```rust
let mut batch = BatchBuilder::new(&env, &contract_id);
for result in match_results {
    batch.add(auth_client.record_result(&result.match_id, &result.winner, &result.loser));
}
batch.authorize(&organizer).invoke_all();
```

### Provider Pattern for Tournament Formats

Different tournament formats have different prize distribution logic:

```rust
pub struct SingleElimProvider;
pub struct DoubleElimProvider;
pub struct RoundRobinProvider;
pub struct SwissProvider;

impl TournamentInternal for DoubleElimProvider {
    fn distribute_prizes(env: &Env) {
        // 1st: 50%, 2nd: 25%, 3rd-4th: 10% each, 5th-8th: 1.25% each
        let pool = TournamentStorage::get_prize_pool(env);
        let standings = BracketStorage::get_final_standings(env);
        // ... distribute based on double-elimination standings
    }
}
```

Swapping `impl_tournament!(MyTournament, DoubleElimProvider)` to
`impl_tournament!(MyTournament, SwissProvider)` changes the entire
prize structure with one token. This is excellent for tournament platforms
that run different format events.

## Concerns

### 1. Disqualification and Prize Redistribution

When a team is disqualified (cheating, no-show, rule violation), prizes
must be redistributed to remaining teams. The current `disqualify` method
records the disqualification, but the redistribution logic depends on
when in the tournament it happens:

- During group stage: team's matches are voided, standings recalculated
- During bracket: opponent advances, bracket rebuilt
- After settlement: prizes clawed back and redistributed

This lifecycle complexity belongs in the Provider, which is correct. But
the `#[auth(Self::owner)]` on `disqualify` means only the organizer can
disqualify. In competitive esports, disqualifications should require
multi-party agreement (organizer + anti-cheat system + referee panel).

This connects to the multi-auth concern raised in other reviews. A
`#[auth(Self::organizer, Self::referee)]` syntax would be valuable here.

### 2. Dispute Resolution and Contract Freezing

The `Pausable` supertrait provides `pause()` and `unpause()`. But
tournament disputes need granular freezing:

- Freeze a specific match result (not the whole tournament)
- Freeze prize distribution (but allow match recording)
- Freeze registrations (but allow existing matches)

A single boolean `is_paused` is too coarse.

**Suggestion**: Support parameterized pause states:

```rust
#[contracttrait]
pub trait GranularPausable: Ownable {
    fn is_paused(env: &Env, scope: Symbol) -> bool;

    #[auth(Self::owner)]
    fn pause_scope(env: &Env, scope: Symbol);

    #[auth(Self::owner)]
    fn unpause_scope(env: &Env, scope: Symbol);
}
```

Providers can then check specific scopes:

```rust
fn distribute_prizes(env: &Env) {
    assert!(!GranularPausable::is_paused(env, symbol!("distribution")));
    // ...
}
```

### 3. Crowd-Funded Prize Pools and Refunds

The `contribute` function allows anyone to fund the prize pool. But if a
tournament is cancelled, contributors need refunds. The current architecture
has no withdrawal or refund pattern.

The Provider can implement this, but the trait-level auth model creates a
tension: `contribute` uses `#[auth(funder)]` (the contributor authorizes
their own deposit), but refunds need either:

- `#[auth(Self::owner)]` -- organizer-initiated refund (what if organizer
  disappears?)
- `#[auth(funder)]` -- contributor withdraws (requires tracking individual
  contributions)
- Permissionless -- automatic refund if tournament is cancelled (best for
  users)

The Provider pattern handles all three, but the documentation should
show the refund pattern as a first-class concern.

### 4. Anti-Cheat Integration

Competitive integrity requires that match results can be verified against
an external anti-cheat system. The oracle pattern from the insurance
review applies here:

```rust
fn anti_cheat_oracle(env: &Env) -> Address;

#[auth(Self::anti_cheat_oracle)]
fn validate_match(env: &Env, match_id: u32, integrity_hash: BytesN<32>);
```

The multi-role auth model (organizer + oracle) works for this. But the
validation must happen BEFORE prize distribution, creating a dependency
chain. The Provider can enforce this ordering, but the trait definition
does not express it.

### 5. The `AuthClient` for Tournament Testing

Testing a 128-team tournament requires:
- 128 team registrations
- 127 match results
- Prize distribution
- Potentially disqualifications and redistributions

The `AuthClient` pattern of `.authorize(&organizer).invoke()` for each
action is verbose for this volume. A test helper that automates a full
tournament lifecycle would be valuable:

```rust
let sim = TournamentSimulator::new(&env, &contract_id, &organizer);
sim.register_teams(128);
sim.run_bracket();  // records all match results
sim.distribute();
sim.verify_payouts();
```

This is outside the scope of `soroban-sdk-tools`, but the `AuthClient`
infrastructure makes it possible to build.

## Summary

The Provider pattern excels for tournament contracts. Swapping tournament
formats (single elim, double elim, Swiss, round robin) with a single
provider change is the right abstraction. Supertrait composition with
Ownable and Pausable covers the basic lifecycle. The gaps are batch
invocation support for high-volume operations, granular pause scoping for
dispute resolution, multi-party auth for disqualification governance,
and documentation of the refund pattern for crowd-funded prize pools.
The batch invocation gap is the most impactful for real-time esports.
