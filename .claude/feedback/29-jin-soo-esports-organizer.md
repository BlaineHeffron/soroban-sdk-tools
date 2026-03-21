# Review: soroban-sdk-tools -- Esports Prize Pool & Tournament Contracts

**Reviewer:** Jin-soo Park
**Background:** Korean esports tournament organizer; ran 200+ tournaments for League of Legends, Valorant, and StarCraft II; pioneered transparent prize pool systems using blockchain; founder of a tournament platform serving 500K+ competitive gamers across Asia-Pacific
**Focus:** Real-time prize distribution, anti-cheat integration, transparent scoring, multi-party payouts

---

## Executive Summary

In esports, trust is everything. Players need to trust that the prize pool exists (not promised and then withdrawn). They need to trust that results are fair (not manipulated by the organizer). They need to trust that payouts happen (not delayed for months, as is sadly common in tier-2/3 esports).

I have lost players to competing organizations because they did not trust our payout system, even though we always paid. The perception problem is as bad as the actual problem. Blockchain-based prize pools solve this by making everything verifiable: the pool size, the payout rules, the results, and the distribution.

soroban-sdk-tools' composability model is relevant because tournament contracts compose multiple concerns that need independent auth: pool management, result submission, payout execution, and dispute handling.

---

## 1. Tournament Contract Architecture

### What a tournament smart contract needs

```
TournamentContract
|
|-- PrizePool (funds management)
|   |-- deposit (sponsor auth)
|   |-- total_pool (public)
|   |-- withdraw_unclaimed (admin auth, after deadline)
|
|-- Bracket (tournament structure)
|   |-- register_player (player auth + entry fee)
|   |-- seed_bracket (admin auth)
|   |-- current_round (public)
|
|-- Results (match outcomes)
|   |-- submit_result (referee/oracle auth)
|   |-- confirm_result (both players auth)
|   |-- dispute_result (player auth)
|
|-- Payout (prize distribution)
|   |-- calculate_payout (public, view function)
|   |-- execute_payout (automatic after finals)
|   |-- claim_prize (player auth)
```

### Mapping to soroban-sdk-tools traits

```rust
#[contracttrait]
pub trait PrizePool: Ownable {
    #[auth(sponsor)]
    fn deposit(env: &Env, sponsor: Address, amount: i128);

    fn total_pool(env: &Env) -> i128;

    #[auth(Self::owner)]
    fn withdraw_unclaimed(env: &Env, deadline_passed: bool);
}

#[contracttrait]
pub trait TournamentBracket: Ownable {
    #[auth(player)]
    fn register(env: &Env, player: Address);

    #[auth(Self::owner)]
    fn seed_bracket(env: &Env, seeds: Vec<Address>);

    fn current_round(env: &Env) -> u32;
    fn bracket_state(env: &Env) -> BracketState;
}

#[contracttrait]
pub trait ResultSubmission: TournamentBracket {
    fn referee(env: &Env) -> Address;

    #[auth(Self::referee)]
    fn submit_result(env: &Env, match_id: u32, winner: Address, score: MatchScore);

    #[auth(player)]
    fn confirm_result(env: &Env, player: Address, match_id: u32);

    #[auth(player)]
    fn dispute_result(env: &Env, player: Address, match_id: u32, evidence: Bytes);
}

#[contracttrait]
pub trait PayoutDistribution: PrizePool + ResultSubmission {
    fn calculate_payout(env: &Env, placement: u32) -> i128;

    #[auth(player)]
    fn claim_prize(env: &Env, player: Address);
}
```

This decomposition works well. The supertrait chain ensures that `PayoutDistribution` can access both `PrizePool` (for funds) and `ResultSubmission` (for results). The auth model ensures:

- Only sponsors can deposit into the pool
- Only the referee can submit official results
- Only players can confirm/dispute their own results
- Only players can claim their own prizes
- Only the admin can seed brackets and withdraw unclaimed funds

### The strength: auth per role

Tournament contracts have 4-5 distinct roles (admin, sponsor, referee, player, spectator). The `#[auth]` attribute handles the most common patterns:

- `#[auth(Self::owner)]` for admin
- `#[auth(Self::referee)]` for referee
- `#[auth(player)]` for player self-service
- `#[auth(sponsor)]` for sponsor self-service
- No auth for public/spectator methods

This is clean and readable. Looking at the trait definition, I can immediately see who controls each method. For transparency (which is the whole point of on-chain tournaments), this readability is valuable.

---

## 2. The Provider Pattern for Tournament Formats

### Different tournament structures

Esports uses many tournament formats:

- **Single elimination:** Lose once, you are out (most StarCraft II events)
- **Double elimination:** Lose twice, you are out (most FGC events, The International)
- **Round robin:** Everyone plays everyone (league stages)
- **Swiss system:** Opponents matched by current record (MTG, CS:GO majors)
- **Battle royale:** Everyone plays simultaneously, last standing wins (Fortnite, PUBG)

### Provider-based format selection

```rust
pub struct SingleElimination;
impl TournamentBracketInternal for SingleElimination {
    fn register(env: &Env, player: Address) {
        let count = BracketStorage::player_count(env);
        assert!(count < MAX_PLAYERS, "bracket full");
        BracketStorage::add_player(env, &player);
    }

    fn seed_bracket(env: &Env, seeds: Vec<Address>) {
        assert!(seeds.len().is_power_of_two(), "need power-of-2 players");
        BracketStorage::set_seeds(env, &seeds);
    }

    fn current_round(env: &Env) -> u32 {
        BracketStorage::get_current_round(env)
    }

    fn bracket_state(env: &Env) -> BracketState {
        BracketStorage::get_state(env)
    }
}

pub struct DoubleElimination;
impl TournamentBracketInternal for DoubleElimination {
    // Same interface, different bracket logic
    // Winners bracket + losers bracket + grand finals
}

pub struct SwissSystem;
impl TournamentBracketInternal for SwissSystem {
    // Same interface, different pairing logic
    // Pair by current record, no fixed bracket
}
```

Swapping `type Provider = SingleElimination` to `type Provider = DoubleElimination` changes the entire tournament format without changing the contract interface. Sponsors, players, and referees interact with the same methods regardless of format.

This is exactly what I want. My platform currently maintains separate contract codebases for each format. With the Provider pattern, I maintain one contract and swap the format. This alone justifies adopting soroban-sdk-tools.

---

## 3. Real-Time Result Submission and Anti-Cheat

### The latency problem

In esports, results need to be submitted quickly:
- After a StarCraft II match: result known within seconds, needs submission within minutes
- After a League of Legends match: result from the Riot API within 1-5 minutes
- After a Fortnite match: kill counts and placements finalized within 10 minutes

On Soroban, transaction finality is approximately 5 seconds. This is fast enough for post-match result submission but not for in-game events.

### The oracle pattern for game results

The `#[auth(Self::referee)]` pattern maps to an oracle-like referee:

```rust
pub struct RiotAPIOracle;
impl ResultSubmissionInternal for RiotAPIOracle {
    fn referee(env: &Env) -> Address {
        OracleConfig::get_riot_oracle(env)
    }

    fn submit_result(env: &Env, match_id: u32, winner: Address, score: MatchScore) {
        // Validate the result came from Riot's API relay
        // Store result with timestamp
        ResultStorage::store_result(env, match_id, &winner, &score, env.ledger().timestamp());
    }
}

pub struct ManualReferee;
impl ResultSubmissionInternal for ManualReferee {
    fn referee(env: &Env) -> Address {
        RefereeConfig::get_head_referee(env)
    }

    fn submit_result(env: &Env, match_id: u32, winner: Address, score: MatchScore) {
        // Manual submission with confirmation window
        ResultStorage::store_pending_result(env, match_id, &winner, &score);
        // Players have 15 minutes to confirm or dispute
    }
}
```

The Provider swap between `RiotAPIOracle` (automated) and `ManualReferee` (human) adapts to different tournament contexts:
- Major sponsored events: API oracle (trustless, fast)
- Community events: Manual referee (flexible, requires trust)
- Hybrid: API oracle with manual override (best of both)

### Anti-cheat integration

Anti-cheat is the most critical concern in competitive esports. The contract cannot detect cheating -- that requires game client analysis -- but it can:

1. **Require anti-cheat attestation:** `register` method checks for a valid anti-cheat certificate
2. **Allow result disputes with evidence:** `dispute_result` includes evidence submission
3. **Support result reversal:** Referee can overturn results if cheating is proven
4. **Freeze payouts pending investigation:** Pausable trait on the payout system

The current trait model handles 2-4. Item 1 would require:

```rust
#[contracttrait]
pub trait AntiCheatVerified: TournamentBracket {
    fn anti_cheat_oracle(env: &Env) -> Address;

    fn verify_player(env: &Env, player: Address) -> bool;

    // Override registration to require anti-cheat verification
    #[auth(player)]
    fn register(env: &Env, player: Address);
}
```

But this creates a conflict: `AntiCheatVerified` and `TournamentBracket` both define `register`. The supertrait model does not support method overriding -- the blog post explicitly cites override prevention as a security feature.

### Recommendation

Document patterns for "enhanced registration" where a supertrait adds pre-checks without redefining the method. Perhaps:

```rust
#[contracttrait]
pub trait AntiCheatGated {
    fn anti_cheat_oracle(env: &Env) -> Address;
    fn is_verified(env: &Env, player: Address) -> bool;
}

// Provider handles the integration:
pub struct AntiCheatBracket;
impl TournamentBracketInternal for AntiCheatBracket {
    fn register(env: &Env, player: Address) {
        // Check anti-cheat status before registration
        assert!(AntiCheatStorage::is_verified(env, &player), "not AC verified");
        // Then proceed with normal registration
        BracketStorage::add_player(env, &player);
    }
}
```

This puts the anti-cheat check in the Provider, which is the correct layer. Document this pattern as "provider-level guards" -- pre-checks that happen inside business logic but are not part of the trait interface.

---

## 4. Multi-Party Prize Distribution

### The payout complexity

Tournament payouts are not simple 1:1 transfers. A typical distribution:

| Placement | Prize Share |
|---|---|
| 1st | 40% |
| 2nd | 20% |
| 3rd-4th | 10% each |
| 5th-8th | 5% each |

Plus:
- Platform fee: 5%
- Sponsor bonus: Additional prizes for specific achievements (MVP, first blood, etc.)
- Tax withholding: Some jurisdictions require withholding tax on prize winnings
- Team splits: Players on teams might have internal revenue-sharing agreements

### A single `execute_payout` cannot handle this

The current trait model has `fn claim_prize(env: &Env, player: Address)` which is player-initiated. This is fine for a simple tournament but does not handle:

1. **Batch payouts:** After the grand finals, pay all 8 finalists at once
2. **Conditional payouts:** MVP bonus only if the sponsor deposited extra
3. **Deferred payouts:** Tax withholding requires calculating net amounts
4. **Split payouts:** Team prize split among 5 players

### Provider for complex distribution

```rust
pub struct TieredPayout;
impl PayoutDistributionInternal for TieredPayout {
    fn calculate_payout(env: &Env, placement: u32) -> i128 {
        let pool = PrizePoolStorage::total(env);
        let share = match placement {
            1 => 4000, // 40% in basis points
            2 => 2000,
            3 | 4 => 1000,
            5..=8 => 500,
            _ => 0,
        };
        (pool * share as i128) / 10000
    }

    fn claim_prize(env: &Env, player: Address) {
        let placement = ResultStorage::get_placement(env, &player);
        let amount = Self::calculate_payout(env, placement);
        assert!(amount > 0, "no prize");
        assert!(!PayoutStorage::has_claimed(env, &player), "already claimed");

        // Platform fee
        let fee = amount * 500 / 10000; // 5%
        let net = amount - fee;

        // Transfer to player
        TokenClient::new(env, &PayoutConfig::token(env)).transfer(
            &env.current_contract_address(),
            &player,
            &net,
        );

        // Transfer fee to platform
        TokenClient::new(env, &PayoutConfig::token(env)).transfer(
            &env.current_contract_address(),
            &PayoutConfig::platform_address(env),
            &fee,
        );

        PayoutStorage::mark_claimed(env, &player);
    }
}
```

This works but has a lot of logic inside a single provider method. The trait interface (`claim_prize`) is too simple for the complexity underneath.

### Recommendation

Consider splitting complex operations into multiple trait methods with clear lifecycle stages:

```rust
#[contracttrait]
pub trait PayoutLifecycle: PrizePool + ResultSubmission {
    // Stage 1: Tournament complete, calculate all payouts
    #[auth(Self::owner)]
    fn finalize_results(env: &Env);

    // Stage 2: View calculated payouts (public transparency)
    fn payout_for(env: &Env, player: Address) -> PayoutDetail;

    // Stage 3: Dispute window (24 hours)
    #[auth(player)]
    fn dispute_payout(env: &Env, player: Address, reason: Bytes);

    // Stage 4: Execute payouts (after dispute window)
    #[auth(player)]
    fn claim_prize(env: &Env, player: Address);

    // Stage 5: Sweep unclaimed (admin, after deadline)
    #[auth(Self::owner)]
    fn sweep_unclaimed(env: &Env);
}
```

This lifecycle mirrors how real tournament payouts work: finalize results, publish payouts, allow disputes, distribute, sweep unclaimed. Each stage has clear auth requirements.

---

## 5. Transparent Scoring and Leaderboards

### Why transparency matters

The #1 complaint from esports players about tournament organizers is: "I do not know how they calculated the standings." League formats, tiebreakers, and seeding criteria are often opaque.

### On-chain scoring

```rust
#[contracttrait]
pub trait Leaderboard: ResultSubmission {
    fn standings(env: &Env) -> Vec<Standing>;  // public, ordered by rank
    fn tiebreaker_rules(env: &Env) -> TiebreakerConfig;  // public

    #[auth(Self::referee)]
    fn apply_penalty(env: &Env, player: Address, penalty: PenaltyType);
}
```

All scoring methods are public (no auth). Anyone can verify standings. The tiebreaker rules are on-chain (auditable). Only the referee can apply penalties (DQ, point deductions).

The Provider pattern enables different scoring systems:

```rust
pub struct EloRating;           // chess-style rating (StarCraft)
pub struct SwissPoints;          // match wins + tiebreakers
pub struct BattleRoyalePoints;   // placement points + kill points (Fortnite)
```

Each scoring system is a different provider implementing the same `LeaderboardInternal` trait. This is clean and exactly what I need.

---

## 6. Entry Fees and Escrow

### The trust problem with entry fees

Players pay entry fees into prize pools. They need assurance that:
1. Their fee goes into the pool (not the organizer's pocket)
2. They can withdraw if the tournament is cancelled
3. The pool cannot be drained before the tournament concludes

### How the auth model helps

```rust
#[contracttrait]
pub trait EntryFeeManager: PrizePool + Pausable {
    #[auth(player)]
    fn pay_entry_fee(env: &Env, player: Address);

    #[auth(player)]
    fn request_refund(env: &Env, player: Address);  // only before tournament starts

    #[auth(Self::owner)]
    fn lock_pool(env: &Env);  // after tournament starts, no refunds
}
```

The Pausable supertrait enables cancellation (pause = tournament cancelled, refunds enabled).

The sealed auth pattern (`impl_entry_fee_manager!`) ensures that:
- Nobody can drain the pool without auth
- Nobody can issue refunds without player consent
- The organizer cannot prevent refunds before the pool is locked

This is a strong trust guarantee. When I tell players "the smart contract prevents me from touching your entry fee until the tournament is over," they believe me -- because the code proves it.

---

## 7. Multi-Game Tournament Support

### The reality of tournament platforms

My platform runs tournaments for 8 different games. Each game has:
- Different match formats (Bo1, Bo3, Bo5, Bo7)
- Different result data (kills, deaths, assists for FPS; game length, APM for RTS)
- Different anti-cheat requirements
- Different player counts (1v1, 5v5, 100-player battle royale)

### Provider pattern for game-specific logic

```rust
pub struct StarCraftII;
impl ResultSubmissionInternal for StarCraftII {
    fn submit_result(env: &Env, match_id: u32, winner: Address, score: MatchScore) {
        // SC2-specific: validate game length, race matchup, map
        assert!(score.game_length > 60, "suspicious: game too short");
        ResultStorage::store(env, match_id, &winner, &score);
    }
}

pub struct Valorant5v5;
impl ResultSubmissionInternal for Valorant5v5 {
    fn submit_result(env: &Env, match_id: u32, winner: Address, score: MatchScore) {
        // Valorant-specific: validate round scores, map pool
        assert!(score.rounds_won >= 13 || score.overtime, "invalid round count");
        ResultStorage::store(env, match_id, &winner, &score);
    }
}

pub struct FortniteBattleRoyale;
impl ResultSubmissionInternal for FortniteBattleRoyale {
    fn submit_result(env: &Env, match_id: u32, winner: Address, score: MatchScore) {
        // BR-specific: multiple placements per match, kill scoring
        // 'winner' is not meaningful in BR -- placement + kills determine score
        ResultStorage::store_br_result(env, match_id, &score);
    }
}
```

The one-line swap (`type Provider = StarCraftII` vs `type Provider = Valorant5v5`) adapts the entire contract to a different game. The interface stays the same. The sponsor's deposit flow stays the same. The payout flow stays the same. Only the result validation changes.

This is a genuine productivity win. Currently, I maintain per-game validation logic inline with the rest of the contract logic. Separating it into providers would make the codebase much cleaner.

---

## 8. Spectator and Community Engagement

### Beyond players: the audience

Esports is a spectator sport. 100M+ people watched the 2025 League of Legends World Championship. Spectators want:

1. **Prize pool tracking:** Real-time pool size (crowd-funded pools like The International)
2. **Prediction markets:** Bet on match outcomes
3. **Fan voting:** MVP awards, all-star selections
4. **Community contributions:** Donate to a player's prize

### How soroban-sdk-tools handles spectator features

All read-only methods (no `#[auth]`) are public. `total_pool()`, `standings()`, `bracket_state()` can be called by anyone. This is good.

For interactive spectator features (prediction markets, fan voting), additional traits:

```rust
#[contracttrait]
pub trait PredictionMarket: TournamentBracket {
    #[auth(predictor)]
    fn predict(env: &Env, predictor: Address, match_id: u32, predicted_winner: Address, amount: i128);

    fn odds(env: &Env, match_id: u32) -> (i128, i128);

    #[auth(predictor)]
    fn claim_prediction_payout(env: &Env, predictor: Address, match_id: u32);
}
```

The `#[auth(predictor)]` param auth ensures each person can only claim their own prediction payout. The Provider could implement different odds-calculation mechanisms (parimutuel, fixed odds, etc.).

---

## 9. Cross-Chain and Multi-Region Concerns

### The esports geography problem

Esports is global. A Korean StarCraft II player, a European CS:GO team, and a Brazilian Valorant squad might all compete in the same tournament. Each has:
- Different local currencies (KRW, EUR, BRL)
- Different regulatory requirements (Korean gambling laws, EU consumer protection, Brazilian digital asset regulations)
- Different latency to the Stellar network

### What soroban-sdk-tools helps with

The Provider pattern could handle regional compliance:

```rust
pub struct KoreanCompliance;     // No gambling features, KYC required
pub struct EUCompliance;          // GDPR-compliant data handling
pub struct BrazilianCompliance;   // CVM digital asset rules
```

But the framework does not address multi-currency prize pools or cross-chain payout. If the prize pool is in USDC but a Korean player wants payout in KRW (via a Stellar anchor), the payout provider needs to handle FX.

### Recommendation

Document how providers can integrate with Stellar anchors for fiat off-ramps. For esports, "fast fiat payout" is a competitive advantage. Players do not want crypto -- they want money in their bank account.

---

## 10. Final Assessment

### For esports tournament contracts specifically

| Capability | Readiness | Impact |
|---|---|---|
| Multi-role auth (admin, referee, player, sponsor) | Ready | High |
| Tournament format swap (SE, DE, Swiss, BR) | Ready | High |
| Prize pool escrow | Ready | High |
| Result submission with dispute | Ready | High |
| Multi-party payout distribution | Partially ready | High |
| Anti-cheat integration | Partially ready | Medium |
| Prediction markets | Ready (separate trait) | Medium |
| Multi-currency payout | Not ready | Medium |
| Real-time leaderboards | Ready | Medium |
| Cross-chain integration | Not ready | Low (for now) |

### What excites me most

1. **The Provider pattern for tournament formats.** Swapping between Single Elimination, Double Elimination, Swiss, and Battle Royale with one line change is a genuine productivity multiplier. This alone would save my team 2-3 months of development time per year.

2. **Structural auth per role.** Looking at the trait definition and immediately seeing "referee submits results, players dispute, admin seeds brackets" is incredibly clear. My non-technical co-founder could read the trait definitions and understand the access control model.

3. **The sealed auth for prize pools.** I can tell players "the code proves I cannot touch your money." The `impl_prize_pool!` macro makes this guarantee structural, not just a promise.

4. **AuthClient for tournament testing.** I can write tests that prove: "A non-referee cannot submit results." "A player cannot claim another player's prize." "The organizer cannot drain the pool." These tests are auditable trust-building tools.

### What I need before adopting

1. **A tournament example.** Not a generic Ownable/Pausable example. An actual tournament contract with bracket management, result submission, and prize distribution.

2. **Batch payout patterns.** 100+ players claiming prizes individually is gas-inefficient. A batch payout mechanism is essential.

3. **Event emission for tournament lifecycle.** Registration events, result events, payout events -- all must be emitted reliably for frontend display and historical records.

4. **Storage patterns for dynamic collections.** Player lists, bracket structures, match results -- these are dynamic arrays and maps. How to handle them efficiently in Soroban storage is not documented.

### Score: 7.5/10

soroban-sdk-tools is well-suited for esports tournament contracts. The Provider pattern maps perfectly to the problem domain (multiple tournament formats, multiple games, multiple scoring systems). The auth model handles the multi-role nature of tournaments cleanly. The sealed auth pattern provides the trust guarantees that players demand.

The gaps are practical rather than architectural: batch operations, event emission, storage patterns, and a domain-specific example. These are addressable without changes to the core framework.

I would start prototyping a tournament contract with soroban-sdk-tools today, with the understanding that production deployment requires the above gaps to be filled.

---

*Reviewed by Jin-soo Park, CEO & Founder, [redacted] Esports Platform*
*Review date: 2026-03-21*
