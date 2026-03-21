---
reviewer: Alessandro Ferretti
role: Parametric Insurance Architect
domain: Weather Derivatives, Catastrophe Bonds, Oracle Integration
date: 2026-03-21
focus: Oracle data feeds, automated claim settlement, actuarial validation
---

# Review: soroban-sdk-tools -- Parametric Insurance Perspective

## Context

I design parametric insurance products -- contracts that pay out automatically
when a measurable trigger occurs (rainfall below 20mm, earthquake above 5.0
Richter, temperature above 42C). There is no claims adjuster, no paperwork.
The oracle reports the measurement, the contract pays or does not.

The critical question: can `soroban-sdk-tools` express the authorization
model where an oracle feeds data, an admin manages policy terms, and payouts
execute automatically without human intervention?

## Architectural Fit

### Multi-Role Auth via Provider Pattern

A parametric insurance contract has at least three roles:

1. **Admin**: Sets policy terms, adds/removes beneficiaries
2. **Oracle**: Submits weather/seismic data
3. **Anyone**: Triggers payout evaluation (permissionless)

The `#[auth]` pattern supports this naturally:

```rust
#[contracttrait]
pub trait ParametricInsurance: Ownable {
    fn oracle(env: &Env) -> Address;

    #[auth(Self::owner)]
    fn set_trigger_threshold(env: &Env, metric: Symbol, threshold: i128);

    #[auth(Self::oracle)]
    fn submit_observation(env: &Env, metric: Symbol, value: i128, timestamp: u64);

    // No auth -- anyone can trigger evaluation
    fn evaluate_and_settle(env: &Env);
}
```

The ability to define multiple auth sources (`Self::owner` for admin,
`Self::oracle` for data feeds, none for settlement) in a single trait is
exactly right. OZ's `#[only_owner]` macro only supports one role; additional
roles require manual `require_auth()` calls.

### Sealed Auth for Regulatory Compliance

Insurance contracts are regulated. Auditors need proof that:
- Only the authorized oracle can submit data
- Only the admin can modify terms
- Payout logic cannot be tampered with

The `impl_parametric_insurance!` sealed macro satisfies all three requirements.
The generated inherent methods cannot be overridden, providing a verifiable
guarantee to regulators.

## Concerns

### 1. Oracle Data Freshness and Staleness

The `submit_observation` method has no built-in staleness check. An oracle
could submit data from a week ago, and the contract would accept it. In
parametric insurance, data freshness is critical -- a rainfall reading must
be from the correct measurement period.

The Provider can handle this:

```rust
impl ParametricInsuranceInternal for WeatherInsuranceProvider {
    fn submit_observation(env: &Env, metric: Symbol, value: i128, timestamp: u64) {
        let current_time = env.ledger().timestamp();
        assert!(current_time - timestamp < MAX_STALENESS, "data too old");
        ObservationStorage::store(env, &metric, value, timestamp);
    }
}
```

But this is a pattern that every parametric insurance provider will need.

**Suggestion**: Consider a `#[fresh(max_age_seconds)]` attribute that
generates staleness checks for methods that accept timestamp parameters:

```rust
#[auth(Self::oracle)]
#[fresh(timestamp, max_age = 3600)]  // 1 hour max age
fn submit_observation(env: &Env, metric: Symbol, value: i128, timestamp: u64);
```

### 2. Oracle Rotation and Multi-Oracle Consensus

A single oracle is a single point of failure. Production parametric insurance
uses multiple oracles with consensus (median value, 2-of-3 agreement).

The `#[auth(Self::oracle)]` pattern authorizes ONE oracle address. For
multi-oracle consensus, the options are:

a) The oracle address is a contract that aggregates multiple data feeds
b) The provider implements consensus logic internally
c) Multiple `submit_observation` calls with a finalize step

Option (b) maps to the Provider pattern:

```rust
pub struct MultiOracleProvider;
impl ParametricInsuranceInternal for MultiOracleProvider {
    fn oracle(env: &Env) -> Address {
        // Return the "oracle committee" contract address
        OracleStorage::get_committee_address(env)
    }
}
```

But option (c) requires per-oracle auth with a different signature:

```rust
#[auth(oracle)]  // parameter-level auth
fn submit_observation(env: &Env, oracle: Address, metric: Symbol, value: i128);
```

Then any registered oracle can submit, and the provider tracks submissions
until quorum is reached. This works with the current `#[auth(param)]` pattern.
Good.

### 3. No Time-Lock or Cooling Period Support

Insurance payouts should have a cooling period (e.g., 24 hours after trigger)
to allow for oracle dispute resolution. The current trait model has no concept
of time-delayed execution.

**Suggestion**: A `#[timelock(ledgers = 17280)]` attribute (approximately
24 hours at 5-second ledger close) that generates a two-step pattern:

```rust
// Step 1: Initiate (stores the action with an unlock ledger)
fn initiate_settle(env: &Env) -> u32;  // returns unlock ledger

// Step 2: Execute (only after unlock ledger)
fn execute_settle(env: &Env);
```

This is similar to OZ's two-step transfer pattern and could be generalized.

### 4. The `evaluate_and_settle` Pattern Needs Reentrancy Protection

The permissionless `evaluate_and_settle` function reads oracle data, computes
payouts, and transfers tokens. If the payout recipient is a contract that
calls back into `evaluate_and_settle`, a reentrancy attack could drain the
pool.

Soroban's architecture mitigates some reentrancy risks, but the documentation
should address this pattern explicitly. The `Pausable` supertrait helps:

```rust
#[contracttrait]
pub trait ParametricInsurance: Ownable + Pausable {
    fn evaluate_and_settle(env: &Env);
}

// In the provider:
fn evaluate_and_settle(env: &Env) {
    assert!(!PausableInternal::is_paused(env), "paused");
    PausableInternal::pause(env);  // prevent reentrancy
    // ... do payouts ...
    PausableInternal::unpause(env);
}
```

But using Pausable for reentrancy protection is a semantic mismatch.

**Suggestion**: Consider a `#[nonreentrant]` attribute or a dedicated guard
mechanism in the generated outer trait.

### 5. Event Emission for Actuarial Analysis

Insurance companies need historical event data for actuarial modeling:
- Every oracle observation (metric, value, timestamp)
- Every payout trigger (which metric, threshold, actual value)
- Every settlement (beneficiary, amount, trigger details)

The lack of automatic event emission in the generated outer trait means
every provider must manually emit events. This is error-prone -- a provider
developer might forget to emit the observation event, breaking the
actuarial data pipeline.

The OZ comparison document acknowledges this gap. For insurance specifically,
this is not a nice-to-have -- it is a regulatory requirement.

## Practical Architecture

```rust
#[contracttrait]
pub trait CropInsurance: Ownable + Pausable {
    fn oracle(env: &Env) -> Address;
    fn coverage_amount(env: &Env) -> i128;
    fn trigger_threshold_mm(env: &Env) -> i128;  // rainfall in mm

    #[auth(Self::owner)]
    fn set_coverage(env: &Env, amount: i128, threshold_mm: i128);

    #[auth(Self::oracle)]
    fn submit_rainfall(env: &Env, station_id: Symbol, mm: i128, timestamp: u64);

    fn check_and_settle(env: &Env) -> bool;

    fn policy_status(env: &Env) -> Symbol;  // active, triggered, settled, expired
}

impl_crop_insurance!(MaizeCropContract, WeatherProvider);
```

The sealed macro ensures oracle data submission and admin configuration
cannot be tampered with. The permissionless `check_and_settle` allows
automated settlement bots. The Provider handles staleness checks,
multi-oracle consensus, and payout logic.

## Summary

soroban-sdk-tools provides a solid foundation for parametric insurance
contracts. The multi-role auth model (admin, oracle, permissionless), sealed
enforcement, and provider-based implementation swapping align well with
insurance requirements. The gaps are data freshness validation, time-locked
execution for cooling periods, reentrancy protection, and automatic event
emission for actuarial data. The most critical addition for insurance use
cases would be generated event emission in the outer trait defaults.
