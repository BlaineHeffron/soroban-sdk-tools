# Review: soroban-sdk-tools -- Parametric Insurance on Blockchain Assessment

**Reviewer:** Alessandro Ferretti, FSA, CERA
**Background:** Insurance actuary with 20 years experience at Generali and Swiss Re; now building parametric insurance products on blockchain; Fellow of the Society of Actuaries; specialist in catastrophe risk modeling
**Focus:** Risk modeling, oracle integration, payout automation, regulatory compliance

---

## Executive Summary

Parametric insurance is the blockchain use case I believe in most deeply. Unlike traditional insurance, parametric products pay out based on measurable triggers (earthquake magnitude, rainfall levels, flight delays) rather than assessed losses. This eliminates claims adjustment, reduces moral hazard, and enables instant payouts. Smart contracts are the natural execution layer.

I evaluate soroban-sdk-tools through the lens of building a production parametric insurance platform on Soroban. The composability patterns are relevant -- insurance contracts compose multiple concerns (risk pools, premium collection, oracle triggers, payout distribution, regulatory reserves). The auth model matters because insurance is heavily regulated. The Provider pattern matters because different products require different risk engines.

---

## 1. Mapping Insurance Concerns to Traits

### The trait decomposition for parametric insurance

A parametric insurance contract needs:

```
Ownable (contract governance)
  |-- Pausable (regulatory halt)
  |-- PolicyManager (create/cancel policies)
  |-- PremiumCollector (collect and escrow premiums)
  |-- OracleConsumer (receive trigger data)
  |-- PayoutEngine (calculate and distribute payouts)
  |-- ReserveManager (maintain statutory reserves)
  |-- ReinsuranceConnector (cede risk to reinsurers)
```

### How soroban-sdk-tools handles this

The supertrait composition pattern maps naturally:

```rust
#[contracttrait]
pub trait PolicyManager: Ownable {
    fn create_policy(env: &Env, policyholder: Address, params: PolicyParams) -> PolicyId;

    #[auth(Self::owner)]
    fn cancel_policy(env: &Env, policy_id: PolicyId);
}

#[contracttrait]
pub trait OracleConsumer: Ownable {
    fn oracle_address(env: &Env) -> Address;

    #[auth(Self::oracle_address)]
    fn submit_trigger(env: &Env, trigger_data: TriggerData);
}

#[contracttrait]
pub trait PayoutEngine: PolicyManager + OracleConsumer {
    fn calculate_payout(env: &Env, policy_id: PolicyId, trigger: TriggerData) -> i128;
    fn execute_payout(env: &Env, policy_id: PolicyId);
}
```

The `#[auth(Self::oracle_address)]` pattern is particularly relevant. In parametric insurance, the oracle (the data feed that reports earthquake magnitudes, rainfall levels, etc.) is a critical trust point. The structural auth enforcement ensures that only the designated oracle can submit trigger data. This is exactly the kind of guarantee that insurance regulators want to see.

### The limitation: multi-oracle consensus

Real parametric insurance products typically require consensus among multiple oracles (e.g., "payout triggers when 2 of 3 weather stations report rainfall below 50mm"). The `#[auth]` attribute supports a single address. Multi-oracle consensus would need to be implemented inside the provider:

```rust
impl OracleConsumerInternal for MultiOracleProvider {
    fn oracle_address(env: &Env) -> Address {
        // Return the multi-sig oracle address
        OracleStorage::get_multisig_oracle(env).unwrap()
    }

    fn submit_trigger(env: &Env, trigger_data: TriggerData) {
        // Multi-oracle consensus logic here
        let submission_count = OracleStorage::record_submission(env, &trigger_data);
        if submission_count >= REQUIRED_CONSENSUS {
            // Trigger accepted
        }
    }
}
```

This works but puts consensus logic in the provider rather than in the auth layer. The `#[auth]` on `submit_trigger` only verifies that the multi-sig address authorized the call -- the individual oracle submissions and consensus counting happen inside business logic.

### Recommendation

Consider supporting `#[auth(any_of(Self::oracle_1, Self::oracle_2, Self::oracle_3))]` or `#[auth(threshold(Self::oracles, 2))]` for multi-party auth patterns. These are not exotic in insurance -- they are standard.

---

## 2. Payout Automation and Timing

### Why timing matters in insurance

When an earthquake hits and a parametric policy triggers, the policyholder needs money within hours, not days. The entire value proposition of parametric insurance is speed. Any framework overhead that delays payout execution is unacceptable.

### The Provider pattern for payout strategies

Different insurance products require different payout calculations:

```rust
pub struct LinearPayout;
impl PayoutEngineInternal for LinearPayout {
    fn calculate_payout(env: &Env, policy_id: PolicyId, trigger: TriggerData) -> i128 {
        // Linear: payout = coverage * (trigger_value - threshold) / range
        let policy = PolicyStorage::get(env, &policy_id);
        let ratio = (trigger.value - policy.threshold) as i128;
        policy.coverage * ratio / policy.range as i128
    }
}

pub struct SteppedPayout;
impl PayoutEngineInternal for SteppedPayout {
    fn calculate_payout(env: &Env, policy_id: PolicyId, trigger: TriggerData) -> i128 {
        // Stepped: discrete payout tiers
        let policy = PolicyStorage::get(env, &policy_id);
        match trigger.value {
            v if v >= policy.tier_3_threshold => policy.tier_3_payout,
            v if v >= policy.tier_2_threshold => policy.tier_2_payout,
            v if v >= policy.tier_1_threshold => policy.tier_1_payout,
            _ => 0,
        }
    }
}
```

Swapping `type Provider = LinearPayout` to `type Provider = SteppedPayout` is the one-line change the blog post promises. For an actuary designing product variations, this is genuinely valuable. I can model different payout structures in my actuarial software, implement them as separate providers, and let the product team choose the appropriate one for each market.

### The gas cost concern

The blog post claims zero overhead. For payout calculations involving floating-point-like arithmetic on Soroban (which uses i128 integers), the critical question is not overhead from the abstraction layer but whether the provider pattern encourages less efficient code patterns.

Specifically: if `calculate_payout` and `execute_payout` are separate methods in the trait, and both need to look up the policy, the policy is read from storage twice. A monolithic function would read it once.

### Recommendation

Support derived methods in providers that can share intermediate state:

```rust
impl PayoutEngineInternal for LinearPayout {
    fn execute_payout(env: &Env, policy_id: PolicyId) {
        let policy = PolicyStorage::get(env, &policy_id);
        let trigger = OracleStorage::get_latest_trigger(env);
        let amount = self.calculate_with_policy(&policy, &trigger); // reuse loaded data
        // execute transfer
    }
}
```

Or document the pattern for combining related operations to minimize storage reads.

---

## 3. Regulatory Reserve Requirements

### The regulatory landscape

Insurance regulators (EIOPA in Europe, state DOIs in the US, MAS in Singapore) require:

1. **Minimum capital reserves:** Premiums collected must be backed by sufficient reserves
2. **Segregation of funds:** Policyholder funds must be separated from operational funds
3. **Real-time solvency monitoring:** Regulators must be able to verify solvency at any time
4. **Payout limits:** No single event can drain the pool below the minimum reserve

### How soroban-sdk-tools maps to these requirements

**Minimum capital reserves:** A `ReserveManager` trait with `#[auth(Self::owner)]` for adjustments and read-only methods for regulator inspection:

```rust
#[contracttrait]
pub trait ReserveManager: Ownable {
    fn current_reserves(env: &Env) -> i128;        // public, read-only
    fn minimum_required(env: &Env) -> i128;         // public, read-only
    fn is_solvent(env: &Env) -> bool;               // public, read-only

    #[auth(Self::owner)]
    fn adjust_minimum(env: &Env, new_minimum: i128); // owner-only
}
```

The structural auth on `adjust_minimum` ensures only the contract owner (the insurance company) can change reserve requirements. The read-only methods are unauthenticated, allowing regulators, auditors, and the public to verify solvency on-chain.

**Segregation of funds:** This is a storage concern, not an auth concern. The Provider pattern does not directly address storage isolation between "policyholder escrow" and "operational funds." The `#[contractstorage]` macro mentioned in the docs could help, but it is not demonstrated.

### The critical gap: regulatory roles

Insurance regulation requires multiple roles beyond "owner":

- **Actuary:** Can adjust pricing models and reserve calculations
- **Compliance officer:** Can pause the contract, report to regulators
- **Claims adjuster:** (for hybrid products) Can override automatic payouts
- **Regulator:** Can inspect state, freeze operations, force liquidation

The current `#[auth(Self::owner)]` model supports one role. The blog post mentions AccessControl as a future direction, but it is not implemented. For insurance, this is a blocker, not a nice-to-have.

### Recommendation

Implement role-based access control as a composable trait:

```rust
#[contracttrait]
pub trait RoleBasedAccess: Ownable {
    fn has_role(env: &Env, account: Address, role: Symbol) -> bool;
    fn role_admin(env: &Env, role: Symbol) -> Address;

    #[auth(Self::role_admin)]
    fn grant_role(env: &Env, admin: Address, account: Address, role: Symbol);

    #[auth(Self::role_admin)]
    fn revoke_role(env: &Env, admin: Address, account: Address, role: Symbol);
}
```

Note: the `#[auth(Self::role_admin)]` would need to be able to pass the `role` parameter to the resolver method. Currently, `#[auth(Self::method)]` calls the method with only `env`. Supporting `#[auth(Self::role_admin(role))]` would be a significant enhancement for role-based systems.

---

## 4. Oracle Integration Patterns

### Types of oracles for parametric insurance

1. **Weather oracles:** Temperature, rainfall, wind speed (Chainlink, Band Protocol)
2. **Seismic oracles:** Earthquake magnitude, depth, location (USGS feed)
3. **Flight oracles:** Delay status, cancellation (FlightAware API)
4. **Financial oracles:** Index values, exchange rates (for index-linked products)
5. **IoT oracles:** Sensor data from connected devices (crop insurance)

### The auth model for oracles

Each oracle type has different trust requirements:

- Weather data from a government agency: single trusted source, `#[auth(Self::weather_oracle)]`
- Financial data: consensus among 3+ feeds, needs threshold auth
- IoT data: device attestation, needs cryptographic verification beyond `require_auth()`

The Provider pattern handles this well at the implementation level:

```rust
pub struct ChainlinkWeatherOracle;
impl OracleConsumerInternal for ChainlinkWeatherOracle {
    fn oracle_address(env: &Env) -> Address {
        OracleConfig::get_chainlink_address(env)
    }
    fn submit_trigger(env: &Env, trigger_data: TriggerData) {
        // Chainlink-specific data validation
        assert!(trigger_data.timestamp > env.ledger().timestamp() - 3600, "stale data");
        TriggerStorage::record(env, &trigger_data);
    }
}
```

### The timing problem

Oracle data has a temporal dimension. A trigger must be:
1. Recent enough to be relevant (freshness check)
2. Within the policy's coverage period (validity check)
3. From a registered oracle address (auth check)
4. Not a duplicate of previously submitted data (idempotency check)

The `#[auth]` attribute handles check 3 structurally. Checks 1, 2, and 4 must be in the provider. This is fine, but the documentation should provide a reference implementation for oracle consumers that includes all four checks.

### Recommendation

Create a `reference-implementations/oracle-consumer/` directory with a production-quality oracle integration example. Insurance developers will not adopt a framework that does not demonstrate oracle patterns explicitly.

---

## 5. Actuarial Modeling Integration

### The pricing pipeline

In traditional insurance:
1. Actuary builds a catastrophe model (hurricane frequency, severity, correlation)
2. Model produces a loss distribution (probability of paying X in any given year)
3. Premium = Expected Loss + Risk Margin + Expenses + Profit Loading
4. These parameters are loaded into the policy administration system

### On-chain pricing

For parametric insurance on Soroban, the pricing parameters need to be on-chain:

```rust
#[contracttrait]
pub trait PricingEngine: Ownable {
    fn premium_rate(env: &Env, risk_zone: u32, coverage_tier: u32) -> i128;
    fn loss_ratio(env: &Env) -> i128;  // actual losses / earned premiums

    #[auth(Self::owner)]
    fn update_rates(env: &Env, risk_zone: u32, coverage_tier: u32, new_rate: i128);
}
```

The Provider pattern enables swapping pricing models:

```rust
pub struct BurningCostPricing;     // historical loss-based
pub struct CatModelPricing;         // simulation-based
pub struct CommunityRatingPricing;  // flat rate for the pool
```

Each pricing provider implements the same `PricingEngineInternal` trait with different algorithms. The contract owner (the insurance company or DAO) selects the appropriate model at deployment.

### The fixed-point arithmetic concern

Insurance pricing uses decimal arithmetic extensively. Soroban's `i128` requires manual fixed-point handling. The framework does not provide or recommend a fixed-point library. For actuarial calculations involving:

- Premium = Sum(Coverage_i * Rate_i * Duration_i / 365)
- Reserve = Premium * (1 - ExpenseRatio) * ReserveFactor

Every multiplication risks overflow. Every division introduces rounding error. The order of operations matters. The framework should document or recommend a fixed-point arithmetic approach for financial calculations.

---

## 6. Claims and Dispute Resolution

### Parametric vs. indemnity disputes

Parametric insurance has fewer disputes than traditional insurance because payouts are automatic. But disputes still occur:

- "The oracle data was wrong" (sensor malfunction, data feed error)
- "The trigger threshold was misconfigured" (administrative error)
- "The payout calculation was incorrect" (bug in the pricing engine)
- "The policy was not in force at the time of the event" (coverage gap)

### On-chain dispute resolution

A dispute resolution trait might look like:

```rust
#[contracttrait]
pub trait DisputeResolution: Ownable {
    fn dispute_resolver(env: &Env) -> Address;

    #[auth(policyholder)]  // using param auth
    fn file_dispute(env: &Env, policyholder: Address, policy_id: PolicyId, reason: Bytes);

    #[auth(Self::dispute_resolver)]
    fn resolve_dispute(env: &Env, dispute_id: DisputeId, resolution: Resolution);
}
```

The `#[auth(policyholder)]` param-based auth is used here because any policyholder can file a dispute for their own policy. The `#[auth(Self::dispute_resolver)]` uses provider-method auth because only the designated resolver can adjudicate.

This demonstrates the versatility of the two auth modes. The framework handles both "self-service" (param auth) and "delegated authority" (provider auth) patterns cleanly.

---

## 7. Reinsurance and Multi-Party Risk Sharing

### The reinsurance problem

Insurance companies rarely hold all risk themselves. They cede portions to reinsurers through:

- **Quota share:** Reinsurer takes a fixed percentage of every policy
- **Excess of loss:** Reinsurer pays for losses above a threshold
- **Catastrophe bonds:** Capital market investors absorb tail risk

### Smart contract reinsurance

The Provider pattern could support different reinsurance structures:

```rust
pub struct QuotaShareReinsurance;
impl ReinsuranceConnectorInternal for QuotaShareReinsurance {
    fn cede_premium(env: &Env, premium: i128) -> i128 {
        let cession_rate = ReinsuranceConfig::get_cession_rate(env);
        let ceded = premium * cession_rate / 10000; // basis points
        // Transfer ceded premium to reinsurer
        transfer_to_reinsurer(env, ceded);
        ceded
    }
}

pub struct ExcessOfLossReinsurance;
impl ReinsuranceConnectorInternal for ExcessOfLossReinsurance {
    fn cede_premium(env: &Env, premium: i128) -> i128 {
        // Different calculation for XOL
        let layer = ReinsuranceConfig::get_layer(env);
        // ...
    }
}
```

### The multi-contract composition question

Reinsurance typically involves multiple contracts:
1. The primary insurance contract (policyholder-facing)
2. The reinsurance treaty contract (reinsurer-facing)
3. The trust account contract (holds collateral)

The `#[contracttrait]` macro handles single-contract composition. Cross-contract composition is a different pattern (contract-to-contract invocations via Soroban's `env.invoke_contract()`). The documentation does not address how providers can invoke other contracts.

### Recommendation

Document the cross-contract invocation pattern within providers. Show how a `ReinsuranceConnector` provider can call methods on a separate reinsurance treaty contract while maintaining auth guarantees.

---

## 8. Audit and Compliance Evidence

### What regulators and auditors need

1. **Immutable audit trail:** Every state change must be traceable
2. **Authorization evidence:** Proof that every action was properly authorized
3. **Solvency snapshots:** Point-in-time reserve adequacy reports
4. **Policy lifecycle documentation:** From issuance through payout or expiry

### The AuthClient as audit evidence

The `AuthClient` testing pattern produces verifiable evidence that auth checks work:

```rust
// This test PROVES that unauthorized access is rejected
let result = auth_client.update_rates(&1, &1, &new_rate)
    .authorize(&unauthorized_person)
    .try_invoke();
assert!(result.is_err());
```

This is directly usable as audit evidence. An external auditor can:
1. Read the test suite
2. Verify that negative auth tests exist for every sensitive method
3. Run the tests independently to confirm results

This is significantly better than OZ's `mock_all_auths()` approach, which provides no evidence of auth enforcement.

### The event emission gap (for insurance)

The OZ comparison notes that event emission is a "provider responsibility" in soroban-sdk-tools. For insurance compliance, this is a significant risk:

- If a provider forgets to emit an event for a payout, the audit trail is broken
- If events have inconsistent formats across providers, compliance tooling breaks
- If events are emitted but not indexed, they are useless for regulatory reporting

Insurance regulators increasingly require real-time reporting. Standardized, guaranteed event emission is a regulatory necessity, not a nice-to-have.

### Recommendation

Generate standardized events in the outer trait (the auth-wrapped layer) for:
- Every `#[auth]`-gated method invocation (pre-call: who is calling, what are they doing)
- Every state-changing method completion (post-call: what changed)
- Every auth failure (who tried, what did they try, why did it fail)

These structural events should be automatic and non-overridable (similar to sealed auth). Providers can emit additional domain-specific events, but the structural events must be guaranteed.

---

## 9. Stress Testing and Catastrophe Scenarios

### The catastrophe scenario

A magnitude 7.5 earthquake triggers 10,000 parametric policies simultaneously. The contract must:

1. Validate the oracle trigger data
2. Calculate payouts for 10,000 policies
3. Execute 10,000 token transfers
4. Update reserve balances
5. Calculate reinsurance cessions
6. Emit events for each payout

### The question for soroban-sdk-tools

Can this be done in a single transaction? Almost certainly not -- Soroban has per-transaction resource limits. The contract will need batch processing across multiple transactions.

Does the Provider pattern support batch processing? The current trait examples show single-operation methods. A batch-aware payout engine would need:

```rust
#[contracttrait]
pub trait BatchPayoutEngine: OracleConsumer {
    fn process_batch(env: &Env, policy_ids: Vec<PolicyId>, batch_size: u32) -> u32;
    fn remaining_in_batch(env: &Env) -> u32;
}
```

The auth story for batch processing is interesting: `process_batch` might be called by anyone (the trigger has already been validated), or it might require operator auth to prevent front-running. The framework should document recommended auth patterns for batch/crank-style operations.

---

## 10. Final Assessment

### For parametric insurance specifically

| Capability | Readiness | Notes |
|---|---|---|
| Basic auth (owner, oracle) | Ready | `#[auth]` pattern works well |
| Role-based access (actuary, compliance, regulator) | Not ready | Needs RBAC trait |
| Oracle integration | Partially ready | Needs reference implementation |
| Payout automation | Ready | Provider pattern fits well |
| Reserve management | Partially ready | Storage isolation unclear |
| Regulatory compliance | Not ready | Event emission not guaranteed |
| Multi-party (reinsurance) | Not ready | Cross-contract patterns undocumented |
| Batch processing | Not ready | No batch patterns documented |
| Dispute resolution | Ready | Dual auth modes work well |
| Audit evidence | Ready | AuthClient is excellent |

### Overall score: 6/10

The foundation is strong. The Provider pattern maps naturally to insurance product variation. The auth model handles the critical oracle trust point well. The AuthClient provides genuine audit value.

But insurance is a regulated industry with specific, non-negotiable requirements: RBAC, guaranteed events, reserve segregation, cross-contract composition, and batch processing. Until these are addressed (even if just documented as patterns), the framework is not production-ready for insurance use cases.

The good news is that the architecture supports all of these extensions. Nothing needs to be torn down. It needs to be built up.

---

*Reviewed by Alessandro Ferretti, FSA, CERA*
*Review date: 2026-03-21*
