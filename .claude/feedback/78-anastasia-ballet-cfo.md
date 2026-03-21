# Review: Anastasia -- Ballet Company CFO

**Reviewer:** Anastasia, Russian ballet company CFO managing international royalties
**Focus:** Cross-border payments, royalty accounting, multi-currency, tax compliance
**Date:** 2026-03-21
**Materials Reviewed:**
- `docs/oz-comparison.md`
- `docs/blog-post-composable-contracts.md`
- `examples/trait-test/src/lib.rs`
- `soroban-sdk-tools-macro/src/contract.rs`

---

## Context: The Business of Ballet

I am the Chief Financial Officer of a ballet company based in Moscow with
touring operations across four continents. Every season, we perform in 15-20
cities. Every performance generates royalty obligations -- to choreographers,
to music estates, to set designers, to guest artists. Every city has different
tax withholding requirements. Every currency fluctuates.

My accounting team spends 40% of their time on royalty calculations and
cross-border payment compliance. I have been looking at blockchain-based
royalty systems for three years. Most are too simple -- they assume one
currency, one jurisdiction, one tax rate. Reality is not that clean.

I am reviewing `soroban-sdk-tools` to determine whether its composability
patterns can handle the real complexity of international performing arts
royalties.

---

## 1. The Royalty Structure: Multiple Stakeholders, Multiple Rights

A single ballet performance generates royalties for:

| Stakeholder | Right Type | Rate | Currency Preference |
|-------------|-----------|------|-------------------|
| Choreographer | Choreographic rights | 2-5% of gross | Usually USD |
| Composer estate | Music rights (ASCAP/BMI) | Statutory rate | Usually USD |
| Librettist estate | Literary rights | 1-2% of gross | Varies |
| Set designer | Design rights | Fixed per performance | Usually EUR |
| Costume designer | Design rights | Fixed per performance | Usually EUR |
| Guest principals | Performance fee + royalty | Fixed + 0.5-1% | Contract currency |
| Resident company | Employment + profit share | Salary + percentage | Local currency |

### Mapping to the Trait System

```rust
#[contracttrait]
pub trait RoyaltyDistribution {
    fn distributor(env: &Env) -> Address;  // the company (my role)

    #[auth(Self::distributor)]
    fn register_stakeholder(
        env: &Env,
        stakeholder: Address,
        right_type: Symbol,
        rate_bps: u32,       // basis points
        currency: Symbol,    // preferred currency
    );

    #[auth(Self::distributor)]
    fn record_performance(
        env: &Env,
        venue: Symbol,
        date: u64,
        gross_revenue: i128,
        currency: Symbol,
    );

    #[auth(Self::distributor)]
    fn calculate_royalties(env: &Env, performance_id: u64);

    #[auth(Self::distributor)]
    fn distribute_royalties(env: &Env, performance_id: u64);

    fn pending_royalty(env: &Env, stakeholder: Address) -> i128;
}
```

### The Two-Trait Split

The `RoyaltyDistributionInternal` trait handles pure calculation:

```rust
impl RoyaltyDistributionInternal for PerformanceRoyaltyProvider {
    fn calculate_royalties(env: &Env, performance_id: u64) {
        let performance = PerformanceStorage::get(env, performance_id);
        let stakeholders = StakeholderStorage::get_all(env);

        for stakeholder in &stakeholders {
            let royalty = performance.gross_revenue
                * stakeholder.rate_bps as i128
                / 10000;
            RoyaltyStorage::add_pending(env, &stakeholder.address, royalty);
        }
    }
}
```

The outer trait ensures only the distributor (the company) can trigger
calculations and distributions. This is correct -- we do not want stakeholders
unilaterally claiming royalties before they have been verified and approved.

**Assessment: The basic royalty distribution pattern works well with
the two-trait split.**

---

## 2. Multi-Currency Challenges

### The Problem

My company receives revenue in:
- Russian Rubles (domestic performances)
- US Dollars (North American tour)
- Euros (European tour)
- British Pounds (London season)
- Japanese Yen (Tokyo engagement)
- Chinese Yuan (Beijing/Shanghai)

Royalties must be calculated in the performance currency and converted
to the stakeholder's preferred currency. Exchange rates fluctuate daily.

### The Provider Approach

Different currency handling strategies could be providers:

```rust
// Fixed rate at time of performance (simple, common in contracts)
pub struct FixedRateProvider;

// Market rate at time of distribution (more accurate, more complex)
pub struct MarketRateProvider;

// Hedged rate (we pre-purchase currency forwards)
pub struct HedgedRateProvider;
```

The provider swap means the company can change its currency strategy
without redeploying the contract. If we switch from fixed-rate to hedged-rate
accounting mid-season, it is a provider change, not a contract change.

### Oracle Integration

For `MarketRateProvider`, we need exchange rate oracles. The provider would
call an oracle contract to get current rates:

```rust
impl RoyaltyDistributionInternal for MarketRateProvider {
    fn calculate_royalties(env: &Env, performance_id: u64) {
        let performance = PerformanceStorage::get(env, performance_id);

        for stakeholder in &StakeholderStorage::get_all(env) {
            let royalty_in_perf_currency = performance.gross_revenue
                * stakeholder.rate_bps as i128
                / 10000;

            // Convert to stakeholder's preferred currency
            let rate = OracleClient::get_rate(
                env,
                &performance.currency,
                &stakeholder.preferred_currency,
            );
            let converted = royalty_in_perf_currency * rate / RATE_PRECISION;

            RoyaltyStorage::add_pending(env, &stakeholder.address, converted);
        }
    }
}
```

The trait does not need to know about currency conversion. The provider
handles it. This is a clean separation.

**Assessment: The provider pattern handles multi-currency well by
encapsulating conversion logic.**

---

## 3. Tax Withholding: Per-Jurisdiction Complexity

### The Reality

When my company performs in New York and pays royalties to a choreographer
in Paris:

1. US federal withholding: 30% on royalties to foreign persons (unless
   reduced by treaty)
2. US-France tax treaty: Reduces to 0% for copyright royalties
3. New York state withholding: 8.82% (may or may not apply)
4. French income tax: The choreographer reports and pays in France
5. VAT/GST: May apply depending on the service classification

My accounting team handles this manually. A smart contract could automate
the withholding portion.

### Withholding Trait

```rust
#[contracttrait]
pub trait TaxWithholding: RoyaltyDistribution {
    fn tax_authority(env: &Env) -> Address;

    #[auth(Self::distributor)]
    fn set_withholding_rate(
        env: &Env,
        stakeholder: Address,
        jurisdiction: Symbol,
        rate_bps: u32,
    );

    #[auth(Self::distributor)]
    fn apply_withholding(env: &Env, performance_id: u64);

    fn withholding_amount(env: &Env, stakeholder: Address) -> i128;

    #[auth(Self::tax_authority)]
    fn confirm_remittance(env: &Env, period: Symbol, total_remitted: i128);
}
```

The supertrait `TaxWithholding: RoyaltyDistribution` is correct --
withholding depends on royalty calculations. You cannot withhold tax
on royalties you have not calculated.

The `tax_authority` auth source is important. The tax authority
(IRS, HMRC, French tax administration) should be able to confirm
that withheld amounts have been remitted. This is an accountability
mechanism.

### Provider for Different Jurisdictions

```rust
pub struct USWithholdingProvider;   // IRS rules, W-8BEN handling
pub struct EUWithholdingProvider;   // EU directive, VAT considerations
pub struct UKWithholdingProvider;   // HMRC rules, post-Brexit
pub struct RussiaWithholdingProvider;  // Russian tax code
```

Each provider implements the same `TaxWithholdingInternal` trait but with
jurisdiction-specific logic. The company selects the provider based on
the performance location.

**Challenge:** A single tour might span multiple jurisdictions. The contract
would need to handle per-performance provider selection, not per-contract.
The current `type Provider` is per-contract.

**Recommendation:** Support per-call provider selection or a provider that
internally delegates based on jurisdiction:

```rust
pub struct MultiJurisdictionProvider;
impl TaxWithholdingInternal for MultiJurisdictionProvider {
    fn apply_withholding(env: &Env, performance_id: u64) {
        let jurisdiction = PerformanceStorage::get_jurisdiction(env, performance_id);
        match jurisdiction.to_string().as_str() {
            "US" => us_withholding(env, performance_id),
            "EU" => eu_withholding(env, performance_id),
            "UK" => uk_withholding(env, performance_id),
            _ => default_withholding(env, performance_id),
        }
    }
}
```

This works but moves the strategy selection into the provider, which is
less elegant than type-level dispatch. The provider pattern is designed
for compile-time strategy selection, not runtime.

---

## 4. Cross-Border Payments on Stellar

### Why Stellar Matters for Royalties

Stellar's anchored asset model is ideal for international royalties:
- USDC (US Dollar stablecoin) for dollar-denominated payments
- EURC or similar for euro payments
- Stellar's path payment for cross-currency settlement
- Low fees ($0.00001 per transaction) vs SWIFT ($25-45 per transfer)

Currently, my company pays $35-45 per international wire transfer. With
60+ stakeholders and 15+ performances per season, that is $35,000+ in
transfer fees alone. On Stellar, it would be pennies.

### The `FungibleToken` Integration

The royalty distribution contract would call a token contract to execute
payments:

```rust
impl RoyaltyDistributionInternal for StellarPaymentProvider {
    fn distribute_royalties(env: &Env, performance_id: u64) {
        let stakeholders = StakeholderStorage::get_all(env);

        for stakeholder in &stakeholders {
            let amount = RoyaltyStorage::get_pending(env, &stakeholder.address);
            if amount > 0 {
                let token = TokenClient::new(env, &stakeholder.preferred_token);
                token.transfer(
                    &env.current_contract_address(),
                    &stakeholder.address,
                    &amount,
                );
                RoyaltyStorage::clear_pending(env, &stakeholder.address);
            }
        }
    }
}
```

The provider handles the payment execution. The trait handles the authorization.
Different providers could support different payment mechanisms:
- `DirectPaymentProvider`: Direct token transfer
- `EscrowPaymentProvider`: Holds funds until stakeholder confirms receipt
- `StreamingPaymentProvider`: Continuous payment stream (for ongoing royalties)

---

## 5. Royalty Accounting: The Audit Trail

### Requirements

My external auditors (currently Deloitte) require:
1. Complete record of all royalty calculations
2. Supporting documentation for each calculation
3. Proof of payment for each distribution
4. Reconciliation between gross revenue and total distributions
5. Tax withholding documentation

### Event Emission (Critical Gap Again)

Without auto-generated events, the audit trail depends on the provider:

```
PerformanceRecorded { id: 42, venue: "Lincoln Center", gross: 450000, currency: "USD" }
RoyaltyCalculated { performance: 42, stakeholder: "G7x...", amount: 13500, rate_bps: 300 }
WithholdingApplied { stakeholder: "G7x...", amount: 4050, rate_bps: 3000, jurisdiction: "US" }
RoyaltyDistributed { stakeholder: "G7x...", net_amount: 9450, token: "USDC", tx: "abc123" }
```

If ANY of these events are missing, the audit fails. The provider developer
might forget to emit one. The macro SHOULD generate them.

**Assessment: For financial reporting, auto-generated events are not optional.
They are a GAAP/IFRS requirement.**

### Reconciliation Method

The trait should include reconciliation methods:

```rust
fn total_gross_revenue(env: &Env, period: Symbol) -> i128;
fn total_royalties_calculated(env: &Env, period: Symbol) -> i128;
fn total_royalties_distributed(env: &Env, period: Symbol) -> i128;
fn total_withholding(env: &Env, period: Symbol) -> i128;
fn reconciliation_balance(env: &Env, period: Symbol) -> i128;
// Should be zero if everything balances
```

These are read-only methods with no auth requirement -- any auditor should
be able to verify the numbers.

---

## 6. Guest Artist Contracts: Complex Auth Scenarios

### The Scenario

When we engage a guest principal dancer:
1. The company CFO (me) authorizes the engagement
2. The artistic director approves the casting
3. The guest artist accepts the terms
4. The guest artist's agent countersigns

This is a four-party authorization for a single contract action. The
current `#[auth]` system supports only one auth source per method.

### What I Need

```rust
#[contracttrait]
pub trait GuestEngagement: RoyaltyDistribution {
    fn cfo(env: &Env) -> Address;
    fn artistic_director(env: &Env) -> Address;

    // Both CFO and artistic director must approve
    #[auth(Self::cfo)]
    fn approve_financial_terms(env: &Env, guest: Address, fee: i128);

    #[auth(Self::artistic_director)]
    fn approve_casting(env: &Env, guest: Address, role: Symbol);

    // Guest must accept
    #[auth(guest)]
    fn accept_engagement(env: &Env, guest: Address);
}
```

This works with the current system because each step has a single auth
source. The multi-step process replaces multi-party auth.

But some actions genuinely require simultaneous multi-party auth:

```rust
// Contract termination requires BOTH CFO and artistic director
fn terminate_engagement(env: &Env, guest: Address);
```

**Recommendation:** Same as other reviewers -- multi-party auth is needed.

---

## 7. Season Planning: Temporal Constraints

### The Problem

Royalty contracts have temporal structure:
- A choreographer's rights expire after a certain period
- A music license is valid for a specific season
- Tax withholding rates change on January 1st
- Currency conversion rates are fixed at contract signing

### Temporal Provider Pattern

```rust
pub struct SeasonalRoyaltyProvider;
impl RoyaltyDistributionInternal for SeasonalRoyaltyProvider {
    fn calculate_royalties(env: &Env, performance_id: u64) {
        let performance = PerformanceStorage::get(env, performance_id);
        let season = get_season(env, performance.date);

        for stakeholder in &StakeholderStorage::get_for_season(env, season) {
            // Check if rights are still valid
            if stakeholder.rights_expiry < performance.date {
                continue;  // rights expired, no royalty
            }

            let royalty = performance.gross_revenue
                * stakeholder.rate_bps as i128
                / 10000;
            RoyaltyStorage::add_pending(env, &stakeholder.address, royalty);
        }
    }
}
```

The temporal logic is in the provider, which is the right place. But the
trait definition does not communicate that royalties have temporal validity.

**Suggestion:** Consider annotations for temporal constraints:

```rust
#[contracttrait]
pub trait RoyaltyDistribution {
    fn season_start(env: &Env) -> u64;
    fn season_end(env: &Env) -> u64;

    #[auth(Self::distributor)]
    #[valid_during(Self::season_start, Self::season_end)]
    fn record_performance(env: &Env, venue: Symbol, date: u64, gross: i128);
}
```

---

## 8. The Sealed Macro for Financial Contracts

For financial contracts, the sealed macro is critical:

```rust
impl_royalty_distribution!(BalletCompany, SeasonalRoyaltyProvider);
```

When auditors ask: "Can anyone distribute royalties without CFO authorization?"
The answer must be "No." The sealed macro guarantees this.

When tax authorities ask: "Can withholding rates be modified without proper
authorization?" The answer must be "No." The sealed macro guarantees this.

For a ballet company handling $20M+ in annual revenue, these guarantees
are not theoretical. They are required for continued operation.

**Assessment: The sealed macro is essential for financial contract compliance.**

---

## 9. Comparison with Current Systems

### What We Use Now

Our current royalty management:
1. Excel spreadsheets for calculations (error-prone)
2. SAP for accounting (expensive, $200K/year license)
3. SWIFT for payments ($35K/year in transfer fees)
4. PwC for audit ($150K/year)
5. Manual tax withholding calculations ($50K/year in accounting staff time)

Total: ~$435K/year for royalty management.

### What Soroban + soroban-sdk-tools Could Provide

1. On-chain calculations (transparent, auditable, no Excel errors)
2. Automated distribution (instant, $0.01/year in fees vs $35K)
3. Auto-generated audit trail (events replace manual documentation)
4. Programmatic withholding (rules encoded in provider)
5. Real-time reconciliation (on-chain balance checks)

Potential savings: $300K+/year after implementation costs.

### What Is Missing

| Current System Feature | soroban-sdk-tools Status |
|----------------------|------------------------|
| Multi-currency | Provider handles it (adequate) |
| Tax withholding | Provider handles it (adequate) |
| Audit trail | Missing (no auto events) |
| Approval workflows | Partially supported (single auth only) |
| Period-end closing | Not supported (no temporal primitives) |
| Regulatory reporting | Not supported (no report generation) |
| Multi-jurisdiction | Provider can handle (runtime dispatch) |

---

## 10. Testing Financial Contracts

The AuthClient is valuable for testing financial authorization:

```rust
#[test]
fn test_only_cfo_can_distribute() {
    let auth = RoyaltyDistributionAuthClient::new(&env, &contract_id);

    // CFO can distribute
    auth.distribute_royalties(&performance_id)
        .authorize(&cfo)
        .invoke();

    // Random person cannot
    let result = auth.distribute_royalties(&performance_id)
        .authorize(&random)
        .try_invoke();
    assert!(result.is_err());
}

#[test]
fn test_stakeholder_cannot_self_pay() {
    let auth = RoyaltyDistributionAuthClient::new(&env, &contract_id);

    // Stakeholder cannot trigger their own distribution
    let result = auth.distribute_royalties(&performance_id)
        .authorize(&stakeholder)
        .try_invoke();
    assert!(result.is_err());
}
```

These tests are exactly what an auditor wants to see: proof that only
authorized personnel can trigger financial operations.

**Assessment: AuthClient testing is superior to mock_all_auths for
financial contract verification.**

---

## 11. Recommendations for Financial Use Cases

| Priority | Recommendation | Financial Rationale |
|----------|---------------|-------------------|
| Critical | Auto-generated events | GAAP/IFRS audit trail requirements |
| Critical | Multi-party auth | Dual-authorization for financial operations |
| High | Temporal validity annotations | Season-based contracts, expiring rights |
| High | Reconciliation method generation | Balance verification for auditors |
| Medium | Batch operations | Process multiple performances at once |
| Medium | Oracle integration patterns | Exchange rate feeds for multi-currency |
| Medium | Period-end closing semantics | Accounting period management |
| Low | Report generation helpers | Regulatory reporting support |
| Low | Currency conversion primitives | Standard exchange rate handling |

---

## 12. Closing Remarks

I manage the finances of a ballet company. Every season, I juggle royalties
in six currencies, withholding taxes in twelve jurisdictions, and audit
requirements from four countries. My current systems cost $435K per year
and still produce errors.

The `soroban-sdk-tools` architecture -- particularly the provider pattern
and the sealed macro -- offers a foundation for automating most of this
complexity. The two-trait split between business logic and authorization
maps perfectly to the separation between financial calculations (which
accountants do) and financial controls (which auditors verify).

The provider pattern is especially valuable: I can swap from fixed-rate
to hedged currency conversion, or from US to EU withholding rules, without
redeploying the contract. This flexibility is essential for a touring
company that operates across jurisdictions.

What is missing -- event emission, multi-party auth, temporal constraints --
represents the difference between a prototype and a production financial
system. These gaps must be filled before I would deploy this for real money.

But the architecture is right. The design philosophy is sound. And if the
missing pieces are added, this could reduce my royalty management costs
by 70% while improving accuracy and auditability.

That is worth paying attention to.

---

**Overall Assessment:** Strong architectural fit for financial contract
management. The provider pattern handles multi-currency and multi-jurisdiction
elegantly. Event emission and multi-party auth are critical gaps for
financial compliance.

**Verdict:** The choreography is excellent. The performance needs a few
more rehearsals before opening night.
