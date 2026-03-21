# Review: soroban-sdk-tools `#[contracttrait]` Macro

**Reviewer:** Nkechi -- Central bank digital currency (CBDC) architect for an African nation
**Focus:** Sovereign requirements, offline capabilities, KYC/AML, financial inclusion

---

## Overall Impression

I am evaluating whether soroban-sdk-tools' composition patterns are suitable
for a central bank digital currency (CBDC) implementation on Stellar. CBDCs
have requirements that differ fundamentally from DeFi protocols: regulatory
compliance, identity verification, transaction limits, geographic restrictions,
and governmental oversight.

The question is not "is this framework good?" but "can this framework express
the regulatory constraints of a sovereign currency?" My nation has 200 million
people, many of whom are unbanked and live in areas with intermittent
connectivity. A CBDC for us must serve a farmer in a remote village the same
way it serves a banker in the capital. The framework must support this range.

After thorough review, I find the architecture promising -- the provider
pattern and sealed auth are conceptually aligned with CBDC requirements. But
the current feature set is insufficient for sovereign digital currency systems.
The gaps are specific and addressable, and I outline them in detail below.

---

## Strengths

### 1. Provider Pattern Enables Sovereign Authority Models

A CBDC requires a hierarchical authority structure:
- **Central bank** (supreme authority): Can pause all operations, mint/burn
  currency, set interest rates, freeze accounts
- **Commercial banks** (delegated authority): Can onboard customers, process
  transactions up to daily limits, report suspicious activity
- **Regulatory body** (oversight): Can freeze specific accounts, request
  transaction reports, mandate compliance updates
- **Individual users** (limited authority): Can transfer within daily limits,
  view balance, delegate to authorized representatives

The provider pattern maps to this hierarchy. Different institutions can be
modeled as different providers, and the same contract interface serves all
tiers. The ability to swap providers means I can start with a centralized
authority model during the pilot phase and transition to a more distributed
model as the system matures, without changing the contract interface.

### 2. Sealed Auth Prevents Unauthorized Policy Changes

In a CBDC, unauthorized monetary policy changes are not just security bugs --
they are existential threats to the national currency. If a junior developer
accidentally removes an auth check on the `set_interest_rate` function,
the consequences could include runaway inflation.

The sealed pattern ensures that only the designated authority can execute
monetary policy operations, and this cannot be overridden by a software
update that accidentally removes the auth check. This structural guarantee
is exactly what sovereign systems need.

### 3. Composable Error Handling Supports Regulatory Reporting

The `#[scerr]` macro with auto-chained error codes is valuable for regulatory
reporting. When a transaction fails, regulators need to know *why*:
- Error 1001: Account frozen (AML flag triggered)
- Error 1002: Daily limit exceeded (consumer protection rule)
- Error 1003: Unauthorized issuer (bank license revoked)
- Error 1004: KYC verification insufficient (identity not confirmed)

Auto-chained codes prevent collision between composed traits, which is
important when multiple regulatory modules are composed onto a single token
contract.

### 4. AuthClient Supports Compliance Testing

Regulatory compliance testing requires testing specific auth scenarios:
- Can a frozen account transfer funds? (must fail)
- Can an unlicensed bank issue currency? (must fail)
- Can a user exceed their daily limit? (must fail)
- Can the central bank freeze an account? (must succeed)
- Can a commercial bank unfreeze an account? (must fail -- only central bank)

The AuthClient pattern provides precise auth testing without `mock_all_auths()`,
which is essential for compliance audits. Regulators want to see specific test
cases that prove the auth model works, not blanket mock tests that prove nothing.

### 5. Offline Reconciliation Compatibility

Our CBDC must support offline transactions for rural areas with intermittent
connectivity. The framework's design influences offline architecture favorably:

- The provider pattern allows an `OfflineReconciliationProvider` that processes
  batched offline transactions when connectivity is restored
- The sealed auth pattern prevents offline-processed transactions from
  bypassing auth checks during reconciliation
- The supertrait composition allows offline-aware traits to extend standard
  traits with reconciliation methods

This is a point in favor of the framework's architecture.

---

## Concerns

### 1. No Compound Auth Expressions

The current `#[auth(Self::owner)]` pattern supports a single authority per
method. A CBDC needs multi-authority patterns:

```rust
#[contracttrait]
pub trait CBDCTransfer {
    // Any of these three authorities can freeze an account
    #[auth(any_of(Self::central_bank, Self::regulator, Self::commercial_bank))]
    fn freeze_account(env: &Env, account: Address);

    // Only central bank can unfreeze (asymmetric authority)
    #[auth(Self::central_bank)]
    fn unfreeze_account(env: &Env, account: Address);

    // User auth + not frozen + within daily limit
    #[auth(from)]
    fn transfer(env: &Env, from: Address, to: Address, amount: i128);
}
```

The `#[auth(any_of(...))]` pattern does not currently exist. The framework
supports `#[auth(Self::method)]` and `#[auth(param)]`, but not compound
auth expressions.

**Critical requirement**: The framework must support:
- `any_of(A, B, C)` -- any one authority is sufficient
- `all_of(A, B)` -- both must authorize (multi-sig regulatory approval)
- `threshold(2, [A, B, C])` -- at least 2 of 3 must authorize

Without compound auth, the framework cannot express CBDC governance
structures without falling back to manual auth code in the provider,
which defeats the purpose of structural auth enforcement. This is the
single biggest gap for CBDC adoption.

### 2. No Structural Transaction Limits or Guards

CBDCs enforce transaction limits: daily limits per user, per-transaction
maximums, geographic restrictions. These are not auth checks but
precondition checks that must execute BEFORE the business logic:

```rust
#[contracttrait]
pub trait CBDCCompliance {
    #[auth(from)]
    #[require(Self::within_daily_limit(from, amount))]
    #[require(Self::not_sanctioned(to))]
    #[require(Self::kyc_verified(from))]
    fn transfer(env: &Env, from: Address, to: Address, amount: i128);
}
```

The `#[require]` pattern would call a provider method and panic if it returns
false. This would structurally enforce compliance checks the same way `#[auth]`
structurally enforces authorization checks.

**Assessment**: The current framework cannot express these constraints
structurally. They must be implemented in the provider, which means every
provider must independently implement compliance checks. This is error-prone
for a sovereign currency where compliance failures have legal consequences.

### 3. No Mandatory Event Emission

CBDCs require that every state change is auditable. The central bank must be
able to reconstruct the complete history of any account. This means:

1. Every `transfer`, `freeze`, `unfreeze`, `mint`, `burn` must emit events
2. Events must include the authorizing entity (which bank, which officer)
3. Events must include timestamps and regulatory classification
4. Events must be tamper-evident and linkable to the authorizing transaction

The current framework delegates event emission to providers. For a CBDC,
events must be structurally enforced -- not optional, not forgettable. The
outer trait should guarantee event emission for every method that modifies
state.

If a provider author forgets to emit an event for a `freeze_account` call,
the central bank loses the audit trail for that action. This is a regulatory
violation that the framework should prevent by construction.

### 4. No KYC/AML Integration Points

Know-Your-Customer (KYC) and Anti-Money-Laundering (AML) checks must be
enforced at the contract level for a CBDC. Every address that interacts
with the CBDC must have a verified identity. Different verification levels
enable different capabilities (tiered KYC):

- Tier 0 (unverified): Can receive, cannot send
- Tier 1 (basic): Can send up to $500/day
- Tier 2 (full): Can send up to $10,000/day
- Tier 3 (institutional): Unlimited

The current auth model checks *who* is calling but not *what level of
verification they have*. This is a fundamental gap for financial compliance.
The `#[require]` pattern suggested above would address this, but the framework
currently has no concept of conditional authorization based on account properties.

### 5. No Multi-Tier Provider Architecture

A CBDC would need a provider hierarchy:

```
CentralBankProvider
  -> CommercialBankProvider (delegated subset of operations)
    -> RetailProvider (further restricted)
```

The current `type Provider = X` pattern selects a single provider per trait
per contract. A CBDC needs different providers for different operational
tiers, potentially within the same contract.

A single provider with internal routing can simulate this, but the routing
logic is not structurally enforced. A commercial bank provider could
accidentally expose central bank operations.

### 6. Scalability for National-Scale Operations

A CBDC for 200 million people will process millions of transactions daily.
The storage patterns in the example use `env.storage().instance()` for owner
and pause state. Instance storage has a single TTL and is loaded for every
contract invocation.

For a CBDC with millions of accounts:
- Instance storage should hold only global configuration (pause state,
  interest rate, daily limit thresholds)
- Account-specific data (balances, freeze status, KYC level) must use
  persistent or temporary storage with per-key TTL management
- The framework should provide guidance on storage architecture for scale

The documentation mentions `#[contractstorage]` but does not show how it
integrates with `#[contracttrait]`. For CBDCs, storage architecture is as
important as auth architecture.

### 7. No Account Freezing in the Auth Layer

Account freezing is a regulatory requirement in virtually every jurisdiction.
When suspicious activity is detected, the central bank must be able to freeze
an account immediately. This requires a *negative* auth pattern: "this address
is explicitly NOT authorized."

The `#[auth]` annotation only supports positive authorization ("this address
IS authorized"). A `#[deny]` annotation for structural enforcement of account
freezing would align with the framework's philosophy.

---

## Suggestions

### 1. Add Compound Auth Expressions

Support `any_of`, `all_of`, and `threshold` auth modifiers:

```rust
#[auth(any_of(Self::central_bank, Self::regulator))]
fn freeze_account(env: &Env, account: Address);

#[auth(all_of(Self::minister, Self::governor))]
fn change_monetary_policy(env: &Env, rate: i128);
```

This is the highest priority for CBDC readiness.

### 2. Add Structural Precondition Checks

Support a `#[require]` annotation that generates precondition checks before
the business logic:

```rust
#[require(Self::kyc_verified(from))]
#[require(Self::within_daily_limit(from, amount))]
fn transfer(env: &Env, from: Address, to: Address, amount: i128);
```

### 3. Add Mandatory Event Emission

Either auto-generate events for all state-modifying methods, or provide an
`#[event]` annotation that generates event emission in the outer trait.
Consider making event emission the default, with an explicit `#[no_event]`
annotation for methods that should not emit events.

### 4. Create a CBDC Example

Build a comprehensive example showing:
- Hierarchical authority (central bank, commercial banks, users)
- Account freezing and unfreezing
- Transaction limits with tiered KYC
- Audit trail via events
- Offline reconciliation provider

### 5. Address Storage Architecture

Document storage best practices for high-scale deployments:
- When to use instance vs. persistent vs. temporary storage
- How to manage TTL for different data categories
- How to partition data for performance
- Integration with `#[contractstorage]`

### 6. Support Deny Annotations

Add `#[deny(Self::is_frozen)]` for structural enforcement of negative
authorization (account freezing, sanctions lists).

---

## Unique Perspective: Sovereignty and Structural Guarantees

The concept of "structural enforcement" resonates deeply with CBDC
architecture. A central bank cannot rely on "convention" for monetary policy
enforcement. The currency must have *architectural* guarantees that certain
operations require sovereign authority.

The sealed pattern (`impl_cbdc_token!`) provides exactly this: an
architecturally enforced guarantee that monetary policy operations require
central bank authorization. No developer, no matter how well-intentioned, can
accidentally remove this requirement.

However, sovereignty also means the central bank must retain the ability to
intervene in exceptional circumstances. This creates a tension with the sealed
pattern: the central bank may need to override the normal auth model in an
emergency (e.g., system-wide freeze, emergency currency recall). The framework
should support an "emergency" or "sovereign override" pattern for these cases.

The provider pattern elegantly handles this: a `SovereignOverrideProvider` can
implement emergency operations with heightened auth requirements, while the
standard provider handles day-to-day operations.

Financial inclusion is the other side of the sovereignty coin. A CBDC that
serves only urban, connected, tech-savvy users is not sovereign -- it is
exclusive. The framework's offline compatibility and the provider pattern's
flexibility for different operational tiers are steps in the right direction.
But the documentation should explicitly address financial inclusion as a
design requirement, not just a nice-to-have.

---

## Would I Use This?

For a CBDC proof-of-concept or pilot, yes. The provider pattern and sealed
auth align well with sovereign authority requirements. The composability model
is flexible enough to evolve as the CBDC system matures.

For a production CBDC, not yet. Missing features are significant:
1. No compound auth expressions (critical gap)
2. No structural precondition checks for compliance
3. No mandatory event emission for audit trails
4. No KYC/AML integration points
5. No multi-tier provider architecture
6. No account freezing in the auth layer
7. No documentation for sovereign digital currency patterns

The foundation is right. The architectural philosophy (structural enforcement
over convention) is exactly what a CBDC needs. But the implementation needs to
grow to meet the specific requirements of sovereign digital currency systems.

**Verdict:** Architecturally aligned with CBDC requirements but practically
incomplete. The provider pattern and sealed auth are the right abstractions.
The tool needs compound auth, structural preconditions, mandatory events, and
compliance features to be viable for sovereign digital currency. I would watch
this project closely, provide requirements input, and plan for adoption once
the regulatory feature set matures. Rating: 5/10 for CBDC readiness -- right
architecture, insufficient regulatory expressiveness.
