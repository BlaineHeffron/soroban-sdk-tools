---
persona: Khadijah
age: 46
background: Islamic finance scholar, PhD in Islamic jurisprudence (fiqh) and fintech, Sharia board advisor for 3 Islamic banks
focus: Sharia-compliant DeFi, riba (interest) prohibition, gharar (uncertainty) avoidance, maqasid al-shariah
tone: Scholarly, bridges traditional jurisprudence with modern technology, cites both fiqh principles and code patterns
---

# Review: soroban-sdk-tools -- Sharia Compliance Assessment for DeFi

## Context

Islamic finance operates under principles derived from the Quran and Sunnah,
codified through centuries of jurisprudential development (fiqh). The core
prohibitions relevant to smart contract design are:

1. **Riba** (usury/interest): Prohibited absolutely. Income must come from
   real economic activity, not from money lending money.
2. **Gharar** (excessive uncertainty): Contracts must have clear terms. Both
   parties must understand what they are agreeing to.
3. **Maysir** (gambling): Speculative transactions that resemble gambling are
   prohibited.
4. **Haram activities**: Contracts must not facilitate prohibited activities
   (alcohol, pork, etc.).

The question: can soroban-sdk-tools' composition patterns express Sharia-
compliant DeFi contracts, and can the framework's structural guarantees help
ENFORCE compliance?

## Gharar and Structural Auth: A Natural Fit

The prohibition of gharar requires that both parties to a contract understand
the terms clearly. In smart contract terms, this means the contract's behavior
must be predictable and inspectable.

The `#[contracttrait]` pattern excels here:

1. **The trait definition IS the contract**: When you write:
   ```rust
   #[contracttrait]
   pub trait Murabaha {
       fn cost_price(env: &Env) -> i128;
       fn markup(env: &Env) -> i128;
       fn sale_price(env: &Env) -> i128;

       #[auth(Self::seller)]
       fn execute_sale(env: &Env, buyer: Address);
   }
   ```
   The trait is a clear, readable statement of the contract terms. The cost
   price, markup, and sale price are all visible. The seller must authorize
   the sale. There is no hidden complexity.

2. **Structural auth prevents hidden terms**: In Sharia, a contract with
   hidden terms (al-shurut al-fasida) is void. The `#[auth]` annotation
   makes authorization requirements visible in the interface. A Sharia
   auditor can inspect the trait definition and verify that all required
   authorizations are present.

3. **The provider pattern enables transparency**: The provider's business
   logic is separate from the auth layer. A Sharia auditor can inspect the
   provider to verify that no riba calculations occur, without needing to
   understand the auth framework.

## Riba-Free Token Design

The most common use of DeFi is lending with interest (riba). A Sharia-
compliant alternative is murabaha (cost-plus financing) or musharakah
(profit-sharing partnership).

Using `#[contracttrait]`:

```rust
#[contracttrait]
pub trait MusharakahPartnership {
    fn capital_ratio(env: &Env, partner: Address) -> u32;  // % of capital
    fn profit_ratio(env: &Env, partner: Address) -> u32;   // % of profit
    fn total_capital(env: &Env) -> i128;

    #[auth(partner)]
    fn contribute_capital(env: &Env, partner: Address, amount: i128);

    #[auth(Self::managing_partner)]
    fn distribute_profit(env: &Env, total_profit: i128);

    #[auth(Self::managing_partner)]
    fn distribute_loss(env: &Env, total_loss: i128);  // loss per capital ratio
}
```

Key Sharia requirements this addresses:

- **Profit sharing, not interest**: Returns are proportional to capital
  contribution, not based on a fixed rate.
- **Loss sharing**: Unlike conventional lending where the lender bears no loss,
  musharakah requires losses to be shared proportionally. The `distribute_loss`
  method enforces this.
- **Auth on contribution**: Each partner must authorize their own contribution.
  No one can be forced into the partnership.

**Gap in the framework**: Sharia requires that the profit/loss ratios be agreed
upon AT THE TIME OF CONTRACT FORMATION, not changed later. The provider pattern
allows ratios to be changed by the managing partner at any time (if the provider
implementation allows it).

**Recommendation**: A `#[immutable_after_init]` annotation would be valuable for
Sharia compliance:

```rust
#[contracttrait]
pub trait MusharakahPartnership {
    #[immutable_after_init]
    fn profit_ratio(env: &Env, partner: Address) -> u32;
}
```

This would generate a check that the value cannot be modified after first being
set, enforcing the Sharia requirement of fixed terms at contract formation.

## The Waqf Pattern (Endowment)

A waqf is an Islamic endowment -- an asset dedicated permanently to a
charitable purpose. The principal cannot be spent; only the returns can be
used for the designated purpose.

This maps perfectly to the provider pattern:

```rust
#[contracttrait]
pub trait Waqf {
    fn beneficiary(env: &Env) -> Address;
    fn purpose(env: &Env) -> String;

    // Principal is locked -- no auth can unlock it
    fn principal(env: &Env) -> i128;

    // Only returns can be distributed
    #[auth(Self::nazir)]  // nazir = waqf administrator
    fn distribute_returns(env: &Env, amount: i128);
}

pub struct PermanentWaqf;
impl WaqfInternal for PermanentWaqf {
    fn distribute_returns(env: &Env, amount: i128) {
        let returns = calculate_returns(env);  // from halal investments
        assert!(amount <= returns, "cannot distribute principal");
        // transfer to beneficiary
    }
}
```

The sealed macro ensures that the principal-protection logic in the provider
cannot be overridden. This aligns with the Sharia principle that waqf assets
are permanently dedicated.

## Zakat Automation

Zakat (obligatory charity, one of the Five Pillars) is 2.5% of wealth held
for one lunar year. A smart contract could automate zakat calculation and
distribution:

```rust
#[contracttrait]
pub trait ZakatCompliant: FungibleToken {
    fn nisab_threshold(env: &Env) -> i128;  // minimum wealth for zakat
    fn zakat_rate(env: &Env) -> u32;         // 2.5% (250 basis points)

    // Auto-calculated based on holding period
    fn zakat_due(env: &Env, holder: Address) -> i128;

    #[auth(holder)]
    fn pay_zakat(env: &Env, holder: Address);  // to designated charity
}
```

The supertrait pattern (`ZakatCompliant: FungibleToken`) is elegant here:
any token can become zakat-compliant by adding the supertrait. The provider
handles the calculation based on holding period (tracking when tokens were
received, calculating the lunar year threshold).

**Scholarly concern**: Automating zakat raises a fiqh question. Zakat must
be given with INTENTION (niyyah). If a smart contract automatically deducts
zakat, does the holder form the required intention? This is debated among
scholars. The consensus position would be that the holder forms niyyah when
they opt into the zakat-compliant token, making the automation permissible.

The `#[auth(holder)]` on `pay_zakat` supports this: the holder must
explicitly authorize the payment, which is the digital equivalent of niyyah.

## Compliance Guards

Sharia compliance requires ongoing monitoring, not just initial design. A
Sharia supervisory board must be able to halt non-compliant operations.

Using supertrait composition:

```rust
#[contracttrait]
pub trait ShariaSupervised: Pausable {
    fn sharia_board(env: &Env) -> Address;

    #[auth(Self::sharia_board)]
    fn declare_non_compliant(env: &Env, reason: String);

    #[auth(Self::sharia_board)]
    fn restore_compliance(env: &Env);
}
```

The `ShariaSupervised: Pausable` supertrait means the Sharia board inherits
the ability to pause all operations if a compliance issue is found. This is
the digital equivalent of a Sharia board's fatwa (ruling) that a financial
product is non-compliant.

**Framework gap**: The current `#[auth]` pattern supports single-entity
authorization. A real Sharia board requires quorum-based authorization
(typically 3 of 5 scholars must agree). The compound auth patterns discussed
by other reviewers (`any_of`, `threshold`) are relevant here.

## Maqasid al-Shariah Assessment

The maqasid al-Shariah (objectives of Islamic law) include:

1. **Preservation of religion (din)**: The framework does not address this.
2. **Preservation of life (nafs)**: Not applicable.
3. **Preservation of intellect (aql)**: The clear trait interface supports
   intellectual understanding of contracts. Positive.
4. **Preservation of lineage (nasl)**: Not applicable.
5. **Preservation of wealth (mal)**: The sealed auth pattern protects wealth
   from unauthorized access. The provider pattern enables compliant financial
   instruments. Strongly positive.

The framework serves the fifth maqsad (preservation of wealth) well through
its structural auth guarantees.

## Verdict

The `#[contracttrait]` pattern is remarkably well-suited for Sharia-compliant
DeFi. The structural auth enforcement maps to the Sharia requirement for
clear, agreed-upon terms (no gharar). The provider pattern enables riba-free
financial instruments. The sealed macro protects assets from unauthorized
access (preservation of wealth).

The main gaps are compound auth for Sharia board governance and immutability
guarantees for contract terms fixed at formation. Both are addressable
extensions, not fundamental limitations.

This framework could enable the first truly Sharia-compliant DeFi ecosystem
on Stellar -- a blockchain already used by Islamic financial institutions for
cross-border payments.

**Rating: 7.5/10 for Sharia compliance** -- strong structural properties for
compliance, needs compound auth and term immutability.
