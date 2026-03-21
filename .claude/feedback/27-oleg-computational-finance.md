# Review: soroban-sdk-tools -- Computational Finance & Numerical Analysis

**Reviewer:** Dr. Oleg Petrov
**Background:** Former nuclear physicist at Kurchatov Institute; transitioned to computational finance in 2001; now head of quantitative research at a systematic trading firm; specializes in fixed-point arithmetic, numerical stability, and pricing model implementation on constrained hardware
**Focus:** Mathematical rigor, numerical stability, fixed-point arithmetic, pricing models

---

## Executive Summary

In nuclear physics, we had a saying: "A calculation without error bounds is not a calculation -- it is a guess." The same principle applies to financial computations on blockchain. soroban-sdk-tools provides elegant compositional patterns for smart contract architecture, but it operates in a numerically primitive environment (128-bit integers, no floating point, no standard fixed-point library) and offers no guidance on the numerical challenges that its composability patterns will inevitably encounter.

This review examines whether the Provider pattern and trait composition can support the numerical rigor required for financial instruments: token pricing, AMM curves, yield calculations, option pricing, and risk metrics.

---

## 1. The Numerical Environment

### What Soroban provides

- `i128`: 128-bit signed integers (-1.7e38 to 1.7e38)
- `u64`, `i64`: 64-bit integers
- No `f32`, `f64` (floating point not available in WASM on Soroban)
- No standard fixed-point library in the Soroban SDK
- No `BigInt` or arbitrary-precision arithmetic

### What financial computations require

| Operation | Precision needed | Example |
|---|---|---|
| Token balance | 7-18 decimal places | 1.2345678 XLM (7 decimals) |
| Interest rate | 8-12 decimal places | 3.14159265% APY |
| Price ratio | 18+ decimal places | ETH/USDC = 3,247.123456789012345678 |
| Logarithm | 15+ significant digits | ln(1.0001) for tick math |
| Square root | 15+ significant digits | sqrt(price) for concentrated liquidity |
| Exponential | 15+ significant digits | e^(r*t) for continuous compounding |
| Cumulative normal | 8+ significant digits | N(d1) for Black-Scholes |

### The fundamental tension

soroban-sdk-tools' Provider pattern encourages clean separation of concerns. A `PricingEngine` provider implements pricing logic. A `YieldCalculator` provider computes yields. But these providers must perform decimal arithmetic using only integers. The framework provides no tools, guidance, or conventions for this.

---

## 2. Fixed-Point Arithmetic: The Missing Foundation

### How fixed-point works

Fixed-point represents decimals as integers scaled by a constant factor:

```
value_fixed = value_real * SCALE
SCALE = 10^DECIMALS

Example with DECIMALS = 7:
  1.2345678 XLM = 12345678 (stored as i128)
  3.14% = 314000 (stored as i128 with 7 decimal scale)
```

### The overflow problem

Multiplication of two fixed-point numbers:

```
a * b = (a_fixed * b_fixed) / SCALE
```

If `a_fixed` and `b_fixed` are both near `i128::MAX / SCALE`, the intermediate product `a_fixed * b_fixed` overflows. With `SCALE = 10^18` (the Ethereum convention), this overflow occurs when both values exceed ~1.7e20 -- which is only 170 XLM-equivalents at 18 decimals.

### How this affects Providers

Consider a yield calculation provider:

```rust
pub struct CompoundYieldCalculator;
impl YieldCalculatorInternal for CompoundYieldCalculator {
    fn calculate_yield(env: &Env, principal: i128, rate: i128, periods: u32) -> i128 {
        // rate is in basis points (1% = 100)
        // Need to compute: principal * (1 + rate/10000)^periods
        //
        // THIS IS THE HARD PART
        // How do we exponentiate a fixed-point number?
        // How do we avoid overflow in intermediate calculations?
        // What is the error bound?

        let mut result = principal;
        for _ in 0..periods {
            result = result + (result * rate) / 10000;
            // Error: truncation in each iteration compounds
            // After 365 iterations (daily compounding for 1 year):
            //   Truncation error ≈ 365 * 0.5 * (1/10000) ≈ 1.8%
            //   This is UNACCEPTABLE for financial calculations
        }
        result
    }
}
```

The naive implementation above has compounding truncation error. A correct implementation requires:

1. Higher internal precision (multiply by a larger scale before dividing)
2. Error-compensating algorithms (Kahan summation for sums)
3. Exponentiation by squaring for `(1 + r)^n`
4. Careful ordering of operations (multiply before divide)

### Recommendation

soroban-sdk-tools should either:

a) Provide a `FixedPoint` type or recommend a specific library (e.g., `soroban-fixed-point` if one exists)
b) Document numerical best practices for Provider implementations
c) At minimum, include a "Numerical Considerations" section in the docs

The Provider pattern is the right abstraction for swapping pricing models. But if every provider must independently solve fixed-point arithmetic, bugs will be rampant and inconsistent.

---

## 3. Analysis of the Auth Pattern Under Financial Load

### The `#[auth(Self::owner)]` caching pattern

The generated code:

```rust
fn transfer_ownership(env: &Env, new_owner: Address) {
    let __auth_addr = Self::Provider::owner(env);  // storage read
    __auth_addr.require_auth();                     // auth check
    Self::Provider::transfer_ownership(env, new_owner)  // business logic
}
```

The auth address is cached (`__auth_addr`), preventing a redundant storage read. This is numerically irrelevant but operationally significant: in financial contracts with many auth-gated methods called in sequence, each method reads the owner from storage independently. There is no cross-method caching within a transaction.

For a DeFi protocol with 20 admin methods, each called in a batch transaction, this means 20 storage reads for the same owner address. At current Soroban costs, this is negligible. But the principle of unnecessary work offends my sensibilities.

### The `#[auth(param)]` pattern for financial operations

```rust
#[contracttrait]
pub trait TradingEngine {
    #[auth(trader)]
    fn place_order(env: &Env, trader: Address, pair: Symbol, side: u32, price: i128, quantity: i128);
}
```

The `trader.require_auth()` is generated automatically. But in a high-frequency trading context, the order parameters (price, quantity) must also be validated:

- Is `price > 0`?
- Is `quantity > 0`?
- Is `price` within the allowed tick size?
- Does the trader have sufficient balance?

These validations happen inside the provider, after the auth check. If a validation fails, the auth check was wasted gas. In financial protocols where failed transactions are common (stale prices, insufficient balance), the ordering matters.

### Recommendation

Consider supporting `#[validate]` or `#[pre_check]` attributes that run before auth:

```rust
#[contracttrait]
pub trait TradingEngine {
    #[validate(price > 0, quantity > 0)]  // hypothetical
    #[auth(trader)]
    fn place_order(env: &Env, trader: Address, pair: Symbol, side: u32, price: i128, quantity: i128);
}
```

This would fail fast on invalid parameters without wasting gas on auth. In practice, the gas savings are small, but the principle of "fail early, fail fast" is important in financial systems.

---

## 4. AMM (Automated Market Maker) Implementation Analysis

### The constant-product AMM

The simplest AMM uses the invariant `x * y = k`:

```rust
pub struct ConstantProductAMM;
impl AMMInternal for ConstantProductAMM {
    fn swap(env: &Env, token_in: Address, amount_in: i128) -> i128 {
        let reserve_in = get_reserve(env, &token_in);
        let reserve_out = get_other_reserve(env, &token_in);

        // amount_out = reserve_out * amount_in / (reserve_in + amount_in)
        // DANGER: reserve_out * amount_in can overflow i128
        //   if reserve_out = 10^18 and amount_in = 10^18
        //   product = 10^36 > i128::MAX ≈ 1.7 * 10^38
        //   This is dangerously close to overflow

        let numerator = reserve_out.checked_mul(amount_in)
            .expect("overflow in AMM calculation");
        let denominator = reserve_in.checked_add(amount_in)
            .expect("overflow in AMM calculation");

        numerator / denominator
        // PROBLEM: integer division truncates
        // This means the AMM always rounds in one direction
        // Over millions of trades, this bias accumulates
    }
}
```

### The concentrated liquidity problem

Uniswap V3-style concentrated liquidity requires:

1. `sqrt(price)` -- square root of a fixed-point number
2. `ln(price)` -- natural logarithm for tick calculation
3. Tick-to-price conversion: `price = 1.0001^tick`

None of these operations are available natively. Each requires a hand-written implementation with careful error analysis.

The Provider pattern is well-suited for swapping AMM implementations:

```rust
type Provider = ConstantProductAMM;    // simple, v2-style
type Provider = ConcentratedLiqAMM;     // complex, v3-style
type Provider = StableSwapAMM;          // Curve-style for stablecoins
```

But each provider must solve the same numerical problems independently. There is no shared numerical library.

### Recommendation

Consider providing or recommending:

1. A `math` module with `sqrt_fixed`, `ln_fixed`, `exp_fixed`, `pow_fixed` functions
2. Each function documented with:
   - Input domain
   - Output precision
   - Maximum error bound
   - Gas cost estimate

This is not the framework's core responsibility, but it is the single biggest barrier to implementing financial providers correctly.

---

## 5. Option Pricing and Black-Scholes

### Why this matters

DeFi options protocols (like Lyra, Dopex) are a growing segment. On-chain option pricing requires the Black-Scholes formula:

```
C = S * N(d1) - K * e^(-rT) * N(d2)
where:
  d1 = (ln(S/K) + (r + σ²/2) * T) / (σ * sqrt(T))
  d2 = d1 - σ * sqrt(T)
  N(x) = cumulative standard normal distribution
```

This requires: `ln`, `exp`, `sqrt`, `N()` (cumulative normal), all in fixed-point arithmetic.

### The Provider pattern for options

```rust
#[contracttrait]
pub trait OptionPricing {
    fn price_call(env: &Env, spot: i128, strike: i128, vol: i128, rate: i128, time: i128) -> i128;
    fn price_put(env: &Env, spot: i128, strike: i128, vol: i128, rate: i128, time: i128) -> i128;
    fn implied_vol(env: &Env, market_price: i128, spot: i128, strike: i128, rate: i128, time: i128) -> i128;
}

pub struct BlackScholes;      // analytical solution
pub struct Binomial;          // tree-based, better for American options
pub struct MonteCarlo;        // simulation, too expensive for on-chain
```

The Provider pattern is perfect here. Different pricing models as different providers. The `#[auth]` pattern handles who can update pricing parameters (oracle, admin, market maker).

But the `MonteCarlo` provider would require randomness -- which Soroban provides via `env.prng()` -- and massive compute, which may exceed transaction limits. The framework should document when a provider pattern reaches the limits of on-chain computation.

### Numerical stability of Black-Scholes in i128

I have implemented Black-Scholes in fixed-point before (for FPGAs in my earlier career). The critical challenges:

1. **ln(S/K)**: For near-the-money options (S close to K), `ln(S/K)` is close to zero. Precision is lost. You need 18+ decimal places for the division before taking the log.

2. **N(d1)**: The cumulative normal distribution is typically approximated. The Abramowitz & Stegun approximation:
   ```
   N(x) ≈ 1 - n(x)(a1*t + a2*t^2 + a3*t^3)
   where t = 1/(1 + 0.33267*x), n(x) = (1/sqrt(2pi))*e^(-x^2/2)
   ```
   This requires multiplication of 5+ fixed-point numbers, each potentially close to the i128 overflow boundary.

3. **σ * sqrt(T)**: Volatility (σ ≈ 0.2 to 2.0) times sqrt of time (T ≈ 0.001 to 2.0). The product is typically O(1), but intermediate calculations can vary widely.

### Recommendation

If soroban-sdk-tools aims to support DeFi derivatives, provide:

1. A reference `BlackScholes` provider with documented precision guarantees
2. Test vectors comparing on-chain results to off-chain (Python/R) calculations
3. Error bound analysis showing when the fixed-point approximation deviates from IEEE 754 double precision

---

## 6. The Error Composition System and Financial Error Codes

### The `#[scerr]` proposal

The blog mentions auto-chaining error codes:

```rust
#[scerr]
pub enum MyError {
    CustomError,
    #[transparent]
    Ownable(#[from] OwnableError),
    #[transparent]
    Pausable(#[from] PausableError),
}
```

### Financial error codes

Financial systems need rich error codes:

```rust
pub enum PricingError {
    Overflow,                    // intermediate calculation overflow
    Underflow,                   // result below minimum representable value
    DivisionByZero,              // reserve is zero
    PrecisionLoss(u32),          // significant digits lost (with count)
    DomainError,                 // e.g., sqrt of negative number
    StalePrice(u64),             // price older than threshold (with age in seconds)
    PriceDeviation(i128),        // price deviates from oracle by too much
    InsufficientLiquidity(i128), // not enough liquidity (with available amount)
}
```

The `#[scerr]` auto-chaining must handle these correctly:
- Error codes must be stable across versions (financial clients cache error mappings)
- Error codes must be unique across composed traits (no collision between `PricingError::Overflow` and `AMMError::Overflow`)
- Error payloads (the `u32`, `i128`, `u64` in the variants above) must be serializable to XDR

### Recommendation

Document whether `#[scerr]` supports error variants with payloads, and whether the auto-chaining preserves variant data. For financial applications, the difference between "error code 42" and "error code 42 with payload showing 15 significant digits were lost" is the difference between debugging in minutes and debugging in days.

---

## 7. Deterministic Computation Guarantees

### Why determinism matters

In finance, two nodes evaluating the same contract with the same inputs must produce the same output, to the last bit. This is guaranteed by Soroban's WASM execution model (deterministic by design, no floating point). But the Provider pattern introduces a question: **does the provider selection itself affect determinism?**

### The concern

If Contract A uses `type Provider = LinearPayout` and Contract B uses `type Provider = SteppedPayout`, they produce different results for the same inputs. This is by design. But within a single contract, the provider must be deterministic across:

- Different validator nodes
- Different execution timestamps (same ledger)
- Re-execution during consensus

### The assessment

soroban-sdk-tools' Provider pattern is fully deterministic because:
1. The provider is selected at compile time (monomorphized)
2. The provider is part of the WASM binary
3. WASM execution is deterministic
4. No runtime dispatch, no dynamic loading

This is correct and good. But it should be documented explicitly for the benefit of financial engineers who (rightly) obsess about determinism.

---

## 8. Risk Metrics and Portfolio Calculations

### On-chain risk metrics

Advanced DeFi protocols compute risk metrics on-chain:

- **Value at Risk (VaR):** Maximum expected loss at a confidence level
- **Greeks (Delta, Gamma, Vega, Theta):** Sensitivity measures for derivatives
- **Sharpe Ratio:** Risk-adjusted return
- **Liquidation thresholds:** When collateral falls below a ratio

These require complex numerical calculations that push the limits of i128 arithmetic.

### The Provider pattern for risk models

```rust
#[contracttrait]
pub trait RiskEngine {
    fn collateral_ratio(env: &Env, account: Address) -> i128;
    fn is_liquidatable(env: &Env, account: Address) -> bool;

    #[auth(Self::liquidator)]
    fn liquidate(env: &Env, account: Address);

    fn liquidator(env: &Env) -> Address;
}

pub struct SimpleRatio;           // collateral / debt
pub struct WeightedRatio;         // risk-weighted assets
pub struct VolatilityAdjusted;    // dynamic based on market vol
```

The Provider pattern shines here. Different risk models for different market conditions, swappable without changing the contract interface. In traditional finance, this is how risk engines work -- pluggable models with a stable API.

### The gas cost of complex risk calculations

A `VolatilityAdjusted` risk engine might need to:
1. Read 10 price feeds from oracles (10 storage reads)
2. Calculate a covariance matrix (O(n^2) multiplications)
3. Compute portfolio VaR (matrix operations)
4. Compare against thresholds

This may exceed Soroban's per-transaction compute limits. The framework should document or provide patterns for splitting complex calculations across multiple transactions (e.g., "calculate risk in stage 1, execute liquidation in stage 2").

---

## 9. Fixed-Point Library Proposal

### What I would want from a companion library

If soroban-sdk-tools included or recommended a fixed-point arithmetic module, it should provide:

```rust
pub struct Fixed<const DECIMALS: u32>(i128);

impl<const D: u32> Fixed<D> {
    pub fn from_raw(raw: i128) -> Self;
    pub fn from_int(n: i64) -> Self;

    // Arithmetic with overflow checking
    pub fn checked_add(self, other: Self) -> Option<Self>;
    pub fn checked_sub(self, other: Self) -> Option<Self>;
    pub fn checked_mul(self, other: Self) -> Option<Self>;  // handles scaling
    pub fn checked_div(self, other: Self) -> Option<Self>;  // handles scaling

    // Rounding modes
    pub fn mul_round_up(self, other: Self) -> Self;
    pub fn mul_round_down(self, other: Self) -> Self;
    pub fn div_round_up(self, other: Self) -> Self;
    pub fn div_round_down(self, other: Self) -> Self;

    // Mathematical functions
    pub fn sqrt(self) -> Self;        // Newton's method, documented precision
    pub fn ln(self) -> Self;          // Taylor series, documented precision
    pub fn exp(self) -> Self;         // Taylor series, documented precision
    pub fn pow(self, n: Self) -> Self; // exp(n * ln(self))

    // Conversion
    pub fn rescale<const D2: u32>(self) -> Fixed<D2>;  // change precision
}
```

With const generics for the decimal count, the compiler can verify that operations between different scales are handled correctly. This is the kind of type-level safety that Rust excels at and that financial computations desperately need.

---

## 10. Final Assessment

### What soroban-sdk-tools gets right for computational finance

1. **The Provider pattern maps perfectly to financial model selection.** Swap pricing models, risk engines, and payout strategies with one line change. This is how quantitative finance works in practice.

2. **Compile-time provider selection ensures determinism.** No runtime dispatch means no non-determinism. Financial regulators approve.

3. **The auth model handles financial access patterns.** `#[auth(Self::owner)]` for admin functions, `#[auth(trader)]` for user operations, `#[auth(Self::oracle_address)]` for price feeds -- all the common patterns are covered.

4. **Zero overhead matters for gas-sensitive DeFi.** If the abstraction truly adds no gas cost, financial protocols can use it without worrying about competitive disadvantage vs. hand-written contracts.

5. **The AuthClient enables proper risk testing.** Testing that unauthorized liquidations fail, that only the oracle can submit prices, that only the owner can change risk parameters -- these are critical test cases that `mock_all_auths()` cannot verify.

### What is missing

1. **No numerical computation support.** No fixed-point library, no mathematical functions, no precision guarantees, no error bound documentation.

2. **No guidance on gas limits for complex calculations.** Financial providers often need heavy computation. The framework should document when computation should be split across transactions.

3. **No rounding mode conventions.** Should AMM swaps round in favor of the protocol or the user? The framework has no opinion, which means every provider will make a different choice.

4. **No overflow protection patterns.** The `checked_mul` pattern is standard Rust, but the framework does not emphasize its importance for financial calculations.

5. **No numerical test utilities.** Testing financial providers requires comparing results to known-correct values with specified tolerances. The AuthClient should be accompanied by a `NumericalTestClient` or similar.

### Score: 6.5/10

The compositional architecture is excellent for financial applications. The numerical infrastructure is absent. For a framework targeting DeFi and financial contracts, this gap is significant but addressable.

The Provider pattern is the right abstraction. What is needed is a companion `soroban-sdk-tools-math` crate that provides the numerical foundation these providers need.

---

*Reviewed by Dr. Oleg Petrov, Quantitative Research, former Kurchatov Institute*
*Review date: 2026-03-21*
