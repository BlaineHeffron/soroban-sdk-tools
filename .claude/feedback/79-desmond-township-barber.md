# Review: Desmond -- Township Barber

**Reviewer:** Desmond, South African township barber accepting crypto payments
**Focus:** Micro-payments, no-smartphone scenarios, community trust, informal economy
**Date:** 2026-03-21
**Materials Reviewed:**
- `docs/oz-comparison.md`
- `docs/blog-post-composable-contracts.md`
- `examples/trait-test/src/lib.rs`
- `soroban-sdk-tools-macro/src/contract.rs`

---

## Background: Cutting Hair in Khayelitsha

I run a barbershop in Khayelitsha, a township outside Cape Town. I have three
chairs. I employ two other barbers. A haircut costs R80 (about $4.50 USD).
A shave is R40. On a good day, we do 30 cuts.

I started accepting crypto payments last year because a customer who works in
tech offered to set me up. Most of my customers still pay cash, but about
20% now pay with their phones. The problem is not the technology -- the
problem is that half my customers do not have smartphones.

I am interested in `soroban-sdk-tools` because I want to build a simple
payment and loyalty system for my shop. But it needs to work for people
who have R200 phones with no data plan, not just for people with iPhones.

---

## 1. Micro-Payments: When R80 Is the Whole Transaction

### The Scale Problem

Most DeFi is designed for transactions of $100 or more. My transactions
are $4.50. At that scale:

- Transaction fees matter. If the fee is $0.01, that is 0.2% of the
  transaction. Acceptable. If the fee is $1, that is 22%. Not acceptable.
- Gas costs matter. Complex contracts that require many storage reads
  are expensive. A payment for a haircut should not cost more in gas
  than the haircut itself.
- Complexity matters. If my customer needs to interact with three
  contracts (token + auth + loyalty), the gas for three cross-contract
  calls might exceed the payment value.

### How soroban-sdk-tools Affects This

The blog post claims:

> "WASM binary size: Zero overhead. Traits are erased after monomorphization."

This is important for gas costs. If the two-trait indirection adds zero
overhead to the WASM, then the gas cost is the same as a hand-written
contract. Good.

But the AuthClient testing pattern (closures, boxed callbacks, cloned
arguments) is test-only code (`#[cfg(not(target_family = "wasm"))]`).
This does not affect the deployed contract. Good -- I checked.

The actual gas cost comes from:
1. `env.storage().instance().get()` -- one read per auth check (to get the owner)
2. `require_auth()` -- Soroban runtime verification
3. The provider's business logic (storage reads/writes)

For a simple payment, this is 2-3 storage operations. On Soroban, that
should be well under the minimum fee. Acceptable for R80 transactions.

**Assessment: The zero-overhead claim appears valid for micro-payments.**

---

## 2. No-Smartphone Scenarios: The Feature Phone Challenge

### The Reality

In Khayelitsha, smartphone penetration is about 60%. The other 40% have
feature phones -- basic phones with SMS and sometimes USSD capability.
No apps. No browsers (or very limited ones). Definitely no MetaMask.

For crypto payments to work for everyone, I need:

1. **USSD-based interaction:** Customer dials a code like `*120*PAY#`,
   enters the amount, enters a PIN, and the payment goes through.
2. **SMS confirmation:** Both customer and I get an SMS confirming
   the payment.
3. **QR code (for smartphone users):** Scan to pay.
4. **NFC tap (future):** Tap to pay with a card or wristband.

### What This Means for soroban-sdk-tools

The contract itself does not know how the transaction was submitted. Whether
it came from a USSD gateway, an SMS relay, or a smartphone app, the Soroban
transaction looks the same. The `#[auth]` check is the same regardless of
the submission channel.

But the AUTH mechanism matters. `require_auth()` in Soroban verifies that
the transaction was signed by the authorized address. For a feature phone
user:

- They cannot sign transactions on-device (no crypto library on a R200 phone)
- A custodial service must hold their keys and sign on their behalf
- The USSD PIN verifies the customer to the custodial service
- The custodial service signs the Soroban transaction

### Custodial Provider Pattern

```rust
pub struct CustodialPaymentProvider;
impl PaymentInternal for CustodialPaymentProvider {
    fn pay(env: &Env, from: Address, to: Address, amount: i128) {
        // The 'from' address is the custodial service, not the end user
        // The custodial service has verified the user via USSD/PIN
        TokenClient::transfer(env, &from, &to, &amount);
    }
}
```

The `#[auth(from)]` on the trait works here -- the custodial service is the
`from` address, and it has signed the transaction. But there is a trust
assumption: the custodial service correctly verified the end user.

**Observation:** The provider pattern lets me swap between custodial
(for feature phone users) and self-custodial (for smartphone users)
without changing the contract. This is useful.

**Concern:** The `#[auth]` system assumes the auth address is the end user.
In custodial scenarios, the auth address is an intermediary. The contract
cannot distinguish between "user authorized this via USSD" and "custodial
service stole the user's funds." This is a fundamental limitation of the
Soroban auth model, not of `soroban-sdk-tools`.

---

## 3. Community Trust: The Stokvel Model

### What Is a Stokvel?

A stokvel is a South African community savings group. 10-20 people
contribute a fixed amount monthly. Each month, one member receives the
full pot. It is a rotating savings and credit association (ROSCA).

Stokvels are based entirely on community trust. There is no contract.
No insurance. If a member takes their pot and disappears, the group
absorbs the loss. Millions of South Africans participate in stokvels.

### Stokvel Contract

```rust
#[contracttrait]
pub trait Stokvel {
    fn organizer(env: &Env) -> Address;
    fn member_count(env: &Env) -> u32;
    fn contribution_amount(env: &Env) -> i128;
    fn current_recipient(env: &Env) -> Address;
    fn pot_balance(env: &Env) -> i128;

    #[auth(member)]
    fn contribute(env: &Env, member: Address);

    #[auth(Self::organizer)]
    fn add_member(env: &Env, new_member: Address);

    #[auth(Self::organizer)]
    fn remove_member(env: &Env, member: Address);

    #[auth(Self::current_recipient)]
    fn claim_pot(env: &Env);

    #[auth(Self::organizer)]
    fn rotate_recipient(env: &Env);
}
```

### Why the Two-Trait Split Matters Here

The `StokvelInternal` trait handles the business logic:

```rust
impl StokvelInternal for CommunityStokvelProvider {
    fn contribute(env: &Env, member: Address) {
        let amount = StokvelStorage::get_contribution_amount(env);
        let token = StokvelStorage::get_token(env);

        // Transfer from member to contract
        TokenClient::new(env, &token).transfer(
            &member,
            &env.current_contract_address(),
            &amount,
        );

        StokvelStorage::mark_contributed(env, &member);
    }

    fn claim_pot(env: &Env) {
        let pot = StokvelStorage::get_pot_balance(env);
        let recipient = StokvelStorage::get_current_recipient(env);
        let token = StokvelStorage::get_token(env);

        // Transfer pot to current recipient
        TokenClient::new(env, &token).transfer(
            &env.current_contract_address(),
            &recipient,
            &pot,
        );
    }
}
```

The auth wrapper ensures:
- Only members can contribute (via `#[auth(member)]`)
- Only the current recipient can claim the pot
- Only the organizer can add/remove members and rotate

This is a formalization of the trust relationships that already exist
in stokvels. The difference is that the rules are enforced by code,
not by social pressure.

### The Provider Swap for Different Stokvel Types

```rust
// Standard ROSCA: fixed contribution, rotating recipient
pub struct StandardROSCA;

// Accumulating: everyone saves, pot grows, distribution at end
pub struct AccumulatingStokvel;

// Investment: pot is invested in an asset, returns distributed
pub struct InvestmentStokvel;
```

Different stokvels have different rules. The provider pattern handles
this diversity.

**Assessment: The stokvel use case is a strong fit for soroban-sdk-tools.**

---

## 4. The Informal Economy: No Registered Business

### The Problem

I do not have a registered business. Most barbers in Khayelitsha do not.
We operate in the informal economy. We pay no VAT. We have no bank accounts
(or simple savings accounts, not business accounts).

Most smart contract systems assume the deployer is a registered entity
with a business account, a tax ID, and legal standing. That is not my
reality.

### What I Need

1. **No KYC requirement:** I should be able to deploy and use a contract
   without providing identity documents.
2. **No minimum balance:** I should be able to start with R0 and build up.
3. **Community governance:** My customers and I should collectively
   manage the system, not a corporation.

### How soroban-sdk-tools Helps

The `Ownable` pattern with `SingleOwner` works for my shop -- I am the
owner, I manage the contract. But for community-managed systems (like
stokvels), the provider needs to support:

- **Multi-owner:** Multiple people share admin rights
- **Democratic governance:** Members vote on changes
- **No single point of failure:** If I get hit by a taxi, the shop
  contract should still work

The `MultisigOwner` provider mentioned in the blog post would handle
multi-owner. But full democratic governance (voting, proposals, quorum)
is more complex.

**Recommendation:** A `CommunityGovernance` provider that implements
`OwnableInternal` with voting-based ownership decisions:

```rust
pub struct CommunityGovernance;
impl OwnableInternal for CommunityGovernance {
    fn owner(env: &Env) -> Address {
        // Returns the contract's own address -- the community owns it
        env.current_contract_address()
    }

    fn transfer_ownership(env: &Env, new_owner: Address) {
        // Requires community vote (checked via separate governance contract)
        GovernanceClient::verify_proposal_passed(env, "transfer_ownership");
        // ... transfer logic
    }
}
```

---

## 5. Loyalty Programs: The Haircut Card

### The Current System

I have a paper loyalty card. Get 10 haircuts, the 11th is free. I stamp
the card each time. The problem: customers lose the cards, they get wet,
they get torn. I lose about R5,000/year in loyalty redemptions that I
cannot verify.

### On-Chain Loyalty

```rust
#[contracttrait]
pub trait LoyaltyProgram: Ownable {
    fn points_balance(env: &Env, customer: Address) -> u32;
    fn redemption_threshold(env: &Env) -> u32;  // e.g., 10 stamps

    #[auth(Self::owner)]  // only the shop owner can stamp
    fn stamp(env: &Env, customer: Address);

    #[auth(customer)]  // customer redeems their own points
    fn redeem(env: &Env, customer: Address);
}
```

The auth model is correct:
- Only I (the shop owner) can stamp a loyalty card. I stamp it after
  I cut the hair. The customer cannot stamp their own card.
- Only the customer can redeem. I cannot redeem on their behalf
  (prevents me from claiming their free cuts).

### Feature Phone Implementation

For a feature phone customer:
1. I cut their hair
2. I text `STAMP 0791234567` to the system
3. The system verifies I am the shop owner (my phone number maps to
   my Stellar address via the custodial service)
4. The system stamps the customer's loyalty card
5. Both of us get an SMS: "Stamp added. 7/10 toward free haircut."

When the customer has 10 stamps:
1. Customer texts `REDEEM` to the system
2. The system verifies they have 10 stamps
3. The system marks the next haircut as free
4. Customer gets SMS: "Free haircut redeemed! Show this to your barber."
5. I get SMS: "Customer XXXX redeemed a free haircut."

### Auth in Offline Scenarios

What if the customer's phone is off? What if they have no phone at all?

I need a way to handle offline redemption:
1. I verify the customer manually (I know my regulars)
2. I stamp and redeem on their behalf

This means the owner needs to be able to act on behalf of a customer
in certain scenarios. The current auth model does not support this well:

```rust
#[auth(customer)]
fn redeem(env: &Env, customer: Address);
```

If the customer cannot sign (no phone), I cannot call `redeem` for them
because `customer.require_auth()` will fail.

**Recommendation:** Support delegated auth in the provider:

```rust
pub struct DelegatedRedemptionProvider;
impl LoyaltyProgramInternal for DelegatedRedemptionProvider {
    fn redeem(env: &Env, customer: Address) {
        // Check if owner is authorized to act on behalf
        let delegations = DelegationStorage::get(env, &customer);
        let caller = env.current_contract_address();
        // ... delegation logic
    }
}
```

But this puts auth logic in the provider, which the design philosophy
tries to avoid. The `#[auth]` system might need a delegation concept:

```rust
#[auth(customer, delegate: Self::owner)]
fn redeem(env: &Env, customer: Address);
```

Where either the customer OR the owner (as delegate) can authorize.

---

## 6. Pricing in Multiple Currencies

### The Reality

My customers pay in:
- South African Rand (ZAR) -- cash
- USDC -- crypto via smartphone
- Local community token (if I set one up) -- feature phone via USSD

### The Token Agnostic Approach

The `FungibleToken` trait handles any token. The payment contract can
accept multiple tokens:

```rust
#[contracttrait]
pub trait PaymentAcceptance: Ownable {
    fn accepted_tokens(env: &Env) -> Vec<Address>;

    #[auth(Self::owner)]
    fn add_accepted_token(env: &Env, token: Address, rate_to_zar: i128);

    #[auth(payer)]
    fn pay(env: &Env, payer: Address, token: Address, amount: i128);

    fn total_received_zar(env: &Env) -> i128;
}
```

The provider converts all payments to ZAR-equivalent for my accounting:

```rust
impl PaymentAcceptanceInternal for MultiTokenProvider {
    fn pay(env: &Env, payer: Address, token: Address, amount: i128) {
        let rate = PaymentStorage::get_rate(env, &token);
        let zar_value = amount * rate / PRECISION;

        TokenClient::new(env, &token).transfer(
            &payer,
            &env.current_contract_address(),
            &amount,
        );

        PaymentStorage::add_received(env, zar_value);
    }
}
```

**Assessment: The provider pattern handles multi-token acceptance well.**

---

## 7. The Spaza Shop Network: Shared Infrastructure

### Context

Khayelitsha has hundreds of spaza shops (informal convenience stores),
taxis, and service providers. If several of us share a payment system,
the costs go down:

- Shared contract deployment cost
- Shared custodial service for feature phone users
- Shared loyalty program across shops

### Multi-Tenant Contract

```rust
#[contracttrait]
pub trait MultiTenantPayment {
    fn platform_admin(env: &Env) -> Address;
    fn merchant_count(env: &Env) -> u32;

    #[auth(Self::platform_admin)]
    fn register_merchant(env: &Env, merchant: Address, name: Symbol);

    #[auth(merchant)]
    fn request_payment(env: &Env, merchant: Address, customer: Address, amount: i128);

    #[auth(customer)]
    fn confirm_payment(env: &Env, customer: Address, payment_id: u64);
}
```

This is a two-step payment: merchant requests, customer confirms. It
prevents unauthorized charges (the customer must explicitly approve).

The `platform_admin` role could be a community organization or cooperative.
The provider pattern would let different communities customize the platform:

```rust
pub struct KhayelitshaProvider;  // ZAR-based, USSD-friendly
pub struct SowetoProvider;       // Different local requirements
pub struct NairobiProvider;      // Kenya, M-Pesa integration model
```

---

## 8. Data Minimization: Privacy in the Township

### The Concern

In a township, everyone knows everyone's business. If my payment history
is on-chain and publicly readable, my competitors know how much I make.
The local protection racket knows how much I make. The tax authority
knows how much I make.

### Privacy Requirements

1. **Transaction amounts should not be publicly visible** on the blockchain
2. **Customer identity should be pseudonymous** (Stellar addresses, not names)
3. **Only the merchant and customer should see the details**

### What soroban-sdk-tools Provides

The `#[contracttrait]` system does not address privacy at all. This is
a protocol-level concern, not a tooling concern. But the provider pattern
could encapsulate privacy techniques:

```rust
pub struct PrivatePaymentProvider;
impl PaymentInternalInternal for PrivatePaymentProvider {
    fn pay(env: &Env, payer: Address, amount: i128) {
        // Store only a hash of the payment details
        let payment_hash = env.crypto().sha256(&(payer.clone(), amount).into_val(env));
        PaymentStorage::store_hash(env, &payment_hash);
        // Actual transfer happens but amounts are not stored in contract state
    }
}
```

**Observation:** Privacy is important for township economies. The provider
pattern can encapsulate privacy strategies, but the framework should
acknowledge this need in its documentation.

---

## 9. The AuthClient: Testing from a Barber's Perspective

I am not a test engineer. But I understand: you test to make sure things
work. The AuthClient tests read like instructions:

```rust
// Can the owner stamp a loyalty card? Yes.
auth_client.stamp(&customer).authorize(&owner).invoke();

// Can a random person stamp? No.
auth_client.stamp(&customer).authorize(&random).try_invoke(); // fails
```

This is clearer than `mock_all_auths()`, which I interpret as: "Pretend
security does not exist and see if the logic works." In my barbershop,
that is like testing the cash register with the drawer permanently open.
It tells you the buttons work but not whether the drawer locks.

**Assessment: AuthClient is intuitive even for non-experts.**

---

## 10. Gas Costs: The R80 Haircut Constraint

### My Budget

A haircut costs R80 (~$4.50 USD). If the transaction fee is:
- $0.001: Acceptable (0.02% of transaction)
- $0.01: Acceptable (0.2%)
- $0.10: Borderline (2.2%)
- $1.00: Not acceptable (22%)

### Soroban's Fee Model

Soroban transactions cost ~100 stroops (0.00001 XLM) base fee, plus
resource fees for CPU, memory, storage reads, and storage writes.

A simple payment (1 storage read for auth, 2 storage writes for balance
update) should cost well under $0.01 on Soroban. The `#[contracttrait]`
macro adds zero overhead to WASM size, so no additional gas.

### Complex Operations

A loyalty stamp + payment in one transaction:
- 1 storage read: get owner (for auth)
- 1 storage read: get loyalty balance
- 1 storage write: update loyalty balance
- 1 cross-contract call: token transfer (2 more reads, 2 more writes)

Total: ~7 storage operations. Still well under $0.01.

**Assessment: Gas costs are acceptable for micro-payments on Soroban.
The macro adds no overhead.**

---

## 11. What I Need That Does Not Exist Yet

| Need | Description | Status |
|------|-------------|--------|
| USSD integration guide | How to connect feature phones to Soroban | Not addressed |
| Custodial service pattern | Standard provider for custodial key management | Not addressed |
| Delegated auth | Owner acts on behalf of offline customer | Not supported |
| Privacy patterns | Hide transaction details from public view | Not addressed |
| Offline-first workflow | Queue transactions when connectivity is down | Not addressed |
| Multi-tenant contracts | Shared infrastructure for small merchants | Possible but no example |
| Community governance | Democratic management of shared contracts | Needs multi-party auth |
| Localization | Error messages / events in local languages | Not addressed |

---

## 12. The Bigger Picture: Financial Inclusion

### Why This Matters

2.5 billion people worldwide are in the informal economy. They have no
bank accounts, no credit scores, no financial infrastructure. They use
cash, community trust, and informal savings groups.

Smart contracts on Soroban -- with Stellar's low fees and existing
anchor network -- could provide financial infrastructure for these
communities WITHOUT requiring them to enter the formal banking system.

The `soroban-sdk-tools` composability patterns could enable:
- **Community savings (stokvels):** Encoded trust, enforced rules
- **Micro-payment systems:** Low-fee, multi-currency payments
- **Loyalty programs:** No more lost paper cards
- **Supply chain for informal traders:** Track inventory, prove provenance

But only if the tooling accommodates the constraints:
- Feature phones (no apps)
- Low literacy (simple interfaces)
- Intermittent connectivity (offline-first)
- Community governance (multi-party, not single owner)

### What soroban-sdk-tools Gets Right

1. **Zero overhead:** Critical for micro-payments where every cent matters
2. **Provider swap:** Different communities can customize without redeployment
3. **Sealed macro:** Community contracts should not have backdoors
4. **Simple trait definitions:** Even I can read and understand the 6-line
   Ownable trait

### What It Needs

1. **Multi-party auth:** Community governance requires it
2. **Delegated auth:** Offline customers need someone to act for them
3. **Event emission:** Community members need visibility into actions
4. **Examples for informal economy:** Not just ownership and tokens, but
   savings groups, micro-payments, community governance

---

## 13. Closing Thoughts

I cut hair. I am not a blockchain developer. But I understand access
control -- who sits in my chair, who pays, who gets a loyalty stamp.
And I understand community trust -- the stokvel where my mother saves
R500 every month, the spaza shop where we run tabs based on handshakes.

This `soroban-sdk-tools` system makes sense to me because it separates
"what happens" from "who can make it happen." That is how trust works
in my community too -- we agree on what should happen, and then we
agree on who is responsible for making it happen.

The technology is not the hard part. The hard part is making it work
for people with R200 phones, spotty data connections, and no formal
financial identity. The smart contract framework is ready. The
on-ramp is not.

If you build the on-ramp -- USSD gateways, custodial services, SMS
notifications -- then I will build the barbershop of the future on
Soroban. And my customers will not even know they are using a blockchain.
They will just know their loyalty stamps never get lost.

---

**Overall Assessment:** The architecture is sound for micro-payment and
community finance use cases. Zero overhead and the provider pattern are
the key strengths. Multi-party auth, delegated auth, and feature phone
integration patterns are the critical gaps.

**Verdict:** Good foundation. But the last mile -- connecting to people
without smartphones -- is where the real work starts. The contract
framework is ready. The world is not.
