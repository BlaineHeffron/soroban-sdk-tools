# Review: soroban-sdk-tools -- Wine Provenance & Luxury Authentication

**Reviewer:** Pierre -- French sommelier, wine provenance tracking advocate
**Date:** 2026-03-21
**Materials Reviewed:** oz-comparison.md, blog-post-composable-contracts.md, trait-test example, contract.rs macro

---

## Overall Impression

I have been a sommelier for twenty-three years. In that time, I have
encountered more counterfeit wine than I care to admit. A bottle of 1982
Chateau Lafite Rothschild sells for EUR 5,000. The profit margin on a
convincing forgery is extraordinary. Estimates suggest that 20% of fine
wine sold at auction is counterfeit.

The wine industry needs provenance tracking that is tamper-proof,
transparent, and trustworthy. I am evaluating `soroban-sdk-tools` as the
foundation for a wine provenance system that could eliminate counterfeiting.

The `#[contracttrait]` macro, with its provider pattern and sealed auth,
provides excellent foundations for anti-counterfeiting. But wine provenance
has unique requirements -- custody chains, storage conditions, ownership
transfer ceremonies -- that the current tooling does not directly address.

---

## The Wine Provenance Problem

### What Must Be Tracked

A bottle's provenance includes:

1. **Origin** -- Chateau/domaine, appellation, vintage, cuvee
2. **Production** -- Harvest date, vinification method, bottling date
3. **Custody chain** -- Every entity that has possessed the bottle
4. **Storage conditions** -- Temperature, humidity, light exposure
5. **Authentication events** -- Expert inspections, chemical analysis
6. **Ownership transfers** -- Sales, auctions, gifts, inheritance
7. **Consumption** -- When and by whom the bottle was opened

Each of these must be recorded immutably. Each must be attributable to a
verified party. Each must be queryable by anyone (a prospective buyer must
be able to verify provenance before purchase).

### How Counterfeiting Works

The most common counterfeiting techniques:

1. **Label forgery** -- Fake labels on cheap wine. Provenance tracking
   defeats this because the bottle has a digital identity independent of
   its label.

2. **Refilling** -- Genuine bottles refilled with inferior wine. Provenance
   tracking must include a "consumption seal" -- once a bottle is opened,
   it cannot be resealed with a valid provenance record.

3. **Phantom bottles** -- Creating provenance records for bottles that do
   not exist. Only the chateau should be able to create origin records
   (the `#[auth(Self::owner)]` pattern).

4. **Custody manipulation** -- Inserting fake custody entries to create a
   plausible history. Only the actual custodian should be able to transfer
   custody (the `#[auth(current_holder)]` pattern).

---

## Mapping Wine Provenance to Composable Traits

### The Trait Hierarchy

```rust
#[contracttrait]
pub trait WineOrigin: Ownable {
    // Only the chateau can register a new bottle
    #[auth(Self::owner)]
    fn register_bottle(
        env: &Env,
        bottle_id: BytesN<32>,  // unique identifier (NFC tag or QR code)
        vintage: u32,
        appellation: Symbol,
        cuvee: Symbol,
        production_date: u64,
    );

    fn get_origin(env: &Env, bottle_id: BytesN<32>) -> BottleOrigin;
}

#[contracttrait]
pub trait WineCustody: WineOrigin {
    fn current_holder(env: &Env, bottle_id: BytesN<32>) -> Address;
    fn custody_history(env: &Env, bottle_id: BytesN<32>) -> Vec<CustodyEntry>;

    // Only the current holder can transfer custody
    #[auth(current_holder)]
    fn transfer_custody(
        env: &Env,
        current_holder: Address,
        bottle_id: BytesN<32>,
        new_holder: Address,
        conditions: TransferConditions,  // temperature at transfer, etc.
    );

    // Only the current holder can mark a bottle as consumed
    #[auth(current_holder)]
    fn mark_consumed(
        env: &Env,
        current_holder: Address,
        bottle_id: BytesN<32>,
        consumption_date: u64,
    );
}

#[contracttrait]
pub trait WineAuthentication: WineCustody {
    fn is_authenticated(env: &Env, bottle_id: BytesN<32>) -> bool;
    fn authentication_history(env: &Env, bottle_id: BytesN<32>) -> Vec<AuthEvent>;

    // Only a registered expert can authenticate
    #[auth(expert)]
    fn authenticate(
        env: &Env,
        expert: Address,
        bottle_id: BytesN<32>,
        method: AuthMethod,  // Visual, Chemical, Spectral
        result: AuthResult,
        notes: Bytes,
    );

    #[auth(Self::owner)]
    fn register_expert(env: &Env, expert: Address, credentials: ExpertCredentials);
}

#[contracttrait]
pub trait WineStorage: WineCustody {
    fn storage_conditions(env: &Env, bottle_id: BytesN<32>) -> Vec<StorageReading>;

    // IoT sensors report storage conditions
    #[auth(sensor)]
    fn report_conditions(
        env: &Env,
        sensor: Address,
        bottle_id: BytesN<32>,
        temperature: i32,    // in 0.01 degrees C
        humidity: u32,       // in 0.01%
        timestamp: u64,
    );

    #[auth(Self::owner)]
    fn register_sensor(env: &Env, sensor: Address, location: Symbol);
}
```

### Why the Provider Pattern Matters for Wine

Different wine regions have different provenance standards:

```rust
// Bordeaux: strict appellation rules, classification system
pub struct BordeauxProvider;
impl WineOriginInternal for BordeauxProvider {
    fn register_bottle(env: &Env, bottle_id: BytesN<32>, vintage: u32,
                       appellation: Symbol, cuvee: Symbol, production_date: u64) {
        // Validate appellation is a recognized Bordeaux AOC
        assert!(is_valid_bordeaux_aoc(&appellation), "Invalid Bordeaux AOC");
        // Validate vintage is within plausible range
        assert!(vintage >= 1900 && vintage <= current_year(env), "Invalid vintage");
        store_origin(env, bottle_id, vintage, appellation, cuvee, production_date);
    }
}

// Burgundy: different classification (Grand Cru, Premier Cru, Village)
pub struct BurgundyProvider;
impl WineOriginInternal for BurgundyProvider {
    fn register_bottle(env: &Env, bottle_id: BytesN<32>, vintage: u32,
                       appellation: Symbol, cuvee: Symbol, production_date: u64) {
        // Validate appellation is a recognized Burgundy AOC
        assert!(is_valid_burgundy_aoc(&appellation), "Invalid Burgundy AOC");
        // Validate classification level
        let classification = get_classification(&appellation);
        store_origin_with_classification(env, bottle_id, vintage, appellation,
                                         cuvee, production_date, classification);
    }
}

// New World: less strict appellation rules, varietal-based
pub struct NewWorldProvider;
impl WineOriginInternal for NewWorldProvider {
    fn register_bottle(env: &Env, bottle_id: BytesN<32>, vintage: u32,
                       appellation: Symbol, cuvee: Symbol, production_date: u64) {
        // Less strict validation -- AVA or GI designation
        store_origin(env, bottle_id, vintage, appellation, cuvee, production_date);
    }
}
```

Swap `BordeauxProvider` for `BurgundyProvider` with one line. The
provenance interface remains the same; the validation rules change. This
is exactly the flexibility a global wine provenance system needs.

---

## The Auth Pattern for Anti-Counterfeiting

### Why Structural Auth Matters for Wine

The two most critical auth checks in wine provenance:

1. **Only the chateau can create origin records** -- This prevents phantom
   bottles. The `#[auth(Self::owner)]` on `register_bottle` means only
   the chateau (the contract owner) can register new bottles. The sealed
   macro (`impl_wine_origin!`) ensures this cannot be bypassed.

2. **Only the current holder can transfer custody** -- This prevents
   custody manipulation. The `#[auth(current_holder)]` on
   `transfer_custody` means only the person currently holding the bottle
   can pass it to someone else.

If these two guarantees hold, counterfeiting becomes extremely difficult:

- You cannot create a fake bottle (only the chateau can register)
- You cannot insert yourself into the custody chain (only the holder can
  transfer)
- You cannot "unreceive" a bottle you received (custody records are
  immutable)

### The Override Problem in Wine Context

The blog post's discussion of the "override problem" is particularly
relevant for wine. Consider a scenario where a corrupt wine merchant
deploys a modified contract that overrides `transfer_custody` to accept
transfers without the current holder's authorization:

```rust
// DANGEROUS: bypasses custody auth
fn transfer_custody(env: &Env, current_holder: Address, bottle_id: BytesN<32>,
                    new_holder: Address, conditions: TransferConditions) {
    // No auth check -- anyone can transfer any bottle
    record_transfer(env, bottle_id, &new_holder, &conditions);
}
```

With OZ's approach, this override is possible. With soroban-sdk-tools'
sealed macro, it is not. For wine provenance, the sealed pattern is not
just a developer convenience -- it is the difference between a trustworthy
provenance system and a compromised one.

---

## NFT Certificates for Wine

### Digital Twin Concept

Each physical bottle has a digital twin -- an NFT that represents its
provenance record. The NFT contains:

- Origin metadata (chateau, vintage, appellation)
- Complete custody chain
- Storage condition history
- Authentication events
- Current holder

When the bottle is sold, the NFT transfers with it. When the bottle is
consumed, the NFT is "burned" (marked as consumed, cannot be transferred
further).

### Composable NFT Traits

```rust
#[contracttrait]
pub trait WineNFT: WineCustody {
    fn token_uri(env: &Env, bottle_id: BytesN<32>) -> Bytes;
    fn total_supply(env: &Env) -> u64;
    fn owner_of(env: &Env, bottle_id: BytesN<32>) -> Address;

    // Transfer NFT alongside physical custody
    #[auth(current_holder)]
    fn transfer(
        env: &Env,
        current_holder: Address,
        bottle_id: BytesN<32>,
        new_owner: Address,
    );
}
```

The supertrait relationship (`WineNFT: WineCustody`) ensures that NFT
transfers are always accompanied by custody transfers. You cannot transfer
the digital certificate without transferring physical custody (and vice
versa).

### Auction Integration

Wine auctions are a major sales channel for fine wine. An auction trait:

```rust
#[contracttrait]
pub trait WineAuction: WineNFT {
    #[auth(seller)]
    fn list_for_auction(
        env: &Env,
        seller: Address,
        bottle_id: BytesN<32>,
        reserve_price: i128,
        auction_end: u64,
    );

    #[auth(bidder)]
    fn place_bid(
        env: &Env,
        bidder: Address,
        bottle_id: BytesN<32>,
        bid_amount: i128,
    );

    fn settle_auction(env: &Env, bottle_id: BytesN<32>);
    // No auth -- anyone can settle a completed auction
    // Automatically transfers NFT to highest bidder
}
```

The `#[auth(seller)]` on `list_for_auction` ensures only the current
holder can list a bottle for auction. This prevents fraudulent listings
of bottles you do not possess.

---

## Storage Condition Monitoring

### IoT Integration for Cellar Monitoring

Fine wine must be stored at 12-14 degrees Celsius, 60-70% humidity, in
darkness. Deviations degrade the wine. Provenance systems must record
storage conditions to assure buyers that the wine has been properly stored.

The `WineStorage` trait with `#[auth(sensor)]` on `report_conditions`
means only registered IoT sensors can report conditions. This prevents
fake storage reports.

### The Provider Pattern for Different Cellars

Different cellars have different monitoring capabilities:

```rust
// Professional cellar: continuous monitoring
pub struct ProfessionalCellarProvider;
impl WineStorageInternal for ProfessionalCellarProvider {
    fn report_conditions(env: &Env, sensor: Address, bottle_id: BytesN<32>,
                         temperature: i32, humidity: u32, timestamp: u64) {
        // Validate reading is within sensor's calibration range
        let calibration = SensorRegistry::get_calibration(env, &sensor);
        assert!(calibration.is_valid_range(temperature, humidity));
        // Alert if out of spec
        if temperature < 1100 || temperature > 1500 {
            emit_alert(env, bottle_id, "Temperature out of range");
        }
        store_reading(env, sensor, bottle_id, temperature, humidity, timestamp);
    }
}

// Private collector: manual readings
pub struct PrivateCollectorProvider;
impl WineStorageInternal for PrivateCollectorProvider {
    fn report_conditions(env: &Env, sensor: Address, bottle_id: BytesN<32>,
                         temperature: i32, humidity: u32, timestamp: u64) {
        // Less strict -- manual readings accepted
        store_reading(env, sensor, bottle_id, temperature, humidity, timestamp);
    }
}
```

---

## The Consumption Ceremony

### Why Consumption Matters

When a bottle is opened and consumed, its provenance journey ends. This is
a critical event:

1. The NFT must be "burned" or marked as consumed
2. No further custody transfers should be possible
3. The consumption event should be recorded (for statistical/historical
   purposes)
4. Any associated auction or resale listings must be cancelled

The `mark_consumed` method with `#[auth(current_holder)]` handles the
authorization. But the cascading effects (NFT burn, listing cancellation)
require cross-trait composition.

### Composable Consumption

```rust
impl WineCustodyInternal for FullProvenance {
    fn mark_consumed(env: &Env, current_holder: Address, bottle_id: BytesN<32>,
                     consumption_date: u64) {
        // Record consumption
        CustodyStorage::set_consumed(env, &bottle_id, consumption_date);
        // Burn NFT (cross-trait call)
        NFTStorage::burn(env, &bottle_id);
        // Cancel any active auction listings
        AuctionStorage::cancel_if_active(env, &bottle_id);
        // Emit consumption event
        emit_consumption(env, &bottle_id, &current_holder, consumption_date);
    }
}
```

The provider handles all cascading effects in one place. This is where
the provider pattern shines -- complex business logic is centralized in
the provider, not scattered across multiple contracts.

---

## Missing Features for Wine Provenance

### 1. Batch Operations

Chateaux produce thousands of bottles per vintage. Registering each bottle
individually is impractical. A batch registration function is needed:

```rust
#[auth(Self::owner)]
fn register_batch(env: &Env, batch_id: u64, count: u32, vintage: u32,
                  appellation: Symbol, cuvee: Symbol, production_date: u64);
```

This creates `count` provenance records in a single transaction. The
`#[contracttrait]` macro handles the auth correctly, but the gas cost of
batch operations needs to be documented.

### 2. Split and Merge (Cases)

Wine is often sold in cases (6 or 12 bottles). A case has its own
provenance (the case was assembled by the chateau, stored in a specific
cellar, etc.). When a case is split, each bottle inherits the case's
custody history but becomes independently tracked.

```rust
#[auth(current_holder)]
fn split_case(env: &Env, current_holder: Address, case_id: BytesN<32>)
    -> Vec<BytesN<32>>;  // Returns individual bottle IDs

#[auth(current_holder)]
fn assemble_case(env: &Env, current_holder: Address,
                 bottle_ids: Vec<BytesN<32>>) -> BytesN<32>;  // Returns case ID
```

### 3. Expert Credentials Management

Wine authentication experts need verifiable credentials. The
`register_expert` function registers them, but there is no mechanism
for credential revocation, expertise domains (Bordeaux expert vs.
Burgundy expert), or credential expiry.

A composable `Credentials` trait:

```rust
#[contracttrait]
pub trait Credentials: Ownable {
    #[auth(Self::owner)]
    fn issue_credential(env: &Env, expert: Address, domain: Symbol, expiry: u64);

    #[auth(Self::owner)]
    fn revoke_credential(env: &Env, expert: Address);

    fn is_credentialed(env: &Env, expert: Address, domain: Symbol) -> bool;
}
```

### 4. Price History

Fine wine appreciates over time. A price history trait would record every
transaction price, creating a transparent market:

```rust
#[contracttrait]
pub trait PriceHistory {
    fn price_history(env: &Env, bottle_id: BytesN<32>) -> Vec<PriceEntry>;

    fn record_sale(env: &Env, bottle_id: BytesN<32>, price: i128, currency: Symbol);
    // Called automatically during custody transfer with payment
}
```

---

## Evaluation of the OZ Comparison for Provenance Use

### What OZ Does Better for Provenance

1. **Event emission** -- Provenance systems depend on events. Every custody
   transfer, authentication, and storage reading should emit an event. OZ's
   built-in event emission is critical for this. The blog post acknowledges
   this gap -- for provenance, it is the single most important missing
   feature.

2. **Two-step transfers** -- For high-value bottles, custody transfers
   should be two-step: the current holder initiates, the new holder
   accepts. OZ's `transfer_ownership` + `accept_ownership` pattern should
   be adopted for custody transfers.

3. **Error codes** -- When a custody transfer fails, the error should be
   specific: "invalid bottle ID," "not the current holder," "bottle
   already consumed." OZ's namespaced error codes are useful here.

### What soroban-sdk-tools Does Better for Provenance

1. **Sealed auth** -- For anti-counterfeiting, non-overridable auth is
   essential. The sealed macro is the difference between "provenance
   guaranteed by convention" and "provenance guaranteed by code."

2. **Provider swapping** -- Different wine regions need different
   validation rules. The provider pattern handles this elegantly.

3. **Supertrait composition** -- The trait hierarchy (WineOrigin ->
   WineCustody -> WineAuthentication -> WineNFT -> WineAuction) maps
   the natural lifecycle of a bottle. Each trait adds capabilities
   while inheriting guarantees from its parents.

4. **AuthClient** -- Testing that only the chateau can register bottles,
   only the holder can transfer, and only experts can authenticate is
   straightforward with the AuthClient pattern.

---

## Verdict

Wine provenance is a compelling use case for `soroban-sdk-tools`. The
trait composition model maps naturally to the wine lifecycle. The provider
pattern handles regional differences in wine regulation. The sealed auth
pattern provides the anti-counterfeiting guarantee that the industry needs.

The gaps are in event emission (critical for provenance tracking), batch
operations (essential for chateau-scale deployment), and two-step transfers
(necessary for high-value bottle transactions).

If these gaps are filled, `soroban-sdk-tools` could become the foundation
for the wine industry's anti-counterfeiting infrastructure. The financial
incentive is clear: preventing even a fraction of the estimated EUR 2.5
billion annual wine fraud market justifies significant investment in
provenance technology.

**Rating:** 8/10 -- Excellent architectural fit for provenance tracking.
The sealed auth pattern is the key differentiator for anti-counterfeiting.
Needs event emission and batch operations to be production-ready.

---

*"A bottle of Romanee-Conti without provenance is just grape juice in an
expensive package. The #[auth(Self::owner)] on register_bottle is the
difference between a EUR 25,000 bottle and a EUR 25 fraud. Sealed auth
makes that difference permanent."*
