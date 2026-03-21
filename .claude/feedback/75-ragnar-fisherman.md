# Review: Ragnar -- Norwegian Fisherman

**Reviewer:** Ragnar, Norwegian fisherman building fish-to-table supply chain tracking
**Focus:** Perishable goods, temperature logging, catch certificates, sustainability verification
**Date:** 2026-03-21
**Materials Reviewed:**
- `docs/oz-comparison.md`
- `docs/blog-post-composable-contracts.md`
- `examples/trait-test/src/lib.rs`
- `soroban-sdk-tools-macro/src/contract.rs`

---

## Introduction: From the Barents Sea to the Blockchain

I fish cod and king crab in the Barents Sea, out of Hammerfest. My boat is
a 15-meter cutter, and I have been running her for twenty years. Three years
ago, the EU passed the IUU Regulation, and suddenly every fish I catch needs
a paper trail from the ocean to the plate. Catch certificates. Temperature
logs. Customs declarations. Traceability from hook to supermarket shelf.

My cousin in Tromso is a software developer. She convinced me that Soroban
could replace my filing cabinet full of carbon-copy catch certificates. I was
skeptical. But here I am, reading macro source code on a Saturday, because if
this works, it saves me forty hours of paperwork per month.

I am looking at `soroban-sdk-tools` and asking: can I track my fish with this?

---

## 1. The Catch Certificate: Ownable Meets Traceable

Every fish landed in Norway gets a landing declaration (landingsseddel). The
EU catch certificate adds an export layer. The critical data:

- **Vessel:** Registration number, flag state, license
- **Catch:** Species, weight, date, area (ICES zone), gear type
- **Landing:** Port, date, first buyer
- **Chain of custody:** Every handler from landing to retail

### Mapping to the Two-Trait Pattern

```rust
#[contracttrait]
pub trait CatchCertificate {
    fn vessel_owner(env: &Env) -> Address;
    fn current_holder(env: &Env) -> Address;

    #[auth(Self::vessel_owner)]
    fn register_catch(env: &Env, species: Symbol, weight_kg: u32, ices_zone: Symbol);

    #[auth(Self::current_holder)]
    fn transfer_custody(env: &Env, new_holder: Address);
}
```

The two-trait split makes sense here:

- `CatchCertificateInternal`: Pure data operations (store species, weight, zone)
- `CatchCertificate`: Auth-enforced operations (only vessel owner registers,
  only current holder transfers)

The `current_holder` function is like `owner` but for chain-of-custody. As the
fish moves from vessel to landing to processor to distributor to retailer,
`current_holder` changes with each `transfer_custody` call.

### Provider Swap for Different Fisheries

Different fisheries have different requirements:

- **Wild catch (me):** Single vessel, single catch event, chain of custody
- **Aquaculture (salmon farms):** Multiple harvests per facility, batch tracking
- **Pelagic (herring/mackerel):** Large volume, auction at landing

```rust
impl_catch_certificate!(WildCatchContract, WildCatchProvider);
impl_catch_certificate!(AquacultureContract, FarmHarvestProvider);
impl_catch_certificate!(PelagicContract, AuctionLandingProvider);
```

The provider swap means the same trait interface works for all fishery types.
The regulatory authority sees the same API regardless of whether the fish
came from my cutter or a salmon farm in Nordland.

**Assessment: The provider pattern is well-suited for fisheries traceability.**

---

## 2. Temperature Logging: The Cold Chain

Fish spoilage is a function of time and temperature. Norwegian food safety
law (Mattilsynet regulations) requires:

- Fresh fish: 0-4 degrees C from landing to retail
- Frozen fish: -18 degrees C or colder
- Temperature must be logged at every custody transfer point

### The Problem: High-Frequency Data

A temperature logger records every 15 minutes during transport. A 48-hour
truck journey from Hammerfest to Oslo produces 192 readings. Storing each
reading on-chain is expensive.

### The Solution: Attestation Pattern

Instead of storing every reading, store attestations:

```rust
#[contracttrait]
pub trait ColdChain: CatchCertificate {
    fn last_attestation(env: &Env, batch_id: Symbol) -> Option<ColdChainAttestation>;

    #[auth(Self::current_holder)]
    fn attest_cold_chain(
        env: &Env,
        batch_id: Symbol,
        min_temp: i32,
        max_temp: i32,
        readings_hash: BytesN<32>,  // hash of all readings stored off-chain
        duration_hours: u32,
    );

    #[auth(Self::current_holder)]
    fn report_cold_chain_break(
        env: &Env,
        batch_id: Symbol,
        max_temp_recorded: i32,
        duration_minutes: u32,
    );
}
```

The supertrait `ColdChain: CatchCertificate` is correct -- temperature
attestations are meaningless without knowing which batch of fish they
refer to, and that comes from the catch certificate.

### Auth Source for IoT Devices

Temperature loggers are devices, not people. The `#[auth(param)]` variant
handles this:

```rust
#[auth(logger_device)]
fn submit_reading(env: &Env, logger_device: Address, batch_id: Symbol, temp: i32);
```

The `AuthSource::Param` variant in the macro (line 53 of `contract.rs`) maps
to this. The device's address is passed as a parameter and `require_auth()`
is called on it. For Soroban, this means the device needs a Stellar keypair,
which is feasible for IoT gateways.

**Question:** Can a device sign Soroban transactions from a constrained
environment (low-power, intermittent connectivity)? This is a Soroban platform
question, not a `soroban-sdk-tools` question, but the tooling should
accommodate the pattern.

---

## 3. Sustainability Verification: MSC and ASC Certification

The Marine Stewardship Council (MSC) and Aquaculture Stewardship Council (ASC)
certify sustainable fisheries. Retailers increasingly require MSC/ASC
certification in the supply chain.

### The Certification Trait

```rust
#[contracttrait]
pub trait SustainabilityCert: CatchCertificate {
    fn certifier(env: &Env) -> Address;
    fn is_certified(env: &Env, batch_id: Symbol) -> bool;
    fn certification_expiry(env: &Env) -> Option<u64>;

    #[auth(Self::certifier)]
    fn grant_certification(env: &Env, batch_id: Symbol, expiry: u64);

    #[auth(Self::certifier)]
    fn revoke_certification(env: &Env, batch_id: Symbol, reason: Symbol);
}
```

The `certifier` auth source is a third party (MSC auditor), not the vessel
owner or current holder. This demonstrates a strength of the `#[auth]` system:
different methods can have different auth sources within the same trait.

### Provider for Different Certification Bodies

```rust
pub struct MSCProvider;
impl SustainabilityCertInternal for MSCProvider {
    fn certifier(env: &Env) -> Address {
        CertStorage::get_msc_auditor(env).unwrap()
    }
    // ...
}

pub struct ASCProvider;
impl SustainabilityCertInternal for ASCProvider {
    fn certifier(env: &Env) -> Address {
        CertStorage::get_asc_auditor(env).unwrap()
    }
    // ...
}
```

Same trait, different certifiers. The provider pattern works well here.

---

## 4. Regulatory Authorities: Multi-Role Access

Norwegian fisheries involve multiple regulatory bodies:

- **Fiskeridirektoratet** (Directorate of Fisheries): Quotas, vessel licenses
- **Mattilsynet** (Food Safety Authority): Hygiene, temperature
- **Tollvesenet** (Customs): Export declarations
- **EU DG MARE** (for EU exports): IUU catch certificates

Each authority needs different access to different data.

### Current Limitation

The `#[auth]` system allows one auth source per method. But regulatory access
is more nuanced:

```rust
// Directorate can view and modify quotas
#[auth(Self::fisheries_directorate)]
fn update_quota(env: &Env, species: Symbol, remaining_kg: u32);

// Food Safety can view temperature logs and issue holds
#[auth(Self::food_safety_authority)]
fn issue_safety_hold(env: &Env, batch_id: Symbol);

// Customs can view export data and stamp certificates
#[auth(Self::customs_authority)]
fn stamp_export_certificate(env: &Env, batch_id: Symbol, destination: Symbol);
```

Each method has a different auth source. The current `#[auth]` system handles
this correctly. But what about methods that multiple authorities can call?

```rust
// Both Fisheries Directorate AND Food Safety can issue a recall
fn issue_recall(env: &Env, batch_id: Symbol, issuing_authority: Address);
```

This needs either multi-auth or role-based auth. The current system does not
support either at the trait level.

**Recommendation:** Support `#[auth(any: Self::authority_a, Self::authority_b)]`
for methods that can be called by any of multiple authorities.

---

## 5. Perishability and Time Constraints

Fish is perishable. A catch certificate has a limited window of validity:

- Fresh cod: Must be processed within 12 days of catch
- Frozen at sea: Longer window, but thaw date starts the clock
- Live crab: Must reach consumer within 72 hours of landing

### Time-Aware Methods

```rust
#[contracttrait]
pub trait PerishableGoods: CatchCertificate {
    fn catch_timestamp(env: &Env) -> u64;
    fn shelf_life_hours(env: &Env) -> u32;
    fn is_expired(env: &Env) -> bool;

    #[auth(Self::current_holder)]
    fn mark_processed(env: &Env, processing_type: Symbol);
}
```

The `is_expired` method checks `env.ledger().timestamp()` against
`catch_timestamp() + shelf_life_hours * 3600`. This is a read-only method
with no auth -- anyone in the supply chain should be able to check expiry.

### Guard Conditions

For perishable goods, certain operations should be blocked after expiry:

```rust
impl PerishableGoodsInternal for FreshFishProvider {
    fn mark_processed(env: &Env, processing_type: Symbol) {
        assert!(!Self::is_expired(env), "product has expired");
        // ... processing logic
    }
}
```

This is similar to the pause check in the `PausableToken` provider example.
The provider handles the guard condition internally. This works, but it
means every provider must remember to check expiry.

**Recommendation:** Consider a `#[guard(Self::is_not_expired)]` annotation
similar to `#[auth]` but for boolean guard conditions:

```rust
#[guard(Self::is_not_expired)]
#[auth(Self::current_holder)]
fn transfer_custody(env: &Env, new_holder: Address);
```

This would structurally enforce that expired products cannot be transferred.
Currently, this guard would have to be in every provider, which is fragile.

---

## 6. The Event Emission Gap (Again)

For fisheries traceability, events ARE the product. The entire point of the
system is to produce an auditable chain of events:

```
CatchRegistered { vessel: "N-1234-H", species: "COD", weight_kg: 2400, zone: "I" }
CustodyTransferred { batch: "B-2024-1847", from: "N-1234-H", to: "PROCESSOR-AS" }
ColdChainAttested { batch: "B-2024-1847", min_temp: 1, max_temp: 3, hours: 6 }
ProcessingCompleted { batch: "B-2024-1847", type: "FILLET", output_kg: 1680 }
ExportCertified { batch: "B-2024-1847", destination: "EU", customs_stamp: "..." }
```

A retailer scanning a QR code on a fish fillet should be able to trace back
through these events to the original catch. Without auto-generated events,
this chain depends on every provider developer remembering to emit them.

**Assessment: For supply chain applications, event emission is not a nice-to-have.
It is the core value proposition.**

---

## 7. Batch Tracking and the Provider Pattern

Fish is not tracked individually (except for high-value species like bluefin
tuna). It is tracked in batches. A batch can be:

- Split: 2400 kg of cod becomes 1200 kg to one processor, 1200 kg to another
- Merged: Multiple catches combined into one processing batch
- Transformed: 2400 kg of whole cod becomes 1680 kg of fillets

### Split and Merge Operations

```rust
#[contracttrait]
pub trait BatchTracking: CatchCertificate {
    #[auth(Self::current_holder)]
    fn split_batch(
        env: &Env,
        source_batch: Symbol,
        new_batch_a: Symbol,
        weight_a_kg: u32,
        new_batch_b: Symbol,
        weight_b_kg: u32,
    );

    #[auth(Self::current_holder)]
    fn merge_batches(
        env: &Env,
        source_a: Symbol,
        source_b: Symbol,
        merged_batch: Symbol,
    );
}
```

The `#[auth(Self::current_holder)]` ensures only the current holder can split
or merge. But there is a complication: after a split, who is the current holder
of the new batches? They should inherit the current holder of the source batch.

This is business logic that belongs in the provider:

```rust
impl BatchTrackingInternal for FisheryBatchProvider {
    fn split_batch(env: &Env, source: Symbol, new_a: Symbol, w_a: u32, new_b: Symbol, w_b: u32) {
        let source_weight = BatchStorage::get_weight(env, &source);
        assert!(w_a + w_b == source_weight, "weights must sum to source");

        let holder = CatchCertificateInternal::current_holder(env);
        BatchStorage::create(env, &new_a, w_a, &holder);
        BatchStorage::create(env, &new_b, w_b, &holder);
        BatchStorage::archive(env, &source);
    }
}
```

The provider pattern handles this correctly. The auth is structural (outer
trait), the business logic is in the provider.

---

## 8. Offline and Intermittent Connectivity

### The Reality of Fishing

I am at sea for 5-7 days at a time. During that time, I have satellite
connectivity (Iridium) for about 2 hours per day, and it costs money per
kilobyte. Submitting Soroban transactions from the boat is not always
possible.

### Offline-First Pattern

The catch registration could use a deferred submission model:

1. Catch data is recorded on a local device (tablet with the app)
2. Data is signed with my Stellar keypair (offline)
3. When connectivity is available, the signed transaction is submitted
4. The contract verifies the signature and records the catch

The `#[auth]` system supports this because `require_auth()` in Soroban
works with pre-signed transactions. The fisherman signs the transaction
data on the boat, and the signed payload is submitted later by any relay.

**Observation:** The `AuthClient` testing pattern could be extended to
test offline signing scenarios:

```rust
// Simulate offline signing
let signed_tx = auth_client.register_catch(&species, &weight, &zone)
    .sign(&vessel_owner_keypair)  // sign with Ed25519 keypair
    .to_signed_payload();

// Simulate later submission
submit_signed_payload(&env, signed_tx);
```

The blog post mentions `.sign(&keypair)` as a feature of `AuthClient`.
This is exactly what I need.

---

## 9. Cross-Border Complications

Norwegian fish exported to the EU passes through:

1. Norwegian customs (Tollvesenet)
2. EU border inspection (Traces NT system)
3. Destination country's food safety authority

Each jurisdiction has its own requirements. The catch certificate must be
stamped by each authority in sequence.

### Sequential Auth Pattern

```rust
#[contracttrait]
pub trait CrossBorderExport: CatchCertificate {
    fn export_status(env: &Env, batch_id: Symbol) -> ExportStatus;

    #[auth(Self::origin_customs)]
    fn stamp_origin(env: &Env, batch_id: Symbol);

    #[auth(Self::border_inspector)]
    fn stamp_border_inspection(env: &Env, batch_id: Symbol, inspection_result: Symbol);

    #[auth(Self::destination_authority)]
    fn stamp_destination(env: &Env, batch_id: Symbol);
}
```

Each stamp must happen in order. The provider enforces the sequence:

```rust
impl CrossBorderExportInternal for EUExportProvider {
    fn stamp_border_inspection(env: &Env, batch_id: Symbol, result: Symbol) {
        let status = ExportStorage::get_status(env, &batch_id);
        assert!(status == ExportStatus::OriginStamped, "origin stamp required first");
        ExportStorage::set_status(env, &batch_id, ExportStatus::BorderInspected);
    }
}
```

The `#[auth]` handles WHO can stamp. The provider handles WHEN they can stamp
(sequence enforcement). This separation is clean and correct.

---

## 10. The Sealed Macro for Regulatory Contracts

For fisheries contracts that interface with government systems, the sealed
macro is essential:

```rust
impl_catch_certificate!(NorwegianFishery, RegulatedCatchProvider);
```

When the Fiskeridirektoratet audits my system, they need to verify that:
1. Only registered vessels can submit catch data
2. Only licensed buyers can accept custody
3. Temperature attestations cannot be falsified
4. Export certificates cannot be forged

The sealed macro provides guarantee #1-4 at the contract level. Auth cannot
be overridden. This is the equivalent of the tamper-evident seal on a
paper catch certificate.

---

## 11. Testing: The AuthClient for Regulatory Testing

The AuthClient is particularly valuable for testing regulatory scenarios:

```rust
#[test]
fn test_only_vessel_owner_can_register_catch() {
    let auth = CatchCertificateAuthClient::new(&env, &contract_id);

    // Vessel owner can register
    auth.register_catch(&species, &weight, &zone)
        .authorize(&vessel_owner)
        .invoke();

    // Random person cannot
    let result = auth.register_catch(&species, &weight, &zone)
        .authorize(&random_person)
        .try_invoke();
    assert!(result.is_err());
}

#[test]
fn test_custody_transfer_chain() {
    let auth = CatchCertificateAuthClient::new(&env, &contract_id);

    // Vessel -> Processor
    auth.transfer_custody(&processor)
        .authorize(&vessel_owner)
        .invoke();

    // Processor -> Distributor (vessel owner can NO LONGER transfer)
    let result = auth.transfer_custody(&distributor)
        .authorize(&vessel_owner)
        .try_invoke();
    assert!(result.is_err());  // vessel owner is no longer current_holder

    auth.transfer_custody(&distributor)
        .authorize(&processor)
        .invoke();
}
```

This tests the complete chain of custody with precise authorization at each
step. `mock_all_auths()` would not catch the custody transfer bug (where
the vessel owner should lose transfer rights after handing off).

**Assessment: AuthClient is essential for supply chain testing.**

---

## 12. Practical Gaps for Fisheries

| Gap | Impact | Explanation |
|-----|--------|-------------|
| Event emission | Critical | The traceability chain IS the events |
| Guard conditions (`#[guard]`) | High | Expiry checks, sequence enforcement |
| Multi-auth (`#[auth(any: ...)]`) | High | Multiple authorities can issue recalls |
| Batch operations (split/merge) | Medium | Standard in fisheries, complex state |
| Off-chain data references | Medium | Temperature logs too large for on-chain |
| Time-based expiry | Medium | Perishable goods fundamental requirement |
| Offline signing support | Medium | Fishing boats have intermittent connectivity |
| Cross-contract traceability | Low | Linking catch certs across contracts |

---

## 13. What Works Well

1. **Provider pattern for different fishery types.** Wild catch, aquaculture,
   and pelagic fisheries all have different workflows but the same regulatory
   interface. Provider swap is exactly right for this.

2. **Sealed macro for regulatory contracts.** Government authorities need
   assurance that auth cannot be bypassed. The sealed macro provides this.

3. **Supertrait composition for supply chain layers.** Cold chain extends
   catch certificate. Sustainability certification extends catch certificate.
   Export extends catch certificate. The dependency graph matches reality.

4. **Param-based auth for IoT devices.** Temperature loggers, GPS trackers,
   and weighing scales can be authorized as Soroban addresses. The
   `#[auth(device)]` pattern handles this.

5. **AuthClient for regulatory testing.** Testing exact authorization
   scenarios is essential for compliance.

---

## 14. Closing Thoughts

I have been fishing the Barents Sea in all weather. I have hauled nets in
a Force 9 gale, and I have sat through flat-calm days where the sea looks
like glass. Building software for fisheries has the same range -- some days
the technology cooperates, and some days you want to throw the laptop
overboard.

This `soroban-sdk-tools` system gets the architecture right for supply chain
tracking. The two-trait split maps naturally to "what data exists" and "who
can change it." The provider pattern handles the diversity of fishery types.
The sealed macro gives regulators confidence.

What it needs is the practical finishing:
- Events (the whole point of traceability)
- Guard conditions (expiry, sequence enforcement)
- Multi-auth (multiple authorities)

Without events, I have a contract that enforces authorization but produces
no audit trail. That is like locking the filing cabinet but never putting
any papers in it.

Add events, and I will deploy this for my fleet.

---

**Overall Assessment:** Strong architectural fit for supply chain traceability.
The provider pattern, sealed macro, and supertrait composition map well to
fisheries regulatory requirements. Event emission is the critical gap.

**Verdict:** Good boat. Good nets. Needs a fish finder. (The fish finder
is event emission.)
