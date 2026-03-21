---
reviewer: Earl Whitfield III
role: Master Distiller & Regulatory Compliance Officer
domain: Bourbon Production, TTB Compliance, Barrel Tracking
date: 2026-03-21
focus: TTB regulatory compliance, barrel-to-bottle traceability, bonded warehouse
---

# Review: soroban-sdk-tools -- Bourbon Barrel Tracking & TTB Compliance

## Context

I am a third-generation bourbon distiller and the compliance officer for
our operation. The Alcohol and Tobacco Tax and Trade Bureau (TTB) requires
meticulous tracking of every barrel from the moment grain enters the
distillery to the moment the bottle reaches a consumer. Federal regulations
(27 CFR Parts 19 and 24) mandate:

- Daily production records
- Barrel entry proof and fill dates
- Warehouse receipts with rick and tier locations
- Gauge records at entry, during aging, and at dumping
- Tax determination at time of withdrawal
- Chain of custody for bonded transfers

A barrel of bourbon in a federally bonded warehouse is, quite literally,
government property until taxes are paid. The TTB can audit at any time,
and discrepancies result in fines, license revocation, or criminal charges.

I evaluate `soroban-sdk-tools` for whether it can enforce the regulatory
compliance requirements that govern every barrel in our warehouse.

## What Maps Well

### 1. The Sealed Macro for Regulatory Integrity

The `impl_barrel_tracking!` sealed macro is the most compelling feature
for TTB compliance. If barrel records are in a contract where auth cannot
be overridden, the TTB can verify that:

- Only authorized gaugers can record proof readings
- Only bonded warehouse operators can transfer barrels
- Only TTB-registered agents can authorize tax withdrawals
- No one can retroactively modify entry records

This is stronger than our current paper-based system, where records CAN
be altered (and occasionally are, leading to discrepancies during audits).

### 2. Multi-Role Auth for Distillery Operations

A bourbon distillery has several regulated roles:

```rust
#[contracttrait]
pub trait BarrelTracking: Ownable {
    fn distiller(env: &Env) -> Address;       // DSP permit holder
    fn gauger(env: &Env) -> Address;          // Certified gauger
    fn warehouse_keeper(env: &Env) -> Address; // Bonded warehouse operator

    #[auth(Self::distiller)]
    fn record_production(env: &Env, batch_id: Symbol, grain_bill: GrainBill,
                         proof_gallons: i128, date: u64);

    #[auth(Self::gauger)]
    fn record_entry_gauge(env: &Env, barrel_id: BytesN<32>,
                          entry_proof: u32, fill_date: u64);

    #[auth(Self::warehouse_keeper)]
    fn assign_warehouse_location(env: &Env, barrel_id: BytesN<32>,
                                  warehouse: Symbol, rick: u32, tier: u32);

    #[auth(Self::distiller)]
    fn request_tax_withdrawal(env: &Env, barrel_ids: Vec<BytesN<32>>,
                               withdrawal_type: Symbol);
}
```

The `#[auth(Self::method)]` pattern correctly assigns each regulatory
function to its authorized role. The TTB audit trail shows WHO performed
each action AND that the macro-enforced auth cannot be bypassed.

### 3. Provider Pattern for Different Permit Types

Different distillery permits have different production rights:

```rust
pub struct DSP_Distiller;  // Distilled Spirits Permit - full production
pub struct DSP_Processor;  // Processing only (no distillation)
pub struct DSP_Warehouse;  // Storage only

impl BarrelTrackingInternal for DSP_Distiller {
    fn record_production(env: &Env, batch_id: Symbol, grain_bill: GrainBill,
                         proof_gallons: i128, date: u64) {
        // Full distiller: can record production
        ProductionStorage::record(env, &batch_id, &grain_bill, proof_gallons, date);
    }
}

impl BarrelTrackingInternal for DSP_Processor {
    fn record_production(env: &Env, _batch_id: Symbol, _grain_bill: GrainBill,
                         _proof_gallons: i128, _date: u64) {
        panic!("Processors cannot record distillation production");
    }
}
```

Swapping `impl_barrel_tracking!(MyDistillery, DSP_Distiller)` to
`impl_barrel_tracking!(MyDistillery, DSP_Processor)` changes the
permitted operations. The TTB could verify the provider matches the
permit type.

## Concerns

### 1. No Append-Only Record Guarantee

TTB regulations require that production and gauge records cannot be
modified after entry. The current Provider pattern allows providers
to overwrite storage:

```rust
fn record_entry_gauge(env: &Env, barrel_id: BytesN<32>,
                      entry_proof: u32, fill_date: u64) {
    // This OVERWRITES any existing record
    BarrelStorage::set_gauge(env, &barrel_id, entry_proof, fill_date);
}
```

A malicious or buggy provider could overwrite a previous gauge reading.
TTB regulations require that corrections be recorded as separate entries
with the original preserved.

**Suggestion**: Support an `#[append_only]` attribute:

```rust
#[auth(Self::gauger)]
#[append_only]  // generates: assert no existing record for this key
fn record_entry_gauge(env: &Env, barrel_id: BytesN<32>,
                      entry_proof: u32, fill_date: u64);
```

Or at minimum, document the append-only pattern for providers:

```rust
fn record_entry_gauge(env: &Env, barrel_id: BytesN<32>,
                      entry_proof: u32, fill_date: u64) {
    assert!(
        !BarrelStorage::has_gauge(env, &barrel_id),
        "gauge record already exists -- use record_correction"
    );
    BarrelStorage::set_gauge(env, &barrel_id, entry_proof, fill_date);
}
```

### 2. Angel's Share Tracking

Bourbon loses approximately 2-4% volume per year to evaporation (the
"angel's share"). TTB allows for this loss but requires it to be
documented. A regauge must account for the difference between entry
proof gallons and current proof gallons.

This is a read-modify pattern that the Provider handles:

```rust
fn record_regauge(env: &Env, barrel_id: BytesN<32>,
                  current_proof: u32, current_gallons: i128) {
    let entry = BarrelStorage::get_gauge(env, &barrel_id);
    let loss_gallons = entry.proof_gallons - current_gallons;
    let loss_pct = (loss_gallons * 10000) / entry.proof_gallons;

    // TTB allows up to 4% annual loss; flag excessive loss
    let age_years = (env.ledger().timestamp() - entry.fill_date) / 31_536_000;
    let max_loss_pct = age_years as i128 * 400;  // 4% per year in basis points
    if loss_pct > max_loss_pct {
        RegaugeStorage::flag_excessive_loss(env, &barrel_id, loss_pct);
    }

    RegaugeStorage::record(env, &barrel_id, current_proof, current_gallons);
}
```

The Provider is the right place for this domain logic. But the excessive
loss flag should trigger an event (for audit trail purposes) and
potentially require additional authorization (TTB agent review).

### 3. Bonded Transfer Between Facilities

When barrels are transferred between bonded warehouses (common during
sourcing or contract aging), TTB requires Form 5110.40 (Transfer in
Bond Record). Both the shipping and receiving warehouses must document
the transfer.

This is a cross-contract operation:

```rust
#[contracttrait]
pub trait BondedTransfer {
    fn shipping_warehouse(env: &Env) -> Address;
    fn receiving_warehouse(env: &Env) -> Address;

    #[auth(Self::shipping_warehouse)]
    fn initiate_transfer(env: &Env, barrel_ids: Vec<BytesN<32>>,
                         receiving_dsp: Symbol);

    #[auth(Self::receiving_warehouse)]
    fn confirm_receipt(env: &Env, barrel_ids: Vec<BytesN<32>>,
                       condition: Symbol);
}
```

The two-step transfer pattern (initiate + confirm) maps to the TTB
requirement that both parties document the transfer. The `#[auth]`
pattern correctly assigns authorization to each facility.

### 4. Tax Determination Timing

Federal excise tax on bourbon is due at the time of "withdrawal from
bond" -- when the bourbon leaves the bonded warehouse for commercial
sale. The tax rate depends on proof gallons at the time of withdrawal.

The `request_tax_withdrawal` method should interact with a tax
calculation oracle or on-chain schedule:

```rust
fn request_tax_withdrawal(env: &Env, barrel_ids: Vec<BytesN<32>>,
                           withdrawal_type: Symbol) {
    let mut total_tax = 0i128;
    for barrel_id in barrel_ids.iter() {
        let gauge = BarrelStorage::get_latest_gauge(env, &barrel_id);
        let tax = calculate_excise_tax(gauge.proof_gallons, &withdrawal_type);
        total_tax += tax;
        BarrelStorage::mark_withdrawn(env, &barrel_id, &withdrawal_type);
    }
    TaxStorage::record_liability(env, total_tax);
}
```

The batch operation (multiple barrels in one withdrawal) is important
for efficiency -- a typical bottling run dumps 50-200 barrels.

### 5. Proof Reading Precision

TTB requires proof readings to the nearest tenth of a degree. The
current trait uses `u32` for proof values. At 80 proof = 40% ABV,
a tenth of a degree is 0.1 proof.

Using `u32` with implied decimal (e.g., 800 = 80.0 proof) works but
should be documented. Alternatively, use `i128` with explicit scaling:

```rust
fn record_entry_gauge(env: &Env, barrel_id: BytesN<32>,
                      entry_proof_tenths: u32,  // 1250 = 125.0 proof
                      fill_date: u64);
```

### 6. Event Emission for Audit Trail

TTB audits require a complete chronological record of every barrel
operation. The lack of automatic event emission in the generated outer
trait means every provider must manually emit events. If a provider
developer forgets to emit an event for a gauge reading, the audit
trail has a gap -- which is a compliance violation.

**This is the single most critical gap for regulatory use cases.**

```rust
// Generated outer trait should emit:
fn record_entry_gauge(env: &Env, barrel_id: BytesN<32>,
                      entry_proof: u32, fill_date: u64) {
    let __auth_addr = Self::Provider::gauger(env);
    __auth_addr.require_auth();
    Self::Provider::record_entry_gauge(env, barrel_id.clone(), entry_proof, fill_date);
    // THIS IS MISSING:
    env.events().publish(
        ("BarrelTracking", "entry_gauge"),
        (barrel_id, entry_proof, fill_date)
    );
}
```

## Bourbon-Specific Contract

```rust
#[contracttrait]
pub trait BourbonBarrel: Ownable {
    fn distiller(env: &Env) -> Address;
    fn gauger(env: &Env) -> Address;
    fn warehouse_keeper(env: &Env) -> Address;

    #[auth(Self::distiller)]
    fn record_mash_bill(env: &Env, batch: Symbol, corn_pct: u32,
                        rye_pct: u32, barley_pct: u32);

    #[auth(Self::gauger)]
    fn record_entry(env: &Env, barrel_id: BytesN<32>, entry_proof: u32,
                    fill_date: u64, barrel_type: Symbol);

    #[auth(Self::warehouse_keeper)]
    fn assign_location(env: &Env, barrel_id: BytesN<32>,
                       warehouse: Symbol, rick: u32, tier: u32);

    #[auth(Self::gauger)]
    fn record_regauge(env: &Env, barrel_id: BytesN<32>,
                      current_proof: u32, current_gallons: i128);

    #[auth(Self::distiller)]
    fn dump_for_bottling(env: &Env, barrel_ids: Vec<BytesN<32>>,
                         bottling_proof: u32);

    fn barrel_info(env: &Env, barrel_id: BytesN<32>) -> BarrelInfo;
    fn warehouse_inventory(env: &Env, warehouse: Symbol) -> Vec<BytesN<32>>;
}

impl_bourbon_barrel!(WhitfieldDistillery, DSP_Distiller);
```

Note: `barrel_type` must be "new charred oak" for bourbon (27 CFR 5.22).
The Provider validates this. The sealed macro ensures barrel_type
validation cannot be bypassed.

## Summary

soroban-sdk-tools is well-suited for bourbon barrel tracking and TTB
compliance. The multi-role auth model (distiller, gauger, warehouse
keeper) maps directly to TTB-regulated roles. The sealed macro provides
tamper-proof records that satisfy audit requirements. The Provider
pattern accommodates different permit types. The critical gap is
automatic event emission in the generated outer trait -- without it,
every provider must manually emit audit events, and any omission is
a compliance violation. Secondary concerns are append-only record
guarantees and documentation of precision requirements for proof
readings. If event emission is added to the generated code, this
would be the strongest compliance framework I have evaluated for
spirit tracking.
