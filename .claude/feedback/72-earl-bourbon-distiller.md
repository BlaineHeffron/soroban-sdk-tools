# Review: Earl -- Kentucky Bourbon Distiller

**Reviewer:** Earl, bourbon distiller who tracks barrels with blockchain
**Focus:** Aging process tracking, barrel provenance, regulatory requirements (TTB)
**Date:** 2026-03-21
**Materials Reviewed:**
- `docs/oz-comparison.md`
- `docs/blog-post-composable-contracts.md`
- `examples/trait-test/src/lib.rs`
- `soroban-sdk-tools-macro/src/contract.rs`

---

## Opening Remarks

I have been distilling bourbon in Bardstown, Kentucky for thirty-two years. My
grandfather's grandfather started this operation, and every barrel we have ever
filled is tracked in a leather-bound ledger that sits in a fireproof vault. When
my nephew convinced me to put our barrel tracking on Soroban two years ago, I
told him, "Son, a blockchain is just a ledger that nobody can tear a page out of."

He was not amused. But he was right that it works.

Now I am looking at this `soroban-sdk-tools` system, and I have thoughts. Mostly
about whether I could use this to track my barrels from the still to the glass
and keep the Alcohol and Tobacco Tax and Trade Bureau (TTB) happy at the same time.

---

## 1. The Ownable Pattern and Barrel Ownership

Every barrel of bourbon has an owner. When it is aging in my rickhouse, I own it.
When I sell it to a distributor, they own it. When a whiskey fund buys futures on
my barrels, things get complicated.

### Direct Application

The `Ownable` trait maps directly to barrel ownership:

```rust
#[contracttrait]
pub trait BarrelOwnable {
    fn owner(env: &Env) -> Address;

    #[auth(Self::owner)]
    fn transfer_barrel(env: &Env, new_owner: Address);
}
```

This is clean. The `#[auth(Self::owner)]` means only the current barrel owner can
transfer it. Nobody is going to drive up to my rickhouse and claim a barrel that
is not theirs.

### The Provider Swap Is Useful Here

Different ownership models matter in bourbon:

- **SingleOwner**: Standard barrel ownership. I own it, I sell it, done.
- **FractionalOwner**: Whiskey funds buy shares of barrels. Multiple owners.
- **ConsignmentOwner**: I keep the barrel in my rickhouse, but a buyer has
  paid for it and it is aging on their behalf.

Being able to swap `type Provider = SingleOwner` to `type Provider = FractionalOwner`
without rewriting the contract is exactly what I need. In OZ's system, I would have
to rewrite the ownership module. That is like rebuilding the rickhouse when you
just want to change how you label the barrels.

---

## 2. Barrel Aging: The Pausable Pattern

Here is where things get interesting. Bourbon must age a minimum of two years in
new charred oak barrels to be called "straight bourbon." Four years to avoid an
age statement on the label. The TTB does not mess around with this.

### Pause as "Barrel Hold"

The `Pausable` trait could represent a regulatory hold on a barrel:

```rust
#[contracttrait]
pub trait BarrelHold: BarrelOwnable {
    fn is_held(env: &Env) -> bool;

    #[auth(Self::owner)]
    fn place_hold(env: &Env);

    #[auth(Self::owner)]
    fn release_hold(env: &Env);
}
```

A held barrel cannot be transferred, sold, or bottled. This is useful for:
- TTB compliance holds (pending inspection)
- Quality holds (sample tested poorly)
- Legal holds (ownership disputes)

The supertrait composition (`BarrelHold: BarrelOwnable`) is correct here --
you cannot place a hold on a barrel if you do not own it. The
`#[auth(Self::owner)]` on `place_hold` and `release_hold` ensures this.

### What Is Missing: Time-Based Logic

Bourbon aging is fundamentally about time. I need:

```rust
fn age_in_days(env: &Env) -> u64;
fn meets_minimum_age(env: &Env) -> bool;  // >= 730 days for straight bourbon
fn meets_label_age(env: &Env) -> bool;    // >= 1460 days to skip age statement
```

The current `#[contracttrait]` system does not have any notion of time-dependent
state. In Soroban, time is accessed through `env.ledger().timestamp()`, which
gives the current ledger close time. This is fine for blockchain time, but barrel
aging is measured in calendar days, not ledger slots.

**Recommendation:** The provider pattern should explicitly support time-aware
state. A `TimestampedProvider` could be a standard component:

```rust
impl BarrelAgingInternal for TimestampedBarrel {
    fn age_in_days(env: &Env) -> u64 {
        let fill_date = BarrelStorage::get_fill_date(env);
        let now = env.ledger().timestamp();
        (now - fill_date) / 86400
    }
}
```

---

## 3. TTB Regulatory Requirements

The TTB requires the following for every barrel of bourbon:

1. **DSP Number**: Distilled Spirits Plant registration number
2. **Serial Number**: Unique barrel identifier
3. **Fill Date**: When the barrel was filled
4. **Proof at Fill**: Alcohol content at filling (max 125 proof for bourbon)
5. **Proof at Dump**: Alcohol content when emptied
6. **Barrel Type**: New charred oak (required for bourbon)
7. **Mash Bill**: Grain recipe (min 51% corn for bourbon)
8. **Warehouse Location**: Rickhouse, tier, rick position

### How the Two-Trait Split Helps

The `Internal` / `Outer` split is useful for regulatory compliance:

- **BarrelRegistryInternal**: Stores and retrieves barrel data. Pure data operations.
  This is what the TTB auditor reviews -- "show me the data layer."
- **BarrelRegistry (outer)**: Auth-enforced operations. Only the DSP owner can
  register new barrels. Only authorized inspectors can update proof readings.

The separation means I can show the TTB the `Internal` trait and say, "Here is
every piece of data we track," and then show the `Outer` trait and say, "Here is
who can change it and under what authorization."

### Auth Sources for Regulatory Actors

The `#[auth]` system needs to handle multiple auth sources:

```rust
#[contracttrait]
pub trait BarrelRegistry {
    fn dsp_owner(env: &Env) -> Address;
    fn ttb_inspector(env: &Env) -> Address;

    #[auth(Self::dsp_owner)]
    fn register_barrel(env: &Env, serial: Symbol, fill_date: u64, proof: u32);

    #[auth(Self::ttb_inspector)]
    fn record_inspection(env: &Env, serial: Symbol, proof_reading: u32, notes: String);

    #[auth(Self::dsp_owner)]
    fn dump_barrel(env: &Env, serial: Symbol, proof_at_dump: u32);
}
```

The current `#[auth]` system supports this. Each method can have a different
auth source. This is correct for regulatory environments where different
actors have different permissions.

### What I Need That Is Not Here

**Multi-party auth.** Some TTB operations require both the DSP owner AND the
inspector to sign off. For example, changing a barrel's classification from
bourbon to "spirit" (if it does not meet the 51% corn requirement). The
current `#[auth]` only supports a single auth source per method.

**Recommendation:** Support `#[auth(Self::dsp_owner, Self::ttb_inspector)]`
for multi-party authorization. The generated code would call `require_auth()`
on both addresses.

---

## 4. Provenance Chain: Transfer History

When a barrel changes hands, the TTB needs to know:
- Who owned it at every point
- When each transfer occurred
- Whether any regulatory holds were in effect during transfer

### Event Emission Gap

The OZ comparison document notes that OZ emits events for every state change.
The blog post acknowledges this is something `soroban-sdk-tools` should adopt.

For barrel provenance, events are not optional. They ARE the provenance chain:

```
BarrelRegistered { serial: "B2024-1847", dsp: "DSP-KY-20001", fill_date: 1711234567 }
BarrelTransferred { serial: "B2024-1847", from: "DSP-KY-20001", to: "DIST-OH-44012" }
BarrelInspected { serial: "B2024-1847", inspector: "TTB-AGENT-J7", proof: 118 }
BarrelDumped { serial: "B2024-1847", proof_at_dump: 112, age_days: 1461 }
```

Without events in the generated trait, I have to remember to emit them in every
provider. That is like asking every employee to remember to write in the ledger.
Someone will forget. And the TTB will find out.

**Strong Recommendation:** Generate event emission in the outer trait's default
methods. The `#[contracttrait]` macro knows the method name and parameters. It
should emit a standardized event before or after delegating to the provider.

---

## 5. The Sealed Macro and Regulatory Confidence

The `impl_{trait_snake}!` sealed macro is the single most important feature for
regulatory compliance.

When a TTB auditor asks, "Can anyone modify barrel records without authorization?"
I need to be able to say "No" with absolute certainty. With OZ's overridable
trait defaults, I would have to say, "Not unless someone overrides the default
method." That "unless" is a problem.

With the sealed macro:

```rust
impl_barrel_registry!(MyDSP, RegulatedBarrelProvider);
```

The auth checks are baked into inherent methods. They cannot be overridden. This
is the equivalent of a physical lock on the rickhouse door -- not a "please
knock" sign.

**Assessment:** The sealed macro is exactly what regulated industries need.

---

## 6. Temperature and Environment Logging

Bourbon aging is affected by:
- Temperature cycles (hot summers expand the barrel, pushing whiskey into the
  wood; cold winters contract it, pulling flavor out)
- Humidity levels
- Rickhouse tier (higher tiers are hotter, age faster)

### IoT Integration Pattern

A provider could integrate with IoT temperature sensors:

```rust
impl BarrelEnvironmentInternal for IoTBarrelProvider {
    fn log_temperature(env: &Env, barrel: Symbol, temp_f: i32, humidity: u32) {
        let timestamp = env.ledger().timestamp();
        BarrelEnvironmentStorage::append_reading(env, &barrel, timestamp, temp_f, humidity);
    }

    fn average_temperature(env: &Env, barrel: Symbol) -> i32 {
        BarrelEnvironmentStorage::get_average_temp(env, &barrel)
    }
}
```

The `#[auth]` for temperature logging is interesting. The sensor itself should
be authorized (to prevent spoofed readings), but it is a device, not a person.
Soroban supports contract-to-contract auth, which maps to this use case.

**Observation:** The `AuthSource::Param` variant in the macro (`#[auth(sensor)]`)
is useful for IoT scenarios where the auth entity is passed as a parameter
rather than resolved from state.

---

## 7. Barrel NFTs and the Provider Pattern

Each barrel is unique. This maps to NFTs. The provider pattern would allow:

- **StandardBarrelNFT**: Basic barrel token with metadata
- **FractionalBarrelNFT**: ERC-1155-style fractional ownership
- **MaturationBarrelNFT**: NFT whose metadata updates as the barrel ages

Swapping providers here means the same barrel registry contract can serve
different business models without redeployment. A small distillery might
use `StandardBarrelNFT`, while a whiskey fund might use `FractionalBarrelNFT`.

This is genuinely useful. Redeploying contracts in a regulated environment
requires TTB notification and review. Being able to change the ownership
model via a provider swap that does not change the contract address is
a significant compliance advantage.

---

## 8. The AuthClient for Testing

The `AuthClient` pattern is useful for testing regulatory scenarios:

```rust
let ttb_client = BarrelRegistryAuthClient::new(&env, &contract_id);

// Verify that only the DSP owner can register barrels
ttb_client.register_barrel(&serial, &fill_date, &proof)
    .authorize(&dsp_owner)
    .invoke();

// Verify that a random address CANNOT register barrels
let result = ttb_client.register_barrel(&serial, &fill_date, &proof)
    .authorize(&random_person)
    .try_invoke();
assert!(result.is_err());
```

This is miles better than `mock_all_auths()`. When the TTB asks "did you test
your authorization controls?", I want to show them these tests, not a test
suite that disables auth entirely.

---

## 9. Cross-State Transfer Complications

Bourbon law varies by state. Kentucky has different tax rules than Tennessee
(which has its own "Tennessee whiskey" requirements -- the Lincoln County
Process). Transferring barrels across state lines involves:

- Federal excise tax (currently $13.50/proof gallon)
- State excise taxes (vary wildly)
- Permits and bonds

The `transfer_ownership` method is too simple for this. A real barrel transfer
needs:

```rust
#[auth(Self::owner)]
fn initiate_transfer(env: &Env, new_owner: Address, destination_state: Symbol);

// No auth -- anyone can check transfer status
fn transfer_status(env: &Env, transfer_id: u64) -> TransferStatus;

// Multi-party: both old and new owner must confirm
fn confirm_transfer(env: &Env, transfer_id: u64);
```

This is the two-step transfer pattern that OZ already implements. The blog post
acknowledges that `soroban-sdk-tools` should adopt this. I agree.

**Recommendation:** Provide a standard `TwoStepTransfer` provider that implements
the initiate/accept pattern. This is not just good security -- it is required
for regulatory compliance in any industry where ownership transfer has legal
implications.

---

## 10. Mash Bill Verification and Immutability

A bourbon's mash bill (grain recipe) is set at filling and cannot change.
The TTB requires that it contain at least 51% corn. This is an immutability
constraint.

The current system has no concept of immutable fields. Once set, `owner` can
be changed via `transfer_ownership`. But some fields -- like `fill_date`,
`mash_bill`, `original_proof` -- must be write-once.

**Recommendation:** Support `#[immutable]` annotations on trait methods:

```rust
#[contracttrait]
pub trait BarrelMetadata {
    #[immutable]
    fn fill_date(env: &Env) -> u64;

    #[immutable]
    fn mash_bill(env: &Env) -> MashBill;

    fn proof_reading(env: &Env) -> u32;  // mutable -- changes as barrel ages
}
```

The macro could generate set methods only for mutable fields, and the
Internal trait would enforce write-once semantics for immutable ones.

---

## 11. Practical Assessment

### What Works for Bourbon Tracking

| Feature | Bourbon Use Case | Assessment |
|---------|-----------------|------------|
| `#[auth(Self::owner)]` | Only barrel owner can transfer | Excellent |
| Provider swap | Different ownership models | Excellent |
| Sealed macro | Regulatory confidence | Essential |
| Supertrait composition | Holds depend on ownership | Correct |
| AuthClient testing | TTB compliance testing | Superior to OZ |
| `#[auth(param)]` | IoT sensor auth | Useful |

### What Is Missing for Bourbon Tracking

| Gap | Impact | Priority |
|-----|--------|----------|
| Event emission in generated code | Provenance chain breaks | Critical |
| Multi-party auth | TTB joint sign-off | High |
| Two-step transfers | Regulatory compliance | High |
| Time-based state | Aging verification | High |
| Immutable fields | Mash bill / fill date | Medium |
| Write-once storage pattern | Barrel registration | Medium |
| Role-based access (beyond owner) | TTB inspectors, lab techs | Medium |

---

## 12. Closing Thoughts

I have been making bourbon longer than most people in this industry have been
alive. I have seen every fad come and go -- flavored whiskey, cask-strength
hype, the Japanese whiskey boom. But the one constant is that the TTB always
wants to know who owns what barrel, when it was filled, and who authorized
every change.

This `soroban-sdk-tools` system gets the fundamentals right. The two-trait split
separates data from auth in a way that makes regulatory review straightforward.
The sealed macro provides the kind of certainty that auditors demand. The
provider swap means I do not have to redeploy when my business model changes.

What it needs is the finishing -- events, multi-party auth, time awareness,
and immutability. These are the difference between a white dog and a finished
bourbon. The spirit is good. It just needs more time in the barrel.

And if you are wondering: yes, I would track my barrels with this system. After
they add events.

---

**Overall Assessment:** Strong foundation for regulated supply chain tracking.
The sealed macro pattern is particularly valuable for TTB compliance. Event
emission and multi-party auth are the critical gaps.

**Verdict:** Good whiskey needs good wood and good time. This is good wood.
Give it time.
