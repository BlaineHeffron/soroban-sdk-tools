# Review: Liwei -- Chinese Space Station Engineer

**Reviewer:** Liwei, Chinese space station engineer interested in interplanetary contracts
**Focus:** Latency-tolerant systems, offline consensus, resource constraints in space
**Date:** 2026-03-21
**Materials Reviewed:**
- `docs/oz-comparison.md`
- `docs/blog-post-composable-contracts.md`
- `examples/trait-test/src/lib.rs`
- `soroban-sdk-tools-macro/src/contract.rs`

---

## Context: Computing in the Void

I am a systems engineer on the Chinese Space Station (CSS, Tiangong). My
specialty is autonomous systems -- software that must operate correctly when
mission control is unreachable. On the CSS, communication latency to Earth
is 0.5-2 seconds. On a Mars mission, it is 4-24 minutes. On a mission to
Jupiter's moons, it is 33-53 minutes.

Smart contracts, as currently designed, assume constant connectivity to a
consensus network. Every transaction requires validators to agree in real
time. This model breaks fundamentally when light-speed delays are measured
in minutes or hours.

I review `soroban-sdk-tools` not for what it can do today on Earth-bound
Soroban, but for whether its architectural patterns could support
interplanetary contract execution in the future.

---

## 1. The Latency Problem: Why Earth-Centric Blockchain Fails in Space

### Round-Trip Times

| Route | One-Way Latency | Round-Trip | Practical Impact |
|-------|----------------|------------|-----------------|
| LEO (ISS/CSS) <-> Earth | 0.5-2 sec | 1-4 sec | Manageable |
| Lunar surface <-> Earth | 1.3 sec | 2.6 sec | Manageable |
| Mars <-> Earth (closest) | 4 min | 8 min | Transaction timeout |
| Mars <-> Earth (farthest) | 24 min | 48 min | Unusable for real-time |
| Jupiter moons <-> Earth | 33-53 min | 66-106 min | Completely offline |
| Saturn moons <-> Earth | 67-84 min | 134-168 min | Completely offline |

### What This Means for Soroban

Soroban's ledger close time is ~5-6 seconds. A transaction submitted from
Mars (closest approach) would take at minimum:
1. 4 minutes for the transaction to reach Earth
2. 5-6 seconds for ledger close
3. 4 minutes for confirmation to reach Mars
= 8+ minutes per transaction

At farthest approach: 48+ minutes per transaction.

This is not just slow. Soroban transactions have timeout mechanisms.
The transaction would expire before it reaches the validators.

### Implication for soroban-sdk-tools

Any composability pattern designed for Soroban is inherently Earth-centric.
But the *architectural patterns* -- the two-trait split, the provider model,
the auth mechanism -- are latency-agnostic. They describe WHAT should happen,
not HOW FAST it should happen.

This is the key insight: **the trait definitions are latency-independent.
The execution environment is latency-dependent.**

---

## 2. The Two-Trait Split in a Space Context

### Why It Matters for Space

In space systems, we separate:
- **Command definition:** What actions are possible (command dictionary)
- **Command execution:** How actions are carried out (flight software)
- **Command authorization:** Who can issue commands (crew roles)

The `#[contracttrait]` macro produces exactly this separation:
- **Internal trait:** Command execution (pure business logic)
- **Outer trait:** Command definition + authorization (interface + auth)

This is not accidental. The pattern is universal in safety-critical systems.
NASA's cFS (core Flight System), ESA's ECSS standards, and CNSA's own
standards all separate command definition from execution.

### Space Station Resource Management

Consider a resource management contract for a space station:

```rust
#[contracttrait]
pub trait ResourceManagement {
    fn station_commander(env: &Env) -> Address;
    fn mission_control(env: &Env) -> Address;

    fn oxygen_level(env: &Env) -> u32;      // percentage
    fn water_reserve(env: &Env) -> u32;     // liters
    fn power_available(env: &Env) -> u32;   // watts

    #[auth(Self::station_commander)]
    fn allocate_power(env: &Env, module: Symbol, watts: u32);

    #[auth(Self::station_commander)]
    fn activate_emergency_protocol(env: &Env, protocol: Symbol);

    #[auth(Self::mission_control)]
    fn upload_schedule(env: &Env, schedule_hash: BytesN<32>);
}
```

The auth model is critical: the station commander has real-time authority
over resource allocation and emergencies. Mission control has authority
over scheduling (which can tolerate latency).

### Provider for Different Mission Phases

```rust
// Near Earth: full communication with ground
pub struct NearEarthProvider;

// Deep space: autonomous decisions, periodic sync
pub struct DeepSpaceProvider;

// Emergency: all authority delegated to crew
pub struct EmergencyProvider;
```

The provider swap represents mission phase transitions. When communication
is lost, the provider switches from `NearEarthProvider` (which might require
ground confirmation) to `DeepSpaceProvider` (which allows autonomous crew
decisions).

**Assessment: The provider pattern maps well to mission phase management.**

---

## 3. Offline Consensus: When Validators Are Light-Minutes Away

### The Problem

A Mars habitat cannot wait 48 minutes for transaction confirmation. The
crew needs to make binding decisions in real time. But those decisions
need to be eventually consistent with Earth-based records.

### Eventual Consistency Model

Instead of real-time consensus, space operations use:
1. **Local execution:** The decision is made and executed locally
2. **Log recording:** The decision is recorded in a local log
3. **Periodic sync:** When communication is available, logs are synchronized
4. **Conflict resolution:** If Earth and Mars made conflicting decisions,
   a resolution protocol runs

### How the Internal Trait Enables This

The `OwnableInternal` trait is pure business logic. It does not depend on
Soroban's consensus mechanism. In theory, it could run on a local runtime:

```rust
impl OwnableInternal for LocalMarsProvider {
    fn owner(env: &Env) -> Address {
        // Read from local Mars habitat storage
        LocalStorage::get_commander(env).unwrap()
    }

    fn transfer_ownership(env: &Env, new_owner: Address) {
        // Execute locally, log for sync
        LocalStorage::set_commander(env, &new_owner);
        SyncLog::record(env, "transfer_ownership", &new_owner);
    }
}
```

The outer trait's `require_auth()` would need to work with local crew
authentication (biometrics, PINs) rather than Stellar network auth.

### Sync Contract on Earth

```rust
#[contracttrait]
pub trait MarsSync {
    fn mars_relay(env: &Env) -> Address;  // the relay satellite

    #[auth(Self::mars_relay)]
    fn sync_state(
        env: &Env,
        state_hash: BytesN<32>,
        operations_log: Vec<OperationEntry>,
        mars_timestamp: u64,
    );

    fn last_sync(env: &Env) -> u64;
    fn pending_conflicts(env: &Env) -> Vec<ConflictEntry>;

    #[auth(Self::mission_control)]
    fn resolve_conflict(env: &Env, conflict_id: u64, resolution: Symbol);
}
```

The `#[auth(Self::mars_relay)]` ensures that only authenticated Mars relay
satellites can submit state updates. The `resolve_conflict` method allows
mission control to handle discrepancies.

**Assessment: The two-trait split naturally supports local execution +
eventual sync. This is a genuine architectural advantage.**

---

## 4. Resource Constraints: Computing on a Power Budget

### The Reality of Space Computing

The CSS uses radiation-hardened processors running at ~200 MHz. A Mars
habitat would have similar constraints. Key limitations:

| Resource | Earth (typical) | Space Station | Mars Habitat |
|----------|----------------|---------------|-------------|
| CPU | 4+ GHz, multi-core | 200 MHz, single | 200 MHz, single |
| RAM | 16+ GB | 256 MB | 128 MB |
| Storage | TB-scale SSD | GB-scale rad-hard | GB-scale rad-hard |
| Power | Unlimited | 100 kW (shared) | 40 kW (shared) |
| Network | Gbps | Kbps-Mbps | Kbps (intermittent) |

### WASM Size and the Zero-Overhead Claim

The blog post claims:

> "WASM binary size: Zero overhead... the final WASM is identical to
> hand-written code."

For space systems, WASM binary size directly translates to:
1. **Upload cost:** Transmitting a 100 KB contract to Mars costs valuable
   communication bandwidth
2. **Memory footprint:** The WASM module must fit in constrained RAM
3. **Execution cost:** Larger binaries mean longer parse and compilation times

If the two-trait indirection truly compiles away, the WASM is as small as
possible. This is important for space applications.

### Verification of the Claim

Looking at the macro output (lines 293-303 of `contract.rs`):

```rust
#vis trait #trait_name {
    type Provider: #internal_trait_name;

    fn owner(env: &Env) -> Address {
        Self::Provider::owner(env)
    }
}
```

The `Self::Provider::owner(env)` call is a static dispatch through an
associated type. With monomorphization, this becomes a direct function
call. With LTO and inlining, it becomes the function body itself. The
intermediate trait indirection is erased.

**Assessment: The zero-overhead claim is credible for monomorphized builds.
This is important for resource-constrained environments.**

---

## 5. Radiation Tolerance: Why Sealed Auth Matters More in Space

### Single Event Upsets (SEUs)

In space, radiation causes bit flips in memory. A single cosmic ray can
flip a bit in RAM, changing a `0` to a `1` or vice versa. This is called
a Single Event Upset.

In software, SEUs can:
- Change a variable's value
- Alter program flow (corrupting a jump address)
- Modify stored data

### Auth Bypass via SEU

Consider the OZ override scenario:

```rust
fn transfer_ownership(e: &Env, new_owner: Address, live_until_ledger: u32) {
    enforce_owner_auth(e);  // What if an SEU skips this instruction?
    // ... transfer logic
}
```

If a radiation event corrupts the program counter and skips the
`enforce_owner_auth` call, the transfer proceeds without authorization.

### Sealed Auth as Radiation Mitigation

The sealed macro generates auth checks as part of the method body, not
as a separate call that could be skipped:

```rust
pub fn transfer_ownership(env: Env, new_owner: Address) {
    let __auth_addr = <SingleOwner as OwnableInternal>::owner(&env);
    __auth_addr.require_auth();
    <SingleOwner as OwnableInternal>::transfer_ownership(&env, new_owner);
}
```

If the `require_auth()` call is skipped due to an SEU, Soroban's runtime
should still enforce authorization at the VM level. But having the auth
check inline (rather than in a separate function call) reduces the
probability of an SEU bypassing it, because there is no function call
instruction to corrupt.

**Assessment: The sealed macro provides marginally better radiation
tolerance due to inline auth checks. This is not a primary design
consideration, but it is a beneficial side effect.**

Note: Real space software uses triple modular redundancy (TMR) and
error-correcting codes (ECC) for radiation protection. The sealed
macro is not a substitute for these, but it aligns with the defense-in-depth
philosophy.

---

## 6. Autonomous Decision Making: AI + Smart Contracts

### The Scenario

A Mars habitat's life support system detects a pressure drop in Module B.
The AI system determines that Module B should be sealed and atmosphere
diverted from reserve. This decision must be:

1. Executed immediately (no time to wait for Earth confirmation)
2. Authorized (the AI must have authority to seal modules)
3. Logged (Earth needs to know what happened)
4. Reversible (if the AI's assessment was wrong, crew can override)

### AI as a Provider

```rust
pub struct AutonomousHabitatAI;
impl ResourceManagementInternal for AutonomousHabitatAI {
    fn activate_emergency_protocol(env: &Env, protocol: Symbol) {
        match protocol.to_string().as_str() {
            "SEAL_MODULE" => {
                ModuleControl::seal(env, &protocol);
                AtmosphereControl::divert_reserve(env);
                SyncLog::record(env, "EMERGENCY_SEAL", &protocol);
            }
            "POWER_DOWN" => {
                PowerControl::shutdown_non_essential(env);
                SyncLog::record(env, "EMERGENCY_POWER_DOWN", &protocol);
            }
            _ => panic!("unknown emergency protocol"),
        }
    }
}
```

The auth model needs adjustment for AI actors. The `station_commander`
might be the AI system itself during autonomous operations:

```rust
pub struct EmergencyProvider;
impl ResourceManagementInternal for EmergencyProvider {
    fn station_commander(env: &Env) -> Address {
        // During emergency, the AI has commander authority
        AIStorage::get_ai_address(env).unwrap()
    }
}
```

The provider swap happens when the emergency is declared: the system
switches from `NearEarthProvider` (human commander) to `EmergencyProvider`
(AI commander). After the emergency, it switches back.

**Assessment: The provider pattern elegantly handles authority transitions
between human and AI control.**

---

## 7. Multi-Crew Authorization: The Two-Person Rule

### The Nuclear Submarine Precedent

On nuclear submarines, launching a missile requires two officers to
simultaneously turn their keys. Neither can launch alone. This is the
"two-person integrity" (TPI) rule.

Space stations have similar requirements:
- Jettisoning a module: Commander AND flight engineer
- Emergency undocking: Any two crew members
- Overriding AI decisions: Commander AND mission specialist

### Current Limitation

The `#[auth]` system supports only single-party authorization. For TPI:

```rust
// THIS DOES NOT WORK with current #[auth]
#[auth(Self::commander, Self::flight_engineer)]  // multi-auth needed
fn jettison_module(env: &Env, module: Symbol);
```

### Provider Workaround

```rust
impl ResourceManagementInternal for TPIProvider {
    fn jettison_module(env: &Env, module: Symbol) {
        // Both must have authorized in the Soroban transaction
        let commander = CrewStorage::get_commander(env);
        let flight_engineer = CrewStorage::get_flight_engineer(env);
        commander.require_auth();
        flight_engineer.require_auth();

        ModuleControl::jettison(env, &module);
    }
}
```

This works but places auth logic in the provider, which the design
philosophy tries to avoid.

**Recommendation (consistent with other reviewers): Multi-party auth
must be a first-class feature. For space applications, this is
safety-critical, not merely convenient.**

---

## 8. Interplanetary Contract Networks: A Future Architecture

### The Vision

As humanity expands into the solar system, each settlement needs local
contract execution with periodic synchronization:

```
Earth Soroban (main chain)
    |
    |-- 1.3 sec --> Lunar Settlement (sidechain or L2)
    |
    |-- 4-24 min --> Mars Colony (independent chain, periodic sync)
    |
    |-- 33-53 min --> Europa Research Station (fully autonomous, rare sync)
```

### How soroban-sdk-tools Patterns Apply

The two-trait split enables a layered architecture:

1. **Trait definition (universal):** The same `ResourceManagement` trait
   is used on Earth, Moon, Mars, and Europa. The interface is universal.

2. **Provider (location-specific):** Each location has providers suited
   to its environment:
   - Earth: `StandardProvider` (full Soroban, real-time consensus)
   - Moon: `LowLatencyProvider` (Soroban L2, 2.6-sec sync)
   - Mars: `AutonomousProvider` (local execution, periodic Earth sync)
   - Europa: `FullyOfflineProvider` (months between syncs)

3. **Sealed macro (safety-critical):** All locations use the sealed macro
   to prevent auth bypass. In space, auth bypass can kill people.

### Cross-Settlement Contracts

How does a contract on Mars interact with a contract on Earth?

```rust
#[contracttrait]
pub trait InterplanetaryTransfer {
    fn source_settlement(env: &Env) -> Symbol;
    fn destination_settlement(env: &Env) -> Symbol;

    #[auth(Self::source_authority)]
    fn initiate_transfer(env: &Env, resource: Symbol, amount: u32);

    // Confirmed by destination settlement (after light-speed delay)
    #[auth(Self::destination_authority)]
    fn confirm_receipt(env: &Env, transfer_id: u64);
}
```

This is a two-phase commit across light-speed delays. The `initiate_transfer`
locks the resource on the source chain. After the light-speed delay, the
destination chain confirms receipt. If confirmation is not received within
a timeout (2x light-speed round-trip), the lock is released.

The provider for this would need to handle:
- Timeout calculation based on orbital mechanics
- Retry logic for failed transmissions
- Conflict resolution when both settlements modify shared state

---

## 9. Life Support Contracts: Where Auth Failure Means Death

### The Stakes

On Earth, a smart contract auth bypass means financial loss. In space,
it can mean loss of life. Consider:

```rust
#[contracttrait]
pub trait LifeSupport {
    fn crew_count(env: &Env) -> u32;
    fn oxygen_hours_remaining(env: &Env) -> u32;

    #[auth(Self::commander)]
    fn adjust_oxygen_flow(env: &Env, module: Symbol, rate: u32);

    // ANYONE on crew can hit the emergency button
    #[auth(crew_member)]
    fn emergency_oxygen(env: &Env, crew_member: Address);

    // Only mission control can override safety limits
    #[auth(Self::mission_control)]
    fn override_safety_limit(env: &Env, parameter: Symbol, new_limit: u32);
}
```

The auth model here is nuanced:
- `adjust_oxygen_flow`: Only the commander (operational authority)
- `emergency_oxygen`: ANY crew member (life safety override)
- `override_safety_limit`: Only mission control (external oversight)

### The "Any Crew Member" Auth Pattern

The `#[auth(crew_member)]` pattern works because the crew member's address
is a parameter. But the provider must verify that the address belongs to
an actual crew member:

```rust
impl LifeSupportInternal for HabitatProvider {
    fn emergency_oxygen(env: &Env, crew_member: Address) {
        assert!(CrewStorage::is_crew(env, &crew_member), "not a crew member");
        OxygenControl::emergency_release(env);
        SyncLog::record(env, "EMERGENCY_O2", &crew_member);
    }
}
```

The `require_auth()` in the outer trait verifies the cryptographic
identity. The provider verifies the crew membership. This is the right
separation.

### Sealed Macro: Non-Negotiable

For life support contracts, the sealed macro is not a design choice.
It is a safety requirement:

```rust
impl_life_support!(HabitatContract, HabitatProvider);
```

Auth bypass in a life support system is a catastrophic failure mode.
The sealed macro reduces the probability of this failure to near zero
(limited to WASM runtime bugs or cryptographic failures).

---

## 10. Time-Keeping in Space: The Clock Problem

### The Problem

`env.ledger().timestamp()` returns the Soroban ledger close time. This is
Earth UTC (approximately). On Mars, local time is measured in sols (Martian
days, ~24h 37m). On a space station, time may be measured in mission elapsed
time (MET).

### Implication for Contracts

Any contract that uses `env.ledger().timestamp()` for time-based logic is
implicitly using Earth time. A Mars habitat using the same contract would
need to convert between Earth time and Mars sol time.

The provider pattern handles this:

```rust
pub struct MarsTimeProvider;
impl ResourceManagementInternal for MarsTimeProvider {
    fn upload_schedule(env: &Env, schedule_hash: BytesN<32>) {
        let earth_time = env.ledger().timestamp();
        let mars_sol = earth_to_mars_sol(earth_time);
        ScheduleStorage::set(env, mars_sol, &schedule_hash);
    }
}
```

The time conversion is encapsulated in the provider. The trait definition
remains time-system-agnostic.

**Assessment: The provider pattern naturally supports different time systems.**

---

## 11. Formal Verification: Space-Grade Assurance

### Requirements

Space software typically requires DO-178C Level A certification (or
equivalent). This includes formal verification of safety-critical
properties.

### Verifiable Properties of the Macro Output

The `#[contracttrait]` macro generates code with properties that
can be formally verified:

1. **Auth completeness:** Every method marked with `#[auth]` includes
   a `require_auth()` call in the generated code. This can be verified
   by static analysis of the macro output.

2. **Auth-internal separation:** No `require_auth()` call appears in
   `Internal` trait implementations. This can be verified by type
   system analysis (the `Internal` trait signatures do not include
   auth-related types).

3. **Delegation completeness:** Every outer trait method delegates to
   the corresponding `Internal` trait method. The `build_delegate_args`
   function ensures argument passing is complete.

4. **Sealed non-overridability:** Methods generated by the sealed macro
   are `pub fn` on an `impl` block (inherent methods), not trait methods.
   Rust's type system guarantees these cannot be overridden.

### What Cannot Be Verified

The macro cannot verify that the provider implementation is correct.
A provider that returns a hardcoded address instead of reading from
storage would pass compilation but fail at runtime.

**Recommendation:** For space-grade assurance, the provider pattern should
include a formal specification layer:

```rust
/// # Safety Invariants
/// - `owner()` must return the address stored at key "commander"
/// - `transfer_ownership()` must update the stored commander address
/// - No other storage keys may be modified
```

These invariants could be checked by formal verification tools like
Kani or Creusot.

---

## 12. Power Budget: Computing Cost in Watts

### The Calculation

On the CSS, computing is allocated ~500W from the station's ~100kW
power budget. Every CPU cycle has a real power cost.

A WASM interpreter running on a 200 MHz rad-hard processor:
- Simple function call: ~100 cycles = 0.5 microseconds
- Storage read (from rad-hard NVRAM): ~10,000 cycles = 50 microseconds
- SHA-256 hash (for auth): ~50,000 cycles = 250 microseconds

A typical auth-checked transaction:
- 1 storage read (get owner): 50 microseconds
- 1 auth check: 250 microseconds
- 1 storage write: 50 microseconds
- Provider business logic: ~500 microseconds
= ~850 microseconds total = ~0.85 milliseconds

Power: 850 microseconds * 200 MHz = 170 million cycles * ~0.1 nJ/cycle
= ~17 microjoules per transaction.

Even at 1000 transactions per sol, that is 17 millijoules. Negligible
compared to the 500W computing budget.

**Assessment: The computational overhead of the two-trait pattern is
negligible even on space-grade hardware.**

---

## 13. Recommendations for Space Applications

| Priority | Recommendation | Space Justification |
|----------|---------------|-------------------|
| Critical | Multi-party auth (TPI) | Safety-critical dual authorization |
| Critical | Offline execution model | Light-speed delay makes real-time consensus impossible |
| High | Event/log generation | Mission logging requirements |
| High | Formal verification annotations | DO-178C / space-grade assurance |
| High | Provider hot-swap at runtime | Mission phase transitions |
| Medium | Time-system abstraction | Earth UTC vs Mars sol vs MET |
| Medium | Conflict resolution primitives | Eventual consistency after sync |
| Medium | Resource usage profiling | Power budget management |
| Low | Interplanetary sync protocol | Cross-settlement state management |
| Low | Radiation tolerance annotations | SEU mitigation documentation |

---

## 14. Closing Thoughts

I have spent twelve years building software for spacecraft. Every system I
have built follows the same fundamental pattern: define what the system should
do (command dictionary), implement how it does it (flight software), and
control who can make it do things (authorization).

The `#[contracttrait]` macro implements exactly this pattern. The two-trait
split is the command dictionary / flight software separation. The `#[auth]`
annotation is the authorization layer. The provider pattern is the mission
configuration system.

These patterns were not designed for space. But they are the right patterns
for space. The reason is simple: space systems and smart contracts share a
fundamental constraint -- once deployed, they must operate correctly without
the ability to "just fix it" by logging in and changing things.

The sealed macro embodies this philosophy: the auth checks are baked in at
build time. In space, that is not a design preference -- it is a survival
requirement.

What the system needs for space is not architectural change. It needs:
1. Multi-party auth (the two-person rule is non-negotiable for safety)
2. Offline execution (light-speed delay is physics, not a bug)
3. Formal verification (space software must be provably correct)
4. Event logging (mission logs are as important as mission execution)

The architecture is ready. The consensus model is not. But the patterns
established in `soroban-sdk-tools` -- the separation of concerns, the
provider-based flexibility, the structural auth enforcement -- are exactly
what interplanetary computing will need.

We are not building interplanetary contracts today. But the patterns we
establish today will determine whether it is possible tomorrow.

---

**Overall Assessment:** The architectural patterns in `soroban-sdk-tools` are
latency-agnostic and resource-efficient, making them suitable for adaptation
to space computing environments. The provider pattern is particularly valuable
for mission phase management. Multi-party auth and offline execution models
are the critical gaps.

**Verdict:** Earth-ready architecture with interplanetary potential. The design
patterns are universal. The execution environment is the constraint, not the
composability layer.
