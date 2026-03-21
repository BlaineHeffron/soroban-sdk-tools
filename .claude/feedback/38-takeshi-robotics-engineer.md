# Review: soroban-sdk-tools -- Autonomous Robotics & Machine-to-Machine Payments

**Reviewer:** Takeshi -- Robotics engineer building autonomous delivery drones
**Date:** 2026-03-21
**Materials Reviewed:** oz-comparison.md, blog-post-composable-contracts.md, trait-test example, contract.rs macro

---

## Overall Impression

I design autonomous delivery drones that need to pay for services: charging
station fees, airspace access, landing pad rentals, maintenance scheduling.
These payments happen between machines, without human intervention. The
drone has a hardware wallet. It authorizes payments autonomously based on
pre-programmed policies. No human signs anything.

When I evaluate smart contract tooling, I care about three things:

1. Can a machine (not a human) authorize transactions?
2. Can authorization policies be encoded structurally (not just as business
   logic)?
3. Can the system handle high-frequency, low-value payments between
   machines?

`soroban-sdk-tools` addresses the first two concerns well. The `#[auth]`
pattern and provider model map cleanly to machine authorization. The third
concern -- payment throughput and economics -- is not addressed.

---

## Machine Authorization Model

### How Drones Pay

A delivery drone's payment flow:

1. Drone arrives at charging station
2. Drone queries the charging contract for price
3. Drone authorizes payment from its on-chain balance
4. Charging station verifies payment and enables charger
5. Drone charges, then departs

The authorization in step 3 is machine-driven. The drone's firmware holds a
private key (in a hardware security module). It signs the payment transaction
based on policies: "pay up to X lumens per charge, up to Y charges per day."

### Mapping to #[contracttrait]

The drone's interaction maps to traits:

```rust
#[contracttrait]
pub trait ChargingStation: Ownable {
    fn price_per_kwh(env: &Env) -> i128;
    fn station_status(env: &Env) -> StationStatus;

    #[auth(Self::owner)]
    fn set_price(env: &Env, price_per_kwh: i128);

    #[auth(drone)]
    fn request_charge(env: &Env, drone: Address, kwh_requested: u32) -> ChargeSession;

    #[auth(Self::owner)]
    fn confirm_charge(env: &Env, session_id: u64, kwh_delivered: u32);
}
```

The `#[auth(drone)]` on `request_charge` means the drone itself must
authorize the charge request. This is perfect. The drone signs with its
hardware wallet key. No human in the loop.

The `#[auth(Self::owner)]` on `confirm_charge` means the station (or its
operator) confirms delivery. Two-party verification: the drone requests, the
station confirms.

### Provider Pattern for Different Drone Types

Different drones have different capabilities:

```rust
// Basic drone: simple Ed25519 key in firmware
pub struct BasicDroneAuth;
impl ChargingStationInternal for BasicDroneAuth {
    fn request_charge(env: &Env, drone: Address, kwh_requested: u32) -> ChargeSession {
        // Simple: check balance, reserve funds, create session
        let cost = Self::price_per_kwh(env) * kwh_requested as i128;
        reserve_funds(env, &drone, cost);
        create_session(env, &drone, kwh_requested)
    }
}

// Fleet drone: corporate key hierarchy (HSM -> fleet key -> drone key)
pub struct FleetDroneAuth;
impl ChargingStationInternal for FleetDroneAuth {
    fn request_charge(env: &Env, drone: Address, kwh_requested: u32) -> ChargeSession {
        // Verify drone is part of authorized fleet
        let fleet = FleetRegistry::get_fleet(env, &drone);
        assert!(fleet.is_some(), "drone not in any fleet");
        // Check fleet-level spending limits
        let fleet_budget = FleetBudget::remaining(env, &fleet.unwrap());
        let cost = Self::price_per_kwh(env) * kwh_requested as i128;
        assert!(cost <= fleet_budget, "fleet budget exceeded");
        reserve_funds(env, &drone, cost);
        create_session(env, &drone, kwh_requested)
    }
}
```

Swap `BasicDroneAuth` for `FleetDroneAuth` with one line. The charging
station contract works with both individual drones and corporate fleets.
This is exactly the flexibility I need.

---

## Autonomous Agent Auth Patterns

### Policy-Based Authorization

Human auth is simple: the human decides and signs. Machine auth is
policy-based: the machine follows pre-programmed rules. The `#[auth]`
pattern enforces WHO authorizes (the drone signs). But it does not enforce
WHAT the drone is authorized to do (spending limits, frequency limits,
geographic constraints).

For autonomous agents, I need:

```rust
#[contracttrait]
pub trait DronePolicy {
    fn max_charge_cost(env: &Env, drone: Address) -> i128;
    fn daily_charge_count(env: &Env, drone: Address) -> u32;
    fn max_daily_charges(env: &Env, drone: Address) -> u32;
    fn allowed_stations(env: &Env, drone: Address) -> Vec<Address>;

    #[auth(drone)]
    fn update_policy(env: &Env, drone: Address, policy: DronePolicy);
    // Only the drone owner (not the drone itself) should update policies
    // Wait -- the drone signs with its own key, but the OWNER of the
    // drone sets the policies. This is a different auth model.
}
```

This reveals a limitation: the `#[auth]` pattern supports one authorizer
per method. But machine-to-machine interactions often require multi-party
auth:

- The DRONE authorizes the charge (it signs the transaction)
- The FLEET MANAGER authorizes the spending policy (they set limits)
- The STATION OWNER authorizes the service (they set prices)

Three parties, each with different auth requirements, in a single
interaction. The current `#[auth]` attribute supports only one.

### Recommendation: Multi-Auth Methods

```rust
#[auth(drone)]             // Drone must sign
#[require_policy(drone)]   // Policy check (provider responsibility)
fn request_charge(env: &Env, drone: Address, kwh_requested: u32);
```

Or, more ambitiously:

```rust
#[auth(drone, station)]  // Both parties must authorize
fn negotiate_charge(env: &Env, drone: Address, station: Address, terms: ChargeTerms);
```

The macro would generate:

```rust
fn negotiate_charge(env: &Env, drone: Address, station: Address, terms: ChargeTerms) {
    drone.require_auth();
    station.require_auth();
    Self::Provider::negotiate_charge(env, drone, station, terms);
}
```

Multi-party auth is essential for machine-to-machine interactions where
multiple autonomous agents must agree.

---

## Hardware Wallet Integration

### The HSM Challenge

Delivery drones use hardware security modules (HSMs) to store private keys.
These HSMs have specific constraints:

1. **Limited computation** -- HSMs cannot perform complex operations
2. **No network access** -- HSMs sign transactions presented to them
3. **Attestation** -- HSMs can prove they are genuine hardware
4. **Key derivation** -- HSMs support hierarchical key derivation (BIP-32
   style)

The `#[auth]` pattern works with HSMs because Soroban's `require_auth()`
only checks the signature, not how the signature was produced. The drone's
firmware constructs the transaction, presents it to the HSM for signing,
and submits the signed transaction to the network.

### AuthClient and Hardware Testing

The `AuthClient` test pattern is useful for simulating hardware wallet
interactions:

```rust
let drone_auth = ChargingStationAuthClient::new(&env, &station_id);

// Simulate drone requesting charge
drone_auth.request_charge(&drone_addr, &kwh_requested)
    .authorize(&drone_addr)  // Simulates HSM signature
    .invoke();
```

But the test does not capture HSM-specific constraints:

1. **Signing latency** -- HSM signing takes 50-200ms. The test should
   support simulating signing delays.
2. **Key rotation** -- HSMs rotate keys periodically. The test should
   support testing with multiple keys for the same drone.
3. **Attestation** -- The contract should verify that the signer is a
   genuine HSM, not a compromised software key. The AuthClient should
   support testing attestation flows.

### Recommendation: HSM Provider

```rust
pub struct HSMAuthProvider;
impl OwnableInternal for HSMAuthProvider {
    fn owner(env: &Env) -> Address {
        // Verify HSM attestation before accepting ownership claims
        let attestation = HSMRegistry::get_attestation(env);
        assert!(attestation.is_valid(), "HSM attestation failed");
        attestation.device_address()
    }
}
```

The provider verifies not just WHO is signing but WHAT is signing (genuine
hardware vs. compromised software). This is a hardware-specific concern
that the provider pattern handles elegantly.

---

## Machine-to-Machine Payment Patterns

### High-Frequency Payments

Autonomous drones may make hundreds of micro-payments per day:

- Charging: 5-10 per day ($1-5 each)
- Landing pad: 20-50 per day ($0.10-0.50 each)
- Airspace access: 50-100 per day ($0.01-0.05 each)
- Weather data: 100+ per day ($0.001-0.01 each)

Total: 200+ transactions per day per drone. For a fleet of 1,000 drones:
200,000 transactions per day.

### Throughput Concerns

The documentation does not address transaction throughput. For machine-to-
machine payments:

1. **Transaction finality time** -- How fast does a charge authorization
   settle? Drones cannot wait 5 minutes at a charging station for
   transaction confirmation. Soroban's 5-second finality is acceptable.

2. **Transaction cost** -- At 200 transactions per drone per day, even a
   $0.01 fee per transaction costs $2/day. For 1,000 drones, that is
   $2,000/day in fees alone. The documentation should address fee
   economics for high-frequency use cases.

3. **Payment channels** -- For weather data payments ($0.001 each, 100+
   per day), on-chain transactions are not viable. Payment channels or
   aggregated settlements are needed. The provider pattern should support
   a `PaymentChannelProvider` that batches payments off-chain and settles
   periodically.

### Payment Channel Trait

```rust
#[contracttrait]
pub trait PaymentChannel {
    fn open_channel(env: &Env, payer: Address, payee: Address, deposit: i128) -> u64;

    #[auth(payer)]
    fn fund_channel(env: &Env, payer: Address, channel_id: u64, amount: i128);

    // Off-chain: payer signs payment vouchers to payee
    // Payee submits the latest voucher to settle

    #[auth(payee)]
    fn settle_channel(env: &Env, payee: Address, channel_id: u64, amount: i128, signature: BytesN<64>);

    #[auth(payer)]
    fn close_channel(env: &Env, payer: Address, channel_id: u64);
}
```

This is a composable trait that any machine-to-machine payment system can
use. The provider handles settlement logic. The sealed macro ensures the
settlement rules cannot be tampered with.

---

## Drone Fleet Management

### Fleet as a Supertrait Hierarchy

A drone fleet management system maps naturally to the supertrait pattern:

```
Ownable (fleet owner)
  -> FleetManager (manages drones in the fleet)
    -> DronePolicy (per-drone spending limits)
      -> ChargingStation (interactions with charging infra)
      -> AirspaceAccess (interactions with airspace contracts)
      -> MaintenanceScheduler (interactions with maintenance providers)
```

Each level in the hierarchy adds auth requirements:

- Fleet owner sets fleet-level policies
- Fleet manager registers/deregisters drones
- Individual drones authorize their own transactions within policy limits

The supertrait pattern (`Pausable: Ownable`) scales to this hierarchy:

```rust
#[contracttrait]
pub trait FleetManager: Ownable { /* ... */ }

#[contracttrait]
pub trait DronePolicy: FleetManager { /* ... */ }

#[contracttrait]
pub trait ChargingIntegration: DronePolicy { /* ... */ }
```

### Emergency Stop

Autonomous systems need emergency stop mechanisms. The `Pausable` trait
maps perfectly to this:

```rust
#[contracttrait]
pub trait Pausable: Ownable {
    fn is_paused(env: &Env) -> bool;

    #[auth(Self::owner)]
    fn pause(env: &Env);    // Emergency stop: all drones grounded

    #[auth(Self::owner)]
    fn unpause(env: &Env);  // Resume operations
}
```

When the fleet is paused, no drone can authorize any transaction. The
provider checks `is_paused()` before allowing any operation. The sealed
macro ensures the pause mechanism cannot be bypassed.

For autonomous systems, the sealed pattern is not just convenient -- it is
a safety requirement. A drone that can bypass its emergency stop is a
liability.

---

## Testing Autonomous Systems

### What the AuthClient Gets Right

The AuthClient's `authorize().invoke()` pattern maps well to testing
autonomous agent behavior:

```rust
// Test: drone can request charge within budget
drone_auth.request_charge(&drone, &10)
    .authorize(&drone)
    .invoke();

// Test: drone cannot exceed daily charge limit
for _ in 0..MAX_DAILY_CHARGES {
    drone_auth.request_charge(&drone, &1)
        .authorize(&drone)
        .invoke();
}
// This should fail: daily limit exceeded
let result = drone_auth.request_charge(&drone, &1)
    .authorize(&drone)
    .try_invoke();
assert!(result.is_err());
```

### What Is Missing for Robotics Testing

1. **Temporal testing** -- Drones operate on schedules. Tests need to
   simulate time progression (advance ledger timestamp by N seconds).
   The AuthClient should support `advance_time()` for temporal policy
   testing.

2. **Multi-agent testing** -- A charging station serving multiple drones
   simultaneously. Tests need to simulate concurrent access:

   ```rust
   // Drone A and Drone B request charge simultaneously
   drone_auth.request_charge(&drone_a, &10).authorize(&drone_a).invoke();
   drone_auth.request_charge(&drone_b, &5).authorize(&drone_b).invoke();
   // Station has capacity for only one -- who gets priority?
   ```

3. **Failure mode testing** -- What happens when a drone crashes mid-charge?
   The funds are reserved but the charge never completes. The system needs
   timeout-based refund mechanisms, and tests should cover these scenarios.

4. **Location-aware testing** -- Drones interact with geographically
   distributed infrastructure. Tests should simulate location-based
   constraints (drone can only charge at nearby stations).

---

## Integration Architecture

### Drone Firmware <-> Smart Contract

The integration between drone firmware and the smart contract looks like:

```
[Drone CPU] -> [Policy Engine] -> [Transaction Builder] -> [HSM] -> [Network]
                                                                        |
                                                                        v
                                                              [Soroban Contract]
                                                                        |
                                                                        v
                                                              [Charging Station]
```

The `#[contracttrait]` macro does not touch the firmware side, but the
generated TypeScript/Rust client could be consumed by drone firmware
directly. The XDR spec metadata mentioned in the blog post would enable
auto-generated firmware SDKs.

### Real-Time Requirements

Drones have real-time requirements. A charging authorization that takes 10
seconds is acceptable. One that takes 60 seconds is not (the drone is
hovering, burning battery). The documentation should address:

- Expected transaction finality time
- How to handle timeout scenarios
- Optimistic execution patterns (act first, settle later)

---

## Verdict

`soroban-sdk-tools` provides a solid foundation for autonomous agent
authorization. The `#[auth]` pattern maps cleanly to machine signing. The
provider pattern supports different hardware authentication models. The
sealed macro provides the safety guarantees required for autonomous
physical systems.

The gaps are in multi-party auth (drone + station), payment economics
(high-frequency micro-payments), and robotics-specific testing patterns
(temporal, concurrent, failure modes).

For drone fleet management, the supertrait hierarchy and emergency stop
(Pausable) patterns are immediately useful. The provider swap mechanism
allows fleet operators to change authentication models without redeploying
the entire system.

The framework is not drone-specific, and it should not be. But a reference
implementation for machine-to-machine payments would significantly
broaden its appeal to the robotics and IoT industries.

**Rating:** 7/10 -- Strong auth primitives for autonomous agents. Needs
multi-party auth, payment channels, and temporal testing to be
production-ready for robotics.

---

*"A drone that cannot prove it paid for its charge is grounded. A drone
with structural auth enforcement never has that problem. The #[auth]
pattern is not a convenience -- it is a flight requirement."*
