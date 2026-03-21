---
reviewer: Takeshi Yamamoto
role: Robotics Systems Engineer
domain: Autonomous Drones, Swarm Coordination, Machine-to-Machine Payments
date: 2026-03-21
focus: Machine identity, autonomous contract execution, M2M payments
---

# Review: soroban-sdk-tools -- Machine-to-Machine Contract Interaction

## Context

I build autonomous drone systems for agricultural monitoring and package
delivery. Each drone has its own cryptographic identity (Ed25519 keypair
burned into a secure element). Drones need to pay for services autonomously:
charging station fees, airspace corridor access, weather data subscriptions.
No human is in the loop for individual transactions -- the drone's firmware
decides when to pay.

My review focuses on whether `soroban-sdk-tools` can support autonomous
machine agents interacting with contracts without human authorization.

## Machine Identity and the Provider Pattern

### Drone as Owner

A drone fleet contract where each drone is a registered operator:

```rust
#[contracttrait]
pub trait DroneFleet: Ownable {
    fn is_registered(env: &Env, drone: Address) -> bool;

    #[auth(Self::owner)]  // fleet operator
    fn register_drone(env: &Env, drone: Address, max_spend_per_tx: i128);

    #[auth(drone)]
    fn request_service(env: &Env, drone: Address, service: Address, amount: i128);
}
```

The `#[auth(drone)]` parameter-level auth is correct for M2M payments.
The drone signs its own transactions using the secure element. The fleet
operator (`Self::owner`) manages registration.

### Spending Limits

Autonomous machines need spending limits. A drone should not be able to
drain the fleet's treasury. The Provider can enforce this:

```rust
pub struct LimitedSpendProvider;
impl DroneFleetInternal for LimitedSpendProvider {
    fn request_service(env: &Env, drone: Address, service: Address, amount: i128) {
        let limit = DroneStorage::get_spend_limit(env, &drone);
        let spent = DroneStorage::get_spent_this_period(env, &drone);
        assert!(spent + amount <= limit, "spend limit exceeded");
        DroneStorage::record_spend(env, &drone, amount);
        // ... transfer to service provider
    }
}
```

The structural auth ensures the drone authenticates, and the provider
enforces spending limits. The sealed macro prevents the limit check from
being bypassed. This is the right layering.

## Concerns

### 1. No Time-Based Authorization Scoping

A drone's authorization should expire when its flight plan expires. If a
drone is decommissioned, stolen, or crashes, its auth credentials should
not remain valid indefinitely.

The current `#[auth]` pattern has no concept of authorization expiry.
The `require_auth()` call succeeds as long as the signature is valid.

**Suggestion**: Support time-bounded auth in the provider pattern:

```rust
pub struct ExpiringRegistration;
impl DroneFleetInternal for ExpiringRegistration {
    fn is_registered(env: &Env, drone: Address) -> bool {
        let expiry = DroneStorage::get_expiry(env, &drone);
        env.ledger().timestamp() < expiry
    }

    fn request_service(env: &Env, drone: Address, service: Address, amount: i128) {
        assert!(Self::is_registered(env, drone.clone()), "registration expired");
        // ... proceed with service request
    }
}
```

This works with the current architecture. The provider checks expiry before
processing. But if `register_drone` set a TTL on the drone's storage entry,
the registration would auto-expire at the storage level too:

```rust
fn register_drone(env: &Env, drone: Address, max_spend_per_tx: i128) {
    DroneStorage::set_registered(env, &drone, max_spend_per_tx);
    // Auto-expire after 30 days (~518,400 ledgers at 5s)
    env.storage().persistent().extend_ttl(&drone_key, 518_400, 518_400);
}
```

### 2. Swarm Coordination Requires Batch Auth

A drone swarm performing coordinated operations (crop spraying pattern,
search grid) needs multiple drones to authorize a coordinated plan
simultaneously. This is not multisig (k-of-n approval) but rather
all-or-nothing group commitment.

```rust
#[auth(plan_coordinator)]
fn commit_to_plan(env: &Env, plan_coordinator: Address, plan_id: BytesN<32>,
                   drones: Vec<Address>);
```

Each drone in the `drones` vector also needs to authorize. The current
`#[auth(param)]` pattern only handles one parameter. The provider must
validate that all drones have pre-authorized the plan.

**Suggestion**: Support list-level auth:

```rust
#[auth(each: drones)]  // require_auth() for each address in the Vec
fn commit_to_plan(env: &Env, plan_id: BytesN<32>, drones: Vec<Address>);
```

### 3. Machine Key Rotation Without Downtime

When a drone's secure element is replaced (maintenance, damage), the new
key must be registered without the drone going offline. The current
`transfer_ownership` pattern does not apply because drones are not owners
-- they are registered operators.

A key rotation mechanism for non-owner addresses:

```rust
#[auth(old_drone)]
fn rotate_drone_key(env: &Env, old_drone: Address, new_drone: Address);
```

The old key authorizes its own replacement. The `#[auth(old_drone)]`
parameter-level auth handles this. The provider updates the registration
to point to the new address. This works with the current architecture.

### 4. Heartbeat and Liveness

Autonomous machines need to prove liveness. A drone that has not checked
in for 24 hours should be flagged (battery dead, crashed, hijacked).

```rust
#[auth(drone)]
fn heartbeat(env: &Env, drone: Address, gps_lat: i64, gps_lon: i64,
             battery_pct: u32);
```

Each heartbeat is an authorized call. The provider records the timestamp
and location. The sealed macro ensures heartbeats cannot be spoofed.

The `CallBuilder` pattern supports automated testing of heartbeat sequences:

```rust
for tick in 0..100 {
    auth_client.heartbeat(&drone_addr, &lat, &lon, &(100 - tick))
        .sign(&drone_keypair)
        .invoke();
    // advance ledger
}
```

The real-auth `.sign()` path is correct here because the drone uses its
actual secure element key, not mock auth.

### 5. Cross-Contract M2M Payments

A drone paying a charging station:

```
Drone -> DroneFleet::request_service -> ChargingStation::charge
```

The drone authorizes the `request_service` call. The fleet contract then
calls the charging station contract. The `add_sub_invoke` method on
`CallBuilder` supports this nested pattern:

```rust
let charge_call = charging_client.charge(&drone_addr, &amount);
let service_call = fleet_auth.request_service(&drone_addr, &charging_addr, &amount)
    .add_sub_invoke(&charge_call)
    .sign(&drone_keypair);
service_call.invoke();
```

This is exactly the right pattern for M2M payments. The sub-invocation
tree is signed by the drone, covering both the fleet debit and the
station payment in one atomic operation.

## Fleet Management Architecture

```rust
#[contracttrait]
pub trait DroneFleet: Ownable + Pausable {
    fn fleet_size(env: &Env) -> u32;
    fn drone_status(env: &Env, drone: Address) -> Symbol;

    #[auth(Self::owner)]
    fn register_drone(env: &Env, drone: Address, limits: DroneLimits);

    #[auth(Self::owner)]
    fn decommission_drone(env: &Env, drone: Address);

    #[auth(drone)]
    fn heartbeat(env: &Env, drone: Address, telemetry: DroneTelemetry);

    #[auth(drone)]
    fn request_service(env: &Env, drone: Address, service: Address, amount: i128);

    #[auth(drone)]
    fn rotate_key(env: &Env, drone: Address, new_address: Address);
}

impl_drone_fleet!(MyFleet, ExpiringRegistration);
```

The sealed macro ensures:
- Only the fleet operator can register/decommission drones
- Only a drone can send its own heartbeat (cannot be spoofed)
- Only a drone can authorize its own spending
- Only a drone can rotate its own key

The provider enforces spending limits, registration expiry, and heartbeat
freshness.

## Summary

soroban-sdk-tools is well-suited for machine-to-machine payment contracts.
The parameter-level `#[auth(param)]` pattern correctly models machine
identity. The Provider pattern enforces spending limits and registration
expiry. The `CallBuilder` with `add_sub_invoke` supports cross-contract
M2M payment flows. The main gaps are time-bounded authorization, batch
auth for swarm coordination, and documentation of the M2M identity
lifecycle (registration, heartbeat, key rotation, decommission). The
real-auth `.sign()` path is critical for M2M -- machines use actual keys,
not mock auth.
