---
persona: Yuki
age: 31
background: On-chain game developer, built two fully on-chain games on StarkNet, porting to Soroban
focus: Gaming composability, entity-component-system patterns, state machines, tick-based updates
tone: Practical, thinks in game loops and entity hierarchies, always asks "but does it work at 1000 TPS"
---

# Review: soroban-sdk-tools -- Through a Game Developer's Lens

## The ECS Parallel

When I see the `#[contracttrait]` pattern, I immediately think: this is an
Entity-Component-System architecture for smart contracts.

| ECS Concept   | soroban-sdk-tools Equivalent      |
|---------------|-----------------------------------|
| Entity        | Contract struct (`TestContract`)   |
| Component     | Trait (`Ownable`, `Pausable`)      |
| System        | Provider (`SingleOwner`)           |

In ECS, entities are just IDs. Components hold data. Systems hold logic.
Components can be mixed and matched on entities freely. This is exactly what the
provider pattern enables: a contract (entity) gets behaviors (components) with
swappable logic (systems).

This makes me optimistic. But games stress-test composition patterns in ways
that DeFi does not.

## Game-Specific Composability Needs

### 1. Tick-Based State Updates

On-chain games need a `tick()` function that updates game state atomically.
Multiple traits might need to participate in a tick:

```rust
#[contracttrait]
pub trait Tickable {
    fn tick(env: &Env, current_ledger: u32);
}

#[contracttrait]
pub trait MovementSystem: Tickable {
    fn get_position(env: &Env, entity_id: u64) -> (i64, i64);

    #[auth(Self::game_master)]  // but who is game_master? Not in this trait!
    fn set_position(env: &Env, entity_id: u64, x: i64, y: i64);
}
```

The problem: `MovementSystem` needs auth from a `game_master` role that is
defined in a different trait (say, `GameAdmin`). The `#[auth(Self::method)]`
syntax requires the method to exist on `Self`, but if `GameAdmin` is not a
supertrait of `MovementSystem`, this fails.

**Question**: Can `#[auth]` reference methods from non-supertrait composed
traits? The current implementation looks like it only supports methods on the
internal trait chain. For games, cross-trait auth references are essential.

### 2. Batch Operations

Games process hundreds of state changes per transaction. The current pattern
calls `require_auth()` per method invocation. If a game master needs to update
100 entity positions in a single transaction, that is 100 `require_auth()` calls.

In Soroban, `require_auth()` is idempotent within a transaction (the auth is
checked once and cached). But the provider's `owner()` / `game_master()` method
is called 100 times, hitting storage 100 times.

**Suggestion**: The outer trait should cache the auth resolution per env
context:

```rust
fn set_position(env: &Env, entity_id: u64, x: i64, y: i64) {
    // Instead of: Self::Provider::game_master(env).require_auth();
    // Cache: only read storage once per tx
    env.storage().temporary().get_or_set("__cached_gm", || {
        Self::Provider::game_master(env)
    }).require_auth();
}
```

This is a game-critical optimization. 100 storage reads vs 1 is the difference
between fitting in a transaction and exceeding resource limits.

### 3. State Machine Composition

Games are state machines. A character can be `Idle`, `Moving`, `Attacking`,
`Dead`. Transitions between states have guards (you cannot attack while dead).

The `#[auth]` pattern handles "who can call this." But games also need "when
can this be called" -- state guards:

```rust
#[contracttrait]
pub trait CombatSystem {
    fn get_state(env: &Env, entity_id: u64) -> CharacterState;

    #[auth(entity_owner)]
    #[guard(Self::get_state(entity_id) != CharacterState::Dead)]
    fn attack(env: &Env, entity_owner: Address, entity_id: u64, target_id: u64);
}
```

The `#[guard]` annotation would be the state-machine equivalent of `#[auth]` --
structural enforcement of preconditions. The provider handles the logic; the
guard ensures the precondition in the outer trait.

**Feature request**: Consider a `#[require]` or `#[guard]` attribute that calls
a provider method and asserts the result before delegating. This would make the
framework useful for state-machine-heavy domains like games.

### 4. Event Composition

The OZ comparison notes that OZ emits events for every state change and suggests
soroban-sdk-tools should adopt this. For games, events are not optional -- they
are the primary way clients reconstruct game state.

The provider pattern puts event emission in the provider's responsibility. But
this means every provider author must remember to emit events. If I write a
`FastMovementProvider` that optimizes position updates but forgets to emit
`PositionChanged` events, my game client breaks silently.

**Suggestion**: The `#[contracttrait]` macro should support `#[event]`
annotations that auto-emit events from the outer trait, before delegating to the
provider:

```rust
#[contracttrait]
pub trait MovementSystem {
    #[auth(entity_owner)]
    #[event(PositionChanged { entity_id, x, y })]
    fn set_position(env: &Env, entity_owner: Address, entity_id: u64, x: i64, y: i64);
}
```

Events in the outer trait means providers cannot forget them. This is the same
principle as structural auth -- structural event emission.

## The Supertrait Chain Depth Problem

Games compose many behaviors. A full game entity might implement:

```
Ownable + Pausable + Tickable + MovementSystem + CombatSystem + InventorySystem + QuestSystem
```

That is 7 traits. If each has a supertrait chain depth of 2, the total
supertrait resolution is O(7 * 2^2) = O(28) trait bound checks at compile time.
In practice, the Rust compiler handles this fine, but the error messages when a
bound is unsatisfied become unreadable.

**Request**: When a provider fails to satisfy a deep supertrait chain, can the
macro generate a custom error that shows the full chain? Something like:

```
error: Provider `FastEngine` implements `MovementSystemInternal` but not
       `TickableInternal`, which is required because:
       MovementSystem: Tickable: Ownable
       ^^^^^^^^^^^^^^^^^^^^^^^^
       `FastEngine` is missing this implementation
```

## What Works for Games

1. **Provider swapping**: Huge for games. I can have a `TestMovementProvider`
   that skips collision detection and a `ProductionMovementProvider` that
   includes it. Same contract, different providers.
2. **Sealed auth**: Games must prevent griefing. The sealed macro means players
   cannot bypass auth even if there is a bug in the contract's `contractimpl`.
3. **AuthClient testing**: Game testing requires simulating many actors. The
   `.authorize(&player).invoke()` pattern makes multi-actor tests readable.

## What Is Missing for Games

1. Cross-trait auth references (game master from a non-supertrait trait)
2. Batch operation caching for auth resolution
3. State guards (`#[guard]` attribute)
4. Structural event emission (`#[event]` attribute)
5. Better error messages for deep supertrait chains

## Verdict

The framework is an excellent foundation. The provider pattern maps cleanly to
ECS. But games need two features that DeFi does not: state guards and cross-trait
composition. Adding `#[guard]` and `#[event]` attributes would make this the
first smart contract framework that is genuinely game-ready.

**Rating: 7/10 for game composability** -- strong foundation, needs game-specific
extensions.
