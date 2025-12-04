# soroban-sdk-tools

[![Crates.io](https://img.shields.io/crates/v/soroban-sdk-tools.svg)](https://crates.io/crates/soroban-sdk-tools)
[![Documentation](https://docs.rs/soroban-sdk-tools/badge.svg)](https://docs.rs/soroban-sdk-tools)

**soroban-sdk-tools** is a companion crate to the [rs-soroban-sdk](https://crates.io/crates/soroban-sdk), providing enhanced tools for Soroban smart contract development. It focuses on advanced storage management with automatic key optimization, composable error handling, and improved authorization testing utilities.

This crate builds upon the Soroban SDK to offer ergonomic abstractions, reducing boilerplate and optimizing on-chain resource usage. It is designed to be modular and compatible with existing Soroban workflows.

## Features

- **Advanced Storage Management**: Typed wrappers for persistent, instance, and temporary storage with automatic key shortening and hashing to minimize ledger footprint.
- **Composable Error Handling**: Procedural macro for defining error enums with automatic code assignment and cross-contract propagation (TODO: Full implementation details forthcoming).
- **Authorization Testing Utilities**: Simplified mocking and assertion for authorization flows in tests (TODO: Full implementation details forthcoming).
- **Key Optimization**: Automatic prefix shortening and hashing for compact storage keys, with user overrides for upgrade safety.
- **Flexible Key Modes**: Choose between hashed keys (compact) or symbolic keys (readable) for different use cases.

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
soroban-sdk = { version = "21.0.0" }  # Or your preferred version
soroban-sdk-tools = { version = "0.1.0" }  # Replace with the actual version
```

For test utilities, enable the `testutils` feature:

```toml
[dependencies]
soroban-sdk-tools = { version = "0.1.0", features = ["testutils"] }
```

## Usage

### Storage Management

The `#[contractstorage]` macro allows defining storage structs with typed wrappers around Soroban's storage interfaces. It provides automatic key management and reduces boilerplate for common storage operations.

#### Storage Types

**Map Types** (key-value storage):
- `PersistentMap<K, V>`: Long-term storage that persists across contract upgrades
- `InstanceMap<K, V>`: Per-instance configuration data with instance-level TTL
- `TemporaryMap<K, V>`: Ephemeral storage with automatic expiry

**Item Types** (single value storage):
- `PersistentItem<V>`: Single persistent value
- `InstanceItem<V>`: Single instance-scoped value
- `TemporaryItem<V>`: Single temporary value

All types support these operations:
- `get(&key)` / `get()` - Retrieve a value
- `set(&key, &value)` / `set(&value)` - Store a value
- `has(&key)` / `has()` - Check if a value exists
- `remove(&key)` / `remove()` - Delete a value
- `extend_ttl(...)` - Extend the time-to-live
- `update(&key, f)` / `update(f)` - Atomically read-modify-write

#### Basic Example

```rust
use soroban_sdk::{contract, contractimpl, Address, Env};
use soroban_sdk_tools::{contractstorage, PersistentMap, PersistentItem};

#[contractstorage]
pub struct TokenStorage {
    balances: PersistentMap<Address, u64>,
    total_supply: PersistentItem<u64>,
}

#[contract]
pub struct Token;

#[contractimpl]
impl Token {
    pub fn set_balance(env: &Env, addr: &Address, amount: u64) {
        TokenStorage::new(env).balances.set(addr, &amount);
    }
    
    pub fn get_balance(env: &Env, addr: &Address) -> Option<u64> {
        TokenStorage::new(env).balances.get(addr)
    }

    pub fn get_total_supply(env: &Env) -> Option<u64> {
        TokenStorage::new(env).total_supply.get()
    }
}
```

By default, this generates readable "DataKey-style" storage keys:
- Map entry for `balances`: `Vec[Symbol("Balances"), Address]`
- Item key for `total_supply`: `Symbol("TotalSupply")`

#### Key Optimization Modes

The macro supports several modes for optimizing storage keys to minimize ledger footprint:

##### 1. Auto-Shorten (Recommended for Production)

Use `auto_shorten = true` to automatically generate compact key prefixes:

```rust
#[contractstorage(auto_shorten = true)]
pub struct TokenStorage {
    balances: PersistentMap<Address, u64>,      // Prefix: "B"
    allowances: PersistentMap<(Address, Address), u64>, // Prefix: "A"
    total_supply: PersistentItem<u64>,          // Key: "T"
}
```

- Field names are shortened to their first letter (with collision avoidance)
- Map keys are hashed to `BytesN<32>` when composite key exceeds 32 bytes
- Item keys use short `Bytes` (1-3 bytes)
- **~30-40% storage savings** compared to symbolic keys

Generated accessor methods when using `auto_shorten`:

```rust
impl TokenStorage {
    pub fn balances(&self) -> &PersistentMap<Address, u64> { &self.balances }
    pub fn total_supply(&self) -> &PersistentItem<u64> { &self.total_supply }
}
```

##### 2. Custom Short Keys

Override automatic prefixes with `#[short_key]` for upgrade safety:

```rust
#[contractstorage(auto_shorten = true)]
pub struct TokenStorage {
    #[short_key = "bal"]
    balances: PersistentMap<Address, u64>,  // Prefix: "bal" (won't change if field renamed)
    
    #[short_key = "ts"]
    total_supply: PersistentItem<u64>,      // Key: "ts"
}
```

Use this when you need to ensure keys remain stable across field renames.

##### 3. Symbolic Keys (Development/Debugging)

Add `symbolic = true` for human-readable keys during development:

```rust
#[contractstorage(auto_shorten = true, symbolic = true)]
pub struct TokenStorage {
    balances: PersistentMap<Address, u64>,  // Key: Vec[Symbol("B"), Address]
    total_supply: PersistentItem<u64>,      // Key: Symbol("T")
}
```

Symbolic keys are easier to inspect in ledger debugging tools but use more storage.

##### 4. Field-Level Overrides

Mix modes by applying `#[symbolic]` to specific fields:

```rust
#[contractstorage(auto_shorten = true)]
pub struct TokenStorage {
    #[symbolic]
    balances: PersistentMap<Address, u64>,      // Readable: Vec[Symbol("B"), Address]
    
    allowances: PersistentMap<(Address, Address), u64>, // Optimized: BytesN<32>
}
```

#### Storage Key Modes Reference

The following table shows how different attribute combinations affect key generation:

| Mode | Field Declaration | Map Key Format | Item Key Format | Key Size | Use Case |
|------|------------------|----------------|-----------------|----------|----------|
| **Default** | `balances: PersistentMap<Address, u64>` | `Vec[Symbol("Balances"), Address]` | `Symbol("Balances")` | Medium | Readable keys, traditional pattern |
| **Default + short_key** | `#[short_key = "bal"]`<br/>`balances: PersistentMap<Address, u64>` | `Vec[Symbol("Bal"), Address]` | `Symbol("Bal")` | Medium | Custom readable keys |
| **auto_shorten** | `#[contractstorage(auto_shorten = true)]`<br/>`balances: PersistentMap<Address, u64>` | `BytesN<32>` (hash of `"B" + Address`) | `Bytes("B")` | **Smallest** | 🏆 Production, minimal footprint |
| **auto_shorten + short_key** | `#[contractstorage(auto_shorten = true)]`<br/>`#[short_key = "bal"]`<br/>`balances: PersistentMap<Address, u64>` | `BytesN<32>` (hash of `"bal" + Address`) | `Bytes("bal")` | Small | Upgrade-safe custom prefix |
| **auto_shorten + symbolic** | `#[contractstorage(auto_shorten = true, symbolic = true)]`<br/>`balances: PersistentMap<Address, u64>` | `Vec[Symbol("B"), Address]` | `Symbol("B")` | Medium | Development, readable short keys |
| **auto_shorten + symbolic + short_key** | `#[contractstorage(auto_shorten = true, symbolic = true)]`<br/>`#[short_key = "bal"]`<br/>`balances: PersistentMap<Address, u64>` | `Vec[Symbol("Bal"), Address]` | `Symbol("Bal")` | Medium | Custom readable short keys |
| **Field-level #[symbolic]** | `#[contractstorage(auto_shorten = true)]`<br/>`#[symbolic]`<br/>`balances: PersistentMap<Address, u64>` | `Vec[Symbol("B"), Address]` | `Symbol("B")` | Medium | Mixed: readable specific fields |

**Key Size Comparison:**
- **Smallest**: `BytesN<32>` for maps (32 bytes), `Bytes` for items (1-3 bytes) - **Production Recommended**
- **Small**: `Bytes` for short prefixed items (2-4 bytes)
- **Medium**: `Vec[Symbol, Key]` for maps (~40-50 bytes), `Symbol` for items (~10-20 bytes)

**Choosing a Mode:**

1. **Production contracts** 🏆: Use `auto_shorten = true` for ~30-40% storage savings
2. **Upgrade-safe contracts**: Use `auto_shorten = true` with explicit `#[short_key = "..."]`
3. **Development/debugging** 🔍: Add `symbolic = true` to make keys human-readable
4. **Traditional pattern**: Use default mode (no auto_shorten) for DataKey enum compatibility
5. **Mixed requirements**: Use `auto_shorten = true` with field-level `#[symbolic]` for specific readable fields

#### Key View Methods

Generated methods like `get_<struct>_<field>_key` allow inspecting the underlying storage key:

```rust
let storage = TokenStorage::new(&env);
let key = storage.get_token_storage_balances_key(addr.clone());
// Use this key with env.storage() directly if needed
```

### Error Handling

The `#[scerr]` macro simplifies defining contract error enums with automatic `u32` code assignment, `repr(u32)`, and required traits (`Copy`, `Clone`, `TryFromVal`, `IntoVal`).

#### Example

```rust
use soroban_sdk_tools::scerr;

#[scerr]
pub enum Error {
    InsufficientBalance,
    Unauthorized,
}
```

This generates a Soroban-compatible error enum with unique codes (e.g., 1 and 2). For composable errors across contracts, use `#[scerr(root)]` on the top-level enum (TODO: Detailed composability features, including `#[transparent]` and `#[from_contract_client]`, are under development).

### Authorization Testing

**TODO**: Full implementation and documentation for authorization utilities are forthcoming. These will include an enhanced contract client with methods like `with_auth` to automatically mock authorizations based on invocation parameters, reducing test boilerplate.

Example (placeholder):

```rust
#[cfg(test)]
use soroban_sdk_tools::auth::ContractClientExt;

// In tests:
client.with_auth(&user).increment(&user, &5);
```

This will automatically generate mock auth entries and execute the call, with support for assertions via `env.auths()`.

## Architecture and Internals

For a detailed technical overview, see [ARCHITECTURE.md](ARCHITECTURE.md). Key components include:

- `storage.rs`: Typed storage wrappers.
- `key.rs`: Key generation with hashing and shortening.
- `error.rs`: Error traits and conversions (TODO: Expansion planned).
- `auth.rs`: Test utilities (feature-gated, TODO: Completion planned).

## Contributing

Contributions are welcome! Please open an issue or pull request on the repository. Ensure changes align with the Soroban SDK and include tests.

## License

This project is licensed under the Apache-2.0 License.

## Acknowledgments

This crate draws inspiration from the [loam-soroban-sdk](https://github.com/iron-fish/loam) project and aims to enhance the Soroban ecosystem.