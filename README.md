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

The `#[contractstorage]` macro allows defining storage structs with automatic key optimization. It generates accessors and optimizes keys by shortening prefixes (via `auto_shorten = true`) or using explicit short keys (via `#[short_key = "shortname"]`).

#### Basic Example

```rust
use soroban_sdk::{contract, contractimpl, Address, Env};
use soroban_sdk_tools::{contractstorage, PersistentMap, PersistentItem};

#[contractstorage(auto_shorten = true)]
pub struct TokenStorage {
    balances: PersistentMap<Address, u64>,
    total_supply: PersistentItem<u64>,
}

#[contract]
pub struct Token;

#[contractimpl]
impl Token {
    pub fn set_balance(env: Env, addr: &Address, amount: u64) {
        TokenStorage::new(&env).balances().set(addr, &amount);
    }

    pub fn get_balance(env: Env, addr: &Address) -> Option<u64> {
        TokenStorage::new(&env).balances().get(addr)
    }
}
```

- **auto_shorten**: Automatically assigns short prefixes (e.g., "B" for balances) to minimize key size. For composite keys >32 bytes, hashes to `BytesN<32>`.
- **short_key**: Override with a custom short prefix, e.g., `#[short_key = "bal"] balances: PersistentMap<Address, u64>`.

#### Symbolic Keys

By default, `auto_shorten` uses hashed keys (`Bytes` or `BytesN<32>`) for optimal compactness. For more readable keys during development or debugging, use the `symbolic` option:

```rust
#[contractstorage(auto_shorten = true, symbolic = true)]
pub struct TokenStorage {
    balances: PersistentMap<Address, u64>,  // Stored as Vec[Symbol("Ba"), Address]
    total: PersistentItem<u64>,             // Stored as Symbol("T")
}
```

With `symbolic = true`:
- **Map keys**: `Vec[Symbol("ShortPrefix"), key_value]` instead of hashed `BytesN<32>`
- **Item keys**: `Symbol("ShortPrefix")` instead of short `Bytes`

This makes keys human-readable in ledger inspection tools, at the cost of slightly larger keys for maps.

You can also apply `#[symbolic]` to individual fields to override the struct-level behavior:

```rust
#[contractstorage(auto_shorten = true)]
pub struct MixedStorage {
    #[symbolic]
    readable: PersistentMap<Address, u64>,  // Uses Vec[Symbol(...), Address]
    optimized: PersistentMap<Address, u64>, // Uses hashed BytesN<32>
}
```

**When to use symbolic keys:**
- Development and debugging for easier inspection
- Contracts where key readability outweighs storage optimization
- Testing scenarios where you need to verify specific key formats

**When to use hashed keys (default):**
- Production contracts prioritizing minimal ledger footprint (~30% key size reduction)
- High-frequency storage operations
- Contracts with many storage fields

#### Key View Methods

Generated methods like `get_storage_<field>_key` allow inspecting the underlying storage key:

```rust
let storage = TokenStorage::new(&env);
let key = storage.get_storage_balances_key(addr.clone());
```

#### Storage Types

- `PersistentMap<K, V>`: Long-term storage.
- `InstanceMap<K, V>`: Per-instance data.
- `TemporaryMap<K, V>`: Ephemeral data.
- Single-value variants: `PersistentItem<V>`, etc.

Operations include `get`, `set`, `has`, `remove`, `extend_ttl`, and `update`.

For more examples, see the [tests](tests/) directory, which covers numeric keys, tuples, auto-shortening, symbolic modes, and DataKey-style patterns.

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