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

The `#[scerr]` macro simplifies defining contract error enums with automatic `u32` code assignment, required traits, and composable error handling across contracts.

#### Basic Mode

For simple error enums within a single contract, use `#[scerr]`:

```rust
use soroban_sdk_tools::scerr;

#[scerr]
pub enum TokenError {
    #[description = "insufficient balance for transfer"]
    InsufficientBalance,
    Unauthorized,
    InvalidAmount,
}
```

This generates:
- A `#[contracterror]` enum with `#[repr(u32)]`
- Unique codes via DJB2 hashing (guaranteed non-zero)
- `ContractError` trait implementation with `into_code()`, `from_code()`, and `description()` methods
- All required Soroban conversion traits (`TryFromVal`, `IntoVal`, etc.)

**Custom descriptions**: Use `#[description = "..."]` on variants for human-readable error messages. Without it, the variant name is used.

#### Root Mode - Composable Errors

For contracts that call other contracts or need to propagate errors from sub-modules, use `#[scerr(root)]`:

```rust
use soroban_sdk_tools::scerr;

// Inner contract error
#[scerr]
pub enum MathError {
    DivisionByZero,
    NegativeInput,
}

// Outer contract with composable error handling
#[scerr(root)]
pub enum AppError {
    // Own error variants
    Unauthorized,
    InvalidState,
    
    // Transparent propagation for in-process calls
    #[transparent]
    Math(#[from] MathError),
    
    // Cross-contract error handling
    #[from_contract_client]
    ExternalMath(MathError),
    
    // Handle abort errors (code 0)
    #[from_abort_error]
    Abort,
}
```

**Root mode features**:

1. **Transparent Propagation** (`#[transparent]`): For in-process error propagation using `?` operator
   ```rust
   pub fn calculate(x: i64, y: i64) -> Result<i64, AppError> {
       // Math::divide returns Result<i64, MathError>
       // The ? operator converts via From<MathError> to AppError::Math
       let result = Math::divide(x, y)?;
       Ok(result)
   }
   ```

2. **Cross-Contract Errors** (`#[from_contract_client]`): For handling errors from contract clients
   ```rust
   pub fn call_external(env: Env, contract_id: Address) -> Result<i64, AppError> {
       let client = MathClient::new(&env, &contract_id);
       // Use ?? to handle both InvokeError and inner error
       let result = client.try_divide(&10, &0)??;
       Ok(result)
   }
   ```

3. **Abort Handling** (`#[from_abort_error]`): Handles `InvokeError::Abort` (code 0)
   ```rust
   #[from_abort_error]
   Abort,
   ```

4. **Automatic Conversions**: The `#[from]` attribute generates `From` impls:
   ```rust
   let math_err = MathError::DivisionByZero;
   let app_err: AppError = math_err.into(); // Converts to AppError::Math
   ```

#### Architecture: 24/8 Bit Split

Root mode uses a 24/8 bit split to prevent code collisions:
- **Unit variants** (e.g., `Unauthorized`): Use codes 1-255 (8-bit namespace)
- **Wrapped variants** (e.g., `Math(MathError)`): Use high 8 bits for namespace (index 1-255), low 24 bits for inner error code

This ensures that `AppError::Unauthorized` (code 1) never collides with `AppError::Math(MathError::DivisionByZero)` (code `2 << 24 | inner_code`).

#### Mixed Usage Example

You can use both `#[transparent]` and `#[from_contract_client]` for the same error type:

```rust
#[scerr(root)]
pub enum AppError {
    // For direct calls (same Wasm instance)
    #[transparent]
    MathDirect(#[from] MathError),
    
    // For cross-contract calls
    #[from_contract_client]
    MathRemote(MathError),
}

// Direct call uses transparent variant
pub fn direct_calc(x: i64, y: i64) -> Result<i64, AppError> {
    Math::divide(x, y)?  // Converts to MathDirect
}

// Cross-contract call uses from_contract_client variant
pub fn remote_calc(env: Env, id: Address, x: i64, y: i64) -> Result<i64, AppError> {
    let client = MathClient::new(&env, &id);
    client.try_divide(&x, &y)??  // Converts to MathRemote
}
```

#### Error Descriptions

Access human-readable descriptions via the `ContractError` trait:

```rust
let err = TokenError::InsufficientBalance;
println!("{}", err.description()); // "insufficient balance for transfer"
```

#### Best Practices

- **Basic mode**: Use for simple contracts without cross-contract calls
- **Root mode**: Use in contracts that call other contracts or need error composition
- **Transparent**: Use for in-process error propagation (same Wasm)
- **from_contract_client**: Use for cross-contract invocations
- **from_abort_error**: Always include one variant with this attribute in root mode to handle unexpected aborts
- **Avoid collisions**: Root mode automatically prevents collisions via bit splitting; basic mode uses hashing

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

This crate draws inspiration from the [loam-soroban-sdk](https://github.com/loambuild/loam) project and aims to enhance the Soroban ecosystem.