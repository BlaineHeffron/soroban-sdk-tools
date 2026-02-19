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

#### Storage Types

- `PersistentMap<K, V>`: Long-term storage.
- `InstanceMap<K, V>`: Per-instance data.
- `TemporaryMap<K, V>`: Ephemeral data.
- Single-value variants: `PersistentItem<V>`, etc.

Operations include `get`, `set`, `has`, `remove`, `extend_ttl`, and `update`.

For more examples, see the [tests](tests/) directory, which covers numeric keys, tuples, auto-shortening, symbolic modes, and DataKey-style patterns.


### Error Handling

The `#[scerr]` macro simplifies defining contract error enums with automatic `u32` code assignment, required traits, and composable error handling across contracts.

#### Basic Usage

For simple error enums within a single contract, use `#[scerr]`:

```rust
use soroban_sdk_tools::scerr;

#[scerr]
pub enum TokenError {
    /// insufficient balance for transfer
    InsufficientBalance,
    Unauthorized,
    InvalidAmount,
}
```

This generates:
- A `#[contracterror]` enum with `#[repr(u32)]`
- Sequential codes starting at 1 (e.g., `InsufficientBalance = 1`, `Unauthorized = 2`, `InvalidAmount = 3`)
- `ContractError` trait implementation with `into_code()`, `from_code()`, and `description()` methods
- `ContractErrorSpec` trait implementation (enables use as inner type in composable errors)
- All required Soroban conversion traits (`TryFromVal`, `IntoVal`, etc.)

**Descriptions**: Doc comments (`///`) on variants become human-readable error descriptions. Without a doc comment, the variant name is used.

#### Auto-Detection: Basic vs Advanced Mode

The scerr macro **automatically detects** whether your error enum needs basic or advanced (composable) mode based on the enum's structure:

**Basic mode** (auto-detected): All variants are unit variants with no special attributes
```rust
#[scerr]
pub enum SimpleError {
    NotFound,
    Unauthorized,
}
```

**Advanced mode** (auto-detected): Any variant has `#[transparent]`, `#[from_contract_client]`, `#[abort]`, `#[sentinel]` attributes, or carries data
```rust
#[scerr]
pub enum ComposableError {
    Unauthorized,
    #[from_contract_client]
    External(ExternalError),  // Auto-detects advanced mode
}
```

#### Composable Errors

For contracts that call other contracts or need to propagate errors from sub-modules:

```rust
use soroban_sdk_tools::scerr;

// Inner contract error
#[scerr]
pub enum MathError {
    DivisionByZero,
    NegativeInput,
}

// Outer contract with composable error handling
// Mode is auto-detected due to #[transparent] and #[from_contract_client]
#[scerr]
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
}
```

**Advanced mode features**:

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

3. **Automatic Conversions**: The `#[from]` attribute generates `From` impls:
   ```rust
   let math_err = MathError::DivisionByZero;
   let app_err: AppError = math_err.into(); // Converts to AppError::Math
   ```

#### Cross-Contract Error Handling Options

When using cross-contract error composition, you can configure handling for edge cases:

##### Abort Handling (`handle_abort`)

Controls how `InvokeError::Abort` (error code 0) is handled:

- **`"auto"` (default)**: Auto-generates an `Aborted` variant
  ```rust
  #[scerr]  // Auto-generates Aborted variant by default
  pub enum AppError {
      #[from_contract_client]
      Math(MathError),
      // Auto-generated: Aborted
  }
  ```

- **`"panic"`**: Panics when an external call aborts
  ```rust
  #[scerr(handle_abort = "panic")]
  pub enum AppError {
      #[from_contract_client]
      Math(MathError),
  }
  ```

- **Custom with `#[abort]`**: Define your own abort handler
  ```rust
  #[scerr]
  pub enum AppError {
      #[abort]
      CallAborted,  // Catches InvokeError::Abort
      #[from_contract_client]
      Math(MathError),
  }
  ```

##### Unknown Error Handling (`handle_unknown`)

Controls how unmapped error codes from external contracts are handled:

- **`"auto"` (default)**: Auto-generates an `UnknownError` variant
  ```rust
  #[scerr]  // Auto-generates UnknownError variant by default
  pub enum AppError {
      #[from_contract_client]
      Math(MathError),
      // Auto-generated: UnknownError
  }
  ```

- **`"panic"`**: Panics on unknown error codes
  ```rust
  #[scerr(handle_unknown = "panic")]
  pub enum AppError {
      #[from_contract_client]
      Math(MathError),
  }
  ```

- **Custom with `#[sentinel]`**: Define your own unknown error handler
  ```rust
  #[scerr]
  pub enum AppError {
      #[sentinel]
      Unmapped,  // Unit variant

      // Or store the unknown code:
      #[sentinel]
      UnknownError(u32),  // Stores original error code
      #[from_contract_client]
      Math(MathError),
  }
  ```

##### Error Logging (`log_unknown_errors`)

When enabled, logs unknown error codes via `env.logs().add()` before mapping to the sentinel variant:

```rust
#[scerr(log_unknown_errors = true)]
pub enum AppError {
    InvalidInput,
    #[from_contract_client]
    Math(MathError),
    // Auto-generated: Aborted, UnknownError(u32) - stores code for logging
}

// Use the logging helper in your contract:
pub fn call_external(env: Env, id: Address) -> Result<(), AppError> {
    let client = ExternalClient::new(&env, &id);
    let result = client.try_some_method();

    match result {
        Ok(v) => v,
        Err(e) => return Err(AppError::from_invoke_error(&env, e)),
    }?;

    Ok(())
}
```

**Requirements**: `log_unknown_errors = true` requires either `handle_unknown = "auto"` or an explicit `#[sentinel]` variant with code storage.

##### Complete Configuration Example

```rust
#[scerr(
    handle_abort = "auto",       // Auto-generate Aborted variant
    handle_unknown = "auto",     // Auto-generate UnknownError variant
    log_unknown_errors = true    // Log unknown codes
)]
pub enum AppError {
    // Your error variants
    Unauthorized,
    InvalidState,

    // Cross-contract errors
    #[from_contract_client]
    Math(MathError),

    #[from_contract_client]
    Token(TokenError),

    // Auto-generated variants:
    // - Aborted
    // - UnknownError(u32)
}
```

#### Architecture: Sequential Code Assignment

Advanced mode assigns error codes **sequentially** starting at 1. When a variant wraps another error type (via `#[transparent]` or `#[from_contract_client]`), the inner type's variants are **flattened** into the sequential range at their position.

For example, given:
```rust
#[scerr]
pub enum AppError {
    Unauthorized,                           // code 1
    #[transparent]
    Math(#[from] MathError),                // codes 2-3 (MathError has 2 variants)
    #[from_contract_client]
    External(ExternalError),                // codes 4-6 (ExternalError has 3 variants)
    // Auto-generated: Aborted = 7, UnknownError = 8
}
```

Code assignment uses **const-chaining** at compile time:
```rust
const UNAUTHORIZED: u32 = 1;
const MATH_OFFSET: u32 = UNAUTHORIZED + 1;                    // 2
const MATH_COUNT: u32 = <MathError as ContractErrorSpec>::SPEC_ENTRIES.len(); // 2
const EXTERNAL_OFFSET: u32 = MATH_OFFSET + MATH_COUNT - 1 + 1; // 4
const EXTERNAL_COUNT: u32 = <ExternalError as ContractErrorSpec>::SPEC_ENTRIES.len(); // 3
const ABORTED: u32 = EXTERNAL_OFFSET + EXTERNAL_COUNT - 1 + 1; // 7
```

This guarantees no code collisions and produces compact, predictable codes.

**Inner type requirement:** All types used with `#[transparent]` or `#[from_contract_client]` must implement `ContractError` and `ContractErrorSpec`. These traits are automatically provided by:
- `#[scerr]` (for locally-defined error types)
- `contractimport!` (for error types imported from WASM)

Using a plain `#[contracterror]` type without these traits will produce a **compile error**.

> **Note on `contractimport!`**: When importing a WASM contract via `contractimport!`, the `ContractErrorSpec` trait is generated for all error types found in the WASM spec — even if the original contract used plain `#[contracterror]` instead of `#[scerr]`. This means cross-contract error composition works with any contract, as long as its errors are imported via `contractimport!`. The compile error only occurs when using a Cargo dependency that defines a plain `#[contracterror]` type without `#[scerr]`.

#### WASM Spec Flattening

Advanced-mode enums produce a fully flattened error enum in the WASM contract spec (`ScSpecUdtErrorEnumV0`). All inner error variants are recursively expanded with prefixed names and sequential codes, making every error code visible to TypeScript bindings and other tooling.

For example, given this nesting:
```rust
#[scerr]
pub enum DeepError { DeepFailureOne, DeepFailureTwo }

#[scerr]
pub enum MiddleError {
    MiddleFailure,
    #[from_contract_client]
    Deep(DeepError),
}

#[scerr]
pub enum OuterError {
    OuterFailure,
    #[from_contract_client]
    Middle(MiddleError),
}
```

The WASM spec for `OuterError` contains:
```
OuterFailure            = 1
Middle_MiddleFailure    = 2
Middle_Deep_DeepFailureOne  = 3
Middle_Deep_DeepFailureTwo  = 4
Middle_Aborted          = 5
Middle_UnknownError     = 6
Aborted                 = 7
UnknownError            = 8
```

TypeScript developers see a single flat enum with all error codes — no gaps, no hidden variants.

**Requirements for flattened specs:** Inner types must provide a `SPEC_TREE` via `ContractErrorSpec`. This is automatically generated by:
- `#[scerr]` (both basic and advanced mode)
- `contractimport!` (for any WASM error type, including plain `#[contracterror]`)

#### Mixed Usage Example

You can use both `#[transparent]` and `#[from_contract_client]` for the same error type:

```rust
#[scerr]
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
    Ok(Math::divide(x, y)?) // Converts to MathDirect
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

- **Simple errors**: Just use `#[scerr]` for contracts without cross-contract calls
- **Composable errors**: Add `#[transparent]` or `#[from_contract_client]` attributes and the macro auto-detects advanced mode
- **Inner types**: All types wrapped by `#[transparent]` or `#[from_contract_client]` must use `#[scerr]` (for local types) or `contractimport!` (for imported types). Plain `#[contracterror]` types will cause a compile error.
- **Transparent**: Use for in-process error propagation (same Wasm)
- **from_contract_client**: Use for cross-contract invocations
- **handle_abort**: Set to `"auto"` or add `#[abort]` variant to gracefully handle aborts instead of panicking
- **handle_unknown**: Set to `"auto"` for resilient error handling when calling contracts with unknown error codes
- **log_unknown_errors**: Enable for debugging/monitoring unknown errors in production

#### Cross-Contract Imports

When importing contracts via WASM files, use `contractimport!` to generate the standard import plus the `ContractError` and `ContractErrorSpec` trait implementations needed by `#[scerr]`:

```rust
mod math_imported {
    soroban_sdk_tools::contractimport!(
        file = "../target/wasm32v1-none/release/math_contract.wasm"
    );
}

#[scerr]
pub enum AppError {
    Unauthorized,

    #[from_contract_client]
    Math(math_imported::MathError),
}
```

The macro generates:
- The standard `contractimport!` output (client, types, etc.)
- `ContractError` impl (`into_code`, `from_code`, `description`) for each imported error type
- `ContractErrorSpec` impl (variant metadata) for each imported error type

**Parameters**:
- `file` (required): Path to the WASM file to import
- `sha256` (optional): SHA256 hash to verify the WASM file

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