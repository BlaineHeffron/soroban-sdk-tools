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
- Unique codes via DJB2 hashing (guaranteed non-zero)
- `ContractError` trait implementation with `into_code()`, `from_code()`, and `description()` methods
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

#### Architecture: 10/22 Bit Split

Advanced mode (auto-detected when using composable errors) uses a 10/22 bit split to prevent code collisions:
- **Unit variants** (e.g., `Unauthorized`, `Aborted`): Use sequential codes 1-1023 (low 10 bits)
- **Wrapped variants** (e.g., `Math(MathError)`): Use high 22 bits for namespace (~4 million values), low 10 bits for inner error code (up to 1024 codes per namespace)

Namespaces are assigned using a hash of the type name (`hash(type_name) % 4194303 + 1`), ensuring deterministic and collision-resistant assignment across contracts.

This ensures that `AppError::Unauthorized` (code 1) never collides with `AppError::Math(MathError::DivisionByZero)` (code `namespace << 10 | inner_code`).

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
- **Transparent**: Use for in-process error propagation (same Wasm)
- **from_contract_client**: Use for cross-contract invocations
- **handle_abort**: Set to `"auto"` or add `#[abort]` variant to gracefully handle aborts instead of panicking
- **handle_unknown**: Set to `"auto"` for resilient error handling when calling contracts with unknown error codes
- **log_unknown_errors**: Enable for debugging/monitoring unknown errors in production

#### Cross-Contract Imports with TypeScript Bindings

When importing contracts via WASM files, use `contractimport_with_errors!` with the `scerr_variant` and `outer_error` parameters to generate well-organized TypeScript bindings:

```rust
mod math_imported {
    soroban_sdk_tools::contractimport_with_errors!(
        file = "../target/wasm32v1-none/release/math_contract.wasm",
        scerr_variant = "Math",        // Must match the variant name in scerr
        outer_error = "AppError"       // Names the flattened spec for clarity
    );
}

#[scerr]
pub enum AppError {
    Unauthorized,

    #[from_contract_client]
    Math(math_imported::MathError),  // Variant name matches scerr_variant
}
```

This produces TypeScript bindings with clear naming relationships:

```typescript
// Inner error codes (raw)
export const MathError = {
  704653: {message: "DivisionByZero"},
  3712432: {message: "NegativeInput"}
}

// Flattened codes - named to show relationship to AppError
export const AppError_Math = {
  2915745933: {message: "Math_DivisionByZero"},
  2918753712: {message: "Math_NegativeInput"}
}

// Main error enum with unit variants
export const AppError = {
  1: {message: "Unauthorized"},
  696: {message: "Aborted"},
  697: {message: "UnknownError"}
}
```

Combine them for comprehensive error matching:

```typescript
// Create a combined error map
const AllErrors = {...AppError, ...AppError_Math};

try {
  await client.div_imported({math_id, num: 10n, denom: 0n});
} catch (e) {
  const code = getErrorCode(e);
  if (code in AllErrors) {
    console.log(`Error: ${AllErrors[code].message}`);
  }
}
```

**Parameters**:
- `scerr_variant`: Must match the variant name in `#[from_contract_client]`. Used for namespace computation.
- `outer_error`: Names the flattened spec as `{outer_error}_{variant}` (e.g., `AppError_Math`). If omitted, defaults to `{variant}_Flattened`.

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