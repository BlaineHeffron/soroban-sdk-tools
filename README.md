# soroban-sdk-tools

[![Crates.io](https://img.shields.io/crates/v/soroban-sdk-tools.svg)](https://crates.io/crates/soroban-sdk-tools)
[![Documentation](https://docs.rs/soroban-sdk-tools/badge.svg)](https://docs.rs/soroban-sdk-tools)
[![CI](https://github.com/BlaineHeffron/soroban-sdk-tools/actions/workflows/ci.yml/badge.svg)](https://github.com/BlaineHeffron/soroban-sdk-tools/actions/workflows/ci.yml)

Proc macros and utilities for [soroban-sdk](https://crates.io/crates/soroban-sdk) that replace the repetitive parts of storage, error definitions, and auth test setup.

- [Getting started](#getting-started)
- [Storage](#storage)
- [Error handling](#error-handling)
- [Contract imports](#contract-imports)
- [AuthClient](#authclient)
- [Examples](#examples)

## Getting started

```sh
cargo add soroban-sdk-tools
cargo add soroban-sdk-tools --dev --features testutils
```

Or add to your contract's `Cargo.toml` directly:

```toml
[dependencies]
soroban-sdk-tools = "0.1"

[dev-dependencies]
soroban-sdk-tools = { version = "0.1", features = ["testutils"] }
```

The `testutils` feature pulls in auth helpers and crypto deps. Keep it in `dev-dependencies` so it doesn't end up in your WASM.

## Storage

`#[contractstorage]` turns a struct into typed storage handles:

```rust
use soroban_sdk::{contract, contractimpl, Address, Env};
use soroban_sdk_tools::{contractstorage, PersistentMap, InstanceItem};

#[contractstorage]
pub struct MyStorage {
    balances: PersistentMap<Address, u64>,
    total_supply: InstanceItem<u64>,
}

#[contract]
pub struct Token;

#[contractimpl]
impl Token {
    pub fn mint(env: &Env, to: &Address, amount: u64) {
        // One-liners for single operations
        let supply = MyStorage::get_total_supply(env).unwrap_or(0);
        MyStorage::set_total_supply(env, &(supply + amount));
        MyStorage::update_balances(env, to, |bal| bal.unwrap_or(0) + amount);
    }

    pub fn transfer(env: &Env, from: &Address, to: &Address, amount: u64) {
        // Grab the struct when you need multiple fields
        let s = MyStorage::new(env);
        let from_bal = s.balances.get(from).unwrap_or(0);
        s.balances.set(from, &(from_bal - amount));
        s.balances.set(to, &(s.balances.get(to).unwrap_or(0) + amount));
    }
}
```

There's a Map and Item variant for each durability: `PersistentMap<K,V>` / `PersistentItem<V>`, `InstanceMap<K,V>` / `InstanceItem<V>`, `TemporaryMap<K,V>` / `TemporaryItem<V>`.

`#[contractstorage(auto_shorten = true)]` compresses keys automatically, which typically saves 30-40% on storage fees. More detail in the [storage docs on docs.rs](https://docs.rs/soroban-sdk-tools/latest/soroban_sdk_tools/storage/).

## Error handling

`#[scerr]` generates a `#[contracterror]` enum with sequential codes and doc-comment descriptions:

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

For cross-contract calls, `#[transparent]` propagates errors in the same WASM via `?`, and `#[from_contract_client]` handles `try_` calls via `??`:

```rust
#[scerr]
pub enum AppError {
    Unauthorized,

    #[transparent]
    Token(#[from] TokenError),       // converts via ? operator

    #[from_contract_client]
    External(ExternalError),         // converts via ?? on try_ calls
}
```

Codes are assigned sequentially and the WASM spec comes out fully flattened, so your TypeScript bindings see every variant without gaps. See [error docs](https://docs.rs/soroban-sdk-tools/latest/soroban_sdk_tools/error/) for the composition rules.

## Contract imports

Works like the SDK's `contractimport!`. The difference is it generates `ContractError` and `ContractErrorSpec` trait impls too, which `#[scerr]` needs for composition:

```rust
mod math {
    soroban_sdk_tools::contractimport!(
        file = "../target/wasm32v1-none/release/math_contract.wasm"
    );
}

#[scerr]
pub enum AppError {
    #[from_contract_client]
    Math(math::MathError),
}
```

## AuthClient

`contractimport!` generates an `AuthClient` alongside the normal `Client`. It saves you from hand-building auth entries in tests. Call a method, chain `.authorize()` or `.sign()`, then `.invoke()`:

```rust
mod my_contract {
    soroban_sdk_tools::contractimport!(
        file = "../target/wasm32v1-none/release/my_contract.wasm"
    );
}

#[test]
fn test_mock_auth() {
    let env = Env::default();
    let contract_id = env.register(my_contract::WASM, ());
    let client = my_contract::AuthClient::new(&env, &contract_id);
    let user = Address::generate(&env);

    // method -> authorize -> invoke
    client.deposit(&user, &100).authorize(&user).invoke();
}

#[test]
fn test_real_auth() {
    let env = Env::default();
    let contract_id = env.register(my_contract::WASM, ());
    let client = my_contract::AuthClient::new(&env, &contract_id);

    let alice = Keypair::random(&env);
    client.transfer(alice.address(), &bob, &300).sign(&alice).invoke();
}
```

Chain multiple authorizers when you need them: `.authorize(&signer1).authorize(&signer2).invoke()`. Supports `ed25519`, `secp256k1`, and `secp256r1`. See the [auth docs](https://docs.rs/soroban-sdk-tools/latest/soroban_sdk_tools/auth/).

## Examples

Working contracts in [`examples/`](https://github.com/BlaineHeffron/soroban-sdk-tools/tree/main/examples) you can build and test:

- [`increment`](https://github.com/BlaineHeffron/soroban-sdk-tools/tree/main/examples/increment) - storage with struct and one-liner patterns
- [`errors/`](https://github.com/BlaineHeffron/soroban-sdk-tools/tree/main/examples/errors) - composable errors across contracts
- [`auth/`](https://github.com/BlaineHeffron/soroban-sdk-tools/tree/main/examples/auth) - token, vault, and atomic swap with auth testing
- [`features/`](https://github.com/BlaineHeffron/soroban-sdk-tools/tree/main/examples/features) - feature-gated storage

## License

Apache-2.0

## Acknowledgments

Inspired by [loam-soroban-sdk](https://github.com/loambuild/loam).
