# soroban-sdk-tools

[![Crates.io](https://img.shields.io/crates/v/soroban-sdk-tools.svg)](https://crates.io/crates/soroban-sdk-tools)
[![Documentation](https://docs.rs/soroban-sdk-tools/badge.svg)](https://docs.rs/soroban-sdk-tools)
[![CI](https://github.com/BlaineHeffron/soroban-sdk-tools/actions/workflows/ci.yml/badge.svg)](https://github.com/BlaineHeffron/soroban-sdk-tools/actions/workflows/ci.yml)

Proc macros and utilities for [soroban-sdk](https://crates.io/crates/soroban-sdk) that replace the repetitive parts of storage, error definitions, and auth test setup.

- [Getting started](#getting-started)
- [Storage](#storage)
- [Error handling](#error-handling)
- [Contract imports](#contract-imports)
- [Auth testing](#auth-testing)
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

## Auth testing

With `testutils` enabled you get helpers for mock and real-signature auth:

```rust
use soroban_sdk_tools::{setup_mock_auth, setup_real_auth, Secp256r1Keypair};

// Skip the crypto, just test your logic
setup_mock_auth(&env, &[&user], &client.address);
client.deposit(&user, &100);

// Or actually sign the payload with a real passkey
let kp = Secp256r1Keypair::generate();
let signer = setup_real_auth(&env, &kp);
client.deposit(&signer, &100);
```

Supports `ed25519`, `secp256k1`, and `secp256r1`. Details in the [auth docs](https://docs.rs/soroban-sdk-tools/latest/soroban_sdk_tools/auth/).

## Examples

Working contracts in [`examples/`](examples/) you can build and test:

- [`increment`](examples/increment/) - storage with struct and one-liner patterns
- [`errors/`](examples/errors/) - composable errors across contracts
- [`auth/`](examples/auth/) - token, vault, and atomic swap with auth testing
- [`features/`](examples/features/) - feature-gated storage

## License

Apache-2.0

## Acknowledgments

Inspired by [loam-soroban-sdk](https://github.com/loambuild/loam).
