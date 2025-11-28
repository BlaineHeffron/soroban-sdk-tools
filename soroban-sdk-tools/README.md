# soroban-sdk-tools

Enhanced tools for Soroban smart contract development, providing:

- **Advanced Storage Management**: Typed storage maps with automatic key optimization
- **Composable Error Handling**: The `#[scerr]` macro for clean error definitions
- **Authorization Testing**: Simplified auth testing utilities

## Quick Start

Add to your `Cargo.toml`:

```toml
[dependencies]
soroban-sdk-tools = "0.1"
```

## Features

### Storage Management

```rust
use soroban_sdk_tools::{contractstorage, PersistentMap, InstanceItem};

#[contractstorage]
pub struct TokenStorage {
    #[short_key("bal")]
    balances: PersistentMap<Address, i128>,
    admin: InstanceItem<Address>,
}
```

### Error Handling

```rust
use soroban_sdk_tools::scerr;

#[scerr]
pub enum Error {
    Unauthorized,
    InsufficientBalance,
}
```

### Authorization Testing

```rust
#[test]
fn test_transfer() {
    let client = TokenClient::new(&env, &contract_id);
    
    // Simplified authorization mocking
    client.with_auth(&user).transfer(&from, &to, &100);
}
```

## Documentation

See the [technical architecture](../ARCHITECTURE.md) for detailed information.

## License

Apache-2.0
