//! Core integration tests for contractstorage macro.
//! Covers basic operations, contract initialization, and storage persistence.

use soroban_sdk::testutils::Address as _;
use soroban_sdk::{contract, contracterror, contractimpl, Address, Env};
use soroban_sdk_tools::{InstanceMap, PersistentItem, PersistentMap, TemporaryItem, TemporaryMap};
use soroban_sdk_tools_macro::contractstorage;

// ============================================================================
// Test Helpers
// ============================================================================

/// Creates a test environment and owner address
fn setup_env_with_owner() -> (Env, Address) {
    let env = Env::default();
    let owner = Address::generate(&env);
    (env, owner)
}

// ============================================================================
// Test Contracts
// ============================================================================

#[contractstorage]
struct BasicStorage {
    #[short_key = "v"]
    value: PersistentItem<u64>,
}

#[contractstorage(auto_shorten = true)]
struct AutoShortenStorage {
    balance: PersistentMap<Address, u64>,
    bonus: PersistentMap<Address, u64>,
    total: PersistentItem<u64>,
}

#[contractstorage(auto_shorten = true)]
struct MixedStorage {
    persistent_data: PersistentMap<u64, u64>,
    instance_data: InstanceMap<u32, u32>,
    temp_item: TemporaryItem<u64>,
}

#[contractstorage]
struct PrivateStorage {
    #[short_key = "pf"]
    pub public_field: PersistentItem<u64>,
    #[short_key = "prf"]
    private_field: PersistentItem<u64>,
}

mod token {
    use super::*;

    #[contractstorage]
    pub struct Storage {
        #[short_key = "b"]
        balances: PersistentMap<Address, u64>,
        #[short_key = "o"]
        owner: PersistentItem<Address>,
        #[short_key = "s"]
        supply: PersistentItem<u64>,
    }

    #[contracterror]
    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    pub enum Error {
        InsufficientBalance = 1,
        NoSenderBalance = 2,
    }

    #[contract]
    pub struct Token;

    #[contractimpl]
    impl Token {
        pub fn initialize(env: Env, owner: Address, supply: u64) {
            let storage = Storage::new(&env);
            storage.owner.set(&owner);
            storage.supply.set(&supply);
        }

        pub fn get_owner(env: Env) -> Option<Address> {
            Storage::new(&env).owner.get()
        }

        pub fn get_supply(env: Env) -> Option<u64> {
            Storage::new(&env).supply.get()
        }

        pub fn set_balance(env: Env, addr: &Address, amount: u64) {
            Storage::new(&env).balances.set(addr, &amount);
        }

        pub fn get_balance(env: Env, addr: &Address) -> Option<u64> {
            Storage::new(&env).balances.get(addr)
        }

        pub fn has_balance(env: Env, addr: &Address) -> bool {
            Storage::new(&env).balances.has(addr)
        }

        pub fn remove_balance(env: Env, addr: &Address) {
            Storage::new(&env).balances.remove(addr);
        }

        pub fn transfer(env: Env, from: &Address, to: &Address, amount: u64) -> Result<(), Error> {
            let storage = Storage::new(&env);

            let from_balance = storage
                .balances
                .get(from)
                .ok_or(Error::NoSenderBalance)?;

            if from_balance < amount {
                return Err(Error::InsufficientBalance);
            }

            let to_balance = storage.balances.get(to).unwrap_or(0);

            storage.balances.set(from, &(from_balance - amount));
            storage.balances.set(to, &(to_balance + amount));

            Ok(())
        }
    }
}

mod temp_storage {
    use super::*;

    #[contractstorage]
    pub struct Storage {
        data: TemporaryMap<Address, u64>,
        item: TemporaryItem<u64>,
    }

    #[contract]
    pub struct TempContract;

    #[contractimpl]
    impl TempContract {
        pub fn set_data(env: Env, addr: &Address, val: u64) {
            Storage::new(&env).data.set(addr, &val);
        }

        pub fn get_data(env: Env, addr: &Address) -> Option<u64> {
            Storage::new(&env).data.get(addr)
        }

        pub fn has_data(env: Env, addr: &Address) -> bool {
            Storage::new(&env).data.has(addr)
        }

        pub fn remove_data(env: Env, addr: &Address) {
            Storage::new(&env).data.remove(addr);
        }

        pub fn set_item(env: Env, val: u64) {
            Storage::new(&env).item.set(&val);
        }

        pub fn get_item(env: Env) -> Option<u64> {
            Storage::new(&env).item.get()
        }

        pub fn has_item(env: Env) -> bool {
            Storage::new(&env).item.has()
        }

        pub fn remove_item(env: Env) {
            Storage::new(&env).item.remove();
        }
    }
}

// ============================================================================
// Basic Macro Tests
// ============================================================================

#[test]
fn test_default_trait() {
    let storage = BasicStorage::default();
    let _value = storage.value;
}

#[test]
fn test_auto_shorten_accessors() {
    let env = Env::default();
    let storage = AutoShortenStorage::new(&env);

    // Verify accessors are generated
    let _balance = storage.balance();
    let _bonus = storage.bonus();
    let _total = storage.total();
}

#[test]
fn test_mixed_storage_types() {
    let env = Env::default();
    let storage = MixedStorage::new(&env);

    let _persistent = storage.persistent_data();
    let _instance = storage.instance_data();
    let _temp = storage.temp_item();
}

#[test]
fn test_field_visibility() {
    let env = Env::default();
    let storage = PrivateStorage::new(&env);

    let _public = storage.public_field;
    let _private = storage.private_field;
}

// ============================================================================
// Token Contract Tests
// ============================================================================

#[test]
fn test_token_initialization() {
    let (env, owner) = setup_env_with_owner();
    let contract_id = env.register(token::Token, ());
    let client = token::TokenClient::new(&env, &contract_id);

    let supply = 1_000_000u64;
    client.initialize(&owner, &supply);

    assert_eq!(client.get_owner(), Some(owner));
    assert_eq!(client.get_supply(), Some(supply));
}

#[test]
fn test_token_balances() {
    let env = Env::default();
    let contract_id = env.register(token::Token, ());
    let client = token::TokenClient::new(&env, &contract_id);

    let addr1 = Address::generate(&env);
    let addr2 = Address::generate(&env);

    // Initially no balance
    assert_eq!(client.get_balance(&addr1), None);
    assert!(!client.has_balance(&addr1));

    // Set balances
    client.set_balance(&addr1, &100);
    client.set_balance(&addr2, &200);

    assert_eq!(client.get_balance(&addr1), Some(100));
    assert_eq!(client.get_balance(&addr2), Some(200));
    assert!(client.has_balance(&addr1));
    assert!(client.has_balance(&addr2));
}

#[test]
fn test_token_remove_balance() {
    let env = Env::default();
    let contract_id = env.register(token::Token, ());
    let client = token::TokenClient::new(&env, &contract_id);

    let addr = Address::generate(&env);

    client.set_balance(&addr, &100);
    assert!(client.has_balance(&addr));

    client.remove_balance(&addr);
    assert_eq!(client.get_balance(&addr), None);
    assert!(!client.has_balance(&addr));
}

#[test]
fn test_token_transfer() {
    let env = Env::default();
    let contract_id = env.register(token::Token, ());
    let client = token::TokenClient::new(&env, &contract_id);

    let from = Address::generate(&env);
    let to = Address::generate(&env);

    client.set_balance(&from, &1000);
    client.set_balance(&to, &500);

    let result = client.try_transfer(&from, &to, &300);
    assert!(result.is_ok());

    assert_eq!(client.get_balance(&from), Some(700));
    assert_eq!(client.get_balance(&to), Some(800));
}

#[test]
#[should_panic(expected = "Error(Contract, #1)")]
fn test_token_transfer_insufficient_balance() {
    let env = Env::default();
    let contract_id = env.register(token::Token, ());
    let client = token::TokenClient::new(&env, &contract_id);

    let from = Address::generate(&env);
    let to = Address::generate(&env);

    client.set_balance(&from, &100);
    client.transfer(&from, &to, &300);
}

#[test]
#[should_panic(expected = "Error(Contract, #2)")]
fn test_token_transfer_no_sender_balance() {
    let env = Env::default();
    let contract_id = env.register(token::Token, ());
    let client = token::TokenClient::new(&env, &contract_id);

    let from = Address::generate(&env);
    let to = Address::generate(&env);

    client.transfer(&from, &to, &100);
}

// ============================================================================
// Temporary Storage Tests
// ============================================================================

#[test]
fn test_temporary_storage_map() {
    let env = Env::default();
    let contract_id = env.register(temp_storage::TempContract, ());
    let client = temp_storage::TempContractClient::new(&env, &contract_id);

    let addr = Address::generate(&env);

    assert!(!client.has_data(&addr));

    client.set_data(&addr, &123);
    assert!(client.has_data(&addr));
    assert_eq!(client.get_data(&addr), Some(123));

    client.remove_data(&addr);
    assert_eq!(client.get_data(&addr), None);
}

#[test]
fn test_temporary_storage_item() {
    let env = Env::default();
    let contract_id = env.register(temp_storage::TempContract, ());
    let client = temp_storage::TempContractClient::new(&env, &contract_id);

    assert!(!client.has_item());

    client.set_item(&999);
    assert!(client.has_item());
    assert_eq!(client.get_item(), Some(999));

    client.remove_item();
    assert_eq!(client.get_item(), None);
}

// ============================================================================
// Storage Persistence Tests
// ============================================================================

#[test]
fn test_persistence_across_calls() {
    let (env, owner) = setup_env_with_owner();
    let contract_id = env.register(token::Token, ());
    let client = token::TokenClient::new(&env, &contract_id);

    let user = Address::generate(&env);

    client.initialize(&owner, &1_000_000);
    client.set_balance(&user, &500);

    // Verify data persists across multiple calls
    assert_eq!(client.get_owner(), Some(owner));
    assert_eq!(client.get_supply(), Some(1_000_000));
    assert_eq!(client.get_balance(&user), Some(500));
}
