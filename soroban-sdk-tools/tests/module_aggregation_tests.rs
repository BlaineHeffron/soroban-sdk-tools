//! Tests for module-wide aggregation of #[contractstorage] structs using
//! #[contractstorage_module] to perform global key minimization.

use soroban_sdk::{
    contract, contractimpl, testutils::Address as _, xdr::ToXdr, Address, Bytes, BytesN, Env,
    TryFromVal, Val,
};
use soroban_sdk_tools::{PersistentItem, PersistentMap};
use soroban_sdk_tools_macro::contractstorage_module;

#[contractstorage_module]
mod aggregated {
    use super::*;

    #[contractstorage(auto_shorten = true)]
    pub struct AStorage {
        pub balance: PersistentMap<Address, u64>,
        pub bonus: PersistentMap<Address, u64>,
        pub burn: PersistentMap<Address, u64>,
        pub total: PersistentItem<u64>,
    }

    #[contractstorage(auto_shorten = true)]
    pub struct BStorage {
        pub balance: PersistentMap<Address, u64>,
        pub bonus: PersistentMap<Address, u64>,
        pub burn: PersistentMap<Address, u64>,
        pub total: PersistentItem<u64>,
    }

    #[contract]
    pub struct Agg;

    #[contractimpl]
    impl Agg {
        pub fn set_a(env: Env, addr: Address, bal: u64, bon: u64, burn: u64, total: u64) {
            let s = AStorage::new(&env);
            s.balance().set(addr.clone(), &bal);
            s.bonus().set(addr.clone(), &bon);
            s.burn().set(addr, &burn);
            s.total().set(&total);
        }

        pub fn set_b(env: Env, addr: Address, bal: u64, bon: u64, burn: u64, total: u64) {
            let s = BStorage::new(&env);
            s.balance().set(addr.clone(), &bal);
            s.bonus().set(addr.clone(), &bon);
            s.burn().set(addr, &burn);
            s.total().set(&total);
        }

        pub fn get_a_balance(env: Env, addr: Address) -> Option<u64> {
            AStorage::new(&env).balance().get(addr)
        }

        pub fn get_b_balance(env: Env, addr: Address) -> Option<u64> {
            BStorage::new(&env).balance().get(addr)
        }
    }
}

#[test]
fn test_module_aggregation_unique_keys() {
    let env = Env::default();
    let id = env.register(aggregated::Agg, ());
    let client = aggregated::AggClient::new(&env, &id);

    let addr = Address::generate(&env);

    client.set_a(&addr, &111, &222, &333, &444);
    client.set_b(&addr, &555, &666, &777, &888);

    env.as_contract(&id, || {
        let addr_bytes: Bytes = addr.to_xdr(&env);

        // AStorage expected prefixes: B, Bo, Bu, T
        let a_balance_prefix = Bytes::from_slice(&env, b"B");
        let a_bonus_prefix = Bytes::from_slice(&env, b"Bo");
        let a_burn_prefix = Bytes::from_slice(&env, b"Bu");
        let a_total_prefix = Bytes::from_slice(&env, b"T");

        // BStorage expected prefixes: Ba, Bon, Bur, To
        let b_balance_prefix = Bytes::from_slice(&env, b"Ba");
        let b_bonus_prefix = Bytes::from_slice(&env, b"Bon");
        let b_burn_prefix = Bytes::from_slice(&env, b"Bur");
        let b_total_prefix = Bytes::from_slice(&env, b"To");

        // Generate and verify AStorage keys
        let a_bal_key: BytesN<32> = hash_key(&env, &a_balance_prefix, &addr_bytes);
        let a_bonus_key = hash_key(&env, &a_bonus_prefix, &addr_bytes);
        let a_burn_key = hash_key(&env, &a_burn_prefix, &addr_bytes);

        // Generate and verify BStorage keys
        let b_bal_key = hash_key(&env, &b_balance_prefix, &addr_bytes);
        let b_bonus_key = hash_key(&env, &b_bonus_prefix, &addr_bytes);
        let b_burn_key = hash_key(&env, &b_burn_prefix, &addr_bytes);

        // Ensure no collisions
        assert_ne!(a_bal_key, b_bal_key);
        assert_ne!(a_bonus_key, b_bonus_key);
        assert_ne!(a_burn_key, b_burn_key);
        assert_ne!(a_total_prefix, b_total_prefix);

        let a_bal_val: Val = env.storage().persistent().get(&a_bal_key).unwrap();
        let a_bal: u64 = u64::try_from_val(&env, &a_bal_val).unwrap();
        assert_eq!(a_bal, 111);

        let b_bal_val: Val = env.storage().persistent().get(&b_bal_key).unwrap();
        let b_bal: u64 = u64::try_from_val(&env, &b_bal_val).unwrap();
        assert_eq!(b_bal, 555);
    });
}

#[test]
fn test_module_aggregation_retrieval() {
    let env = Env::default();
    let id = env.register(aggregated::Agg, ());
    let client = aggregated::AggClient::new(&env, &id);

    let addr = Address::generate(&env);

    client.set_a(&addr, &100, &200, &300, &400);
    client.set_b(&addr, &500, &600, &700, &800);

    // Verify independent retrieval
    assert_eq!(client.get_a_balance(&addr), Some(100));
    assert_eq!(client.get_b_balance(&addr), Some(500));
}

// ============================================================================
// Helper Functions
// ============================================================================

fn hash_key(env: &Env, prefix: &Bytes, data: &Bytes) -> BytesN<32> {
    let mut combined = prefix.clone();
    combined.append(data);
    env.crypto().sha256(&combined).into()
}
