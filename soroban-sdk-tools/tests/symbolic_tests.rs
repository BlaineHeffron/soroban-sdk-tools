//! Tests for symbolic key mode in contractstorage macro.
//! Covers combinations of symbolic at struct level, per-field,
//! with/without auto_shorten, and with explicit short_key.

use soroban_sdk::{contract, contractimpl, testutils::Address as _};
use soroban_sdk::{Address, Bytes, Env, IntoVal, Symbol, Val, Vec as HostVec};
use soroban_sdk_tools::{PersistentItem, PersistentMap};
use soroban_sdk_tools_macro::contractstorage;

// ============================================================================
// Symbolic with Auto-Shorten
// ============================================================================

mod symbolic_auto {
    use super::*;

    #[contractstorage(auto_shorten = true, symbolic = true)]
    pub struct Storage {
        balance: PersistentMap<Address, u64>,
        #[short_key = "B"]
        total: PersistentItem<u64>,
    }

    #[contract]
    pub struct SymbolicAuto;

    #[contractimpl]
    impl SymbolicAuto {
        pub fn set_balance(env: Env, addr: &Address, val: u64) {
            Storage::new(&env).balance.set(addr, &val);
        }

        pub fn set_total(env: Env, val: u64) {
            Storage::new(&env).total.set(&val);
        }
    }
}

// ============================================================================
// Symbolic Per-Field (with auto_shorten at top level)
// ============================================================================

mod symbolic_per_field {
    use super::*;

    #[contractstorage(auto_shorten = true)]
    pub struct Storage {
        #[symbolic]
        balance: PersistentMap<Address, u64>,
        total: PersistentItem<u64>,
        #[symbolic]
        owner: PersistentItem<Address>,
    }

    #[contract]
    pub struct SymbolicPerField;

    #[contractimpl]
    impl SymbolicPerField {
        pub fn set_balance(env: Env, addr: &Address, val: u64) {
            Storage::new(&env).balance.set(addr, &val);
        }

        pub fn set_total(env: Env, val: u64) {
            Storage::new(&env).total.set(&val);
        }

        pub fn set_owner(env: Env, addr: Address) {
            Storage::new(&env).owner.set(&addr);
        }
    }
}

// ============================================================================
// Symbolic without Auto-Shorten
// ============================================================================

mod symbolic_no_auto {
    use super::*;

    #[contractstorage(symbolic = true)]
    pub struct Storage {
        balance_map: PersistentMap<Address, u64>,
        total_supply: PersistentItem<u64>,
    }

    #[contract]
    pub struct SymbolicNoAuto;

    #[contractimpl]
    impl SymbolicNoAuto {
        pub fn set_balance(env: Env, addr: &Address, val: u64) {
            Storage::new(&env).balance_map.set(addr, &val);
        }

        pub fn set_total(env: Env, val: u64) {
            Storage::new(&env).total_supply.set(&val);
        }
    }
}

// ============================================================================
// Symbolic with Short Key
// ============================================================================

mod symbolic_shortkey {
    use super::*;

    #[contractstorage(symbolic = true)]
    pub struct Storage {
        #[short_key = "bal"]
        balance: PersistentMap<Address, u64>,
        #[short_key = "tot"]
        total: PersistentItem<u64>,
    }

    #[contractstorage(auto_shorten = true, symbolic = true)]
    pub struct AutoStorage {
        #[short_key = "b"]
        balance: PersistentMap<Address, u64>,
        #[short_key = "t"]
        total: PersistentItem<u64>,
    }

    #[contract]
    pub struct SymbolicShortKey;

    #[contractimpl]
    impl SymbolicShortKey {
        pub fn set_balance(env: Env, addr: &Address, val: u64) {
            Storage::new(&env).balance.set(addr, &val);
        }

        pub fn set_total(env: Env, val: u64) {
            Storage::new(&env).total.set(&val);
        }

        pub fn set_balance_auto(env: Env, addr: &Address, val: u64) {
            AutoStorage::new(&env).balance.set(addr, &val);
        }

        pub fn set_total_auto(env: Env, val: u64) {
            AutoStorage::new(&env).total.set(&val);
        }
    }
}

// ============================================================================
// Mixed Symbolic with Short Key
// ============================================================================

mod mixed_symbolic {
    use super::*;

    #[contractstorage(auto_shorten = true)]
    pub struct Storage {
        #[symbolic]
        #[short_key = "bal"]
        balance: PersistentMap<Address, u64>,
        total: PersistentItem<u64>,
    }

    #[contract]
    pub struct MixedSymbolic;

    #[contractimpl]
    impl MixedSymbolic {
        pub fn set_balance(env: Env, addr: &Address, val: u64) {
            Storage::new(&env).balance.set(addr, &val);
        }

        pub fn set_total(env: Env, val: u64) {
            Storage::new(&env).total.set(&val);
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[test]
fn test_symbolic_auto_keys() {
    let env = Env::default();
    let cid = env.register(symbolic_auto::SymbolicAuto, ());
    let client = symbolic_auto::SymbolicAutoClient::new(&env, &cid);

    let addr = Address::generate(&env);

    client.set_balance(&addr, &111);
    client.set_total(&999);

    env.as_contract(&cid, || {
        // balance map key: Vec[Symbol("Ba"), Address]
        let mut kv_bal: HostVec<Val> = HostVec::new(&env);
        kv_bal.push_back(Symbol::new(&env, "Ba").into_val(&env));
        kv_bal.push_back(addr.into_val(&env));
        let k_bal: Val = kv_bal.into_val(&env);
        assert!(env.storage().persistent().has(&k_bal));

        // total item key: Symbol("B")
        let k_total: Val = Symbol::new(&env, "B").into_val(&env);
        assert!(env.storage().persistent().has(&k_total));
    });
}

#[test]
fn test_symbolic_per_field_keys() {
    let env = Env::default();
    let cid = env.register(symbolic_per_field::SymbolicPerField, ());
    let client = symbolic_per_field::SymbolicPerFieldClient::new(&env, &cid);

    let addr = Address::generate(&env);

    client.set_balance(&addr, &111);
    client.set_total(&999);
    client.set_owner(&addr);

    env.as_contract(&cid, || {
        // balance (symbolic): Vec[Symbol("B"), Address]
        let mut kv_bal: HostVec<Val> = HostVec::new(&env);
        kv_bal.push_back(Symbol::new(&env, "B").into_val(&env));
        kv_bal.push_back(addr.clone().into_val(&env));
        let k_bal: Val = kv_bal.into_val(&env);
        assert!(env.storage().persistent().has(&k_bal));

        // total (hashed): Bytes("T")
        let k_total: Val = Bytes::from_slice(&env, b"T").into_val(&env);
        assert!(env.storage().persistent().has(&k_total));

        // owner (symbolic): Symbol("O")
        let k_owner: Val = Symbol::new(&env, "O").into_val(&env);
        assert!(env.storage().persistent().has(&k_owner));
    });
}

#[test]
fn test_symbolic_no_auto_keys() {
    let env = Env::default();
    let cid = env.register(symbolic_no_auto::SymbolicNoAuto, ());
    let client = symbolic_no_auto::SymbolicNoAutoClient::new(&env, &cid);

    let addr = Address::generate(&env);

    client.set_balance(&addr, &111);
    client.set_total(&999);

    env.as_contract(&cid, || {
        // balance_map: Vec[Symbol("BalanceMap"), Address]
        let mut kv_bal: HostVec<Val> = HostVec::new(&env);
        kv_bal.push_back(Symbol::new(&env, "BalanceMap").into_val(&env));
        kv_bal.push_back(addr.into_val(&env));
        let k_bal: Val = kv_bal.into_val(&env);
        assert!(env.storage().persistent().has(&k_bal));

        // total_supply: Symbol("TotalSupply")
        let k_total: Val = Symbol::new(&env, "TotalSupply").into_val(&env);
        assert!(env.storage().persistent().has(&k_total));
    });
}

#[test]
fn test_symbolic_shortkey_keys() {
    let env = Env::default();
    let cid = env.register(symbolic_shortkey::SymbolicShortKey, ());
    let client = symbolic_shortkey::SymbolicShortKeyClient::new(&env, &cid);

    let addr = Address::generate(&env);

    // Without auto_shorten
    client.set_balance(&addr, &111);
    client.set_total(&999);

    env.as_contract(&cid, || {
        // balance: Vec[Symbol("Bal"), Address]
        let mut kv_bal: HostVec<Val> = HostVec::new(&env);
        kv_bal.push_back(Symbol::new(&env, "Bal").into_val(&env));
        kv_bal.push_back(addr.clone().into_val(&env));
        let k_bal: Val = kv_bal.into_val(&env);
        assert!(env.storage().persistent().has(&k_bal));

        // total: Symbol("Tot")
        let k_total: Val = Symbol::new(&env, "Tot").into_val(&env);
        assert!(env.storage().persistent().has(&k_total));
    });

    // With auto_shorten
    client.set_balance_auto(&addr, &222);
    client.set_total_auto(&888);

    env.as_contract(&cid, || {
        // balance: Vec[Symbol("B"), Address]
        let mut kv_bal_auto: HostVec<Val> = HostVec::new(&env);
        kv_bal_auto.push_back(Symbol::new(&env, "B").into_val(&env));
        kv_bal_auto.push_back(addr.into_val(&env));
        let k_bal_auto: Val = kv_bal_auto.into_val(&env);
        assert!(env.storage().persistent().has(&k_bal_auto));

        // total: Symbol("T")
        let k_total_auto: Val = Symbol::new(&env, "T").into_val(&env);
        assert!(env.storage().persistent().has(&k_total_auto));
    });
}

#[test]
fn test_mixed_symbolic_shortkey_keys() {
    let env = Env::default();
    let cid = env.register(mixed_symbolic::MixedSymbolic, ());
    let client = mixed_symbolic::MixedSymbolicClient::new(&env, &cid);

    let addr = Address::generate(&env);

    client.set_balance(&addr, &111);
    client.set_total(&999);

    env.as_contract(&cid, || {
        // balance (symbolic + short_key): Vec[Symbol("Bal"), Address]
        let mut kv_bal: HostVec<Val> = HostVec::new(&env);
        kv_bal.push_back(Symbol::new(&env, "Bal").into_val(&env));
        kv_bal.push_back(addr.into_val(&env));
        let k_bal: Val = kv_bal.into_val(&env);
        assert!(env.storage().persistent().has(&k_bal));

        // total (hashed, auto_shorten -> "t"): Bytes("T")
        let k_total: Val = Bytes::from_slice(&env, b"T").into_val(&env);
        assert!(env.storage().persistent().has(&k_total));
    });
}

#[test]
fn test_symbolic_independence() {
    let env = Env::default();
    let cid = env.register(symbolic_auto::SymbolicAuto, ());
    let client = symbolic_auto::SymbolicAutoClient::new(&env, &cid);

    let addr1 = Address::generate(&env);
    let addr2 = Address::generate(&env);

    // Set multiple balances
    client.set_balance(&addr1, &100);
    client.set_balance(&addr2, &200);
    client.set_total(&300);

    env.as_contract(&cid, || {
        // Verify all keys exist independently
        let mut kv1: HostVec<Val> = HostVec::new(&env);
        kv1.push_back(Symbol::new(&env, "Ba").into_val(&env));
        kv1.push_back(addr1.into_val(&env));
        let kv1_val: Val = kv1.into_val(&env);
        assert!(env.storage().persistent().has(&kv1_val));

        let mut kv2: HostVec<Val> = HostVec::new(&env);
        kv2.push_back(Symbol::new(&env, "Ba").into_val(&env));
        kv2.push_back(addr2.into_val(&env));
        let kv2_val: Val = kv2.into_val(&env);
        assert!(env.storage().persistent().has(&kv2_val));

        let k_total: Val = Symbol::new(&env, "B").into_val(&env);
        assert!(env.storage().persistent().has(&k_total));
    });
}
