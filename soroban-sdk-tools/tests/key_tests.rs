//! Tests for key materialization with numeric, tuple, and address keys.
//! Covers auto-shorten, explicit short_key, and DataKey-style enum keys.

use soroban_sdk::{contract, contractimpl, testutils::Address as _};
use soroban_sdk::{Address, Bytes, Env, IntoVal, Symbol, Val, Vec as HostVec};
use soroban_sdk_tools::{PersistentItem, PersistentMap};
use soroban_sdk_tools_macro::contractstorage;

// ============================================================================
// Numeric Keys Contract
// ============================================================================

mod numeric {
    use super::*;

    #[contractstorage]
    pub struct Storage {
        counts: PersistentMap<u32, u64>,
    }

    #[contract]
    pub struct Numbers;

    #[contractimpl]
    impl Numbers {
        pub fn set_count(env: Env, key: u32, val: u64) {
            Storage::new(&env).counts.set(&key, &val);
        }

        pub fn get_count(env: Env, key: u32) -> Option<u64> {
            Storage::new(&env).counts.get(&key)
        }

        pub fn has_count(env: Env, key: u32) -> bool {
            Storage::new(&env).counts.has(&key)
        }
    }
}

// ============================================================================
// Tuple Keys Contract
// ============================================================================

mod tuple {
    use super::*;

    #[contractstorage]
    pub struct Storage {
        cells: PersistentMap<(u32, u32), u64>,
    }

    #[contract]
    pub struct Grid;

    #[contractimpl]
    impl Grid {
        pub fn set_cell(env: Env, x: u32, y: u32, val: u64) {
            Storage::new(&env).cells.set(&(x, y), &val);
        }

        pub fn get_cell(env: Env, x: u32, y: u32) -> Option<u64> {
            Storage::new(&env).cells.get(&(x, y))
        }
    }
}

// ============================================================================
// Auto-Shorten Keys Contract
// ============================================================================

mod autoshorten {
    use super::*;

    #[contractstorage(auto_shorten = true)]
    pub struct Storage {
        pub balance: PersistentMap<Address, u64>,
        pub total: PersistentItem<u64>,
        pub pair: PersistentMap<(u32, u32), u64>,
    }

    #[contract]
    pub struct AutoShorten;

    #[contractimpl]
    impl AutoShorten {
        pub fn set_balance(env: Env, addr: &Address, v: u64) {
            Storage::new(&env).balance().set(addr, &v);
        }

        pub fn set_total(env: Env, v: u64) {
            Storage::new(&env).total().set(&v);
        }

        pub fn set_pair(env: Env, x: u32, y: u32, v: u64) {
            Storage::new(&env).pair().set(&(x, y), &v);
        }
    }
}

// ============================================================================
// Auto-Shorten with Explicit Overrides
// ============================================================================

mod override_keys {
    use super::*;

    #[contractstorage(auto_shorten = true)]
    pub struct Storage {
        #[short_key = "x"]
        mapx: PersistentMap<Address, u64>,
        #[short_key = "y"]
        itemy: PersistentItem<u64>,
        #[short_key = "c"]
        tuple: PersistentMap<(u32, u32), u64>,
    }

    #[contract]
    pub struct Override;

    #[contractimpl]
    impl Override {
        pub fn set_mapx(env: Env, addr: &Address, val: u64) {
            Storage::new(&env).mapx().set(addr, &val);
        }

        pub fn set_itemy(env: Env, val: u64) {
            Storage::new(&env).itemy().set(&val);
        }

        pub fn set_tuple(env: Env, x: u32, y: u32, val: u64) {
            Storage::new(&env).tuple().set(&(x, y), &val);
        }
    }
}

// ============================================================================
// Non-Auto-Shorten (DataKey Style) Contract
// ============================================================================

mod datakey_style {
    use super::*;

    #[contractstorage]
    pub struct Storage {
        #[short_key = "b"]
        balances: PersistentMap<Address, u64>,
        owner: PersistentItem<Address>,
        cells: PersistentMap<(u32, u32), u64>,
    }

    #[contract]
    pub struct DataKeyStyle;

    #[contractimpl]
    impl DataKeyStyle {
        pub fn set_balance(env: Env, addr: &Address, val: u64) {
            Storage::new(&env).balances.set(addr, &val);
        }

        pub fn set_owner(env: Env, owner: Address) {
            Storage::new(&env).owner.set(&owner);
        }

        pub fn set_cell(env: Env, x: u32, y: u32, val: u64) {
            Storage::new(&env).cells.set(&(x, y), &val);
        }
    }
}

// ============================================================================
// Namespace Test Contract
// ============================================================================

mod namespace {
    use super::*;

    #[contractstorage]
    pub struct AStorage {
        #[short_key = "ao"]
        owner: PersistentItem<Address>,
    }

    #[contractstorage]
    pub struct BStorage {
        #[short_key = "bo"]
        owner: PersistentItem<Address>,
    }

    #[contract]
    pub struct NS;

    #[contractimpl]
    impl NS {
        pub fn set_a_owner(env: Env, addr: Address) {
            AStorage::new(&env).owner.set(&addr);
        }

        pub fn set_b_owner(env: Env, addr: Address) {
            BStorage::new(&env).owner.set(&addr);
        }

        pub fn get_a_owner(env: Env) -> Option<Address> {
            AStorage::new(&env).owner.get()
        }

        pub fn get_b_owner(env: Env) -> Option<Address> {
            BStorage::new(&env).owner.get()
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[test]
fn test_numeric_and_tuple_keys() {
    let env = Env::default();

    // Numeric keys
    let num_id = env.register(numeric::Numbers, ());
    let num_client = numeric::NumbersClient::new(&env, &num_id);

    assert!(!num_client.has_count(&42));
    num_client.set_count(&42, &777);
    assert!(num_client.has_count(&42));
    assert_eq!(num_client.get_count(&42), Some(777));

    // Tuple keys
    let grid_id = env.register(tuple::Grid, ());
    let grid_client = tuple::GridClient::new(&env, &grid_id);

    assert_eq!(grid_client.get_cell(&1, &2), None);
    grid_client.set_cell(&1, &2, &99);
    assert_eq!(grid_client.get_cell(&1, &2), Some(99));
    grid_client.set_cell(&2, &1, &100);
    assert_eq!(grid_client.get_cell(&2, &1), Some(100));
}

#[test]
fn test_global_key_uniqueness() {
    let env = Env::default();
    let id = env.register(namespace::NS, ());
    let client = namespace::NSClient::new(&env, &id);

    let a_owner = Address::generate(&env);
    let b_owner = Address::generate(&env);

    client.set_a_owner(&a_owner);
    client.set_b_owner(&b_owner);

    // Both storages use different enum types, ensuring no collision
    assert_eq!(client.get_a_owner(), Some(a_owner));
    assert_eq!(client.get_b_owner(), Some(b_owner));
}

#[test]
fn test_autoshorten_keys_materialize() {
    let env = Env::default();
    let cid = env.register(autoshorten::AutoShorten, ());
    let client = autoshorten::AutoShortenClient::new(&env, &cid);
    let addr = Address::generate(&env);

    client.set_balance(&addr, &111);
    client.set_total(&999);
    client.set_pair(&5, &7, &42);

    env.as_contract(&cid, || {
        use soroban_sdk_tools::key::make_map_key;

        // balance map key: hash("B" + address_xdr)
        let prefix_b = Bytes::from_slice(&env, b"B");
        let k_bal: Val = make_map_key(&env, &prefix_b, &addr).into_val(&env);
        assert!(env.storage().persistent().has(&k_bal));

        // total item key: short Bytes "T" (not hashed)
        let k_total: Val = Bytes::from_slice(&env, b"T").into_val(&env);
        assert!(env.storage().persistent().has(&k_total));

        // pair map key: hash("P" + tuple_xdr)
        let prefix_p = Bytes::from_slice(&env, b"P");
        let k_pair: Val = make_map_key(&env, &prefix_p, &(5u32, 7u32)).into_val(&env);
        assert!(env.storage().persistent().has(&k_pair));
    });
}

#[test]
fn test_override_keys_materialize() {
    let env = Env::default();
    let cid = env.register(override_keys::Override, ());
    let client = override_keys::OverrideClient::new(&env, &cid);

    let addr = Address::generate(&env);

    client.set_mapx(&addr, &123);
    client.set_itemy(&456);
    client.set_tuple(&10, &20, &999);

    env.as_contract(&cid, || {
        use soroban_sdk_tools::key::make_map_key;

        // mapx -> prefix "x"
        let px = Bytes::from_slice(&env, b"x");
        let kx: Val = make_map_key(&env, &px, &addr).into_val(&env);
        assert!(env.storage().persistent().has(&kx));

        // itemy -> short Bytes "y"
        let ky: Val = Bytes::from_slice(&env, b"y").into_val(&env);
        assert!(env.storage().persistent().has(&ky));

        // tuple -> prefix "c"
        let pc = Bytes::from_slice(&env, b"c");
        let kt: Val = make_map_key(&env, &pc, &(10u32, 20u32)).into_val(&env);
        assert!(env.storage().persistent().has(&kt));
    });
}

#[test]
fn test_datakey_style_keys_materialize() {
    let env = Env::default();
    let cid = env.register(datakey_style::DataKeyStyle, ());
    let client = datakey_style::DataKeyStyleClient::new(&env, &cid);

    let owner = Address::generate(&env);
    let addr = Address::generate(&env);

    client.set_owner(&owner);
    client.set_balance(&addr, &314);
    client.set_cell(&3, &4, &2718);

    env.as_contract(&cid, || {
        // Item 'owner' (no short_key) -> Symbol("Owner")
        let k_owner: Val = Symbol::new(&env, "Owner").into_val(&env);
        assert!(env.storage().persistent().has(&k_owner));

        // Map 'balances' (#[short_key="b"]) -> Vec[Symbol("B"), Address]
        let mut kv_bal: HostVec<Val> = HostVec::new(&env);
        kv_bal.push_back(Symbol::new(&env, "B").into_val(&env));
        kv_bal.push_back(addr.into_val(&env));
        let k_bal: Val = kv_bal.into_val(&env);
        assert!(env.storage().persistent().has(&k_bal));

        // Map 'cells' (no short_key) -> Vec[Symbol("Cells"), tuple]
        let mut t: HostVec<Val> = HostVec::new(&env);
        t.push_back((3u32).into_val(&env));
        t.push_back((4u32).into_val(&env));

        let mut kv_cells: HostVec<Val> = HostVec::new(&env);
        kv_cells.push_back(Symbol::new(&env, "Cells").into_val(&env));
        kv_cells.push_back(t.into_val(&env));
        let k_cells: Val = kv_cells.into_val(&env);
        assert!(env.storage().persistent().has(&k_cells));
    });
}

#[test]
fn test_numeric_keys_materialize() {
    let env = Env::default();
    let id = env.register(numeric::Numbers, ());
    let client = numeric::NumbersClient::new(&env, &id);

    client.set_count(&42, &777);

    env.as_contract(&id, || {
        // Key: Vec[Symbol("Counts"), u32]
        let mut kv: HostVec<Val> = HostVec::new(&env);
        kv.push_back(Symbol::new(&env, "Counts").into_val(&env));
        kv.push_back((42u32).into_val(&env));
        let k: Val = kv.into_val(&env);
        assert!(env.storage().persistent().has(&k));
    });
}

#[test]
fn test_tuple_keys_materialize() {
    let env = Env::default();
    let id = env.register(tuple::Grid, ());
    let client = tuple::GridClient::new(&env, &id);

    client.set_cell(&1, &2, &99);

    env.as_contract(&id, || {
        // Build tuple as Vec[(1),(2)]
        let mut t: HostVec<Val> = HostVec::new(&env);
        t.push_back((1u32).into_val(&env));
        t.push_back((2u32).into_val(&env));

        // Key: Vec[Symbol("Cells"), tuple]
        let mut kv: HostVec<Val> = HostVec::new(&env);
        kv.push_back(Symbol::new(&env, "Cells").into_val(&env));
        kv.push_back(t.into_val(&env));
        let k: Val = kv.into_val(&env);
        env.logs().add("", &[k.clone()]);
        assert!(env.storage().persistent().has(&k));
    });
}
