#![no_std]
/// This is a simple contract that just extends TTL for its keys.
/// It's main purpose is to demonstrate how TTL extension can be tested,
/// so the most interesting part here is `test.rs`.
use soroban_sdk::{contract, contractimpl, Env};
use soroban_sdk_tools::{contractstorage, InstanceItem, PersistentItem, TemporaryItem};

#[contractstorage]
struct Storage {
    persistent: PersistentItem<u32>,
    instance: InstanceItem<u32>,
    temporary: TemporaryItem<u32>,
}

#[contract]
pub struct TtlContract;

#[contractimpl]
impl TtlContract {
    /// Creates a contract entry in every kind of storage.
    pub fn setup(env: Env) {
        let storage = Storage::new(&env);
        storage.persistent.set(&0);
        storage.instance.set(&1);
        storage.temporary.set(&2);
    }

    /// Extend the persistent entry TTL to 5000 ledgers, when its
    /// TTL is smaller than 1000 ledgers.
    pub fn extend_persistent(env: Env) {
        Storage::new(&env).persistent.extend_ttl(1000, 5000);
    }

    /// Extend the instance entry TTL to become at least 10000 ledgers,
    /// when its TTL is smaller than 2000 ledgers.
    pub fn extend_instance(env: Env) {
        Storage::new(&env).instance.extend_ttl(2000, 10000);
    }

    /// Extend the temporary entry TTL to become at least 7000 ledgers,
    /// when its TTL is smaller than 3000 ledgers.
    pub fn extend_temporary(env: Env) {
        Storage::new(&env).temporary.extend_ttl(3000, 7000);
    }
}

mod test;
