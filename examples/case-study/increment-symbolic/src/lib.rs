#![no_std]

use soroban_sdk::{contract, contractimpl, xdr::ToXdr, Bytes, Env};
use soroban_sdk_tools::{contractstorage, InstanceItem};

#[contractstorage(auto_shorten = true, symbolic = true)]
struct Storage {
    counter: InstanceItem<u32>,
}

#[contract]
pub struct IncrementSymbolic;

#[contractimpl]
impl IncrementSymbolic {
    pub fn increment(env: Env) -> u32 {
        let storage = Storage::new(&env);
        let next = storage.counter.get().unwrap_or(0) + 1;
        storage.counter.set(&next);
        next
    }

    pub fn counter_key_len(env: Env) -> u32 {
        Storage::new(&env)
            .get_storage_counter_key()
            .to_xdr(&env)
            .len()
    }

    pub fn counter_key_xdr(env: Env) -> Bytes {
        Storage::new(&env).get_storage_counter_key().to_xdr(&env)
    }
}
