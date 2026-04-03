#![no_std]

use soroban_sdk::{contract, contractimpl, Env};
use soroban_sdk_tools::{contractstorage, InstanceItem};

#[contract]
pub struct Contract;

#[contractstorage]
struct Storage {
    #[short_key = "value"]
    value: InstanceItem<u32>,
}

#[contractimpl]
impl Contract {
    pub fn __constructor(env: Env, value: u32) {
        Storage::new(&env).value.set(&value);
    }

    pub fn value(env: Env) -> u32 {
        Storage::new(&env).value.get().unwrap()
    }
}
