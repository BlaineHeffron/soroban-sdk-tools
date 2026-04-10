#![no_std]
use soroban_sdk::{contract, contractimpl, Env};
use soroban_sdk_tools::{contractstorage, InstanceItem};

#[contractstorage(auto_shorten = true)]
struct Storage {
    paused: InstanceItem<bool>,
}

#[contract]
pub struct Pause;

#[contractimpl]
impl Pause {
    pub fn paused(env: Env) -> bool {
        Storage::new(&env).paused.get().unwrap_or_default()
    }

    pub fn set(env: Env, paused: bool) {
        Storage::new(&env).paused.set(&paused);
    }
}

mod test;
