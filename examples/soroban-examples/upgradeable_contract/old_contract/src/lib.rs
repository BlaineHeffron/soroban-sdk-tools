#![no_std]

use soroban_sdk::{contract, contractimpl, Address, BytesN, Env};
use soroban_sdk_tools::{contractstorage, InstanceItem};

#[contractstorage]
struct Storage {
    admin: InstanceItem<Address>,
}

#[contract]
pub struct UpgradeableContract;

#[contractimpl]
impl UpgradeableContract {
    pub fn __constructor(e: Env, admin: Address) {
        Storage::new(&e).admin.set(&admin);
    }

    pub fn version() -> u32 {
        1
    }

    pub fn upgrade(e: Env, new_wasm_hash: BytesN<32>) {
        let admin = Storage::new(&e).admin.get().unwrap();
        admin.require_auth();

        e.deployer().update_current_contract_wasm(new_wasm_hash);
    }
}

mod test;
