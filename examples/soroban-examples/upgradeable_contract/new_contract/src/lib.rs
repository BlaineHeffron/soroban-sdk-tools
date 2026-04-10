#![no_std]

use soroban_sdk::{contract, contractimpl, Address, BytesN, Env};
use soroban_sdk_tools::{contractstorage, InstanceItem};

#[contractstorage]
struct Storage {
    admin: InstanceItem<Address>,
    new_admin: InstanceItem<Address>,
}

#[contract]
pub struct UpgradeableContract;

#[contractimpl]
impl UpgradeableContract {
    // Note, that constructor is not called when the contract is upgraded.
    // Thus we introduce a new function `handle_upgrade` that brings the
    // freshly upgraded contract to proper state (specifically, initializes
    // the `NewAdmin` key).
    pub fn __constructor(e: Env, admin: Address) {
        let storage = Storage::new(&e);
        storage.admin.set(&admin);
        storage.new_admin.set(&admin);
    }

    pub fn handle_upgrade(e: Env) {
        let storage = Storage::new(&e);
        let admin = storage.admin.get().unwrap();
        admin.require_auth();
        if !storage.new_admin.has() {
            storage.new_admin.set(&admin);
        }
    }

    pub fn version() -> u32 {
        2
    }

    pub fn new_v2_fn() -> u32 {
        1010101
    }

    pub fn upgrade(e: Env, new_wasm_hash: BytesN<32>) {
        let admin = Storage::new(&e).new_admin.get().unwrap();
        admin.require_auth();

        e.deployer().update_current_contract_wasm(new_wasm_hash);
    }
}
