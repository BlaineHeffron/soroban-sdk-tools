#![no_std]

use soroban_sdk::{contract, contractimpl, xdr::ToXdr, Address, Bytes, Env};
use soroban_sdk_tools::{contractstorage, InstanceItem};

#[contractstorage(auto_shorten = true)]
struct Storage {
    owner: InstanceItem<Address>,
    guardian: InstanceItem<Address>,
    nonce: InstanceItem<u32>,
}

#[contract]
pub struct AccountAuto;

#[contractimpl]
impl AccountAuto {
    pub fn initialize(env: Env, owner: Address, guardian: Address) {
        let storage = Storage::new(&env);
        if storage.owner.has() {
            panic!("already initialized");
        }
        storage.owner.set(&owner);
        storage.guardian.set(&guardian);
        storage.nonce.set(&0u32);
    }

    pub fn rotate_guardian(env: Env, guardian: Address) {
        Storage::new(&env).guardian.set(&guardian);
    }

    pub fn bump_nonce(env: Env) -> u32 {
        let storage = Storage::new(&env);
        let next = storage.nonce.get().unwrap_or(0) + 1;
        storage.nonce.set(&next);
        next
    }

    pub fn owner_key_len(env: Env) -> u32 {
        Storage::new(&env)
            .get_storage_owner_key()
            .to_xdr(&env)
            .len()
    }

    pub fn guardian_key_len(env: Env) -> u32 {
        Storage::new(&env)
            .get_storage_guardian_key()
            .to_xdr(&env)
            .len()
    }

    pub fn nonce_key_len(env: Env) -> u32 {
        Storage::new(&env)
            .get_storage_nonce_key()
            .to_xdr(&env)
            .len()
    }

    pub fn owner_key_xdr(env: Env) -> Bytes {
        Storage::new(&env).get_storage_owner_key().to_xdr(&env)
    }

    pub fn guardian_key_xdr(env: Env) -> Bytes {
        Storage::new(&env).get_storage_guardian_key().to_xdr(&env)
    }

    pub fn nonce_key_xdr(env: Env) -> Bytes {
        Storage::new(&env).get_storage_nonce_key().to_xdr(&env)
    }
}
