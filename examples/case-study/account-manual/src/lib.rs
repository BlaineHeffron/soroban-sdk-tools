#![no_std]

use soroban_sdk::{contract, contractimpl, contracttype, xdr::ToXdr, Address, Bytes, Env};

#[derive(Clone)]
#[contracttype]
pub enum DataKey {
    Owner,
    Guardian,
    Nonce,
}

#[contract]
pub struct AccountManual;

#[contractimpl]
impl AccountManual {
    pub fn initialize(env: Env, owner: Address, guardian: Address) {
        if env.storage().instance().has(&DataKey::Owner) {
            panic!("already initialized");
        }
        env.storage().instance().set(&DataKey::Owner, &owner);
        env.storage().instance().set(&DataKey::Guardian, &guardian);
        env.storage().instance().set(&DataKey::Nonce, &0u32);
    }

    pub fn rotate_guardian(env: Env, guardian: Address) {
        env.storage().instance().set(&DataKey::Guardian, &guardian);
    }

    pub fn bump_nonce(env: Env) -> u32 {
        let next = env
            .storage()
            .instance()
            .get(&DataKey::Nonce)
            .unwrap_or(0u32)
            + 1;
        env.storage().instance().set(&DataKey::Nonce, &next);
        next
    }

    pub fn owner_key_len(env: Env) -> u32 {
        DataKey::Owner.to_xdr(&env).len()
    }

    pub fn guardian_key_len(env: Env) -> u32 {
        DataKey::Guardian.to_xdr(&env).len()
    }

    pub fn nonce_key_len(env: Env) -> u32 {
        DataKey::Nonce.to_xdr(&env).len()
    }

    pub fn owner_key_xdr(env: Env) -> Bytes {
        DataKey::Owner.to_xdr(&env)
    }

    pub fn guardian_key_xdr(env: Env) -> Bytes {
        DataKey::Guardian.to_xdr(&env)
    }

    pub fn nonce_key_xdr(env: Env) -> Bytes {
        DataKey::Nonce.to_xdr(&env)
    }
}
