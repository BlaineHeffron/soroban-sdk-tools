#![no_std]

use soroban_sdk::{contract, contractimpl, contracttype, xdr::ToXdr, Address, Bytes, Env};

#[derive(Clone)]
#[contracttype]
pub struct AllowanceKey {
    pub owner: Address,
    pub spender: Address,
}

#[derive(Clone)]
#[contracttype]
pub enum DataKey {
    Admin,
    Balance(Address),
    Allowance(AllowanceKey),
}

#[contract]
pub struct TokenLiteManual;

#[contractimpl]
impl TokenLiteManual {
    pub fn initialize(env: Env, admin: Address) {
        if env.storage().instance().has(&DataKey::Admin) {
            panic!("already initialized");
        }
        env.storage().instance().set(&DataKey::Admin, &admin);
    }

    pub fn mint(env: Env, to: Address, amount: i128) {
        let key = DataKey::Balance(to);
        let next = env.storage().persistent().get(&key).unwrap_or(0i128) + amount;
        env.storage().persistent().set(&key, &next);
    }

    pub fn approve(env: Env, owner: Address, spender: Address, amount: i128) {
        let key = DataKey::Allowance(AllowanceKey { owner, spender });
        env.storage().persistent().set(&key, &amount);
    }

    pub fn admin_key_len(env: Env) -> u32 {
        DataKey::Admin.to_xdr(&env).len()
    }

    pub fn balance_key_len(env: Env, addr: Address) -> u32 {
        DataKey::Balance(addr).to_xdr(&env).len()
    }

    pub fn allowance_key_len(env: Env, owner: Address, spender: Address) -> u32 {
        DataKey::Allowance(AllowanceKey { owner, spender })
            .to_xdr(&env)
            .len()
    }

    pub fn admin_key_xdr(env: Env) -> Bytes {
        DataKey::Admin.to_xdr(&env)
    }

    pub fn balance_key_xdr(env: Env, addr: Address) -> Bytes {
        DataKey::Balance(addr).to_xdr(&env)
    }

    pub fn allowance_key_xdr(env: Env, owner: Address, spender: Address) -> Bytes {
        DataKey::Allowance(AllowanceKey { owner, spender }).to_xdr(&env)
    }
}
