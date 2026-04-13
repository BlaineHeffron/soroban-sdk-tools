#![no_std]

use soroban_sdk::{contract, contractimpl, xdr::ToXdr, Address, Bytes, Env};
use soroban_sdk_tools::{contractstorage, InstanceItem, PersistentMap};

#[contractstorage(auto_shorten = true)]
struct Storage {
    admin: InstanceItem<Address>,
    balances: PersistentMap<Address, i128>,
    allowances: PersistentMap<(Address, Address), i128>,
}

#[contract]
pub struct TokenLiteAuto;

#[contractimpl]
impl TokenLiteAuto {
    pub fn initialize(env: Env, admin: Address) {
        let storage = Storage::new(&env);
        if storage.admin.has() {
            panic!("already initialized");
        }
        storage.admin.set(&admin);
    }

    pub fn mint(env: Env, to: Address, amount: i128) {
        Storage::new(&env)
            .balances
            .update(&to, |balance| balance.unwrap_or(0) + amount);
    }

    pub fn approve(env: Env, owner: Address, spender: Address, amount: i128) {
        Storage::new(&env)
            .allowances
            .set(&(owner, spender), &amount);
    }

    pub fn admin_key_len(env: Env) -> u32 {
        Storage::new(&env)
            .get_storage_admin_key()
            .to_xdr(&env)
            .len()
    }

    pub fn balance_key_len(env: Env, addr: Address) -> u32 {
        Storage::new(&env)
            .get_storage_balances_key(addr)
            .to_xdr(&env)
            .len()
    }

    pub fn allowance_key_len(env: Env, owner: Address, spender: Address) -> u32 {
        Storage::new(&env)
            .get_storage_allowances_key((owner, spender))
            .to_xdr(&env)
            .len()
    }

    pub fn admin_key_xdr(env: Env) -> Bytes {
        Storage::new(&env).get_storage_admin_key().to_xdr(&env)
    }

    pub fn balance_key_xdr(env: Env, addr: Address) -> Bytes {
        Storage::new(&env)
            .get_storage_balances_key(addr)
            .to_xdr(&env)
    }

    pub fn allowance_key_xdr(env: Env, owner: Address, spender: Address) -> Bytes {
        Storage::new(&env)
            .get_storage_allowances_key((owner, spender))
            .to_xdr(&env)
    }
}
