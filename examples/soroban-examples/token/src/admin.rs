use soroban_sdk::{Address, Env};

use crate::storage_types::TokenStorage;

pub fn read_administrator(e: &Env) -> Address {
    TokenStorage::new(e).admin.get().unwrap()
}

pub fn write_administrator(e: &Env, id: &Address) {
    TokenStorage::new(e).admin.set(id);
}
