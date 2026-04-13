#![no_std]

use soroban_sdk::{contract, contractimpl, symbol_short, xdr::ToXdr, Bytes, Env, Symbol};

const COUNTER: Symbol = symbol_short!("COUNTER");

#[contract]
pub struct IncrementManual;

#[contractimpl]
impl IncrementManual {
    pub fn increment(env: Env) -> u32 {
        let next = env.storage().instance().get(&COUNTER).unwrap_or(0u32) + 1;
        env.storage().instance().set(&COUNTER, &next);
        next
    }

    pub fn counter_key_len(env: Env) -> u32 {
        COUNTER.to_xdr(&env).len()
    }

    pub fn counter_key_xdr(env: Env) -> Bytes {
        COUNTER.to_xdr(&env)
    }
}
