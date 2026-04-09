//! This contract demonstrates how to implement authorization using
//! Soroban-managed auth framework for a simple case (a single user that needs
//! to authorize a single contract invocation).
//!
//! See `timelock` and `single_offer` examples for demonstration of performing
//! authorized token operations on behalf of the user.
//!
//! See `atomic_swap` and `atomic_multiswap` examples for demonstration of
//! multi-party authorizaton.
//!
//! See `account` example for demonstration of an acount contract with
//! a custom authentication scheme and a custom authorization policy.
#![no_std]
use soroban_sdk::{contract, contractimpl, Address, Env};
use soroban_sdk_tools::{contractstorage, InstanceMap};

// Mirrors upstream stellar/soroban-examples: counters live in instance
// storage so that the whole map shares one TTL bump and one storage entry.
#[contractstorage(auto_shorten = true)]
#[allow(dead_code)] // contractstorage generates static accessors that bypass the field
struct Storage {
    counters: InstanceMap<Address, u32>,
}

#[contract]
pub struct IncrementContract;

#[contractimpl]
impl IncrementContract {
    /// Increment increments a counter for the user, and returns the value.
    pub fn increment(env: Env, user: Address, value: u32) -> u32 {
        // Requires `user` to have authorized call of the `increment` of this
        // contract with all the arguments passed to `increment`, i.e. `user`
        // and `value`. This will panic if auth fails for any reason.
        // When this is called, Soroban host performs the necessary
        // authentication, manages replay prevention and enforces the user's
        // authorization policies.
        // The contracts normally shouldn't worry about these details and just
        // write code in generic fashion using `Address` and `require_auth` (or
        // `require_auth_for_args`).
        user.require_auth();

        // This call is equilvalent to the above:
        // user.require_auth_for_args((&user, value).into_val(&env));

        // The following has less arguments but is equivalent in authorization
        // scope to the above calls (the user address doesn't have to be
        // included in args as it's guaranteed to be authenticated).
        // user.require_auth_for_args((value,).into_val(&env));

        Storage::update_counters(&env, &user, |count| count.unwrap_or_default() + value)
    }
}

mod test;
