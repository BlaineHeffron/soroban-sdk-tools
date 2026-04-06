#![no_std]
use soroban_sdk::{contract, contractimpl, log, Env};
use soroban_sdk_tools::{contractstorage, InstanceItem};

#[contractstorage(auto_shorten = true)]
struct Storage {
    counter: InstanceItem<u32>,
}

#[contract]
pub struct IncrementContract;

#[contractimpl]
impl IncrementContract {
    /// Increment increments an internal counter, and returns the value.
    pub fn increment(env: Env) -> u32 {
        let storage = Storage::new(&env);
        // Get the current count.
        let mut count: u32 = storage.counter.get().unwrap_or(0);
        log!(&env, "count: {}", count);

        // Increment the count.
        count += 1;

        // Save the count.
        storage.counter.set(&count);

        // The contract instance will be bumped to have a lifetime of at least 100 ledgers if the current expiration lifetime at most 50.
        // If the lifetime is already more than 100 ledgers, this is a no-op. Otherwise,
        // the lifetime is extended to 100 ledgers. This lifetime bump includes the contract
        // instance itself and all entries in storage().instance(), i.e, COUNTER.
        storage.counter.extend_ttl(50, 100);

        // Return the count to the caller.
        count
    }
}

mod test;
