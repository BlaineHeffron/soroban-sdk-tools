#![no_std]
use soroban_sdk::{contract, contractimpl, log, Address, Env};
use soroban_sdk_tools::{contractstorage, InstanceItem, PersistentMap};

#[contractstorage]
pub struct Counter {
    value: InstanceItem<u32>,
    values: PersistentMap<Address, u32>,
}

#[contract]
pub struct IncrementContract;

#[contractimpl]
impl IncrementContract {
    /// Increment increments an internal counter, and returns the value.
    #[must_use]
    pub fn increment(env: &Env) -> u32 {
        let storage = Counter::new(env);
        // Get the current count.
        let mut count = storage.value.get().unwrap_or(0); // If no value set, assume 0.
        log!(env, "count: {}", count);

        // Increment the count.
        count += 1;

        // Save the count.
        storage.value.set(&count);

        // The contract instance will be bumped to have a lifetime of at least 100 ledgers if the current expiration lifetime at most 50.
        // If the lifetime is already more than 100 ledgers, this is a no-op. Otherwise,
        // the lifetime is extended to 100 ledgers. This lifetime bump includes the contract
        // instance itself and all entries in storage().instance(), i.e, COUNTER.
        env.storage().instance().extend_ttl(50, 100);

        // Return the count to the caller.
        count
    }

    #[must_use]
    pub fn increment_for(env: &Env, addr: &Address) -> u32 {
        let storage = Counter::new(env);
        // Get the current count for the address.
        let mut count = storage.values.get(addr).unwrap_or(0); // If no value set, assume 0.
        log!(env, "count for {}: {}", addr, count);

        // Increment the count.
        count += 1;

        // Save the count.
        storage.values.set(addr, &count);

        // Return the count to the caller.
        count
    }
}

mod test;
