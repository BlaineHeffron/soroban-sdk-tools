#![no_std]
use soroban_sdk::{contract, contractimpl, log, Env};
use soroban_sdk_tools::{contractstorage, scerr, InstanceItem};

#[scerr]
pub enum Error {
    LimitReached,
}

#[contractstorage]
struct Storage {
    #[short_key = "counter"]
    counter: InstanceItem<u32>,
}

const MAX: u32 = 5;

#[contract]
pub struct IncrementContract;

#[contractimpl]
impl IncrementContract {
    /// Increment increments an internal counter, and returns the value. Errors
    /// if the value is attempted to be incremented past 5.
    pub fn increment(env: Env) -> Result<u32, Error> {
        let storage = Storage::new(&env);
        // Get the current count.
        let mut count: u32 = storage.counter.get().unwrap_or(0);
        log!(&env, "count: {}", count);

        // Increment the count.
        count += 1;

        // Check if the count exceeds the max.
        if count <= MAX {
            // Save the count.
            storage.counter.set(&count);

            // Return the count to the caller.
            Ok(count)
        } else {
            // Return an error if the max is exceeded.
            Err(Error::LimitReached)
        }
    }
}

mod test;
