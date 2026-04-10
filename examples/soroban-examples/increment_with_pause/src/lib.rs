#![no_std]
use soroban_sdk::{
    contract, contractclient, contractimpl, Address, Env,
};
use soroban_sdk_tools::{contractstorage, scerr, InstanceItem};

#[contractstorage(auto_shorten = true)]
struct Storage {
    counter: InstanceItem<u32>,
    pause: InstanceItem<Address>,
}

#[contract]
pub struct IncrementContract;

#[scerr]
pub enum Error {
    Paused,
}

#[contractclient(name = "PauseClient")]
pub trait Pause {
    fn paused(env: Env) -> bool;
}

#[contractimpl]
impl IncrementContract {
    pub fn __constructor(env: Env, pause: Address) {
        Storage::new(&env).pause.set(&pause);
    }

    /// Increment increments an internal counter, and returns the value.
    pub fn increment(env: Env) -> Result<u32, Error> {
        let storage = Storage::new(&env);
        let pause_address = storage.pause.get().unwrap();
        let pause = PauseClient::new(&env, &pause_address);

        if pause.paused() {
            return Err(Error::Paused);
        }

        // Get the current count.
        let mut count: u32 = storage.counter.get().unwrap_or(0);

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
        Ok(count)
    }
}

mod test_mock;
mod test_real;
