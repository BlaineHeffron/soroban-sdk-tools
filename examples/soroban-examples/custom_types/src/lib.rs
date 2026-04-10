#![no_std]
use soroban_sdk::{contract, contractimpl, contracttype, Env};
use soroban_sdk_tools::{contractstorage, InstanceItem};

#[contracttype]
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct State {
    pub count: u32,
    pub last_incr: u32,
}

#[contractstorage(auto_shorten = true)]
struct Storage {
    state: InstanceItem<State>,
}

#[contract]
pub struct IncrementContract;

#[contractimpl]
impl IncrementContract {
    /// Increment increments an internal counter, and returns the value.
    pub fn increment(env: Env, incr: u32) -> u32 {
        let storage = Storage::new(&env);
        // Get the current count.
        let mut state = Self::get_state(env.clone());

        // Increment the count.
        state.count += incr;
        state.last_incr = incr;

        // Save the count.
        storage.state.set(&state);

        // Return the count to the caller.
        state.count
    }
    /// Return the current state.
    pub fn get_state(env: Env) -> State {
        Storage::new(&env).state.get().unwrap_or(State {
            count: 0,
            last_incr: 0,
        }) // If no value set, assume 0.
    }
}

mod test;
