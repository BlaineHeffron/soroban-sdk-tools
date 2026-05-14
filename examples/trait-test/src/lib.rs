#![no_std]

//! Composable contract traits example: Ownable + Pausable.
//!
//! Demonstrates `#[contracttrait]` with `#[auth]` enforcement,
//! `#[contractstorage]` for typed storage, and `#[contractimpl]`
//! for wiring -- all from `soroban_sdk_tools`.

use soroban_sdk::{contract, contracterror, Address, Env};
use soroban_sdk_tools::{contractimpl, contractstorage, contracttrait, InstanceItem};

// ============================================================
// Errors
// ============================================================

#[contracterror]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[repr(u32)]
pub enum ContractError {
    AlreadyInitialized = 1,
    NotInitialized = 2,
    ContractPaused = 3,
}

// ============================================================
// Storage (namespaced via #[contractstorage])
// ============================================================

#[contractstorage]
pub struct OwnableStorage {
    #[short_key = "own"]
    owner: InstanceItem<Address>,
}

#[contractstorage]
pub struct PausableStorage {
    #[short_key = "psd"]
    paused: InstanceItem<bool>,
}

// ============================================================
// Trait definitions
// ============================================================

/// Ownership management with auth enforcement.
#[contracttrait]
pub trait Ownable {
    /// Returns the current owner.
    fn owner(env: &Env) -> Address;

    /// Transfer ownership. Requires current owner authorization.
    #[auth(Self::owner)]
    fn transfer_ownership(env: &Env, new_owner: Address);
}

/// Pause/unpause functionality. Auth comes from Ownable's owner.
#[contracttrait]
pub trait Pausable: Ownable {
    /// Check if the contract is paused.
    fn is_paused(env: &Env) -> bool;

    /// Pause the contract. Requires owner authorization.
    #[auth(Self::owner)]
    fn pause(env: &Env);

    /// Unpause the contract. Requires owner authorization.
    #[auth(Self::owner)]
    fn unpause(env: &Env);
}

// ============================================================
// Provider: business logic (no auth code here)
// ============================================================

pub struct StandardProvider;

impl OwnableInternal for StandardProvider {
    fn owner(env: &Env) -> Address {
        OwnableStorage::get_owner(env).unwrap_or_else(|| {
            panic!("{}", ContractError::NotInitialized as u32)
        })
    }

    fn transfer_ownership(env: &Env, new_owner: Address) {
        OwnableStorage::set_owner(env, &new_owner);
    }
}

impl PausableInternal for StandardProvider {
    fn is_paused(env: &Env) -> bool {
        PausableStorage::get_paused(env).unwrap_or(false)
    }

    fn pause(env: &Env) {
        PausableStorage::set_paused(env, &true);
    }

    fn unpause(env: &Env) {
        PausableStorage::set_paused(env, &false);
    }
}

// ============================================================
// Contract: wire traits via type Provider
// ============================================================

#[contract]
pub struct TraitTestContract;

/// Wire Ownable -- auth is in the trait defaults, no impl_ownable! needed.
#[contractimpl(contracttrait)]
impl Ownable for TraitTestContract {
    type Provider = StandardProvider;
}

/// Wire Pausable -- auth comes from Ownable's owner via supertrait.
#[contractimpl(contracttrait)]
impl Pausable for TraitTestContract {
    type Provider = StandardProvider;
}

/// Contract-specific methods.
#[contractimpl]
impl TraitTestContract {
    /// Initialize the contract with an owner. Protected against double-init.
    pub fn init(env: Env, owner: Address) -> Result<(), ContractError> {
        if OwnableStorage::has_owner(&env) {
            return Err(ContractError::AlreadyInitialized);
        }
        OwnableStorage::set_owner(&env, &owner);
        Ok(())
    }

    /// Example guarded action -- checks pause state before proceeding.
    pub fn do_something(env: Env, caller: Address) -> Result<(), ContractError> {
        if PausableStorage::get_paused(&env).unwrap_or(false) {
            return Err(ContractError::ContractPaused);
        }
        caller.require_auth();
        Ok(())
    }
}

// ============================================================
// Tests using contractimport! (loads compiled WASM)
// ============================================================

#[cfg(test)]
mod test {
    use soroban_sdk::testutils::Address as _;
    use soroban_sdk::Env;

    // Import the compiled WASM for realistic testing
    mod contract {
        soroban_sdk_tools::contractimport!(
            file = "../../../target/stellar/soroban_trait_test.wasm"
        );
    }

    fn setup() -> (
        Env,
        soroban_sdk::Address,
        contract::Client<'static>,
        contract::AuthClient<'static>,
    ) {
        let env = Env::default();
        let id = env.register(contract::WASM, ());
        let client = contract::Client::new(&env, &id);
        let auth = contract::AuthClient::new(&env, &id);
        let owner = soroban_sdk::Address::generate(&env);
        client.init(&owner);
        (env, owner, client, auth)
    }

    // ---- Positive tests ----

    #[test]
    fn test_owner_returns_initialized_address() {
        let (_env, owner, client, _auth) = setup();
        assert_eq!(client.owner(), owner);
    }

    #[test]
    fn test_transfer_ownership_with_auth() {
        let (env, owner, client, auth) = setup();
        let new_owner = soroban_sdk::Address::generate(&env);

        auth.transfer_ownership(&new_owner)
            .authorize(&owner)
            .invoke();

        assert_eq!(client.owner(), new_owner);
    }

    #[test]
    fn test_pause_unpause_with_auth() {
        let (_env, owner, client, auth) = setup();

        assert!(!client.is_paused());

        auth.pause().authorize(&owner).invoke();
        assert!(client.is_paused());

        auth.unpause().authorize(&owner).invoke();
        assert!(!client.is_paused());
    }

    #[test]
    fn test_do_something_while_paused_fails() {
        let (env, owner, client, auth) = setup();
        let caller = soroban_sdk::Address::generate(&env);

        auth.pause().authorize(&owner).invoke();

        let result = client.try_do_something(&caller);
        assert!(result.is_err());
    }

    // ---- Negative security tests ----

    #[test]
    #[should_panic(expected = "HostError")]
    fn test_transfer_without_auth_panics() {
        let (env, _owner, client, _auth) = setup();
        let new_owner = soroban_sdk::Address::generate(&env);
        // No auth -- structural enforcement means this panics
        client.transfer_ownership(&new_owner);
    }

    #[test]
    #[should_panic(expected = "HostError")]
    fn test_pause_with_wrong_address_panics() {
        let (env, _owner, _client, auth) = setup();
        let impostor = soroban_sdk::Address::generate(&env);
        auth.pause().authorize(&impostor).invoke();
    }

    #[test]
    fn test_double_init_fails() {
        let (env, owner, client, _auth) = setup();
        let result = client.try_init(&owner);
        assert!(result.is_err());
    }

    #[test]
    fn test_ownership_transfer_then_pause_requires_new_owner() {
        let (env, owner, client, auth) = setup();
        let new_owner = soroban_sdk::Address::generate(&env);

        // Transfer ownership
        auth.transfer_ownership(&new_owner)
            .authorize(&owner)
            .invoke();

        // Old owner can no longer pause
        let pause_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            auth.pause().authorize(&owner).invoke();
        }));
        assert!(pause_result.is_err());

        // New owner can pause
        auth.pause().authorize(&new_owner).invoke();
        assert!(client.is_paused());
    }
}
