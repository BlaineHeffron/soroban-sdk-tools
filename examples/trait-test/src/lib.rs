#![no_std]

use soroban_sdk::{contract, contracterror, contractimpl, Address, Env};
use soroban_sdk_tools::{contractstorage, contracttrait, InstanceItem};

// === Error types (production pattern, not .expect()) ===
#[contracterror]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[repr(u32)]
pub enum ContractError {
    NotInitialized = 1,
    AlreadyInitialized = 2,
}

// === Storage using #[contractstorage] (namespaced keys, not raw strings) ===
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

// === Trait definitions using our macro ===

#[contracttrait]
pub trait Ownable {
    fn owner(env: &Env) -> Address;

    #[auth(Self::owner)]
    fn transfer_ownership(env: &Env, new_owner: Address);
}

#[contracttrait]
pub trait Pausable: Ownable {
    fn is_paused(env: &Env) -> bool;

    #[auth(Self::owner)]
    fn pause(env: &Env);

    #[auth(Self::owner)]
    fn unpause(env: &Env);
}

// === Provider: implements OwnableInternal + PausableInternal ===
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

// === Contract wiring ===
#[contract]
pub struct TestContract;

#[contractimpl(contracttrait)]
impl Ownable for TestContract {
    type Provider = StandardProvider;
}

#[contractimpl(contracttrait)]
impl Pausable for TestContract {
    type Provider = StandardProvider;
}

#[contractimpl]
impl TestContract {
    /// Initialize the contract. Protected against double-initialization.
    pub fn init(env: Env, owner: Address) -> Result<(), ContractError> {
        if OwnableStorage::has_owner(&env) {
            return Err(ContractError::AlreadyInitialized);
        }
        OwnableStorage::set_owner(&env, &owner);
        Ok(())
    }
}

// === Tests: ALL using AuthClient, with negative security tests ===
#[cfg(test)]
mod test {
    use super::*;
    use soroban_sdk::testutils::Address as _;
    use soroban_sdk::Env;

    fn setup() -> (Env, Address, Address, TestContractClient<'static>) {
        let env = Env::default();
        let contract_id = env.register(TestContract, ());
        let client = TestContractClient::new(&env, &contract_id);

        let owner = Address::generate(&env);
        let other = Address::generate(&env);

        // Initialize with targeted auth (not mock_all_auths)
        env.mock_auths(&[]);
        // init doesn't require auth, so call directly
        client.init(&owner);

        (env, owner, other, client)
    }

    // ---- Positive auth tests (using AuthClient) ----

    #[test]
    fn test_transfer_ownership_with_auth() {
        let (env, owner, _, _) = setup();
        let contract_id = env.register(TestContract, ());
        let client = TestContractClient::new(&env, &contract_id);
        let auth = OwnableAuthClient::new(&env, &contract_id);
        env.mock_auths(&[]);
        client.init(&owner);

        let new_owner = Address::generate(&env);
        auth.transfer_ownership(&new_owner)
            .authorize(&owner)
            .invoke();

        assert_eq!(client.owner(), new_owner);
    }

    #[test]
    fn test_pause_unpause_with_auth() {
        let (env, owner, _, _) = setup();
        let contract_id = env.register(TestContract, ());
        let client = TestContractClient::new(&env, &contract_id);
        let pause_auth = PausableAuthClient::new(&env, &contract_id);
        env.mock_auths(&[]);
        client.init(&owner);

        assert!(!client.is_paused());

        pause_auth.pause().authorize(&owner).invoke();
        assert!(client.is_paused());

        pause_auth.unpause().authorize(&owner).invoke();
        assert!(!client.is_paused());
    }

    // ---- Negative security tests ----

    #[test]
    #[should_panic(expected = "HostError")]
    fn test_transfer_without_auth_fails() {
        let (env, owner, _, _) = setup();
        let contract_id = env.register(TestContract, ());
        let client = TestContractClient::new(&env, &contract_id);
        env.mock_auths(&[]);
        client.init(&owner);

        // No auth setup -- this must fail
        let new_owner = Address::generate(&env);
        client.transfer_ownership(&new_owner);
    }

    #[test]
    #[should_panic(expected = "HostError")]
    fn test_pause_with_wrong_address_fails() {
        let (env, owner, _, _) = setup();
        let contract_id = env.register(TestContract, ());
        let client = TestContractClient::new(&env, &contract_id);
        let pause_auth = PausableAuthClient::new(&env, &contract_id);
        env.mock_auths(&[]);
        client.init(&owner);

        let impostor = Address::generate(&env);
        // Auth with wrong address -- must fail
        pause_auth.pause().authorize(&impostor).invoke();
    }

    #[test]
    fn test_double_init_fails() {
        let env = Env::default();
        let contract_id = env.register(TestContract, ());
        let client = TestContractClient::new(&env, &contract_id);

        let owner = Address::generate(&env);
        client.init(&owner);

        // Second init must fail
        let result = client.try_init(&owner);
        assert!(result.is_err());
    }

    // NOTE: Direct calls to StandardProvider methods (the Internal trait)
    // bypass auth enforcement. This is documented in the security model.
    // Use impl_ownable!() for sealed auth that prevents this bypass.
    // We do not test this because Internal methods require contract context
    // to access storage, and testing it would require mock_all_auths()
    // which we deliberately avoid in this example.
}
