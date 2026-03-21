#![no_std]

use soroban_sdk::{contract, contractimpl, Address, Env};
use soroban_sdk_tools::contracttrait;

// === Trait definition using our macro ===
// Generates:
//   - OwnableInternal trait (inner, business logic)
//   - Ownable trait (outer, auth-enforced, with type Provider)
//   - OwnableAuthClient (test helper)
//   - impl_ownable! macro (sealed auth wiring)

#[contracttrait]
pub trait Ownable {
    fn owner(env: &Env) -> Address;

    #[auth(Self::owner)]
    fn transfer_ownership(env: &Env, new_owner: Address);
}

// === Provider: implements OwnableInternal (business logic only) ===
pub struct SingleOwner;

impl OwnableInternal for SingleOwner {
    fn owner(env: &Env) -> Address {
        env.storage()
            .instance()
            .get(&soroban_sdk::Symbol::new(env, "owner"))
            .expect("not initialized")
    }

    fn transfer_ownership(env: &Env, new_owner: Address) {
        env.storage()
            .instance()
            .set(&soroban_sdk::Symbol::new(env, "owner"), &new_owner);
    }
}

// === Supertrait: Pausable extends Ownable ===
#[contracttrait]
pub trait Pausable: Ownable {
    fn is_paused(env: &Env) -> bool;

    #[auth(Self::owner)]
    fn pause(env: &Env);

    #[auth(Self::owner)]
    fn unpause(env: &Env);
}

impl PausableInternal for SingleOwner {
    fn is_paused(env: &Env) -> bool {
        env.storage()
            .instance()
            .get(&soroban_sdk::Symbol::new(env, "paused"))
            .unwrap_or(false)
    }

    fn pause(env: &Env) {
        env.storage()
            .instance()
            .set(&soroban_sdk::Symbol::new(env, "paused"), &true);
    }

    fn unpause(env: &Env) {
        env.storage()
            .instance()
            .set(&soroban_sdk::Symbol::new(env, "paused"), &false);
    }
}

// === Contract: wires providers ===
#[contract]
pub struct TestContract;

// Option A: Use #[contractimpl(contracttrait)] -- flexible but overridable
#[contractimpl(contracttrait)]
impl Ownable for TestContract {
    type Provider = SingleOwner;
}

#[contractimpl(contracttrait)]
impl Pausable for TestContract {
    type Provider = SingleOwner;
}

// Option B: Use impl_ownable!(TestContract, SingleOwner) for sealed auth
// (cannot be used alongside the contractimpl above for the same methods)

#[contractimpl]
impl TestContract {
    pub fn init(env: Env, owner: Address) {
        env.storage()
            .instance()
            .set(&soroban_sdk::Symbol::new(&env, "owner"), &owner);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use soroban_sdk::testutils::Address as _;
    use soroban_sdk::Env;

    #[test]
    fn test_ownership_with_auth_enforcement() {
        let env = Env::default();
        env.mock_all_auths();

        let contract_id = env.register(TestContract, ());
        let client = TestContractClient::new(&env, &contract_id);

        let owner = Address::generate(&env);
        client.init(&owner);

        assert_eq!(client.owner(), owner);

        let new_owner = Address::generate(&env);
        client.transfer_ownership(&new_owner);
        assert_eq!(client.owner(), new_owner);
    }

    #[test]
    fn test_pausable_supertrait_composition() {
        let env = Env::default();
        env.mock_all_auths();

        let contract_id = env.register(TestContract, ());
        let client = TestContractClient::new(&env, &contract_id);

        let owner = Address::generate(&env);
        client.init(&owner);

        assert!(!client.is_paused());
        client.pause();
        assert!(client.is_paused());
        client.unpause();
        assert!(!client.is_paused());
    }

    #[test]
    fn test_ownable_auth_client() {
        let env = Env::default();

        let contract_id = env.register(TestContract, ());
        let client = TestContractClient::new(&env, &contract_id);
        let auth_client = OwnableAuthClient::new(&env, &contract_id);

        let owner = Address::generate(&env);
        env.mock_all_auths();
        client.init(&owner);

        let new_owner = Address::generate(&env);
        auth_client
            .transfer_ownership(&new_owner)
            .authorize(&owner)
            .invoke();

        assert_eq!(client.owner(), new_owner);
    }

    #[test]
    fn test_pausable_auth_client() {
        let env = Env::default();

        let contract_id = env.register(TestContract, ());
        let client = TestContractClient::new(&env, &contract_id);
        let pause_auth = PausableAuthClient::new(&env, &contract_id);

        let owner = Address::generate(&env);
        env.mock_all_auths();
        client.init(&owner);

        pause_auth.pause().authorize(&owner).invoke();
        assert!(client.is_paused());

        pause_auth.unpause().authorize(&owner).invoke();
        assert!(!client.is_paused());
    }
}
