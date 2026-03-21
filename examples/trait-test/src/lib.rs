#![no_std]

use soroban_sdk::{contract, contractimpl, contracttrait, Address, Env};

// === Inner trait: business logic only ===
pub trait OwnableImpl {
    fn owner(env: &Env) -> Address;
    fn transfer_ownership(env: &Env, new_owner: &Address);
}

// === Outer trait: auth wrapper with type Impl ===
#[contracttrait]
pub trait Ownable {
    type Impl: OwnableImpl;

    fn owner(env: &Env) -> Address {
        Self::Impl::owner(env)
    }

    fn transfer_ownership(env: &Env, new_owner: Address) {
        // Structural auth enforcement
        Self::Impl::owner(env).require_auth();
        Self::Impl::transfer_ownership(env, &new_owner);
    }
}

// === Provider: concrete implementation ===
pub struct SingleOwner;

impl OwnableImpl for SingleOwner {
    fn owner(env: &Env) -> Address {
        env.storage()
            .instance()
            .get(&soroban_sdk::Symbol::new(env, "owner"))
            .expect("not initialized")
    }

    fn transfer_ownership(env: &Env, new_owner: &Address) {
        env.storage()
            .instance()
            .set(&soroban_sdk::Symbol::new(env, "owner"), new_owner);
    }
}

// === Contract: wires provider via type Impl ===
#[contract]
pub struct TestContract;

#[contractimpl(contracttrait)]
impl Ownable for TestContract {
    type Impl = SingleOwner;
}

#[contractimpl]
impl TestContract {
    pub fn init(env: Env, owner: Address) {
        env.storage()
            .instance()
            .set(&soroban_sdk::Symbol::new(&env, "owner"), &owner);
    }
}

// === Supertrait: Pausable extends Ownable ===
pub trait PausableImpl: OwnableImpl {
    fn is_paused(env: &Env) -> bool;
    fn set_paused(env: &Env, paused: bool);
}

#[contracttrait]
pub trait Pausable {
    type Impl: PausableImpl;

    fn is_paused(env: &Env) -> bool {
        Self::Impl::is_paused(env)
    }

    fn pause(env: &Env) {
        // Auth enforcement: uses owner from OwnableImpl supertrait
        Self::Impl::owner(env).require_auth();
        Self::Impl::set_paused(env, true);
    }

    fn unpause(env: &Env) {
        Self::Impl::owner(env).require_auth();
        Self::Impl::set_paused(env, false);
    }
}

// === Provider implements both ===
impl PausableImpl for SingleOwner {
    fn is_paused(env: &Env) -> bool {
        env.storage()
            .instance()
            .get(&soroban_sdk::Symbol::new(env, "paused"))
            .unwrap_or(false)
    }
    fn set_paused(env: &Env, paused: bool) {
        env.storage()
            .instance()
            .set(&soroban_sdk::Symbol::new(env, "paused"), &paused);
    }
}

#[contractimpl(contracttrait)]
impl Pausable for TestContract {
    type Impl = SingleOwner;
}

#[cfg(test)]
mod test {
    use super::*;
    use soroban_sdk::testutils::Address as _;
    use soroban_sdk::Env;

    #[test]
    fn test_type_impl_pattern() {
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
    fn test_supertrait_composition() {
        let env = Env::default();
        env.mock_all_auths();

        let contract_id = env.register(TestContract, ());
        let client = TestContractClient::new(&env, &contract_id);

        let owner = Address::generate(&env);
        client.init(&owner);

        // Pause uses owner from OwnableImpl supertrait
        assert!(!client.is_paused());
        client.pause();
        assert!(client.is_paused());
        client.unpause();
        assert!(!client.is_paused());
    }
}
