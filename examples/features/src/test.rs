#![cfg(test)]
use crate::{FeaturesContract, FeaturesContractClient};
use soroban_sdk::{
    testutils::{Address as _, Ledger},
    Address, Env,
};

#[test]
fn test_mixed_storage_flow() {
    let env = &Env::default();
    env.mock_all_auths();

    let contract_id = env.register(FeaturesContract, ());
    let client = FeaturesContractClient::new(env, &contract_id);

    let admin = Address::generate(env);
    let user = Address::generate(env);

    // 1. Initialize (Instance Storage)
    client.init(&admin);
    assert!(client.is_enabled());

    // 2. Mint (Persistent Storage)
    client.mint(&user, &1000);
    assert_eq!(client.get_balance(&user), 1000);

    // 3. Pause contract
    client.set_state(&false);
    assert!(!client.is_enabled());

    // 4. Verify pause prevents minting
    let result = client.try_mint(&user, &500);
    assert!(result.is_err());
}

#[test]
fn test_rate_limiting() {
    let env = &Env::default();
    env.mock_all_auths();

    let contract_id = env.register(FeaturesContract, ());
    let client = FeaturesContractClient::new(env, &contract_id);

    let admin = Address::generate(env);
    let user = Address::generate(env);

    client.init(&admin);

    // First faucet claim succeeds
    client.faucet(&user);
    assert_eq!(client.get_balance(&user), 10);

    // Second claim immediately fails (rate limited)
    let result = client.try_faucet(&user);
    assert!(result.is_err());

    // Advance ledger by 100
    env.ledger().with_mut(|li| li.sequence_number += 100);

    // Now claim succeeds
    client.faucet(&user);
    assert_eq!(client.get_balance(&user), 20);
}

#[test]
fn test_symbolic_vs_hashed_keys() {
    let env = &Env::default();
    env.mock_all_auths();

    let contract_id = env.register(FeaturesContract, ());
    let client = FeaturesContractClient::new(env, &contract_id);

    let admin = Address::generate(env);
    client.init(&admin);

    // Verify Instance storage uses symbolic keys
    env.as_contract(&contract_id, || {
        use soroban_sdk::{IntoVal, Symbol, Val};

        // Config.admin is stored as Symbol("Admin")
        let key: Val = Symbol::new(env, "Admin").into_val(env);
        assert!(env.storage().instance().has(&key));
    });
}
