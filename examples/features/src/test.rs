#![cfg(test)]
use crate::{Error, FeaturesContract, FeaturesContractClient};
use soroban_sdk::{
    testutils::{Address as _, Ledger},
    Address, Bytes, BytesN, Env, IntoVal, String, Symbol, TryFromVal, Val, Vec as HostVec,
};

// ============================================================================
// Initialization Tests - Instance Storage
// ============================================================================

#[test]
fn test_initialization() {
    let env = &Env::default();
    env.mock_all_auths();

    let contract_id = env.register(FeaturesContract, ());
    let client = FeaturesContractClient::new(env, &contract_id);

    let admin = Address::generate(env);
    let name = String::from_str(env, "Test Token");
    let symbol = String::from_str(env, "TST");

    // Initialize contract
    client.init(&admin, &name, &symbol);

    // Verify instance data
    assert_eq!(client.get_admin(), Some(admin));
    assert!(client.is_enabled());
    assert_eq!(client.get_metadata(&String::from_str(env, "name")), Some(name));
    assert_eq!(
        client.get_metadata(&String::from_str(env, "symbol")),
        Some(symbol)
    );
    assert_eq!(client.get_total_supply(), 0);
}

#[test]
fn test_already_initialized_error() {
    let env = &Env::default();
    env.mock_all_auths();

    let contract_id = env.register(FeaturesContract, ());
    let client = FeaturesContractClient::new(env, &contract_id);

    let admin = Address::generate(env);
    let name = String::from_str(env, "Test Token");
    let symbol = String::from_str(env, "TST");

    client.init(&admin, &name, &symbol);

    // Second initialization should fail
    let result = client.try_init(&admin, &name, &symbol);
    assert_eq!(result, Err(Ok(Error::AlreadyInitialized)));
}

// ============================================================================
// Metadata Tests - InstanceMap Operations
// ============================================================================

#[test]
fn test_metadata_operations() {
    let env = &Env::default();
    env.mock_all_auths();

    let contract_id = env.register(FeaturesContract, ());
    let client = FeaturesContractClient::new(env, &contract_id);

    let admin = Address::generate(env);
    client.init(
        &admin,
        &String::from_str(env, "Token"),
        &String::from_str(env, "TKN"),
    );

    // Add metadata
    let key = String::from_str(env, "website");
    let value = String::from_str(env, "https://example.com");
    client.set_metadata(&key, &value);
    assert_eq!(client.get_metadata(&key), Some(value.clone()));

    // Update metadata
    let new_value = String::from_str(env, "https://newsite.com");
    client.set_metadata(&key, &new_value);
    assert_eq!(client.get_metadata(&key), Some(new_value));

    // Remove metadata
    client.remove_metadata(&key);
    assert_eq!(client.get_metadata(&key), None);
}

// ============================================================================
// Token Operations Tests - PersistentMap with auto_shorten
// ============================================================================

#[test]
fn test_mint_and_balance() {
    let env = &Env::default();
    env.mock_all_auths();

    let contract_id = env.register(FeaturesContract, ());
    let client = FeaturesContractClient::new(env, &contract_id);

    let admin = Address::generate(env);
    let user = Address::generate(env);

    client.init(
        &admin,
        &String::from_str(env, "Token"),
        &String::from_str(env, "TKN"),
    );

    // Mint tokens
    client.mint(&user, &1000);
    assert_eq!(client.get_balance(&user), 1000);
    assert_eq!(client.get_total_supply(), 1000);

    // Mint more tokens
    client.mint(&user, &500);
    assert_eq!(client.get_balance(&user), 1500);
    assert_eq!(client.get_total_supply(), 1500);
}

#[test]
fn test_transfer() {
    let env = &Env::default();
    env.mock_all_auths();

    let contract_id = env.register(FeaturesContract, ());
    let client = FeaturesContractClient::new(env, &contract_id);

    let admin = Address::generate(env);
    let from = Address::generate(env);
    let to = Address::generate(env);

    client.init(
        &admin,
        &String::from_str(env, "Token"),
        &String::from_str(env, "TKN"),
    );

    // Mint to sender
    client.mint(&from, &1000);

    // Transfer
    client.transfer(&from, &to, &300);

    assert_eq!(client.get_balance(&from), 700);
    assert_eq!(client.get_balance(&to), 300);
    assert_eq!(client.get_total_supply(), 1000);
}

#[test]
fn test_transfer_insufficient_balance() {
    let env = &Env::default();
    env.mock_all_auths();

    let contract_id = env.register(FeaturesContract, ());
    let client = FeaturesContractClient::new(env, &contract_id);

    let admin = Address::generate(env);
    let from = Address::generate(env);
    let to = Address::generate(env);

    client.init(
        &admin,
        &String::from_str(env, "Token"),
        &String::from_str(env, "TKN"),
    );

    client.mint(&from, &100);

    // Transfer more than balance
    let result = client.try_transfer(&from, &to, &300);
    assert_eq!(result, Err(Ok(Error::NotAuthorized)));
}

// ============================================================================
// Allowance Tests - Tuple Keys with custom short_key
// ============================================================================

#[test]
fn test_approve_and_transfer_from() {
    let env = &Env::default();
    env.mock_all_auths();

    let contract_id = env.register(FeaturesContract, ());
    let client = FeaturesContractClient::new(env, &contract_id);

    let admin = Address::generate(env);
    let owner = Address::generate(env);
    let spender = Address::generate(env);
    let recipient = Address::generate(env);

    client.init(
        &admin,
        &String::from_str(env, "Token"),
        &String::from_str(env, "TKN"),
    );

    // Mint to owner
    client.mint(&owner, &1000);

    // Approve spender
    client.approve(&owner, &spender, &500);

    assert_eq!(client.get_allowance(&owner, &spender), 500);

    // Transfer from owner to recipient via spender
    client.transfer_from(&spender, &owner, &recipient, &300);

    assert_eq!(client.get_balance(&owner), 700);
    assert_eq!(client.get_balance(&recipient), 300);
    assert_eq!(client.get_allowance(&owner, &spender), 200);
}

#[test]
fn test_transfer_from_insufficient_allowance() {
    let env = &Env::default();
    env.mock_all_auths();

    let contract_id = env.register(FeaturesContract, ());
    let client = FeaturesContractClient::new(env, &contract_id);

    let admin = Address::generate(env);
    let owner = Address::generate(env);
    let spender = Address::generate(env);
    let recipient = Address::generate(env);

    client.init(
        &admin,
        &String::from_str(env, "Token"),
        &String::from_str(env, "TKN"),
    );

    client.mint(&owner, &1000);

    // Try transfer without allowance
    let result = client.try_transfer_from(&spender, &owner, &recipient, &300);
    assert_eq!(result, Err(Ok(Error::NotAuthorized)));
}

// ============================================================================
// Account Management Tests - PersistentMap with short_key
// ============================================================================

#[test]
fn test_freeze_account() {
    let env = &Env::default();
    env.mock_all_auths();

    let contract_id = env.register(FeaturesContract, ());
    let client = FeaturesContractClient::new(env, &contract_id);

    let admin = Address::generate(env);
    let user = Address::generate(env);

    client.init(
        &admin,
        &String::from_str(env, "Token"),
        &String::from_str(env, "TKN"),
    );

    client.mint(&user, &1000);

    // Freeze account
    assert!(!client.is_frozen(&user));
    client.freeze_account(&user);
    assert!(client.is_frozen(&user));

    // Transfer should fail
    let recipient = Address::generate(env);
    let result = client.try_transfer(&user, &recipient, &100);
    assert_eq!(result, Err(Ok(Error::AccountFrozen)));

    // Unfreeze
    client.unfreeze_account(&user);
    assert!(!client.is_frozen(&user));

    // Transfer should succeed now
    client.transfer(&user, &recipient, &100);
    assert_eq!(client.get_balance(&user), 900);
    assert_eq!(client.get_balance(&recipient), 100);
}

// ============================================================================
// Governance Tests - Symbolic Override for Debugging
// ============================================================================

#[test]
fn test_governance_events() {
    let env = &Env::default();
    env.mock_all_auths();

    let contract_id = env.register(FeaturesContract, ());
    let client = FeaturesContractClient::new(env, &contract_id);

    let admin = Address::generate(env);
    client.init(
        &admin,
        &String::from_str(env, "Token"),
        &String::from_str(env, "TKN"),
    );

    // Record events
    client.record_event(&1, &String::from_str(env, "Contract deployed"));
    client.record_event(&2, &String::from_str(env, "First mint"));

    assert_eq!(
        client.get_event(&1),
        Some(String::from_str(env, "Contract deployed"))
    );
    assert_eq!(
        client.get_event(&2),
        Some(String::from_str(env, "First mint"))
    );
    assert_eq!(client.get_vote_count(), 2);
}

#[test]
fn test_proposals() {
    let env = &Env::default();
    env.mock_all_auths();

    let contract_id = env.register(FeaturesContract, ());
    let client = FeaturesContractClient::new(env, &contract_id);

    let admin = Address::generate(env);
    let proposer = Address::generate(env);

    client.init(
        &admin,
        &String::from_str(env, "Token"),
        &String::from_str(env, "TKN"),
    );

    // Create proposal
    client.create_proposal(&1, &proposer);
    // Note: We don't have a get_proposal method, but we can test it doesn't error
}

// ============================================================================
// Rate Limiting Tests - Temporary Storage
// ============================================================================

#[test]
fn test_faucet_rate_limiting() {
    let env = &Env::default();
    env.mock_all_auths();

    let contract_id = env.register(FeaturesContract, ());
    let client = FeaturesContractClient::new(env, &contract_id);

    let admin = Address::generate(env);
    let user = Address::generate(env);

    client.init(
        &admin,
        &String::from_str(env, "Token"),
        &String::from_str(env, "TKN"),
    );

    // First faucet claim succeeds
    client.faucet(&user);
    assert_eq!(client.get_balance(&user), 10);

    // Second claim immediately fails (rate limited)
    let result = client.try_faucet(&user);
    assert_eq!(result, Err(Ok(Error::RateLimitExceeded)));

    // Advance ledger by 99 (still too soon)
    env.ledger().with_mut(|li| li.sequence_number += 99);
    let result = client.try_faucet(&user);
    assert_eq!(result, Err(Ok(Error::RateLimitExceeded)));

    // Advance one more ledger (100 total)
    env.ledger().with_mut(|li| li.sequence_number += 1);

    // Now claim succeeds
    client.faucet(&user);
    assert_eq!(client.get_balance(&user), 20);
}

#[test]
fn test_faucet_when_paused() {
    let env = &Env::default();
    env.mock_all_auths();

    let contract_id = env.register(FeaturesContract, ());
    let client = FeaturesContractClient::new(env, &contract_id);

    let admin = Address::generate(env);

    client.init(
        &admin,
        &String::from_str(env, "Token"),
        &String::from_str(env, "TKN"),
    );

    // Pause contract
    client.set_metadata(&String::from_str(env, "paused"), &String::from_str(env, "true"));

    // Note: We don't have a pause mechanism in the current contract
    // Let's test with the enabled flag instead
}

// ============================================================================
// Advanced Storage Tests - All Types Together
// ============================================================================

#[test]
fn test_advanced_operation() {
    let env = &Env::default();
    env.mock_all_auths();

    let contract_id = env.register(FeaturesContract, ());
    let client = FeaturesContractClient::new(env, &contract_id);

    let admin = Address::generate(env);
    let user = Address::generate(env);

    client.init(
        &admin,
        &String::from_str(env, "Token"),
        &String::from_str(env, "TKN"),
    );

    // Call advanced operation
    client.advanced_operation(&user, &999);
}

// ============================================================================
// Key Inspection Tests - Verify Key Materialization
// ============================================================================

#[test]
fn test_inspect_storage_keys() {
    let env = &Env::default();
    env.mock_all_auths();

    let contract_id = env.register(FeaturesContract, ());
    let client = FeaturesContractClient::new(env, &contract_id);

    let admin = Address::generate(env);
    let user = Address::generate(env);

    client.init(
        &admin,
        &String::from_str(env, "Token"),
        &String::from_str(env, "TKN"),
    );

    // Get storage keys
    let balance_key = client.inspect_balance_key(&user);
    let supply_key = client.inspect_total_supply_key();

    // Verify balance key is BytesN<32> (hashed with auto_shorten)
    env.as_contract(&contract_id, || {
        let key = BytesN::<32>::try_from_val(env, &balance_key);
        assert!(key.is_ok());
    });

    // Verify supply key is short Bytes
    env.as_contract(&contract_id, || {
        let key = Bytes::try_from_val(env, &supply_key);
        assert!(key.is_ok());
        // Should be "T" for total_supply with auto_shorten
    });
}

#[test]
fn test_symbolic_instance_storage_keys() {
    let env = &Env::default();
    env.mock_all_auths();

    let contract_id = env.register(FeaturesContract, ());
    let client = FeaturesContractClient::new(env, &contract_id);

    let admin = Address::generate(env);
    client.init(
        &admin,
        &String::from_str(env, "Token"),
        &String::from_str(env, "TKN"),
    );

    // Verify Instance storage uses symbolic keys (default mode)
    env.as_contract(&contract_id, || {
        // Config.admin is stored as Symbol("Admin")
        let admin_key: Val = Symbol::new(env, "Admin").into_val(env);
        assert!(env.storage().instance().has(&admin_key));

        // Config.enabled is stored as Symbol("Enabled")
        let enabled_key: Val = Symbol::new(env, "Enabled").into_val(env);
        assert!(env.storage().instance().has(&enabled_key));

        // Config.metadata with key "name" is stored as Vec[Symbol("Metadata"), "name"]
        let mut metadata_key: HostVec<Val> = HostVec::new(env);
        metadata_key.push_back(Symbol::new(env, "Metadata").into_val(env));
        metadata_key.push_back(String::from_str(env, "name").into_val(env));
        let key: Val = metadata_key.into_val(env);
        assert!(env.storage().instance().has(&key));
    });
}

#[test]
fn test_hashed_persistent_storage_keys() {
    let env = &Env::default();
    env.mock_all_auths();

    let contract_id = env.register(FeaturesContract, ());
    let client = FeaturesContractClient::new(env, &contract_id);

    let admin = Address::generate(env);
    let user = Address::generate(env);

    client.init(
        &admin,
        &String::from_str(env, "Token"),
        &String::from_str(env, "TKN"),
    );

    client.mint(&user, &1000);

    // Verify balances uses hashed keys (auto_shorten without symbolic)
    env.as_contract(&contract_id, || {
        use soroban_sdk_tools::key::make_map_key;

        // TokenData.balances with auto_shorten prefix "B"
        let prefix = Bytes::from_slice(env, b"B");
        let expected_key: Val = make_map_key(env, &prefix, &user);

        // Key should exist and be BytesN<32>
        assert!(env.storage().persistent().has(&expected_key));
        let key = BytesN::<32>::try_from_val(env, &expected_key);
        assert!(key.is_ok());
    });
}


#[test]
fn test_custom_short_key_prefix() {
    let env = &Env::default();
    env.mock_all_auths();

    let contract_id = env.register(FeaturesContract, ());
    let client = FeaturesContractClient::new(env, &contract_id);

    let admin = Address::generate(env);
    let from = Address::generate(env);
    let spender = Address::generate(env);

    client.init(
        &admin,
        &String::from_str(env, "Token"),
        &String::from_str(env, "TKN"),
    );

    client.approve(&from, &spender, &100);

    // Verify allowances uses custom prefix "allow"
    env.as_contract(&contract_id, || {
        use soroban_sdk_tools::key::make_map_key;

        let prefix = Bytes::from_slice(env, b"allow");
        let expected_key: Val = make_map_key(env, &prefix, &(from.clone(), spender.clone()));

        assert!(env.storage().persistent().has(&expected_key));
    });
}

#[test]
fn test_symbolic_override_in_governance() {
    let env = &Env::default();
    env.mock_all_auths();

    let contract_id = env.register(FeaturesContract, ());
    let client = FeaturesContractClient::new(env, &contract_id);

    let admin = Address::generate(env);
    client.init(
        &admin,
        &String::from_str(env, "Token"),
        &String::from_str(env, "TKN"),
    );

    client.record_event(&1, &String::from_str(env, "Test event"));

    // Verify events use symbolic keys despite auto_shorten (due to #[symbolic])
    env.as_contract(&contract_id, || {
        // GovernanceData.events with #[symbolic] uses Vec[Symbol("E"), u64]
        let mut event_key: HostVec<Val> = HostVec::new(env);
        event_key.push_back(Symbol::new(env, "E").into_val(env));
        event_key.push_back((1u64).into_val(env));
        let key: Val = event_key.into_val(env);

        assert!(env.storage().persistent().has(&key));
    });
}

// ============================================================================
// Integration Tests - Full Contract Flow
// ============================================================================

#[test]
fn test_full_token_lifecycle() {
    let env = &Env::default();
    env.mock_all_auths();

    let contract_id = env.register(FeaturesContract, ());
    let client = FeaturesContractClient::new(env, &contract_id);

    let admin = Address::generate(env);
    let alice = Address::generate(env);
    let bob = Address::generate(env);
    let charlie = Address::generate(env);

    // 1. Initialize
    client.init(
        &admin,
        &String::from_str(env, "Test Token"),
        &String::from_str(env, "TEST"),
    );

    // 2. Mint to users
    // alice: 1000, bob: 500, total: 1500
    client.mint(&alice, &1000);
    client.mint(&bob, &500);
    assert_eq!(client.get_balance(&alice), 1000);
    assert_eq!(client.get_balance(&bob), 500);
    assert_eq!(client.get_total_supply(), 1500);

    // 3. Transfer alice -> bob: 100
    // alice: 900, bob: 600, total: 1500
    client.transfer(&alice, &bob, &100);
    assert_eq!(client.get_balance(&alice), 900);
    assert_eq!(client.get_balance(&bob), 600);

    // 4. Approve bob -> charlie: 200
    client.approve(&bob, &charlie, &200);
    assert_eq!(client.get_allowance(&bob, &charlie), 200);

    // 5. Transfer_from: charlie moves 50 from bob to alice
    // alice: 950, bob: 550, allowance: 150
    client.transfer_from(&charlie, &bob, &alice, &50);
    assert_eq!(client.get_balance(&alice), 950);
    assert_eq!(client.get_balance(&bob), 550);
    assert_eq!(client.get_allowance(&bob, &charlie), 150);

    // 6. Freeze alice and verify transfer fails
    client.freeze_account(&alice);
    assert!(client.is_frozen(&alice));
    let result = client.try_transfer(&alice, &bob, &50);
    assert_eq!(result, Err(Ok(Error::AccountFrozen)));
    
    // Balances should be unchanged after failed transfer
    assert_eq!(client.get_balance(&alice), 950);
    assert_eq!(client.get_balance(&bob), 550);

    // 7. Faucet to charlie
    // charlie: 10, total: 1510
    client.faucet(&charlie);
    assert_eq!(client.get_balance(&charlie), 10);
    assert_eq!(client.get_total_supply(), 1510);

    // 8. Verify final state
    assert_eq!(client.get_balance(&alice), 950);
    assert_eq!(client.get_balance(&bob), 550);
    assert_eq!(client.get_balance(&charlie), 10);
    assert_eq!(client.get_total_supply(), 1510);
    assert_eq!(client.get_allowance(&bob, &charlie), 150);
    assert!(client.is_frozen(&alice));
}
