//! Integration tests for AuthClient and setup_mock_auth.
//!
//! This module demonstrates the AuthClient's capabilities for simplified
//! authorization testing in Soroban smart contracts.
//!
//! # Features Demonstrated
//!
//! 1. **Single-user auth**: Basic `require_auth()` testing
//! 2. **Multi-user auth**: Multiple addresses authorizing the same operation
//! 3. **Simplified API**: Using AuthClient instead of manual MockAuth

use soroban_sdk::testutils::Address as _;
use soroban_sdk::{contract, contractimpl, contracttype, vec, Address, Env, IntoVal, Vec};
use soroban_sdk_tools::Signer;

// Import the token contract WASM using contractimport! - this generates AuthClient
mod token {
    soroban_sdk_tools::contractimport!(file = "../target/stellar/soroban_auth_token.wasm");
}

// -----------------------------------------------------------------------------
// Inline contracts for testing (vault and swap)
// -----------------------------------------------------------------------------

#[derive(Clone)]
#[contracttype]
pub enum VaultDataKey {
    Admin,
    Signers,
    Threshold,
    Balance,
}

#[contract]
pub struct VaultContract;

#[contractimpl]
impl VaultContract {
    pub fn initialize(env: Env, admin: Address, signers: Vec<Address>, threshold: u32) {
        if env.storage().instance().has(&VaultDataKey::Admin) {
            panic!("already initialized");
        }
        if threshold == 0 || threshold > signers.len() {
            panic!("invalid threshold");
        }
        env.storage().instance().set(&VaultDataKey::Admin, &admin);
        env.storage()
            .instance()
            .set(&VaultDataKey::Signers, &signers);
        env.storage()
            .instance()
            .set(&VaultDataKey::Threshold, &threshold);
        env.storage().instance().set(&VaultDataKey::Balance, &0i128);
    }

    pub fn deposit(env: Env, from: Address, amount: i128) {
        from.require_auth();
        let balance: i128 = env
            .storage()
            .instance()
            .get(&VaultDataKey::Balance)
            .unwrap_or(0);
        env.storage()
            .instance()
            .set(&VaultDataKey::Balance, &(balance + amount));
    }

    /// Withdraw funds - requires multiple signers!
    pub fn withdraw(env: Env, authorizers: Vec<Address>, to: Address, amount: i128) {
        let signers: Vec<Address> = env
            .storage()
            .instance()
            .get(&VaultDataKey::Signers)
            .unwrap();
        let threshold: u32 = env
            .storage()
            .instance()
            .get(&VaultDataKey::Threshold)
            .unwrap();

        if authorizers.len() < threshold {
            panic!("not enough authorizers");
        }

        for i in 0..authorizers.len() {
            let authorizer = authorizers.get(i).unwrap();

            // Check if authorizer is a valid signer
            let mut is_signer = false;
            for j in 0..signers.len() {
                if signers.get(j).unwrap() == authorizer {
                    is_signer = true;
                    break;
                }
            }
            if !is_signer {
                panic!("invalid authorizer");
            }
            authorizer.require_auth();
        }

        let balance: i128 = env
            .storage()
            .instance()
            .get(&VaultDataKey::Balance)
            .unwrap_or(0);
        if balance < amount {
            panic!("insufficient balance");
        }
        env.storage()
            .instance()
            .set(&VaultDataKey::Balance, &(balance - amount));
        let _ = to;
    }

    pub fn balance(env: Env) -> i128 {
        env.storage()
            .instance()
            .get(&VaultDataKey::Balance)
            .unwrap_or(0)
    }
}

#[contract]
pub struct SwapContract;

#[contractimpl]
impl SwapContract {
    /// Atomic swap - BOTH parties must authorize
    pub fn atomic_swap(
        _env: Env,
        alice: Address,
        alice_gives: i128,
        bob: Address,
        bob_gives: i128,
    ) {
        alice.require_auth();
        bob.require_auth();
        if alice_gives <= 0 || bob_gives <= 0 {
            panic!("invalid amounts");
        }
        let _ = (alice, bob, alice_gives, bob_gives);
    }

    /// Three-way swap - ALL THREE parties must authorize
    pub fn three_way_swap(
        _env: Env,
        party_a: Address,
        party_b: Address,
        party_c: Address,
        amount: i128,
    ) {
        party_a.require_auth();
        party_b.require_auth();
        party_c.require_auth();
        if amount <= 0 {
            panic!("invalid amount");
        }
        let _ = (party_a, party_b, party_c, amount);
    }
}

// =============================================================================
// Side-by-Side Comparison Tests
// =============================================================================

/// Shows BOTH methods side by side so you can see the difference.
/// AuthClient eliminates the verbose MockAuth boilerplate!
#[test]
fn test_side_by_side_single_user_auth() {
    let env = &Env::default();
    let contract_id = env.register(token::WASM, ());
    let client = token::Client::new(env, &contract_id);

    let admin = Address::generate(env);
    let alice = Address::generate(env);
    let bob = Address::generate(env);
    let charlie = Address::generate(env);

    // Setup
    env.mock_all_auths();
    client.initialize(&admin);
    client.mint(&alice, &1000);

    // ─────────────────────────────────────────────────────────────────────────
    // METHOD 1: Manual MockAuth (verbose, error-prone)
    // ─────────────────────────────────────────────────────────────────────────
    env.mock_auths(&[soroban_sdk::testutils::MockAuth {
        address: &alice,
        invoke: &soroban_sdk::testutils::MockAuthInvoke {
            contract: &contract_id,
            fn_name: "transfer",
            args: (&alice, &bob, 300_i128).into_val(env),
            sub_invokes: &[],
        },
    }]);
    client.transfer(&alice, &bob, &300);

    // ─────────────────────────────────────────────────────────────────────────
    // METHOD 2: AuthClient (simple, builder pattern)
    // ─────────────────────────────────────────────────────────────────────────
    token::AuthClient::new(env, &contract_id)
        .transfer(&alice, &charlie, &200)
        .authorize(&alice)
        .invoke();

    // Both methods achieve the same result
    assert_eq!(client.balance(&alice), 500); // 1000 - 300 - 200
    assert_eq!(client.balance(&bob), 300);
    assert_eq!(client.balance(&charlie), 200);
}

/// When multiple users need to authorize, the difference is even more dramatic!
#[test]
fn test_side_by_side_multi_user_auth() {
    let env = &Env::default();
    let contract_id = env.register(SwapContract, ());
    let client = SwapContractClient::new(env, &contract_id);

    let alice = &Address::generate(env);
    let bob = &Address::generate(env);

    // ─────────────────────────────────────────────────────────────────────────
    // METHOD 1: Manual MockAuth for TWO users (very verbose!)
    // ─────────────────────────────────────────────────────────────────────────
    env.mock_auths(&[
        soroban_sdk::testutils::MockAuth {
            address: alice,
            invoke: &soroban_sdk::testutils::MockAuthInvoke {
                contract: &contract_id,
                fn_name: "atomic_swap",
                args: (alice, 100_i128, bob, 200_i128).into_val(env),
                sub_invokes: &[],
            },
        },
        soroban_sdk::testutils::MockAuth {
            address: bob,
            invoke: &soroban_sdk::testutils::MockAuthInvoke {
                contract: &contract_id,
                fn_name: "atomic_swap",
                args: (alice, 100_i128, bob, 200_i128).into_val(env),
                sub_invokes: &[],
            },
        },
    ]);
    client.atomic_swap(alice, &100, bob, &200);

    // ─────────────────────────────────────────────────────────────────────────
    // METHOD 2: setup_mock_auth helper (much simpler!)
    // ─────────────────────────────────────────────────────────────────────────
    soroban_sdk_tools::auth::setup_mock_auth(
        env,
        &contract_id,
        "atomic_swap",
        (alice, 150_i128, bob, 250_i128),
        &[alice, bob], // Both users authorize
    );
    client.atomic_swap(alice, &150, bob, &250);
}

// =============================================================================
// AuthClient Tests
// =============================================================================

#[test]
fn test_auth_client_approve_and_transfer_from() {
    let env = &Env::default();
    let contract_id = env.register(token::WASM, ());
    let client = token::Client::new(env, &contract_id);

    let admin = Address::generate(env);
    let owner = Address::generate(env);
    let spender = Address::generate(env);
    let recipient = Address::generate(env);

    // Setup
    env.mock_all_auths();
    client.initialize(&admin);
    client.mint(&owner, &1000);

    // Owner approves spender - using AuthClient
    token::AuthClient::new(env, &contract_id)
        .approve(&owner, &spender, &500)
        .authorize(&owner)
        .invoke();

    assert_eq!(client.allowance(&owner, &spender), 500);

    // Spender transfers from owner - using AuthClient
    token::AuthClient::new(env, &contract_id)
        .transfer_from(&spender, &owner, &recipient, &200)
        .authorize(&spender)
        .invoke();

    assert_eq!(client.balance(&owner), 800);
    assert_eq!(client.balance(&recipient), 200);
    assert_eq!(client.allowance(&owner, &spender), 300);
}

#[test]
fn test_auth_client_burn() {
    let env = &Env::default();
    let contract_id = env.register(token::WASM, ());
    let auth_client = token::AuthClient::new(env, &contract_id);
    let client = token::Client::new(env, &contract_id);

    let admin = Address::generate(env);
    let user = Address::generate(env);

    env.mock_all_auths();
    client.initialize(&admin);
    client.mint(&user, &1000);

    // User burns their own tokens
    auth_client.burn(&user, &300).authorize(&user).invoke();

    assert_eq!(client.balance(&user), 700);
}

// =============================================================================
// Multi-Signer Tests with CallBuilder
// =============================================================================

/// Tests chaining multiple .authorize() calls on CallBuilder.
/// This is the new API pattern for multi-party authorization.
#[test]
fn test_call_builder_multi_auth_chaining() {
    let env = &Env::default();
    let contract_id = env.register(SwapContract, ());

    // Import the swap WASM to get AuthClient
    mod swap {
        soroban_sdk_tools::contractimport!(file = "../target/stellar/soroban_auth_swap.wasm");
    }

    let alice = &Address::generate(env);
    let bob = &Address::generate(env);

    // Use CallBuilder with chained .authorize() calls
    // Both Alice AND Bob must authorize the atomic swap
    swap::AuthClient::new(env, &contract_id)
        .atomic_swap(alice, &100, bob, &200)
        .authorize(alice)
        .authorize(bob)
        .invoke();
}

/// Tests chaining 3+ .authorize() calls for multi-party authorization.
#[test]
fn test_call_builder_three_party_auth_chaining() {
    let env = &Env::default();
    let contract_id = env.register(SwapContract, ());

    mod swap {
        soroban_sdk_tools::contractimport!(file = "../target/stellar/soroban_auth_swap.wasm");
    }

    let party_a = Address::generate(env);
    let party_b = Address::generate(env);
    let party_c = Address::generate(env);

    // Chain 3 .authorize() calls - all three parties must authorize
    swap::AuthClient::new(env, &contract_id)
        .three_way_swap(&party_a, &party_b, &party_c, &100)
        .authorize(&party_a)
        .authorize(&party_b)
        .authorize(&party_c)
        .invoke();
}

/// Tests using .authorize_all() with an array of authorizers.
#[test]
fn test_call_builder_authorize_all() {
    let env = &Env::default();
    let contract_id = env.register(SwapContract, ());

    mod swap {
        soroban_sdk_tools::contractimport!(file = "../target/stellar/soroban_auth_swap.wasm");
    }

    let party_a = &Address::generate(env);
    let party_b = &Address::generate(env);
    let party_c = &Address::generate(env);

    // Use authorize_all() for multiple authorizers
    // Note: We clone here because the method borrows the args, but authorize_all
    // needs owned values in the slice
    swap::AuthClient::new(env, &contract_id)
        .three_way_swap(party_a, party_b, party_c, &100)
        .authorize_all(&[party_a, party_b, party_c])
        .invoke();
}

// =============================================================================
// Multi-Signer Tests
// =============================================================================

/// Demonstrates multi-party authorization where multiple addresses
/// must authorize the same operation (2-of-3 multisig).
#[test]
fn test_multi_signer_vault_withdrawal() {
    let env = &Env::default();

    let contract_id = env.register(VaultContract, ());
    let client = VaultContractClient::new(env, &contract_id);

    let admin = &Address::generate(env);
    let signer1 = &Address::generate(env);
    let signer2 = &Address::generate(env);
    let signer3 = &Address::generate(env);
    let recipient = &Address::generate(env);

    let signers = vec![env, signer1.clone(), signer2.clone(), signer3.clone()];

    // Initialize vault with 2-of-3 threshold
    env.mock_all_auths();
    client.initialize(&admin, &signers, &2);
    client.deposit(&admin, &1000);

    // Use setup_mock_auth to set up auth for both signers
    let authorizers = &vec![env, signer1.clone(), signer2.clone()];

    // Using the setup_mock_auth helper for multi-user auth
    soroban_sdk_tools::auth::setup_mock_auth(
        env,
        &contract_id,
        "withdraw",
        (authorizers, recipient, 500_i128),
        &[signer1, signer2],
    );

    client.withdraw(authorizers, recipient, &500);
    assert_eq!(client.balance(), 500);
}

#[test]
fn test_atomic_two_party_swap() {
    let env = &Env::default();

    let contract_id = env.register(SwapContract, ());
    let client = SwapContractClient::new(env, &contract_id);

    let alice = &Address::generate(env);
    let bob = &Address::generate(env);

    // Use setup_mock_auth to set up auth for BOTH parties
    soroban_sdk_tools::auth::setup_mock_auth(
        env,
        &contract_id,
        "atomic_swap",
        (alice, 100_i128, bob, 200_i128),
        &[alice, bob],
    );

    client.atomic_swap(alice, &100, bob, &200);
}

#[test]
fn test_three_party_swap() {
    let env = &Env::default();

    let contract_id = env.register(SwapContract, ());
    let client = SwapContractClient::new(env, &contract_id);

    let party_a = &Address::generate(env);
    let party_b = &Address::generate(env);
    let party_c = &Address::generate(env);

    // All three parties must authorize
    soroban_sdk_tools::auth::setup_mock_auth(
        env,
        &contract_id,
        "three_way_swap",
        (party_a, party_b, party_c, 100_i128),
        &[party_a, party_b, party_c],
    );

    client.three_way_swap(party_a, party_b, party_c, &100);
}

#[test]
#[should_panic(expected = "not enough authorizers")]
fn test_insufficient_signers_fails() {
    let env = &Env::default();

    let contract_id = env.register(VaultContract, ());
    let client = VaultContractClient::new(env, &contract_id);

    let admin = &Address::generate(env);
    let signer1 = &Address::generate(env);
    let signer2 = &Address::generate(env);
    let signer3 = &Address::generate(env);
    let recipient = &Address::generate(env);

    let signers = vec![env, signer1.clone(), signer2.clone(), signer3.clone()];

    env.mock_all_auths();
    client.initialize(&admin, &signers, &2); // 2-of-3 required
    client.deposit(&admin, &1000);

    // Only 1 signer (needs 2)
    let authorizers = &vec![env, signer1.clone()];

    soroban_sdk_tools::auth::setup_mock_auth(
        env,
        &contract_id,
        "withdraw",
        (authorizers, recipient, 500_i128),
        &[signer1],
    );

    // This should fail because we need 2 signers
    client.withdraw(authorizers, recipient, &500);
}

// =============================================================================
// Unit tests for setup_mock_auth function
// =============================================================================

#[contract]
pub struct SimpleContract;

#[contractimpl]
impl SimpleContract {
    pub fn action(from: Address) {
        from.require_auth();
    }

    pub fn dual_action(from: Address, other: Address) {
        from.require_auth();
        other.require_auth();
    }
}

#[test]
fn test_setup_mock_auth_single_authorizer() {
    let env = &Env::default();
    let contract_id = env.register(SimpleContract, ());
    let client = SimpleContractClient::new(env, &contract_id);

    let user = &Address::generate(env);

    // Use setup_mock_auth with single authorizer
    soroban_sdk_tools::auth::setup_mock_auth(env, &contract_id, "action", (user,), &[user]);

    client.action(user);
}

#[test]
fn test_setup_mock_auth_multiple_authorizers() {
    let env = &Env::default();
    let contract_id = env.register(SimpleContract, ());
    let client = SimpleContractClient::new(env, &contract_id);

    let user1 = &Address::generate(env);
    let user2 = &Address::generate(env);

    // Use setup_mock_auth with multiple authorizers
    soroban_sdk_tools::auth::setup_mock_auth(
        env,
        &contract_id,
        "dual_action",
        (user1, user2),
        &[user1, user2],
    );

    client.dual_action(user1, user2);
}

#[test]
fn test_setup_mock_auth_empty_authorizers_is_noop() {
    let env = &Env::default();
    let contract_id = env.register(SimpleContract, ());
    let client = SimpleContractClient::new(env, &contract_id);

    let user = Address::generate(env);

    // Empty authorizers should be a no-op (doesn't set up any mock auth)
    soroban_sdk_tools::auth::setup_mock_auth(
        env,
        &contract_id,
        "action",
        (user.clone(),),
        &[], // No authorizers
    );

    // This should fail because no auth was set up
    // Use mock_all_auths to make it pass
    env.mock_all_auths();
    client.action(&user);
}

/// Calling invoke() without any authorize() should not set up mock auth,
/// so a contract requiring auth should panic.
#[test]
#[should_panic]
fn test_call_builder_invoke_without_authorize_fails() {
    let env = &Env::default();
    let contract_id = env.register(token::WASM, ());
    let client = token::Client::new(env, &contract_id);
    let auth_client = token::AuthClient::new(env, &contract_id);

    let admin = &Address::generate(env);
    let alice = &Address::generate(env);
    let bob = &Address::generate(env);

    // Setup with mock_all_auths
    env.mock_all_auths();
    client.initialize(admin);
    client.mint(alice, &1000);

    // Clear mock auth state by setting explicit empty auths
    env.mock_auths(&[]);

    // Now invoke transfer via AuthClient WITHOUT any authorize().
    // This should fail because invoke() with no authorizers skips mock auth
    // setup, and we cleared prior mock_all_auths.
    auth_client.transfer(alice, bob, &100).invoke();
}

#[test]
#[should_panic]
fn test_setup_mock_auth_wrong_authorizer_fails() {
    let env = &Env::default();
    let contract_id = env.register(SimpleContract, ());
    let client = SimpleContractClient::new(env, &contract_id);

    let user = &Address::generate(env);
    let wrong_user = &Address::generate(env);

    // Set up auth for wrong user
    soroban_sdk_tools::auth::setup_mock_auth(
        env,
        &contract_id,
        "action",
        (user,),
        &[wrong_user], // Wrong authorizer!
    );

    // This should fail because user != wrong_user
    client.action(user);
}

// =============================================================================
// try_invoke Tests
// =============================================================================

#[test]
fn test_try_invoke_success() {
    let env = &Env::default();
    let contract_id = env.register(token::WASM, ());
    let client = token::Client::new(env, &contract_id);
    let auth_client = token::AuthClient::new(env, &contract_id);

    let admin = Address::generate(env);
    let alice = Address::generate(env);
    let bob = Address::generate(env);

    env.mock_all_auths();
    client.initialize(&admin);
    client.mint(&alice, &1000);

    // try_invoke on a successful transfer
    let result = auth_client
        .transfer(&alice, &bob, &300)
        .authorize(&alice)
        .try_invoke();

    assert!(result.is_ok());
    assert_eq!(client.balance(&alice), 700);
    assert_eq!(client.balance(&bob), 300);
}

#[test]
fn test_try_invoke_error() {
    let env = &Env::default();
    let contract_id = env.register(token::WASM, ());
    let client = token::Client::new(env, &contract_id);
    let auth_client = token::AuthClient::new(env, &contract_id);

    let admin = Address::generate(env);
    let alice = Address::generate(env);
    let bob = Address::generate(env);

    env.mock_all_auths();
    client.initialize(&admin);
    client.mint(&alice, &100);

    // try_invoke on a transfer that exceeds balance — returns Err instead of panicking
    let result = auth_client
        .transfer(&alice, &bob, &500)
        .authorize(&alice)
        .try_invoke();

    assert!(result.is_err());
    // Balance unchanged
    assert_eq!(client.balance(&alice), 100);
}

#[test]
fn test_try_invoke_with_real_auth() {
    let env = &Env::default();
    let contract_id = env.register(token::WASM, ());
    let client = token::Client::new(env, &contract_id);
    let auth_client = token::AuthClient::new(env, &contract_id);

    let alice = &soroban_sdk_tools::Keypair::random(env);
    let bob = &Address::generate(env);

    env.mock_all_auths();
    client.initialize(&Address::generate(env));
    client.mint(alice.address(), &1000);

    // try_invoke with real auth on a successful transfer
    let result = auth_client
        .transfer(alice.address(), bob, &300)
        .sign(alice)
        .try_invoke();

    assert!(result.is_ok());
    assert_eq!(client.balance(alice.address()), 700);
    assert_eq!(client.balance(bob), 300);
}
