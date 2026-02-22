//! Integration tests for real (cryptographic) authorization.
//!
//! Tests Ed25519, Secp256k1, Secp256r1 key types using the Signer trait
//! and CallBuilder.sign() pattern.

use soroban_sdk::testutils::Address as _;
use soroban_sdk::{contract, contractimpl, Address, Env};
use soroban_sdk_tools::{Keypair, Secp256k1Keypair, Secp256r1Keypair, Signer};

// Import the token contract WASM
mod token {
    soroban_sdk_tools::contractimport!(file = "../target/stellar/soroban_auth_token.wasm");
}

// A simple contract that requires auth from a single address
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

// =============================================================================
// Ed25519 Tests
// =============================================================================

#[test]
fn test_real_auth_ed25519_transfer() {
    let env = Env::default();
    let contract_id = env.register(token::WASM, ());
    let client = token::Client::new(&env, &contract_id);
    let auth = token::AuthClient::new(&env, &contract_id);

    let alice = Keypair::random(&env);
    let bob = Address::generate(&env);

    // Setup: use mock_all_auths for admin operations
    env.mock_all_auths();
    client.initialize(&Address::generate(&env));
    client.mint(alice.address(), &1000);

    // Real auth transfer using .sign()
    auth.transfer(alice.address(), &bob, &300)
        .sign(&alice)
        .invoke();

    assert_eq!(client.balance(alice.address()), 700);
    assert_eq!(client.balance(&bob), 300);
}

#[test]
fn test_real_auth_ed25519_multi_signer() {
    let env = Env::default();
    let contract_id = env.register(SimpleContract, ());
    let client = SimpleContractClient::new(&env, &contract_id);

    let alice = Keypair::random(&env);
    let bob = Keypair::random(&env);

    soroban_sdk_tools::setup_real_auth(
        &env,
        &contract_id,
        "dual_action",
        (alice.address().clone(), bob.address().clone()),
        &[&alice, &bob],
    );

    client.dual_action(alice.address(), bob.address());
}

#[test]
#[should_panic]
fn test_real_auth_ed25519_wrong_signer_fails() {
    let env = Env::default();
    let contract_id = env.register(SimpleContract, ());
    let client = SimpleContractClient::new(&env, &contract_id);

    let alice = Keypair::random(&env);
    let wrong = Keypair::random(&env);

    // Sign with wrong keypair - should fail
    soroban_sdk_tools::setup_real_auth(
        &env,
        &contract_id,
        "action",
        (alice.address().clone(),),
        &[&wrong],
    );

    client.action(alice.address());
}

// =============================================================================
// Secp256k1 Tests
// =============================================================================

#[test]
fn test_real_auth_secp256k1_single() {
    let env = Env::default();
    let contract_id = env.register(SimpleContract, ());
    let client = SimpleContractClient::new(&env, &contract_id);

    let alice_k1 = Secp256k1Keypair::random(&env);

    soroban_sdk_tools::setup_real_auth(
        &env,
        &contract_id,
        "action",
        (alice_k1.address().clone(),),
        &[&alice_k1],
    );

    client.action(alice_k1.address());
}

#[test]
fn test_real_auth_secp256k1_transfer() {
    let env = Env::default();
    let contract_id = env.register(token::WASM, ());
    let client = token::Client::new(&env, &contract_id);
    let auth = token::AuthClient::new(&env, &contract_id);

    let alice_k1 = Secp256k1Keypair::random(&env);
    let bob = Address::generate(&env);

    env.mock_all_auths();
    client.initialize(&Address::generate(&env));
    client.mint(alice_k1.address(), &1000);

    auth.transfer(alice_k1.address(), &bob, &300)
        .sign(&alice_k1)
        .invoke();

    assert_eq!(client.balance(alice_k1.address()), 700);
    assert_eq!(client.balance(&bob), 300);
}

#[test]
#[should_panic]
fn test_real_auth_secp256k1_wrong_key_fails() {
    let env = Env::default();
    let contract_id = env.register(SimpleContract, ());
    let client = SimpleContractClient::new(&env, &contract_id);

    let alice_k1 = Secp256k1Keypair::random(&env);
    let wrong_k1 = Secp256k1Keypair::random(&env);

    // Sign with wrong key - should fail
    soroban_sdk_tools::setup_real_auth(
        &env,
        &contract_id,
        "action",
        (alice_k1.address().clone(),),
        &[&wrong_k1],
    );

    client.action(alice_k1.address());
}

// =============================================================================
// Secp256r1 Tests
// =============================================================================

#[test]
fn test_real_auth_secp256r1_single() {
    let env = Env::default();
    let contract_id = env.register(SimpleContract, ());
    let client = SimpleContractClient::new(&env, &contract_id);

    let alice_r1 = Secp256r1Keypair::random(&env);

    soroban_sdk_tools::setup_real_auth(
        &env,
        &contract_id,
        "action",
        (alice_r1.address().clone(),),
        &[&alice_r1],
    );

    client.action(alice_r1.address());
}

#[test]
fn test_real_auth_secp256r1_transfer() {
    let env = Env::default();
    let contract_id = env.register(token::WASM, ());
    let client = token::Client::new(&env, &contract_id);
    let auth = token::AuthClient::new(&env, &contract_id);

    let alice_r1 = Secp256r1Keypair::random(&env);
    let bob = Address::generate(&env);

    env.mock_all_auths();
    client.initialize(&Address::generate(&env));
    client.mint(alice_r1.address(), &1000);

    auth.transfer(alice_r1.address(), &bob, &300)
        .sign(&alice_r1)
        .invoke();

    assert_eq!(client.balance(alice_r1.address()), 700);
    assert_eq!(client.balance(&bob), 300);
}

#[test]
#[should_panic]
fn test_real_auth_secp256r1_wrong_key_fails() {
    let env = Env::default();
    let contract_id = env.register(SimpleContract, ());
    let client = SimpleContractClient::new(&env, &contract_id);

    let alice_r1 = Secp256r1Keypair::random(&env);
    let wrong_r1 = Secp256r1Keypair::random(&env);

    soroban_sdk_tools::setup_real_auth(
        &env,
        &contract_id,
        "action",
        (alice_r1.address().clone(),),
        &[&wrong_r1],
    );

    client.action(alice_r1.address());
}

// =============================================================================
// Cross-type Tests
// =============================================================================

#[test]
fn test_real_auth_mixed_signers() {
    let env = Env::default();
    let contract_id = env.register(SimpleContract, ());
    let client = SimpleContractClient::new(&env, &contract_id);

    let alice = Keypair::random(&env);
    let bob_k1 = Secp256k1Keypair::random(&env);

    // Both an Ed25519 and a Secp256k1 signer authorize the same call
    soroban_sdk_tools::setup_real_auth(
        &env,
        &contract_id,
        "dual_action",
        (alice.address().clone(), bob_k1.address().clone()),
        &[&alice, &bob_k1],
    );

    client.dual_action(alice.address(), bob_k1.address());
}

#[test]
fn test_setup_real_auth_standalone() {
    let env = Env::default();
    let contract_id = env.register(token::WASM, ());
    let client = token::Client::new(&env, &contract_id);

    let alice = Keypair::random(&env);
    let bob = Address::generate(&env);

    env.mock_all_auths();
    client.initialize(&Address::generate(&env));
    client.mint(alice.address(), &1000);

    // Direct function usage without CallBuilder
    soroban_sdk_tools::setup_real_auth(
        &env,
        &contract_id,
        "transfer",
        (alice.address().clone(), bob.clone(), 300_i128),
        &[&alice],
    );
    client.transfer(alice.address(), &bob, &300);

    assert_eq!(client.balance(alice.address()), 700);
    assert_eq!(client.balance(&bob), 300);
}

#[test]
fn test_sign_payload_with_env_param() {
    let env = Env::default();
    let alice = Keypair::random(&env);
    let sig = alice.sign_payload(&env, &[0xAB; 32]);
    assert!(matches!(sig, soroban_sdk::xdr::ScVal::Vec(Some(_))));
}

#[test]
fn test_keypair_deterministic_from_seed() {
    let env = Env::default();
    let seed = [42u8; 32];

    // Ed25519
    let kp1 = Keypair::from_seed(&env, &seed);
    let kp2 = Keypair::from_seed(&env, &seed);
    assert_eq!(kp1.public_key_bytes(), kp2.public_key_bytes());

    // Secp256k1
    let k1a = Secp256k1Keypair::from_seed(&env, &seed);
    let k1b = Secp256k1Keypair::from_seed(&env, &seed);
    assert_eq!(k1a.public_key_bytes(), k1b.public_key_bytes());

    // Secp256r1
    let r1a = Secp256r1Keypair::from_seed(&env, &seed);
    let r1b = Secp256r1Keypair::from_seed(&env, &seed);
    assert_eq!(r1a.public_key_bytes(), r1b.public_key_bytes());
}
