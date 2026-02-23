//! Authorization testing utilities
//!
//! Provides utilities for simplified authorization testing by automatically
//! generating mock or real authorization entries from method invocation details.
//!
//! # Mock Auth Example
//!
//! ```ignore
//! use soroban_sdk_tools::contractimport;
//!
//! mod my_contract {
//!     soroban_sdk_tools::contractimport!(
//!         file = "path/to/contract.wasm"
//!     );
//! }
//!
//! #[test]
//! fn test_with_mock_auth() {
//!     let env = Env::default();
//!     let contract_id = env.register(my_contract::WASM, ());
//!     let client = my_contract::AuthClient::new(&env, &contract_id);
//!     let user = Address::generate(&env);
//!
//!     // Builder pattern: method -> authorize -> invoke
//!     client.my_method(&user, &5).authorize(&user).invoke();
//! }
//! ```
//!
//! # Real Auth Example
//!
//! ```ignore
//! use soroban_sdk_tools::{Keypair, Secp256k1Keypair, Signer};
//!
//! #[test]
//! fn test_with_real_auth() {
//!     let env = Env::default();
//!     let contract_id = env.register(my_contract::WASM, ());
//!     let client = my_contract::AuthClient::new(&env, &contract_id);
//!
//!     let alice = Keypair::random(&env);
//!     client.transfer(alice.address(), &bob, &300).sign(&alice).invoke();
//! }
//! ```

pub mod builder;
pub mod signers;

pub use builder::{setup_mock_auth, setup_real_auth, CallBuilder};
pub use signers::{Keypair, Secp256k1Keypair, Secp256r1Keypair, Signer};
