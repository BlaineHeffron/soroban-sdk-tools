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

extern crate alloc;

extern crate std;

use alloc::{boxed::Box, rc::Rc, vec::Vec as StdVec};

use soroban_sdk::{
    testutils::{MockAuth, MockAuthInvoke},
    Address, Env, IntoVal, TryFromVal, Val, Vec,
};

use soroban_sdk::xdr::{
    self, HashIdPreimage, HashIdPreimageSorobanAuthorization, InvokeContractArgs, LedgerKeyAccount,
    Limited, Limits, ScAddress, ScSymbol, ScVal, SorobanAddressCredentials,
    SorobanAuthorizationEntry, SorobanAuthorizedFunction, SorobanAuthorizedInvocation,
    SorobanCredentials, WriteXdr,
};

// ---------------------------------------------------------------------------
// Signer trait
// ---------------------------------------------------------------------------

/// A signer that can produce real cryptographic signatures for Soroban auth.
pub trait Signer {
    /// The Soroban address controlled by this signer.
    fn address(&self) -> &Address;
    /// Sign an auth payload hash (32 bytes), returning the ScVal for
    /// `SorobanAddressCredentials.signature`.
    fn sign_payload(&self, env: &Env, payload: &[u8]) -> ScVal;
}

// ===========================================================================
// Ed25519 Keypair (native Stellar account)
// ===========================================================================

/// An Ed25519 keypair that registers as a native Stellar account.
///
/// The host's built-in `check_account_authentication` verifies the signature.
pub struct Keypair {
    signing_key: ed25519_dalek::SigningKey,
    address: Address,
}

impl Keypair {
    /// Generate a random Ed25519 keypair and register the account on the ledger.
    pub fn random(env: &Env) -> Self {
        let mut rng = rand::thread_rng();
        let signing_key = ed25519_dalek::SigningKey::generate(&mut rng);
        Self::from_signing_key(env, signing_key)
    }

    /// Create an Ed25519 keypair from a 32-byte seed and register the account.
    pub fn from_seed(env: &Env, seed: &[u8; 32]) -> Self {
        let signing_key = ed25519_dalek::SigningKey::from_bytes(seed);
        Self::from_signing_key(env, signing_key)
    }

    fn from_signing_key(env: &Env, signing_key: ed25519_dalek::SigningKey) -> Self {
        let pk_bytes = signing_key.verifying_key().to_bytes();
        let account_id =
            xdr::AccountId(xdr::PublicKey::PublicKeyTypeEd25519(xdr::Uint256(pk_bytes)));

        // Build the ledger key directly (to_account_key is pub(crate) on Host)
        let key = Rc::new(xdr::LedgerKey::Account(LedgerKeyAccount {
            account_id: account_id.clone(),
        }));

        let acc_entry = xdr::AccountEntry {
            account_id: account_id.clone(),
            balance: 100_000_000, // 10 XLM
            seq_num: xdr::SequenceNumber(0),
            num_sub_entries: 0,
            inflation_dest: None,
            flags: 0,
            home_domain: Default::default(),
            thresholds: xdr::Thresholds([1, 1, 1, 1]),
            signers: Default::default(),
            ext: xdr::AccountEntryExt::V0,
        };

        env.host()
            .add_ledger_entry(
                &key,
                &Rc::new(xdr::LedgerEntry {
                    last_modified_ledger_seq: 0,
                    data: xdr::LedgerEntryData::Account(acc_entry),
                    ext: xdr::LedgerEntryExt::V0,
                }),
                None,
            )
            .unwrap();

        let address =
            Address::try_from_val(env, &ScVal::Address(ScAddress::Account(account_id))).unwrap();

        Self {
            signing_key,
            address,
        }
    }

    /// The 32-byte Ed25519 public key.
    pub fn public_key_bytes(&self) -> [u8; 32] {
        self.signing_key.verifying_key().to_bytes()
    }
}

impl Signer for Keypair {
    fn address(&self) -> &Address {
        &self.address
    }

    fn sign_payload(&self, _env: &Env, payload: &[u8]) -> ScVal {
        use ed25519_dalek::Signer as DalekSigner;
        let signature = self.signing_key.sign(payload);
        let pk_bytes = self.signing_key.verifying_key().to_bytes();

        // The host expects a Vec<AccountEd25519Signature> where each element is
        // a Map with keys "public_key" (Bytes<32>) and "signature" (Bytes<64>).
        ScVal::Vec(Some(
            alloc::vec![ScVal::Map(Some(
                alloc::vec![
                    xdr::ScMapEntry {
                        key: ScVal::Symbol(xdr::ScSymbol("public_key".try_into().unwrap())),
                        val: ScVal::Bytes(xdr::ScBytes(pk_bytes.try_into().unwrap())),
                    },
                    xdr::ScMapEntry {
                        key: ScVal::Symbol(xdr::ScSymbol("signature".try_into().unwrap())),
                        val: ScVal::Bytes(xdr::ScBytes(
                            signature.to_bytes().to_vec().try_into().unwrap(),
                        )),
                    },
                ]
                .try_into()
                .unwrap()
            ))]
            .try_into()
            .unwrap(),
        ))
    }
}

// ===========================================================================
// Secp256k1 custom account contract
// ===========================================================================

mod secp256k1_account {
    use soroban_sdk::{auth, contract, contractimpl, contracttype, BytesN, Env, Vec};

    #[derive(Clone)]
    #[contracttype]
    pub(crate) enum DataKey {
        PublicKey,
    }

    #[contract]
    pub struct Secp256k1AccountContract;

    #[contractimpl]
    impl Secp256k1AccountContract {
        #[allow(non_snake_case)]
        pub fn __check_auth(
            env: Env,
            signature_payload: soroban_sdk::crypto::Hash<32>,
            signature: soroban_sdk::Bytes,
            _auth_context: Vec<auth::Context>,
        ) {
            // signature is 65 bytes: 64-byte sig + 1-byte recovery_id
            assert!(
                signature.len() == 65,
                "secp256k1 signature must be 65 bytes"
            );

            let mut sig_bytes = [0u8; 64];
            for i in 0..64u32 {
                sig_bytes[i as usize] = signature.get(i).unwrap();
            }
            let recovery_id = signature.get(64).unwrap() as u32;

            let sig_fixed = BytesN::from_array(&env, &sig_bytes);
            let recovered =
                env.crypto()
                    .secp256k1_recover(&signature_payload, &sig_fixed, recovery_id);

            let stored_pk: BytesN<65> = env.storage().instance().get(&DataKey::PublicKey).unwrap();

            assert!(recovered == stored_pk, "secp256k1 key mismatch");
        }
    }
}

/// A secp256k1 keypair that registers as a custom account contract.
pub struct Secp256k1Keypair {
    signing_key: k256::ecdsa::SigningKey,
    address: Address,
}

impl Secp256k1Keypair {
    /// Generate a random secp256k1 keypair and register the custom account contract.
    pub fn random(env: &Env) -> Self {
        let mut rng = rand::thread_rng();
        let signing_key = k256::ecdsa::SigningKey::random(&mut rng);
        Self::from_signing_key(env, signing_key)
    }

    /// Create a secp256k1 keypair from a 32-byte seed.
    pub fn from_seed(env: &Env, seed: &[u8; 32]) -> Self {
        let signing_key = k256::ecdsa::SigningKey::from_bytes(seed.into()).unwrap();
        Self::from_signing_key(env, signing_key)
    }

    fn from_signing_key(env: &Env, signing_key: k256::ecdsa::SigningKey) -> Self {
        let contract_addr = env.register(secp256k1_account::Secp256k1AccountContract, ());

        // Store the public key in the contract's instance storage
        let pk_point = signing_key.verifying_key().to_encoded_point(false);
        let pk_bytes: [u8; 65] = pk_point.as_bytes().try_into().unwrap();

        env.as_contract(&contract_addr, || {
            env.storage().instance().set(
                &secp256k1_account::DataKey::PublicKey,
                &soroban_sdk::BytesN::<65>::from_array(env, &pk_bytes),
            );
        });

        Self {
            signing_key,
            address: contract_addr,
        }
    }

    /// The SEC-1 uncompressed 65-byte public key.
    pub fn public_key_bytes(&self) -> [u8; 65] {
        let pk_point = self.signing_key.verifying_key().to_encoded_point(false);
        pk_point.as_bytes().try_into().unwrap()
    }
}

impl Signer for Secp256k1Keypair {
    fn address(&self) -> &Address {
        &self.address
    }

    fn sign_payload(&self, _env: &Env, payload: &[u8]) -> ScVal {
        use k256::ecdsa::{RecoveryId, Signature};
        let (signature, recovery_id): (Signature, RecoveryId) =
            <k256::ecdsa::SigningKey as k256::ecdsa::signature::hazmat::PrehashSigner<_>>::sign_prehash(&self.signing_key, payload).unwrap();
        // Normalize to low-S (required by Soroban host's ecdsa_signature_from_bytes).
        // When S is negated the recovery ID must be flipped.
        let (signature, recovery_id) = if let Some(normalized) = signature.normalize_s() {
            let flipped = RecoveryId::from_byte(recovery_id.to_byte() ^ 1).unwrap();
            (normalized, flipped)
        } else {
            (signature, recovery_id)
        };
        let sig_bytes: [u8; 64] = signature.to_bytes().into();
        let rid: u8 = recovery_id.to_byte();

        let mut combined = [0u8; 65];
        combined[..64].copy_from_slice(&sig_bytes);
        combined[64] = rid;

        ScVal::Bytes(xdr::ScBytes(combined.try_into().unwrap()))
    }
}

// ===========================================================================
// Secp256r1 (P-256) custom account contract
// ===========================================================================

mod secp256r1_account {
    use soroban_sdk::{auth, contract, contractimpl, contracttype, BytesN, Env, Vec};

    #[derive(Clone)]
    #[contracttype]
    pub(crate) enum DataKey {
        PublicKey,
    }

    #[contract]
    pub struct Secp256r1AccountContract;

    #[contractimpl]
    impl Secp256r1AccountContract {
        #[allow(non_snake_case)]
        pub fn __check_auth(
            env: Env,
            signature_payload: soroban_sdk::crypto::Hash<32>,
            signature: BytesN<64>,
            _auth_context: Vec<auth::Context>,
        ) {
            let stored_pk: BytesN<65> = env.storage().instance().get(&DataKey::PublicKey).unwrap();

            // secp256r1_verify panics on failure
            env.crypto()
                .secp256r1_verify(&stored_pk, &signature_payload, &signature);
        }
    }
}

/// A secp256r1 (NIST P-256) keypair that registers as a custom account contract.
pub struct Secp256r1Keypair {
    signing_key: p256::ecdsa::SigningKey,
    address: Address,
}

impl Secp256r1Keypair {
    /// Generate a random P-256 keypair and register the custom account contract.
    pub fn random(env: &Env) -> Self {
        let mut rng = rand::thread_rng();
        let signing_key = p256::ecdsa::SigningKey::random(&mut rng);
        Self::from_signing_key(env, signing_key)
    }

    /// Create a P-256 keypair from a 32-byte seed.
    pub fn from_seed(env: &Env, seed: &[u8; 32]) -> Self {
        let signing_key = p256::ecdsa::SigningKey::from_bytes(seed.into()).unwrap();
        Self::from_signing_key(env, signing_key)
    }

    fn from_signing_key(env: &Env, signing_key: p256::ecdsa::SigningKey) -> Self {
        let contract_addr = env.register(secp256r1_account::Secp256r1AccountContract, ());

        // Store the public key in the contract's instance storage
        let pk_point = signing_key.verifying_key().to_encoded_point(false);
        let pk_bytes: [u8; 65] = pk_point.as_bytes().try_into().unwrap();

        env.as_contract(&contract_addr, || {
            env.storage().instance().set(
                &secp256r1_account::DataKey::PublicKey,
                &soroban_sdk::BytesN::<65>::from_array(env, &pk_bytes),
            );
        });

        Self {
            signing_key,
            address: contract_addr,
        }
    }

    /// The SEC-1 uncompressed 65-byte public key.
    pub fn public_key_bytes(&self) -> [u8; 65] {
        let pk_point = self.signing_key.verifying_key().to_encoded_point(false);
        pk_point.as_bytes().try_into().unwrap()
    }
}

impl Signer for Secp256r1Keypair {
    fn address(&self) -> &Address {
        &self.address
    }

    fn sign_payload(&self, _env: &Env, payload: &[u8]) -> ScVal {
        use p256::ecdsa::Signature;
        let signature: Signature =
            <p256::ecdsa::SigningKey as p256::ecdsa::signature::hazmat::PrehashSigner<_>>::sign_prehash(&self.signing_key, payload).unwrap();
        // Soroban requires low-S normalized signatures
        let signature = signature.normalize_s().unwrap_or(signature);
        let sig_bytes: [u8; 64] = signature.to_bytes().into();

        ScVal::Bytes(xdr::ScBytes(sig_bytes.try_into().unwrap()))
    }
}

// ===========================================================================
// setup_real_auth
// ===========================================================================

/// Build and register real (cryptographically signed) authorization entries.
///
/// For each signer, this constructs a `SorobanAuthorizationEntry` with
/// proper `SorobanCredentials::Address` containing a real signature over the
/// authorization payload hash.
///
/// # Arguments
///
/// * `env` - The Soroban environment
/// * `contract` - The contract address being called
/// * `fn_name` - The function name being invoked
/// * `args` - The function arguments
/// * `signers` - The signers that will authorize this invocation
use core::cell::Cell;
std::thread_local! {
    static NONCE_COUNTER: Cell<i64> = const { Cell::new(0) };
}

pub fn setup_real_auth<A>(
    env: &Env,
    contract: &Address,
    fn_name: &str,
    args: A,
    signers: &[&dyn Signer],
) where
    A: IntoVal<Env, Vec<Val>>,
{
    if signers.is_empty() {
        return;
    }

    let args_val: Vec<Val> = args.into_val(env);
    // Convert soroban_sdk::Vec<Val> to xdr vec of ScVal
    let mut xdr_args: StdVec<ScVal> = StdVec::new();
    for i in 0..args_val.len() {
        let val: Val = args_val.get(i).unwrap();
        let sc_val = ScVal::try_from_val(env, &val).unwrap();
        xdr_args.push(sc_val);
    }

    let root_invocation = SorobanAuthorizedInvocation {
        function: SorobanAuthorizedFunction::ContractFn(InvokeContractArgs {
            contract_address: ScAddress::from(contract),
            function_name: ScSymbol(fn_name.try_into().unwrap()),
            args: xdr_args.try_into().unwrap(),
        }),
        sub_invocations: Default::default(),
    };

    let curr_ledger = env.ledger().sequence();
    let max_ttl = env.storage().max_ttl();
    let signature_expiration_ledger = curr_ledger.saturating_add(max_ttl);

    let network_id_bytes = env.ledger().network_id().to_array();

    let mut entries = StdVec::new();

    for (_i, signer) in signers.iter().enumerate() {
        let nonce: i64 = NONCE_COUNTER.with(|c| {
            let n = c.get() + 1;
            c.set(n);
            n
        });

        let preimage = HashIdPreimage::SorobanAuthorization(HashIdPreimageSorobanAuthorization {
            network_id: xdr::Hash(network_id_bytes),
            nonce,
            signature_expiration_ledger,
            invocation: root_invocation.clone(),
        });

        // Serialize and hash the preimage
        let mut buf = StdVec::<u8>::new();
        preimage
            .write_xdr(&mut Limited::new(&mut buf, Limits::none()))
            .unwrap();

        let payload_hash = {
            let bytes = soroban_sdk::Bytes::from_slice(env, &buf);
            env.crypto().sha256(&bytes).to_array()
        };

        let signature = signer.sign_payload(env, &payload_hash);

        let sc_address = ScAddress::from(signer.address());

        entries.push(SorobanAuthorizationEntry {
            credentials: SorobanCredentials::Address(SorobanAddressCredentials {
                address: sc_address,
                nonce,
                signature_expiration_ledger,
                signature,
            }),
            root_invocation: root_invocation.clone(),
        });
    }

    env.set_auths(&entries);
}

// ===========================================================================
// CallBuilder
// ===========================================================================

/// A builder for contract method calls with authorization.
///
/// Created by calling a method on `AuthClient`. Use `.authorize()` for mock auth
/// or `.sign()` for real cryptographic auth, then `.invoke()` to execute.
///
/// # Example
///
/// ```ignore
/// // Mock auth
/// client.transfer(&alice, &bob, &100).authorize(&alice).invoke();
///
/// // Real auth with Ed25519 keypair
/// let kp = Keypair::random(&env);
/// client.transfer(kp.address(), &bob, &100).sign(&kp).invoke();
/// ```
pub struct CallBuilder<'a, R, TryR = ()> {
    env: &'a Env,
    contract: &'a Address,
    fn_name: &'a str,
    args: Vec<Val>,
    authorizers: StdVec<Address>,
    signers: StdVec<&'a dyn Signer>,
    invoker: Box<dyn FnOnce() -> R + 'a>,
    try_invoker: Option<Box<dyn FnOnce() -> TryR + 'a>>,
}

impl<'a, R, TryR> CallBuilder<'a, R, TryR> {
    /// Create a new CallBuilder.
    ///
    /// This is typically called by generated `AuthClient` methods, not directly.
    pub fn new(
        env: &'a Env,
        contract: &'a Address,
        fn_name: &'a str,
        args: Vec<Val>,
        invoker: Box<dyn FnOnce() -> R + 'a>,
        try_invoker: Option<Box<dyn FnOnce() -> TryR + 'a>>,
    ) -> Self {
        Self {
            env,
            contract,
            fn_name,
            args,
            authorizers: StdVec::new(),
            signers: StdVec::new(),
            invoker,
            try_invoker,
        }
    }

    /// Add an address that will authorize this call using mock auth.
    ///
    /// Can be chained multiple times for multi-party authorization.
    /// Cannot be mixed with `.sign()`.
    #[must_use]
    pub fn authorize(mut self, addr: &Address) -> Self {
        self.authorizers.push(addr.clone());
        self
    }

    /// Add multiple addresses that will authorize this call using mock auth.
    /// Cannot be mixed with `.sign()`.
    #[must_use]
    pub fn authorize_all(mut self, addrs: &[Address]) -> Self {
        self.authorizers.extend(addrs.iter().cloned());
        self
    }

    /// Add a signer that will produce a real cryptographic signature.
    ///
    /// Can be chained multiple times for multi-party authorization.
    /// Cannot be mixed with `.authorize()`.
    #[must_use]
    pub fn sign(mut self, signer: &'a dyn Signer) -> Self {
        self.signers.push(signer);
        self
    }

    /// Add multiple signers for real cryptographic authorization.
    /// Cannot be mixed with `.authorize()`.
    #[must_use]
    pub fn sign_all(mut self, signers: &[&'a dyn Signer]) -> Self {
        self.signers.extend_from_slice(signers);
        self
    }

    fn run_auth_setup(
        env: &Env,
        contract: &Address,
        fn_name: &str,
        args: Vec<Val>,
        signers: &[&dyn Signer],
        authorizers: &[Address],
    ) {
        if !signers.is_empty() && !authorizers.is_empty() {
            panic!("cannot mix .sign() and .authorize() on the same CallBuilder");
        }

        if !signers.is_empty() {
            setup_real_auth(env, contract, fn_name, args, signers);
        } else {
            setup_mock_auth(env, contract, fn_name, args, authorizers);
        }
    }

    /// Execute the call with the configured authorizations.
    ///
    /// If signers are configured, uses real cryptographic auth.
    /// If authorizers are configured, uses mock auth.
    /// Panics if both are configured.
    pub fn invoke(self) -> R {
        Self::run_auth_setup(
            self.env,
            self.contract,
            self.fn_name,
            self.args,
            &self.signers,
            &self.authorizers,
        );
        (self.invoker)()
    }

    /// Execute the call with the configured authorizations, returning a
    /// `Result` instead of panicking on contract errors.
    ///
    /// Uses the same authorization setup as `invoke()`.
    ///
    /// # Panics
    ///
    /// Panics if no try invoker was provided (i.e. the `CallBuilder` was not
    /// constructed by a generated `AuthClient` method).
    pub fn try_invoke(self) -> TryR {
        Self::run_auth_setup(
            self.env,
            self.contract,
            self.fn_name,
            self.args,
            &self.signers,
            &self.authorizers,
        );
        let try_invoker = self
            .try_invoker
            .expect("try_invoker not set; use try_invoke through AuthClient methods");
        (try_invoker)()
    }
}

// ===========================================================================
// setup_mock_auth (unchanged)
// ===========================================================================

/// Build and register mock authorization entries for a contract invocation.
///
/// This function is called by generated `AuthClient` wrapper methods to
/// automatically set up mock authorization before delegating to the
/// underlying client.
///
/// # Arguments
///
/// * `env` - The Soroban environment
/// * `contract` - The contract address being called
/// * `fn_name` - The function name being invoked
/// * `args` - The function arguments (as a tuple that implements IntoVal)
/// * `authorizers` - The addresses that should authorize this invocation
///
/// # Example
///
/// ```ignore
/// setup_mock_auth(
///     &env,
///     &contract_id,
///     "transfer",
///     (from.clone(), to.clone(), amount),
///     &[from.clone()],
/// );
/// ```
pub fn setup_mock_auth<A>(
    env: &Env,
    contract: &Address,
    fn_name: &str,
    args: A,
    authorizers: &[Address],
) where
    A: IntoVal<Env, Vec<Val>>,
{
    // If no authorizers specified, clear any prior mock auth state
    // (e.g. mock_all_auths) so calls run with no authorization.
    if authorizers.is_empty() {
        env.mock_auths(&[]);
        return;
    }

    // Convert args to Vec<Val>
    let args_val: Vec<Val> = args.into_val(env);

    // Create the shared MockAuthInvoke that all authorizers will reference
    let invoke = MockAuthInvoke {
        contract,
        fn_name,
        args: args_val,
        sub_invokes: &[],
    };

    // Build MockAuth entries for each authorizer
    // Each authorizer gets a separate MockAuth entry for the same invocation
    let mock_auths: StdVec<_> = authorizers
        .iter()
        .map(|addr| MockAuth {
            address: addr,
            invoke: &invoke,
        })
        .collect();

    // Register the mock authorizations
    env.mock_auths(&mock_auths);
}
