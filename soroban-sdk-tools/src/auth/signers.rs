extern crate alloc;

use alloc::rc::Rc;

use soroban_sdk::{
    xdr::{self, LedgerKeyAccount, ScAddress, ScVal},
    Address, Env, TryFromVal,
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
