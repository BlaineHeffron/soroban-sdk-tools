//! Multi-Signer Vault Contract
//!
//! This contract demonstrates complex multi-user authorization patterns:
//!
//! - **Multi-signer withdrawals**: Withdrawals require authorization from
//!   multiple signers (configurable threshold, e.g., 2-of-3)
//! - **Role-based access**: Different roles (admin, signer) with different permissions
//!
//! # Authorization Patterns Demonstrated
//!
//! 1. Single admin auth for configuration changes
//! 2. Multi-party auth for high-value operations (withdrawals)

#![no_std]

use soroban_sdk::{contract, contractimpl, Address, Env, Vec};
use soroban_sdk_tools::{contractstorage, InstanceItem};

#[contractstorage(auto_shorten = true)]
struct Storage {
    admin: InstanceItem<Address>,
    signers: InstanceItem<Vec<Address>>,
    threshold: InstanceItem<u32>,
    balance: InstanceItem<i128>,
}

#[contract]
pub struct VaultContract;

#[contractimpl]
impl VaultContract {
    /// Initialize the vault with an admin, initial signers, and threshold
    ///
    /// # Arguments
    /// * `admin` - The vault administrator
    /// * `signers` - Initial list of authorized signers
    /// * `threshold` - Number of signatures required for withdrawal
    pub fn initialize(env: Env, admin: Address, signers: Vec<Address>, threshold: u32) {
        let storage = Storage::new(&env);
        if storage.admin.has() {
            panic!("already initialized");
        }

        let signer_count = signers.len();
        if threshold == 0 || threshold > signer_count {
            panic!("invalid threshold");
        }

        storage.admin.set(&admin);
        storage.signers.set(&signers);
        storage.threshold.set(&threshold);
        storage.balance.set(&0i128);
    }

    /// Add a new signer (admin only)
    ///
    /// Requires authorization from the admin.
    pub fn add_signer(env: Env, new_signer: Address) {
        let storage = Storage::new(&env);
        let admin = storage.admin.get().unwrap();
        admin.require_auth();

        let mut signers = storage.signers.get().unwrap();

        // Check not already a signer
        for i in 0..signers.len() {
            if signers.get(i).unwrap() == new_signer {
                panic!("already a signer");
            }
        }

        signers.push_back(new_signer);
        storage.signers.set(&signers);
    }

    /// Remove a signer (admin only)
    ///
    /// Requires authorization from the admin.
    pub fn remove_signer(env: Env, signer: Address) {
        let storage = Storage::new(&env);
        let admin = storage.admin.get().unwrap();
        admin.require_auth();

        let signers = storage.signers.get().unwrap();
        let threshold = storage.threshold.get().unwrap();

        // Ensure we maintain minimum signers for threshold
        if signers.len() <= threshold {
            panic!("cannot remove: would go below threshold");
        }

        let mut new_signers = Vec::new(&env);
        let mut found = false;
        for i in 0..signers.len() {
            let s = signers.get(i).unwrap();
            if s == signer {
                found = true;
            } else {
                new_signers.push_back(s);
            }
        }

        if !found {
            panic!("not a signer");
        }

        storage.signers.set(&new_signers);
    }

    /// Update the signature threshold (admin only)
    ///
    /// Requires authorization from the admin.
    pub fn set_threshold(env: Env, new_threshold: u32) {
        let storage = Storage::new(&env);
        let admin = storage.admin.get().unwrap();
        admin.require_auth();

        let signers = storage.signers.get().unwrap();

        if new_threshold == 0 || new_threshold > signers.len() {
            panic!("invalid threshold");
        }

        storage.threshold.set(&new_threshold);
    }

    /// Deposit funds to the vault
    ///
    /// Requires authorization from the depositor.
    pub fn deposit(env: Env, from: Address, amount: i128) {
        let storage = Storage::new(&env);
        from.require_auth();

        storage
            .balance
            .update(|balance| balance.unwrap_or(0) + amount);
    }

    /// Withdraw funds from the vault
    ///
    /// **This is the key multi-signer demonstration!**
    ///
    /// Requires authorization from at least `threshold` number of signers.
    /// Each signer in `authorizers` must call `require_auth()`.
    ///
    /// # Arguments
    /// * `authorizers` - List of signers authorizing this withdrawal
    /// * `to` - Recipient address
    /// * `amount` - Amount to withdraw
    pub fn withdraw(env: Env, authorizers: Vec<Address>, to: Address, amount: i128) {
        let storage = Storage::new(&env);
        let signers = storage.signers.get().unwrap();
        let threshold = storage.threshold.get().unwrap();

        // Verify we have enough authorizers
        if authorizers.len() < threshold {
            panic!("not enough authorizers");
        }

        // Verify each authorizer is a valid signer and require their auth
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
                panic!("invalid authorizer: not a signer");
            }

            // Require auth from this signer
            // This is where multi-user auth happens!
            authorizer.require_auth();
        }

        // Perform the withdrawal
        let balance = storage.balance.get().unwrap_or(0);
        if balance < amount {
            panic!("insufficient balance");
        }
        storage.balance.set(&(balance - amount));

        // In production, we'd transfer tokens to `to` here
        let _ = to;
    }

    /// Get the vault's balance
    pub fn balance(env: Env) -> i128 {
        Storage::get_balance(&env).unwrap_or(0)
    }

    /// Get the list of signers
    pub fn get_signers(env: Env) -> Vec<Address> {
        Storage::get_signers(&env).unwrap()
    }

    /// Get the current threshold
    pub fn get_threshold(env: Env) -> u32 {
        Storage::get_threshold(&env).unwrap()
    }
}

#[cfg(test)]
mod test {
    use soroban_sdk::testutils::Address as _;
    use soroban_sdk::{vec, Address, Env};

    mod vault {
        soroban_sdk_tools::contractimport!(
            file = "../../../target/stellar/soroban_auth_vault.wasm"
        );
    }

    /// Helper: register contract and initialize a 2-of-3 vault.
    /// Returns (contract_id, admin, signer1, signer2, signer3).
    fn setup(env: &Env) -> (Address, Address, Address, Address, Address) {
        let contract_id = env.register(vault::WASM, ());
        let admin = Address::generate(env);
        let signer1 = Address::generate(env);
        let signer2 = Address::generate(env);
        let signer3 = Address::generate(env);

        let signers = vec![env, signer1.clone(), signer2.clone(), signer3.clone()];

        env.mock_all_auths();
        vault::Client::new(env, &contract_id).initialize(&admin, &signers, &2);

        (contract_id, admin, signer1, signer2, signer3)
    }

    /// Demonstrates multi-signer withdrawal using AuthClient with authorize_all.
    ///
    /// This is a 2-of-3 multisig vault where withdrawals require
    /// authorization from at least 2 signers.
    #[test]
    fn test_multi_signer_withdrawal() {
        let env = &Env::default();
        let (contract_id, admin, signer1, signer2, _) = &setup(env);
        let client = vault::AuthClient::new(env, contract_id);
        let recipient = &Address::generate(env);

        client.deposit(admin, &1000).authorize(admin).invoke();
        assert_eq!(client.balance().invoke(), 1000);

        let authorizers = vec![env, signer1.clone(), signer2.clone()];
        client
            .withdraw(&authorizers, recipient, &500)
            .authorize_all(&[signer1, signer2])
            .invoke();
        assert_eq!(client.balance().invoke(), 500);
    }

    /// Demonstrates single-signer deposit using AuthClient.
    #[test]
    fn test_deposit() {
        let env = &Env::default();
        let (contract_id, _, _, _, _) = &setup(env);
        let client = vault::AuthClient::new(env, contract_id);
        let depositor = &Address::generate(env);

        client
            .deposit(depositor, &500)
            .authorize(depositor)
            .invoke();

        assert_eq!(client.balance().invoke(), 500);
    }

    /// Demonstrates admin operations (add signer, set threshold) using AuthClient.
    #[test]
    fn test_admin_operations() {
        let env = &Env::default();
        let (contract_id, admin, _, _, _) = &setup(env);
        let client = vault::AuthClient::new(env, contract_id);
        let new_signer = &Address::generate(env);

        client.add_signer(new_signer).authorize(admin).invoke();
        assert_eq!(client.get_signers().invoke().len(), 4);

        client.set_threshold(&3).authorize(admin).invoke();
        assert_eq!(client.get_threshold().invoke(), 3);
    }

    /// Tests that withdrawal fails without enough signers.
    #[test]
    #[should_panic]
    fn test_insufficient_signers() {
        let env = &Env::default();
        let (contract_id, admin, signer1, _, _) = &setup(env);
        let client = vault::AuthClient::new(env, contract_id);
        let recipient = &Address::generate(env);

        client.deposit(admin, &1000).authorize(admin).invoke();

        // Try with only 1 signer (needs 2)
        let authorizers = vec![env, signer1.clone()];
        client
            .withdraw(&authorizers, recipient, &500)
            .authorize(signer1)
            .invoke();
    }
}
