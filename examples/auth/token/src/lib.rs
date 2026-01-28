//! Simple Token Contract for Auth Testing
//!
//! This is a minimal token implementation that demonstrates authorization
//! requirements for transfers, approvals, and burns.

#![no_std]

use soroban_sdk::{contract, contractimpl, contracttype, Address, Env};

#[derive(Clone)]
#[contracttype]
pub enum DataKey {
    Balance(Address),
    Allowance(Address, Address), // (owner, spender)
    Admin,
}

#[contract]
pub struct TokenContract;

/// Token error codes
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[repr(u32)]
pub enum TokenError {
    InsufficientBalance = 1,
    InsufficientAllowance = 2,
    Unauthorized = 3,
}

impl From<TokenError> for soroban_sdk::Error {
    fn from(e: TokenError) -> Self {
        soroban_sdk::Error::from_contract_error(e as u32)
    }
}

#[contractimpl]
impl TokenContract {
    /// Initialize the token with an admin
    pub fn initialize(env: Env, admin: Address) {
        if env.storage().instance().has(&DataKey::Admin) {
            panic!("already initialized");
        }
        env.storage().instance().set(&DataKey::Admin, &admin);
    }

    /// Mint tokens to an address (admin only)
    pub fn mint(env: Env, to: Address, amount: i128) {
        let admin: Address = env.storage().instance().get(&DataKey::Admin).unwrap();
        admin.require_auth();

        let key = DataKey::Balance(to.clone());
        let balance: i128 = env.storage().persistent().get(&key).unwrap_or(0);
        env.storage().persistent().set(&key, &(balance + amount));
    }

    /// Transfer tokens from caller to recipient
    ///
    /// Requires authorization from the `from` address.
    pub fn transfer(env: Env, from: Address, to: Address, amount: i128) {
        // The sender must authorize this transfer
        from.require_auth();

        Self::spend_balance(&env, &from, amount);
        Self::receive_balance(&env, &to, amount);
    }

    /// Transfer tokens using an allowance
    ///
    /// Requires authorization from the `spender` address.
    /// The spender must have sufficient allowance from the owner.
    pub fn transfer_from(env: Env, spender: Address, from: Address, to: Address, amount: i128) {
        // The spender must authorize this action
        spender.require_auth();

        Self::spend_allowance(&env, &from, &spender, amount);
        Self::spend_balance(&env, &from, amount);
        Self::receive_balance(&env, &to, amount);
    }

    /// Approve a spender to transfer tokens on behalf of the owner
    ///
    /// Requires authorization from the `owner` address.
    pub fn approve(env: Env, owner: Address, spender: Address, amount: i128) {
        // The owner must authorize this approval
        owner.require_auth();

        let key = DataKey::Allowance(owner, spender);
        env.storage().persistent().set(&key, &amount);
    }

    /// Burn tokens from an address
    ///
    /// Requires authorization from the `from` address.
    pub fn burn(env: Env, from: Address, amount: i128) {
        // The holder must authorize the burn
        from.require_auth();

        Self::spend_balance(&env, &from, amount);
    }

    /// Get the balance of an address
    pub fn balance(env: Env, addr: Address) -> i128 {
        let key = DataKey::Balance(addr);
        env.storage().persistent().get(&key).unwrap_or(0)
    }

    /// Get the allowance for a spender
    pub fn allowance(env: Env, owner: Address, spender: Address) -> i128 {
        let key = DataKey::Allowance(owner, spender);
        env.storage().persistent().get(&key).unwrap_or(0)
    }

    // --- Internal helpers ---

    fn spend_balance(env: &Env, addr: &Address, amount: i128) {
        let key = DataKey::Balance(addr.clone());
        let balance: i128 = env.storage().persistent().get(&key).unwrap_or(0);
        if balance < amount {
            panic!("insufficient balance");
        }
        env.storage().persistent().set(&key, &(balance - amount));
    }

    fn receive_balance(env: &Env, addr: &Address, amount: i128) {
        let key = DataKey::Balance(addr.clone());
        let balance: i128 = env.storage().persistent().get(&key).unwrap_or(0);
        env.storage().persistent().set(&key, &(balance + amount));
    }

    fn spend_allowance(env: &Env, owner: &Address, spender: &Address, amount: i128) {
        let key = DataKey::Allowance(owner.clone(), spender.clone());
        let allowance: i128 = env.storage().persistent().get(&key).unwrap_or(0);
        if allowance < amount {
            panic!("insufficient allowance");
        }
        env.storage().persistent().set(&key, &(allowance - amount));
    }
}

#[cfg(test)]
mod test {
    use soroban_sdk::testutils::Address as _;
    use soroban_sdk::{Address, Env};

    // Import this contract's WASM to get AuthClient
    mod token {
        soroban_sdk_tools::contractimport!(
            file = "../../../target/stellar/soroban_auth_token.wasm"
        );
    }

    /// Demonstrates AuthClient for single-user authorization.
    ///
    /// Instead of verbose MockAuth setup, AuthClient lets you write:
    /// ```ignore
    /// auth_client.transfer(&alice, &bob, &amount).authorize(&alice).invoke();
    /// ```
    #[test]
    fn test_transfer_with_auth_client() {
        let env = Env::default();
        let contract_id = env.register(token::WASM, ());

        // Standard client for queries
        let client = token::Client::new(&env, &contract_id);

        // AuthClient for authorized operations - generated by contractimport!
        let auth = token::AuthClient::new(&env, &contract_id);

        let admin = Address::generate(&env);
        let alice = Address::generate(&env);
        let bob = Address::generate(&env);

        // Setup with mock_all_auths for simplicity
        env.mock_all_auths();
        client.initialize(&admin);
        client.mint(&alice, &1000);

        // Use AuthClient for the transfer - builder pattern!
        auth.transfer(&alice, &bob, &300).authorize(&alice).invoke();

        assert_eq!(client.balance(&alice), 700);
        assert_eq!(client.balance(&bob), 300);
    }

    /// Demonstrates AuthClient for approve/transfer_from flow.
    #[test]
    fn test_allowance_with_auth_client() {
        let env = Env::default();
        let contract_id = env.register(token::WASM, ());
        let client = token::Client::new(&env, &contract_id);

        let admin = Address::generate(&env);
        let owner = Address::generate(&env);
        let spender = Address::generate(&env);
        let recipient = Address::generate(&env);

        // Setup
        env.mock_all_auths();
        client.initialize(&admin);
        client.mint(&owner, &1000);

        // Owner approves spender using AuthClient
        token::AuthClient::new(&env, &contract_id)
            .approve(&owner, &spender, &500)
            .authorize(&owner)
            .invoke();

        assert_eq!(client.allowance(&owner, &spender), 500);

        // Spender uses allowance via AuthClient
        token::AuthClient::new(&env, &contract_id)
            .transfer_from(&spender, &owner, &recipient, &200)
            .authorize(&spender)
            .invoke();

        assert_eq!(client.balance(&owner), 800);
        assert_eq!(client.balance(&recipient), 200);
        assert_eq!(client.allowance(&owner, &spender), 300);
    }

    /// Demonstrates AuthClient for burn operations.
    #[test]
    fn test_burn_with_auth_client() {
        let env = Env::default();
        let contract_id = env.register(token::WASM, ());
        let client = token::Client::new(&env, &contract_id);

        let admin = Address::generate(&env);
        let user = Address::generate(&env);

        env.mock_all_auths();
        client.initialize(&admin);
        client.mint(&user, &1000);

        // Burn tokens using AuthClient
        token::AuthClient::new(&env, &contract_id)
            .burn(&user, &400)
            .authorize(&user)
            .invoke();

        assert_eq!(client.balance(&user), 600);
    }
}
