#![no_std]
use soroban_sdk::{contract, contractimpl, contracterror, Address, Env};
use soroban_sdk_tools::{
    contractstorage, InstanceItem, PersistentMap, TemporaryMap
};

// ----------------------------------------------------------------------------
// 1. Instance Storage (Symbolic)
// ----------------------------------------------------------------------------
// By default, contractstorage stores keys as readable Symbols
// (e.g., Symbol("Admin")) instead of hashes. This is ideal for Instance storage
// or configuration settings where debugging readability is preferred over
// saving a few bytes of key size, and cardinality is low.
#[contractstorage]
pub struct Config {
    admin: InstanceItem<Address>,
    enabled: InstanceItem<bool>,
}

// ----------------------------------------------------------------------------
// 2. Persistent Storage (Auto-Shorten)
// ----------------------------------------------------------------------------
// We use `auto_shorten = true` here. This optimizes storage keys for gas efficiency.
//
// - `balances`: We manually override the prefix to "b". The key in storage
//   will be: hash("b" + address).
// - `allowances`: We let the macro auto-shorten. It will take the field name "allowances",
//   shorten it to unique bytes (e.g., "a"). We use symbolic to override the usual hashing.
//   The key will be: [Symbol("a"), (from, spender)).
#[contractstorage(auto_shorten = true)]
pub struct Data {
    #[short_key = "b"]    
    balances: PersistentMap<Address, i128>,
    #[symbolic]
    allowances: PersistentMap<(Address, Address), i128>,
}

// ----------------------------------------------------------------------------
// 3. Temporary Storage
// ----------------------------------------------------------------------------
// Temporary storage expires automatically when TTL expires and is cheaper than
// persistent storage. It's suitable for:
//   - Easily replaceable data (e.g., nonces, rate limits)
//   - Data only relevant for a short, well-defined time period
//   - Cost optimization when data loss is acceptable
#[contractstorage]
pub struct RateLimit {
    // Stores the ledger number when an action was last performed.
    // Note: Since entries can be extended by anyone, we can't rely on expiration
    // alone. We check the stored ledger number to enforce the rate limit.
    last_action: TemporaryMap<Address, u32>,
}

#[contracterror]
#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
#[repr(u32)]
pub enum Error {
    NotInitialized = 1,
    AlreadyInitialized = 2,
    NotAuthorized = 3,
    ContractPaused = 4,
    RateLimitExceeded = 5,
}

#[contract]
pub struct FeaturesContract;

#[contractimpl]
impl FeaturesContract {
    /// Initialize the contract with an admin.
    pub fn init(env: Env, admin: Address) -> Result<(), Error> {
        let config = Config::new(&env);
        
        if config.admin.has() {
            return Err(Error::AlreadyInitialized);
        }

        config.admin.set(&admin);
        config.enabled.set(&true);
        
        // Bump instance lifetime
        env.storage().instance().extend_ttl(50, 100);
        
        Ok(())
    }

    /// Update configuration (Instance storage access).
    pub fn set_state(env: Env, enabled: bool) -> Result<(), Error> {
        let config = Config::new(&env);
        let admin = config.admin.get().ok_or(Error::NotInitialized)?;
        
        admin.require_auth();
        config.enabled.set(&enabled);
        
        Ok(())
    }

    /// Mint tokens (Persistent storage with short keys).
    pub fn mint(env: Env, to: Address, amount: i128) -> Result<(), Error> {
        let config = Config::new(&env);
        if !config.enabled.get().unwrap_or(false) {
            return Err(Error::ContractPaused);
        }

        let admin = config.admin.get().ok_or(Error::NotInitialized)?;
        admin.require_auth();

        let data = Data::new(&env);
        let current_balance = data.balances.get(&to).unwrap_or(0);
        data.balances.set(&to, &(current_balance + amount));

        // Extend TTL for the user's balance entry
        data.balances.extend_ttl(&to, 50, 100);

        Ok(())
    }


    /// Claim a faucet reward (Demonstrates Temporary storage).
    /// Users can only claim once every 100 ledgers.
    /// 
    /// This uses temporary storage because:
    /// - Rate limit data is easily replaceable and not critical
    /// - If the entry expires early (loses data), user can just claim again (acceptable)
    /// - We enforce the time bound by checking the stored ledger number, not relying on expiration
    pub fn faucet(env: Env, user: Address) -> Result<(), Error> {
        user.require_auth();
        
        let config = Config::new(&env);
        if !config.enabled.get().unwrap_or(false) {
            return Err(Error::ContractPaused);
        }

        let limits = RateLimit::new(&env);
        let current_ledger = env.ledger().sequence();

        // Check rate limit by comparing stored ledger number
        // (not by relying on entry expiration, which can be extended by anyone)
        if let Some(last_ledger) = limits.last_action.get(&user) {
            if current_ledger < last_ledger + 100 {
                return Err(Error::RateLimitExceeded);
            }
        }

        // Update rate limit in temporary storage
        limits.last_action.set(&user, &current_ledger);
        
        // Set TTL to slightly longer than the rate limit period (100 ledgers)
        // to account for clock drift and ensure the entry lives long enough.
        // We set both min and max TTL to the same value for predictability.
        let ttl = 110;
        limits.last_action.extend_ttl(&user, ttl, ttl);
        
        // Give tokens
        let data = Data::new(&env);
        let bal = data.balances.get(&user).unwrap_or(0);
        data.balances.set(&user, &(bal + 10));
        
        Ok(())
    }

    // Read methods
    pub fn get_balance(env: Env, user: Address) -> i128 {
        Data::new(&env).balances.get(&user).unwrap_or(0)
    }

    pub fn is_enabled(env: Env) -> bool {
        Config::new(&env).enabled.get().unwrap_or(false)
    }
}

mod test;