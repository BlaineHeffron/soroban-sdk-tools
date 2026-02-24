#![no_std]
use soroban_sdk::{Address, Env, String, contract, contracterror, contractimpl};
use soroban_sdk_tools::{
    contractstorage, InstanceItem, InstanceMap, PersistentItem, PersistentMap, TemporaryItem,
    TemporaryMap,
};

// ----------------------------------------------------------------------------
// 1. Default Storage (Symbolic Keys) - Best for Configuration
// ----------------------------------------------------------------------------
// By default, contractstorage generates readable symbolic keys:
//   - admin: Symbol("Admin")
//   - metadata: Vec[Symbol("Metadata"), key]
//
// This is ideal for:
//   - Instance storage (configuration, low cardinality)
//   - Development and debugging
//   - When human-readable keys are valuable
#[contractstorage]
pub struct Config {
    /// Contract administrator - stored as Symbol("Admin")
    admin: InstanceItem<Address>,
    
    /// Contract enabled flag - stored as Symbol("Enabled")
    enabled: InstanceItem<bool>,
    
    /// String metadata map - stored as Vec[Symbol("Metadata"), String]
    metadata: InstanceMap<String, String>,
}

// ----------------------------------------------------------------------------
// 2. Auto-Shorten Storage (Optimized) - Best for Production
// ----------------------------------------------------------------------------
// Using auto_shorten = true generates compact keys:
//   - balances: hash("B" + address) -> BytesN<32>
//   - total_supply: Bytes("T")
//
// Benefits:
//   - 30-40% storage savings compared to symbolic keys
//   - Automatic collision-free prefix assignment
//   - Optimal for high-cardinality data
//
// Generated accessor methods: .balances(), .total_supply()
#[contractstorage(auto_shorten = true)]
pub struct TokenData {
    /// User balances - optimized with auto-generated prefix "B"
    balances: PersistentMap<Address, i128>,
    
    /// Total token supply - stored as Bytes("T")
    total_supply: PersistentItem<i128>,
}

// ----------------------------------------------------------------------------
// 3. Custom Short Keys - Best for Upgrade Safety
// ----------------------------------------------------------------------------
// Explicitly set short_key to ensure keys remain stable across refactoring:
//   - allowances: hash("allow" + (from, spender)) -> BytesN<32>
//   - frozen: hash("frz" + address) -> BytesN<32>
//
// Use this when:
//   - You need upgrade-safe keys that won't change if fields are renamed
//   - You want meaningful custom prefixes
//   - You're migrating from an existing contract
#[contractstorage(auto_shorten = true)]
pub struct TokenRegistry {
    /// Allowances with explicit prefix for upgrade safety
    #[short_key = "allow"]
    allowances: PersistentMap<(Address, Address), i128>,
    
    /// Frozen accounts with stable prefix
    #[short_key = "frz"]
    frozen: PersistentMap<Address, bool>,
}

// ----------------------------------------------------------------------------
// 4. Symbolic Override - Mixed Mode for Debugging
// ----------------------------------------------------------------------------
// Use auto_shorten for most fields, but override specific fields with #[symbolic]
// for easier debugging:
//   - events: Vec[Symbol("E"), u64] - readable for debugging
//   - proposals: hash("P" + u64) -> BytesN<32> - optimized
//
// This gives you:
//   - Optimized storage where it matters
//   - Readable keys for critical fields you need to inspect
#[contractstorage(auto_shorten = true)]
pub struct GovernanceData {
    /// Events stored symbolically for easy ledger inspection
    #[symbolic]
    events: PersistentMap<u64, String>,
    
    /// Proposals stored with optimized hashing
    proposals: PersistentMap<u64, Address>,
    
    /// Vote count stored as short Bytes("V")
    vote_count: PersistentItem<u64>,
}

// ----------------------------------------------------------------------------
// 5. Temporary Storage - Best for Ephemeral Data
// ----------------------------------------------------------------------------
// Temporary storage automatically expires and is cheaper than persistent storage.
//
// Ideal for:
//   - Rate limits and nonces
//   - Session data
//   - Caching
//   - Data where loss is acceptable
//
// Important: TTL can be extended by anyone, so validate actual values!
#[contractstorage]
pub struct RateLimitData {
    /// Last action timestamp per user
    last_action: TemporaryMap<Address, u32>,
    
    /// Daily action counter
    action_count: TemporaryItem<u32>,
}

// ----------------------------------------------------------------------------
// 6. Full Feature Showcase - All Storage Types
// ----------------------------------------------------------------------------
// This struct demonstrates using all storage types together with
// different key optimization strategies based on use case.
#[contractstorage(auto_shorten = true)]
pub struct AdvancedStorage {
    // Persistent data (survives upgrades)
    user_data: PersistentMap<Address, i128>,
    global_counter: PersistentItem<u64>,
    
    // Instance data (contract-level config)
    #[symbolic]
    settings: InstanceMap<String, u64>,
    pause_state: InstanceItem<bool>,
    
    // Temporary data (expires automatically)
    #[short_key = "cache"]
    cached_values: TemporaryMap<u64, i128>,
    temp_flag: TemporaryItem<bool>,
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
    AccountFrozen = 6,
}

#[contract]
pub struct FeaturesContract;

#[contractimpl]
impl FeaturesContract {
    // ========================================================================
    // Initialization - Demonstrates InstanceItem operations
    // ========================================================================
    
    pub fn __constructor(env: &Env, admin: &Address, name: String, symbol: String) -> Result<(), Error> {
        let config = Config::new(env);
        
        // Check if already initialized using .has()
        if config.admin.has() {
            return Err(Error::AlreadyInitialized);
        }
        
        // Set instance items
        config.admin.set(admin);
        config.enabled.set(&true);
        
        // Set instance map entries
        config.metadata.set(&String::from_str(env, "name"), &name);
        config.metadata.set(&String::from_str(env, "symbol"), &symbol);
        
        // Initialize token data with one-liner
        TokenData::set_total_supply(env, &0);
        
        // Extend instance TTL
        env.storage().instance().extend_ttl(50, 100);
        
        Ok(())
    }
    
    // ========================================================================
    // Configuration - Demonstrates InstanceMap operations
    // ========================================================================
    
    pub fn set_metadata(env: &Env, key: String, value: String) -> Result<(), Error> {
        Config::get_admin(env).ok_or(Error::NotInitialized)?.require_auth();
        Config::set_metadata(env, &key, &value);
        Ok(())
    }

    pub fn get_metadata(env: &Env, key: String) -> Option<String> {
        Config::get_metadata(env, &key)
    }

    pub fn remove_metadata(env: &Env, key: String) -> Result<(), Error> {
        Config::get_admin(env).ok_or(Error::NotInitialized)?.require_auth();
        Config::remove_metadata(env, &key);
        Ok(())
    }
    
    // ========================================================================
    // Token Operations - Demonstrates PersistentMap with auto_shorten
    // ========================================================================
    
    pub fn mint(env: &Env, to: &Address, amount: i128) -> Result<(), Error> {
        if !Config::get_enabled(env).unwrap_or(false) {
            return Err(Error::ContractPaused);
        }
        Config::get_admin(env).ok_or(Error::NotInitialized)?.require_auth();

        // Use one-liner .update() for atomic read-modify-write
        TokenData::update_balances(env, to, |balance| balance.unwrap_or(0) + amount);
        TokenData::update_total_supply(env, |supply| supply.unwrap_or(0) + amount);
        TokenData::extend_balances_ttl(env, to, 50, 100);

        Ok(())
    }
    
    pub fn transfer(
        env: &Env,
        from: &Address,
        to: &Address,
        amount: i128,
    ) -> Result<(), Error> {
        from.require_auth();

        // Check if sender is frozen (demonstrates custom short_key map one-liner)
        if TokenRegistry::get_frozen(env, from).unwrap_or(false) {
            return Err(Error::AccountFrozen);
        }

        // Get and validate sender balance
        let from_balance = TokenData::get_balances(env, from).unwrap_or(0);
        if from_balance < amount {
            return Err(Error::NotAuthorized);
        }

        // Update balances
        TokenData::set_balances(env, from, &(from_balance - amount));
        TokenData::update_balances(env, to, |balance| balance.unwrap_or(0) + amount);

        Ok(())
    }

    // ========================================================================
    // Allowances - Demonstrates tuple keys with custom short_key
    // ========================================================================
    
    pub fn approve(env: &Env, from: &Address, spender: &Address, amount: i128) -> Result<(), Error> {
        from.require_auth();
        // Set allowance using tuple key (from, spender) - one-liner
        TokenRegistry::set_allowances(env, &(from.clone(), spender.clone()), &amount);
        Ok(())
    }
    
    pub fn transfer_from(
        env: &Env,
        spender: &Address,
        from: &Address,
        to: &Address,
        amount: i128,
    ) -> Result<(), Error> {
        spender.require_auth();

        let pair = (from.clone(), spender.clone());
        let allowance = TokenRegistry::get_allowances(env, &pair).unwrap_or(0);
        if allowance < amount {
            return Err(Error::NotAuthorized);
        }
        TokenRegistry::set_allowances(env, &pair, &(allowance - amount));

        let from_balance = TokenData::get_balances(env, from).unwrap_or(0);
        if from_balance < amount {
            return Err(Error::NotAuthorized);
        }
        TokenData::set_balances(env, from, &(from_balance - amount));
        TokenData::update_balances(env, to, |balance| balance.unwrap_or(0) + amount);

        Ok(())
    }
    
    // ========================================================================
    // Account Management - Demonstrates PersistentMap with short_key
    // ========================================================================
    
    pub fn freeze_account(env: &Env, account: &Address) -> Result<(), Error> {
        Config::get_admin(env).ok_or(Error::NotInitialized)?.require_auth();
        TokenRegistry::set_frozen(env, account, &true);
        Ok(())
    }

    pub fn unfreeze_account(env: &Env, account: &Address) -> Result<(), Error> {
        Config::get_admin(env).ok_or(Error::NotInitialized)?.require_auth();
        TokenRegistry::remove_frozen(env, account);
        Ok(())
    }

    pub fn is_frozen(env: &Env, account: &Address) -> bool {
        TokenRegistry::get_frozen(env, account).unwrap_or(false)
    }
    
    // ========================================================================
    // Governance - Demonstrates symbolic override for debugging
    // ========================================================================
    
    pub fn record_event(env: &Env, event_id: u64, description: String) -> Result<(), Error> {
        Config::get_admin(env).ok_or(Error::NotInitialized)?.require_auth();
        // Events use symbolic keys for easy inspection
        GovernanceData::set_events(env, &event_id, &description);
        GovernanceData::update_vote_count(env, |count| count.unwrap_or(0) + 1);
        Ok(())
    }

    pub fn create_proposal(env: &Env, proposal_id: u64, proposer: &Address) -> Result<(), Error> {
        proposer.require_auth();
        // Proposals use optimized hashed keys
        GovernanceData::set_proposals(env, &proposal_id, proposer);
        Ok(())
    }
    
    // ========================================================================
    // Rate Limiting - Demonstrates TemporaryMap operations
    // ========================================================================
    
    /// Claim faucet tokens (rate-limited using temporary storage)
    /// Users can only claim once every 100 ledgers.
    ///
    /// Important: We validate the stored ledger number because TTL can be
    /// extended by anyone. Never rely solely on expiration for security!
    pub fn faucet(env: &Env, user: &Address) -> Result<(), Error> {
        user.require_auth();

        if !Config::get_enabled(env).unwrap_or(false) {
            return Err(Error::ContractPaused);
        }

        let current_ledger = env.ledger().sequence();

        // Check rate limit by comparing stored ledger number
        if let Some(last_ledger) = RateLimitData::get_last_action(env, user) {
            if current_ledger < last_ledger + 100 {
                return Err(Error::RateLimitExceeded);
            }
        }

        // Update rate limit in temporary storage
        RateLimitData::set_last_action(env, user, &current_ledger);
        // Set TTL slightly longer than rate limit period
        RateLimitData::extend_last_action_ttl(env, user, 110, 110);

        // Give tokens
        TokenData::update_balances(env, user, |balance| balance.unwrap_or(0) + 10);
        TokenData::update_total_supply(env, |balance| balance.unwrap_or(0) + 10);

        Ok(())
    }
    
    // ========================================================================
    // Advanced Operations - Demonstrates all storage types together
    // ========================================================================
    
    pub fn advanced_operation(env: &Env, user: &Address, value: i128) -> Result<(), Error> {
        user.require_auth();
        
        let storage = AdvancedStorage::new(env);
        
        // Persistent storage: survives upgrades
        storage.user_data().set(user, &value);
        storage.global_counter().update(|count| count.unwrap_or(0) + 1);
        
        // Instance storage: contract-level config (symbolic for readability)
        let setting_key = String::from_str(env, "max_value");
        storage.settings().set(&setting_key, &1000);
        storage.pause_state().set(&false);
        
        // Temporary storage: expires automatically (custom key prefix)
        let cache_key = env.ledger().sequence() as u64;
        storage.cached_values().set(&cache_key, &value);
        storage.cached_values().extend_ttl(&cache_key, 50, 50);
        
        storage.temp_flag().set(&true);
        storage.temp_flag().extend_ttl(10, 10);
        
        Ok(())
    }
    
    // ========================================================================
    // Read Methods - Query operations
    // ========================================================================
    
    pub fn get_balance(env: &Env, user: &Address) -> i128 {
        TokenData::get_balances(env, user).unwrap_or(0)
    }

    pub fn get_total_supply(env: &Env) -> i128 {
        TokenData::get_total_supply(env).unwrap_or(0)
    }

    pub fn get_allowance(env: &Env, from: &Address, spender: &Address) -> i128 {
        TokenRegistry::get_allowances(env, &(from.clone(), spender.clone())).unwrap_or(0)
    }

    pub fn is_enabled(env: &Env) -> bool {
        Config::get_enabled(env).unwrap_or(false)
    }

    pub fn get_admin(env: &Env) -> Option<Address> {
        Config::get_admin(env)
    }

    pub fn get_event(env: &Env, event_id: u64) -> Option<String> {
        GovernanceData::get_events(env, &event_id)
    }

    pub fn get_vote_count(env: &Env) -> u64 {
        GovernanceData::get_vote_count(env).unwrap_or(0)
    }
    
    // ========================================================================
    // Storage Key Inspection - Demonstrates get_*_key methods
    // ========================================================================
    
    /// Get the actual storage key used for a balance entry
    /// Useful for debugging and understanding key materialization
    pub fn inspect_balance_key(env: &Env, user: &Address) -> soroban_sdk::Val {
        TokenData::new(env).get_token_data_balances_key(user.clone())
    }
    
    /// Get the storage key for total supply
    pub fn inspect_total_supply_key(env: &Env) -> soroban_sdk::Val {
        TokenData::new(env).get_token_data_total_supply_key()
    }
}

mod test;