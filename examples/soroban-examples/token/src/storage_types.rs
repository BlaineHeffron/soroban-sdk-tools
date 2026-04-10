use soroban_sdk::{contracttype, Address};
use soroban_sdk_tools::{contractstorage, InstanceItem, PersistentMap, TemporaryMap};

pub(crate) const DAY_IN_LEDGERS: u32 = 17280;
pub(crate) const INSTANCE_BUMP_AMOUNT: u32 = 7 * DAY_IN_LEDGERS;
pub(crate) const INSTANCE_LIFETIME_THRESHOLD: u32 = INSTANCE_BUMP_AMOUNT - DAY_IN_LEDGERS;

pub(crate) const BALANCE_BUMP_AMOUNT: u32 = 30 * DAY_IN_LEDGERS;
pub(crate) const BALANCE_LIFETIME_THRESHOLD: u32 = BALANCE_BUMP_AMOUNT - DAY_IN_LEDGERS;

#[contracttype]
pub struct AllowanceValue {
    pub amount: i128,
    pub expiration_ledger: u32,
}

#[contractstorage(auto_shorten = true)]
pub struct TokenStorage {
    pub admin: InstanceItem<Address>,
    pub balances: PersistentMap<Address, i128>,
    pub allowances: TemporaryMap<(Address, Address), AllowanceValue>,
}
