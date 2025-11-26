//! Implementation of the contractstorage macro
//!
//! This macro transforms structs with storage field declarations into
//! fully-functional storage structures with automatic key generation.
//!
//! The implementation is split into three modules:
//! - `single`: Handles single-struct processing via #[contractstorage]
//! - `module`: Handles module-level processing via #[`contractstorage_module`]
//! - `common`: Shared utilities and core transformation logic

mod common;
mod module;
mod single;

use proc_macro::TokenStream;

/// Attribute macro for processing a single storage struct
///
/// # Example
/// ```ignore
/// #[contractstorage(auto_shorten)]
/// pub struct MyStorage {
///     pub balance: PersistentMap<Address, i128>,
///     pub total: PersistentItem<i128>,
/// }
/// ```
pub fn contractstorage_attr_impl(attr: TokenStream, item: TokenStream) -> TokenStream {
    single::contractstorage_attr_impl(attr, item)
}

/// Attribute macro for processing multiple storage structs in a module
///
/// This enables global key management across all structs to prevent
/// collisions and optimize key space usage.
///
/// # Example
/// ```ignore
/// #[contractstorage_module]
/// mod storage {
///     #[contractstorage(auto_shorten)]
///     pub struct BalanceStorage {
///         pub balances: PersistentMap<Address, i128>,
///     }
///     
///     #[contractstorage(auto_shorten)]
///     pub struct TokenStorage {
///         pub total_supply: PersistentItem<i128>,
///     }
/// }
/// ```
pub fn contractstorage_module_impl(attr: TokenStream, item: TokenStream) -> TokenStream {
    module::contractstorage_module_impl(attr, item)
}
