//! Procedural macros for soroban-sdk-tools

use proc_macro::TokenStream;

mod contract;
mod error;
mod storage;
mod util;

/// Define storage structures with automatic key management
///
/// This is a function-like macro that processes multiple struct definitions,
/// performing global key shortening and collision checks.
///
/// # Example
/// ```ignore
///     #[contractstorage(auto_shorten = true)]
///     pub struct MyStorage {
///         #[short_key("bal")]
///         balances: PersistentMap<Address, i128>,
///         owner: PersistentItem<Address>,
///     }
/// }
/// ```
#[proc_macro_attribute]
pub fn contractstorage(attr: TokenStream, item: TokenStream) -> TokenStream {
    storage::contractstorage_attr_impl(attr, item)
}

/// Module-level aggregator:
/// #[`contractstorage_module`]
/// mod `my_contract` { /* structs with #[contractstorage] */ }
#[proc_macro_attribute]
pub fn contractstorage_module(attr: TokenStream, item: TokenStream) -> TokenStream {
    storage::contractstorage_module_impl(attr, item)
}

/// Define a contract error enum with automatic code assignment
///
/// # Example
/// ```ignore
/// #[scerr]
/// pub enum Error {
///     InvalidInput,
///     Unauthorized,
/// }
/// ```
#[proc_macro_attribute]
pub fn scerr(attr: TokenStream, item: TokenStream) -> TokenStream {
    error::scerr_impl(attr, item)
}
