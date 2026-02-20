//! Procedural macros for soroban-sdk-tools

use proc_macro::TokenStream;

mod contract;
mod contractimport;
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

/// Define a contract error enum with automatic sequential code assignment.
///
/// Auto-detects basic mode (all unit variants) or composable mode (errors
/// with `#[transparent]`, `#[from_contract_client]`, or data-carrying variants).
///
/// In basic mode, generates a `#[contracterror]` `#[repr(u32)]` enum with
/// sequential codes starting at 1, plus `ContractError` and `ContractErrorSpec`
/// trait implementations.
///
/// In advanced mode, uses const-chaining to assign sequential codes where
/// wrapped inner types are flattened at their position. An `Aborted` variant
/// (code 0) and `UnknownError` sentinel variant (code `UNKNOWN_ERROR_CODE`)
/// are always auto-generated. Inner types must implement `ContractError` and
/// `ContractErrorSpec` (provided by `#[scerr]` or `contractimport!`).
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

/// Import a contract from WASM with enhanced error handling support.
///
/// This macro wraps `soroban_sdk::contractimport!` and additionally generates:
/// - `ContractError` implementations for imported error types (`into_code`, `from_code`, `description`)
/// - `ContractErrorSpec` implementations for imported error types (variant metadata)
/// - Spec constants providing programmatic access to error variant info
///
/// These trait implementations enable imported error types to be used as inner
/// types in `#[scerr]` root enums with `#[transparent]` or `#[from_contract_client]`.
///
/// # Parameters
///
/// - `file` (required): Path to the WASM file to import
/// - `sha256` (optional): SHA256 hash to verify the WASM file
///
/// # Example
///
/// ```ignore
/// mod math_imported {
///     soroban_sdk_tools::contractimport!(
///         file = "../target/wasm32v1-none/release/math_contract.wasm",
///     );
/// }
///
/// // Mode is auto-detected due to #[from_contract_client] attribute
/// #[scerr]
/// pub enum AppError {
///     Unauthorized,
///
///     #[from_contract_client]
///     Math(math_imported::MathError),
/// }
/// ```
#[proc_macro]
pub fn contractimport(attr: TokenStream) -> TokenStream {
    contractimport::contractimport_impl(attr)
}
