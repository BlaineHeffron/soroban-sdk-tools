//! Procedural macros for soroban-sdk-tools

use proc_macro::TokenStream;

mod contract;
mod contractimport;
mod error;
mod storage;
mod util;

/// Define storage structures with automatic key management
///
/// This attribute macro transforms a struct with storage field declarations into
/// a fully-functional storage structure with automatic key generation and
/// static convenience methods.
///
/// # Generated API
///
/// For each field, the macro generates static one-liner methods on the struct:
///
/// **Item fields** (`PersistentItem<V>`, `InstanceItem<V>`, `TemporaryItem<V>`):
/// - `get_{field}(env) -> Option<V>`
/// - `set_{field}(env, &V)`
/// - `has_{field}(env) -> bool`
/// - `remove_{field}(env)`
/// - `update_{field}(env, f) -> V`
/// - `extend_{field}_ttl(env, threshold, extend_to)`
///
/// **Map fields** (`PersistentMap<K, V>`, `InstanceMap<K, V>`, `TemporaryMap<K, V>`):
/// - `get_{field}(env, &K) -> Option<V>`
/// - `set_{field}(env, &K, &V)`
/// - `has_{field}(env, &K) -> bool`
/// - `remove_{field}(env, &K)`
/// - `update_{field}(env, &K, f) -> V`
/// - `extend_{field}_ttl(env, &K, threshold, extend_to)`
///
/// A `new(env) -> Self` constructor is also generated for multi-operation access.
///
/// # Example
/// ```ignore
/// #[contractstorage(auto_shorten = true)]
/// pub struct MyStorage {
///     #[short_key("bal")]
///     balances: PersistentMap<Address, i128>,
///     owner: PersistentItem<Address>,
/// }
///
/// // One-liner access:
/// let bal = MyStorage::get_balances(&env, &addr);
/// MyStorage::set_owner(&env, &admin);
///
/// // Or struct-based for multiple operations:
/// let s = MyStorage::new(&env);
/// s.balances.set(&addr, &100);
/// s.owner.set(&admin);
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

/// Import a contract from WASM with enhanced error handling and auth testing support.
///
/// This macro wraps `soroban_sdk::contractimport!` and additionally generates:
/// - `ContractError` implementations for imported error types (`into_code`, `from_code`, `description`)
/// - `ContractErrorSpec` implementations for imported error types (variant metadata)
/// - Spec constants providing programmatic access to error variant info
/// - `AuthClient` for simplified authorization testing (testutils feature)
///
/// These trait implementations enable imported error types to be used as inner
/// types in `#[scerr]` root enums with `#[transparent]` or `#[from_contract_client]`.
///
/// # Parameters
///
/// - `file` (required): Path to the WASM file to import
/// - `sha256` (optional): SHA256 hash to verify the WASM file
///
/// # Generated Types
///
/// - `Client<'a>`: Standard contract client (from soroban-spec-rust)
/// - `AuthClient<'a>`: Auth-testing wrapper client (when testutils enabled)
/// - Error enums with `ContractErrorSpec` implementations
///
/// # Example
///
/// ```ignore
/// mod math_imported {
///     soroban_sdk_tools::contractimport!(
///         file = "../target/wasm32v1-none/release/math_contract.wasm"
///     );
/// }
///
/// // Use AuthClient for simplified auth testing
/// #[test]
/// fn test_with_auth() {
///     let env = Env::default();
///     let contract_id = env.register_contract_wasm(None, &math_imported::WASM);
///     let client = math_imported::AuthClient::new(&env, &contract_id);
///     let user = Address::generate(&env);
///
///     // Auto-generates MockAuth entries
///     client.with_auth(&user).increment(&user, &5);
/// }
///
/// // Use with scerr for composable errors
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
