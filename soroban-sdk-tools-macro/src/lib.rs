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

/// Import a contract from WASM with enhanced error handling support.
///
/// This macro wraps `soroban_sdk::contractimport!` and additionally generates:
/// - `ContractErrorSpec` implementations for imported error types
/// - Spec XDR statics that embed inner error specs in the outer WASM
/// - Flattened error spec with combined codes (when `scerr_variant` is provided)
///
/// # Parameters
///
/// - `file` (required): Path to the WASM file to import
/// - `sha256` (optional): SHA256 hash to verify the WASM file
/// - `scerr_variant` (optional): Variant name for flattened spec generation
/// - `outer_error` (optional): Names the flattened spec as `{outer_error}_{variant}`
///
/// # Flattened Spec Generation
///
/// When `scerr_variant` is provided, the macro generates a flattened error spec
/// with combined error codes (namespace + inner code). This enables TypeScript
/// bindings to directly match runtime error codes.
///
/// The `scerr_variant` value must match the variant name used in your
/// `#[from_contract_client]` attribute, as both use the same hash-based
/// namespace computation.
///
/// When `outer_error` is also provided, the flattened spec is named
/// `{outer_error}_{variant}` (e.g., `AppError_Math`), creating a clear
/// naming relationship in TypeScript bindings.
///
/// # Example
///
/// ```ignore
/// mod math_imported {
///     soroban_sdk_tools::contractimport_with_errors!(
///         file = "../target/wasm32v1-none/release/math_contract.wasm",
///         scerr_variant = "Math",
///         outer_error = "AppError"  // Optional: names flattened spec AppError_Math
///     );
/// }
///
/// // Mode is auto-detected due to #[from_contract_client] attribute
/// #[scerr]
/// pub enum AppError {
///     Unauthorized,
///
///     #[from_contract_client]
///     Math(math_imported::MathError),  // Variant name matches scerr_variant
/// }
/// ```
///
/// This generates TypeScript bindings with:
/// - `MathError`: Original error codes from the inner contract
/// - `AppError_Math`: Combined codes with namespace for runtime matching
/// - `AppError`: Main error enum with unit variants
#[proc_macro]
pub fn contractimport_with_errors(attr: TokenStream) -> TokenStream {
    contractimport::contractimport_with_errors_impl(attr)
}
