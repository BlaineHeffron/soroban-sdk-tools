//! Error handling utilities for Soroban contracts
//!
//! Provides traits and helper types for composable error handling
//! with the `#[scerr]` macro.

// Re-export contracterror for users
pub use soroban_sdk::contracterror;

/// Base trait for contract errors
pub trait ContractError: Sized {
    /// Convert this error into a u32 code
    fn into_code(self) -> u32;

    /// Try to construct this error from a u32 code
    fn from_code(code: u32) -> Option<Self>;

    /// Get a human-readable description
    fn description(&self) -> &'static str;
}

/// Trait for converting errors from contract clients
pub trait FromContractError<E> {
    /// Convert from a contract client error
    fn from_contract_error(error: E) -> Self;
}

// TODO: Implement helper macros/functions for error handling
// - panic_with_context!() wrapper
// - Result type aliases
// - Conversion utilities

/// Helper to panic with an error and optional context
#[macro_export]
macro_rules! panic_with_error {
    ($env:expr, $error:expr) => {
        $env.panic_with_error($error)
    };
    ($env:expr, $error:expr, $msg:literal) => {{
        // TODO: Log message before panicking
        $env.panic_with_error($error)
    }};
}
