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

/// Helper to panic with an error and optional context
#[macro_export]
macro_rules! panic_with_error {
    ($env:expr, $error:expr) => {{
        let env: &soroban_sdk::Env = $env;
        env.panic_with_error($error)
    }};
    ($env:expr, $error:expr, $msg:literal) => {{
        let env: &soroban_sdk::Env = $env;
        // In the future we could log `$msg` via events or debug logging
        let _ = $msg;
        env.panic_with_error($error)
    }};
}
