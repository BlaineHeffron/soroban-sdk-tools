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

/// Spec entry for a single error variant.
/// Used for flattening inner error types into outer contract specs.
#[derive(Debug, Clone, Copy)]
pub struct ErrorSpecEntry {
    /// The error code (u32)
    pub code: u32,
    /// The variant name (e.g., "DivisionByZero")
    pub name: &'static str,
    /// Human-readable description
    pub description: &'static str,
}

/// Trait providing spec metadata for error types.
///
/// This is automatically implemented by `#[scerr]` and can be used by
/// root error enums to flatten inner error types into the contract spec.
///
/// For types imported via `contractimport_with_errors!`, a similar const
/// is generated that can be used for spec flattening.
pub trait ContractErrorSpec {
    /// Array of spec entries for all variants in this error type.
    const SPEC_ENTRIES: &'static [ErrorSpecEntry];
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
