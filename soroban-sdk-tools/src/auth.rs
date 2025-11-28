//! Authorization testing utilities
//!
//! Provides enhanced contract client wrappers that simplify authorization
//! testing by automatically generating mock authorizations.

#[cfg(any(test, feature = "testutils"))]
use soroban_sdk::{Address, Env, Vec};

/// Extension trait for contract clients to enable simplified auth testing
pub trait ContractClientExt: Sized {
    /// Invoke the next contract call with authorization from the given addresses
    fn with_auth(&self, authorizers: &[Address]) -> Self;

    /// Invoke the next contract call with authorization from a single address
    fn with_auth_single(&self, authorizer: &Address) -> Self {
        // Note: This is a simplified skeleton - actual implementation will be in macros
        // For now, just forward to with_auth method
        let _ = authorizer; // Suppress unused warning
        self.with_auth(&[])
    }
}

// TODO: Implement ContractClientExt for generated contract clients
// This will require macro support or a wrapper type

/// Helper to build mock authorization entries
pub fn build_mock_auth(
    _env: &Env,
    _contract_id: &Address,
    _function_name: &str,
    _args: Vec<soroban_sdk::Val>,
    _authorizers: &[Address],
) {
    // TODO: Implement automatic mock auth generation
    // 1. Create AuthorizedInvocation structure
    // 2. Call env.mock_auths() with generated entries
    unimplemented!("build_mock_auth")
}
