//! Authorization testing utilities
//!
//! Provides utilities for simplified authorization testing by automatically
//! generating mock authorization entries from method invocation details.
//!
//! # Example
//!
//! ```ignore
//! use soroban_sdk_tools::contractimport;
//!
//! mod my_contract {
//!     soroban_sdk_tools::contractimport!(
//!         file = "path/to/contract.wasm"
//!     );
//! }
//!
//! #[test]
//! fn test_with_auth() {
//!     let env = Env::default();
//!     let contract_id = env.register_contract_wasm(None, &my_contract::WASM);
//!     let client = my_contract::AuthClient::new(&env, &contract_id);
//!     let user = Address::generate(&env);
//!
//!     // Builder pattern: method -> authorize -> invoke
//!     client.my_method(&user, &5).authorize(&user).invoke();
//! }
//! ```

#[cfg(any(test, feature = "testutils"))]
extern crate alloc;

#[cfg(any(test, feature = "testutils"))]
use alloc::{boxed::Box, vec::Vec as StdVec};

#[cfg(any(test, feature = "testutils"))]
use soroban_sdk::{
    testutils::{MockAuth, MockAuthInvoke},
    Address, Env, IntoVal, Val, Vec,
};

/// A builder for contract method calls with authorization.
///
/// Created by calling a method on `AuthClient`. Use `.authorize()` to specify
/// which addresses should authorize the call, then `.invoke()` to execute.
///
/// # Example
///
/// ```ignore
/// // Single authorizer
/// client.transfer(&alice, &bob, &100).authorize(&alice).invoke();
///
/// // Multiple authorizers
/// client.withdraw(&signers, &recipient, &amount)
///     .authorize(&signer1)
///     .authorize(&signer2)
///     .invoke();
/// ```
#[cfg(any(test, feature = "testutils"))]
pub struct CallBuilder<'a, R> {
    env: &'a Env,
    contract: &'a Address,
    fn_name: &'static str,
    args: Vec<Val>,
    authorizers: StdVec<Address>,
    invoker: Box<dyn FnOnce() -> R + 'a>,
}

#[cfg(any(test, feature = "testutils"))]
impl<'a, R> CallBuilder<'a, R> {
    /// Create a new CallBuilder.
    ///
    /// This is typically called by generated `AuthClient` methods, not directly.
    pub fn new(
        env: &'a Env,
        contract: &'a Address,
        fn_name: &'static str,
        args: Vec<Val>,
        invoker: Box<dyn FnOnce() -> R + 'a>,
    ) -> Self {
        Self {
            env,
            contract,
            fn_name,
            args,
            authorizers: StdVec::new(),
            invoker,
        }
    }

    /// Add an address that will authorize this call.
    ///
    /// Can be chained multiple times for multi-party authorization.
    #[must_use]
    pub fn authorize(mut self, addr: &Address) -> Self {
        self.authorizers.push(addr.clone());
        self
    }

    /// Add multiple addresses that will authorize this call.
    #[must_use]
    pub fn authorize_all(mut self, addrs: &[Address]) -> Self {
        self.authorizers.extend(addrs.iter().cloned());
        self
    }

    /// Execute the call with the configured authorizations.
    ///
    /// Sets up mock authorization for all specified authorizers, then
    /// invokes the underlying contract method.
    pub fn invoke(self) -> R {
        setup_mock_auth(
            self.env,
            self.contract,
            self.fn_name,
            self.args,
            &self.authorizers,
        );
        (self.invoker)()
    }
}

/// Build and register mock authorization entries for a contract invocation.
///
/// This function is called by generated `AuthClient` wrapper methods to
/// automatically set up mock authorization before delegating to the
/// underlying client.
///
/// # Arguments
///
/// * `env` - The Soroban environment
/// * `contract` - The contract address being called
/// * `fn_name` - The function name being invoked
/// * `args` - The function arguments (as a tuple that implements IntoVal)
/// * `authorizers` - The addresses that should authorize this invocation
///
/// # Example
///
/// ```ignore
/// setup_mock_auth(
///     &env,
///     &contract_id,
///     "transfer",
///     (from.clone(), to.clone(), amount),
///     &[from.clone()],
/// );
/// ```
#[cfg(any(test, feature = "testutils"))]
pub fn setup_mock_auth<A>(
    env: &Env,
    contract: &Address,
    fn_name: &str,
    args: A,
    authorizers: &[Address],
) where
    A: IntoVal<Env, Vec<Val>>,
{
    // If no authorizers specified, skip mock setup
    if authorizers.is_empty() {
        return;
    }

    // Convert args to Vec<Val>
    let args_val: Vec<Val> = args.into_val(env);

    // Create the shared MockAuthInvoke that all authorizers will reference
    let invoke = MockAuthInvoke {
        contract,
        fn_name,
        args: args_val,
        sub_invokes: &[],
    };

    // Build MockAuth entries for each authorizer
    // Each authorizer gets a separate MockAuth entry for the same invocation
    let mock_auths: StdVec<_> = authorizers
        .iter()
        .map(|addr| MockAuth {
            address: addr,
            invoke: &invoke,
        })
        .collect();

    // Register the mock authorizations
    env.mock_auths(&mock_auths);
}
