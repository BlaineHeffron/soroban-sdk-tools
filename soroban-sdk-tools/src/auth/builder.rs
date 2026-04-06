extern crate alloc;
extern crate std;

use alloc::{boxed::Box, vec::Vec as StdVec};
use core::cell::Cell;

use soroban_sdk::{
    testutils::{MockAuth, MockAuthInvoke},
    xdr::{
        self, HashIdPreimage, HashIdPreimageSorobanAuthorization, InvokeContractArgs, Limited,
        Limits, ScAddress, ScSymbol, ScVal, SorobanAddressCredentials, SorobanAuthorizationEntry,
        SorobanAuthorizedFunction, SorobanAuthorizedInvocation, SorobanCredentials, WriteXdr,
    },
    Address, Env, IntoVal, TryFromVal, Val, Vec,
};

use super::signers::Signer;

// ===========================================================================
// setup_real_auth
// ===========================================================================

std::thread_local! {
    static NONCE_COUNTER: Cell<i64> = const { Cell::new(0) };
}

/// Convert a [`MockAuthInvoke`] tree into a [`SorobanAuthorizedInvocation`]
/// XDR structure, recursively converting all sub-invocations.
fn mock_invoke_to_xdr(env: &Env, invoke: &MockAuthInvoke) -> SorobanAuthorizedInvocation {
    let mut xdr_args: StdVec<ScVal> = StdVec::new();
    for i in 0..invoke.args.len() {
        let val: Val = invoke.args.get(i).unwrap();
        let sc_val = ScVal::try_from_val(env, &val).unwrap();
        xdr_args.push(sc_val);
    }

    let sub_invocations: StdVec<SorobanAuthorizedInvocation> = invoke
        .sub_invokes
        .iter()
        .map(|sub| mock_invoke_to_xdr(env, sub))
        .collect();

    SorobanAuthorizedInvocation {
        function: SorobanAuthorizedFunction::ContractFn(InvokeContractArgs {
            contract_address: ScAddress::from(invoke.contract),
            function_name: ScSymbol(invoke.fn_name.try_into().unwrap()),
            args: xdr_args.try_into().unwrap(),
        }),
        sub_invocations: sub_invocations.try_into().unwrap(),
    }
}

/// Build and register real (cryptographically signed) authorization entries.
///
/// For each signer, this constructs a `SorobanAuthorizationEntry` with
/// proper `SorobanCredentials::Address` containing a real signature over the
/// authorization payload hash, including any nested sub-invocations.
///
/// # Arguments
///
/// * `env` - The Soroban environment
/// * `signers` - The signers that will authorize this invocation
/// * `invoke` - The invocation tree (root + sub-invocations) to sign
pub fn setup_real_auth<'a>(env: &Env, signers: &[&dyn Signer], invoke: MockAuthInvoke<'a>) {
    if signers.is_empty() {
        return;
    }

    let root_invocation = mock_invoke_to_xdr(env, &invoke);

    let curr_ledger = env.ledger().sequence();
    let max_ttl = env.storage().max_ttl();
    let signature_expiration_ledger = curr_ledger.saturating_add(max_ttl);

    let network_id_bytes = env.ledger().network_id().to_array();

    let mut entries = StdVec::new();

    for signer in signers.iter() {
        let nonce: i64 = NONCE_COUNTER.with(|c| {
            let n = c.get() + 1;
            c.set(n);
            n
        });

        let preimage = HashIdPreimage::SorobanAuthorization(HashIdPreimageSorobanAuthorization {
            network_id: xdr::Hash(network_id_bytes),
            nonce,
            signature_expiration_ledger,
            invocation: root_invocation.clone(),
        });

        let mut buf = StdVec::<u8>::new();
        preimage
            .write_xdr(&mut Limited::new(&mut buf, Limits::none()))
            .unwrap();

        let payload_hash = {
            let bytes = soroban_sdk::Bytes::from_slice(env, &buf);
            env.crypto().sha256(&bytes).to_array()
        };

        let signature = signer.sign_payload(env, &payload_hash);

        let address = ScAddress::from(signer.address());

        entries.push(SorobanAuthorizationEntry {
            credentials: SorobanCredentials::Address(SorobanAddressCredentials {
                address,
                nonce,
                signature_expiration_ledger,
                signature,
            }),
            root_invocation: root_invocation.clone(),
        });
    }

    env.set_auths(&entries);
}

// ===========================================================================
// CallBuilder
// ===========================================================================

/// A builder for contract method calls with authorization.
///
/// Created by calling a method on `AuthClient`. Use `.authorize()` for mock auth
/// or `.sign()` for real cryptographic auth, then `.invoke()` to execute.
///
/// # Example
///
/// ```ignore
/// // Mock auth
/// client.transfer(&alice, &bob, &100).authorize(&alice).invoke();
///
/// // Real auth with Ed25519 keypair
/// let kp = Keypair::random(&env);
/// client.transfer(kp.address(), &bob, &100).sign(&kp).invoke();
/// ```
pub struct CallBuilder<'a, R, TryR = ()> {
    env: &'a Env,
    contract: &'a Address,
    fn_name: &'a str,
    args: Vec<Val>,
    authorizers: StdVec<&'a Address>,
    signers: StdVec<&'a dyn Signer>,
    invoker: Box<dyn FnOnce() -> R + 'a>,
    try_invoker: Option<Box<dyn FnOnce() -> TryR + 'a>>,
    sub_invokes: StdVec<MockAuthInvoke<'a>>,
}

impl<'a, R, TryR> CallBuilder<'a, R, TryR> {
    /// Create a new CallBuilder.
    ///
    /// This is typically called by generated `AuthClient` methods, not directly.
    pub fn new(
        env: &'a Env,
        contract: &'a Address,
        fn_name: &'a str,
        args: Vec<Val>,
        invoker: Box<dyn FnOnce() -> R + 'a>,
        try_invoker: Option<Box<dyn FnOnce() -> TryR + 'a>>,
    ) -> Self {
        Self {
            env,
            contract,
            fn_name,
            args,
            authorizers: StdVec::new(),
            signers: StdVec::new(),
            invoker,
            try_invoker,
            sub_invokes: StdVec::new(),
        }
    }

    /// Add an address that will authorize this call using mock auth.
    ///
    /// Can be chained multiple times for multi-party authorization.
    ///
    /// # Panics
    ///
    /// Panics at `invoke()` / `try_invoke()` time if `.sign()` has also been
    /// called on this builder — mock and real auth cannot be mixed.
    #[must_use]
    pub fn authorize(mut self, addr: &'a Address) -> Self {
        self.authorizers.push(addr);
        self
    }

    /// Add multiple addresses that will authorize this call using mock auth.
    ///
    /// # Panics
    ///
    /// Panics at `invoke()` / `try_invoke()` time if `.sign()` has also been
    /// called on this builder — mock and real auth cannot be mixed.
    #[must_use]
    pub fn authorize_all(mut self, addrs: &[&'a Address]) -> Self {
        self.authorizers.extend(addrs.iter().cloned());
        self
    }

    /// Add a signer that will produce a real cryptographic signature.
    ///
    /// Can be chained multiple times for multi-party authorization.
    ///
    /// # Panics
    ///
    /// Panics at `invoke()` / `try_invoke()` time if `.authorize()` has also
    /// been called on this builder — mock and real auth cannot be mixed.
    #[must_use]
    pub fn sign(mut self, signer: &'a dyn Signer) -> Self {
        self.signers.push(signer);
        self
    }

    /// Add multiple signers for real cryptographic authorization.
    ///
    /// # Panics
    ///
    /// Panics at `invoke()` / `try_invoke()` time if `.authorize()` has also
    /// been called on this builder — mock and real auth cannot be mixed.
    #[must_use]
    pub fn sign_all(mut self, signers: &[&'a dyn Signer]) -> Self {
        self.signers.extend_from_slice(signers);
        self
    }

    fn run_auth_setup(&self) {
        if !self.signers.is_empty() && !self.authorizers.is_empty() {
            panic!("cannot mix .sign() and .authorize() on the same CallBuilder");
        }

        if !self.signers.is_empty() {
            setup_real_auth(self.env, &self.signers, self.mock_auth_invocation());
        } else {
            setup_mock_auth(self.env, &self.authorizers, self.mock_auth_invocation());
        }
    }

    /// Execute the call with the configured authorizations.
    ///
    /// If signers are configured, uses real cryptographic auth.
    /// If authorizers are configured, uses mock auth.
    /// Panics if both are configured.
    pub fn invoke(self) -> R {
        self.run_auth_setup();
        (self.invoker)()
    }

    /// Execute the call with the configured authorizations, returning a
    /// `Result` instead of panicking on contract errors.
    ///
    /// Uses the same authorization setup as `invoke()`.
    ///
    /// # Panics
    ///
    /// Panics if no try invoker was provided (i.e. the `CallBuilder` was not
    /// constructed by a generated `AuthClient` method).
    pub fn try_invoke(self) -> TryR {
        self.run_auth_setup();
        let try_invoker = self
            .try_invoker
            .expect("try_invoker not set; use try_invoke through AuthClient methods");
        (try_invoker)()
    }

    pub fn add_sub_invoke<B: AsMockAuthInvoke<'a>>(mut self, builder: &'a B) -> Self {
        self.sub_invokes.push(builder.mock_auth_invocation());
        self
    }
}

impl<'a, R, TryR> AsMockAuthInvoke<'a> for CallBuilder<'a, R, TryR> {
    fn mock_auth_invocation(&self) -> MockAuthInvoke<'_> {
        let Self {
            contract,
            fn_name,
            args,
            sub_invokes,
            ..
        } = &self;
        let args = args.into_val(self.env);
        MockAuthInvoke {
            contract,
            fn_name,
            args,
            sub_invokes,
        }
    }
}

pub trait AsMockAuthInvoke<'a> {
    fn mock_auth_invocation(&'a self) -> MockAuthInvoke<'a>;
}

// ===========================================================================
// setup_mock_auth
// ===========================================================================

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
pub fn setup_mock_auth<'a>(env: &Env, authorizers: &[&Address], invoke: MockAuthInvoke<'a>) {
    // If no authorizers specified, clear any prior mock auth state
    // (e.g. mock_all_auths) so calls run with no authorization.
    if authorizers.is_empty() {
        env.mock_auths(&[]);
        return;
    }

    // Build MockAuth entries for each authorizer
    // Each authorizer gets a separate MockAuth entry for the same invocation
    let mock_auths: StdVec<_> = authorizers
        .iter()
        .map(|address| MockAuth {
            address,
            invoke: &invoke,
        })
        .collect();

    // Register the mock authorizations
    env.mock_auths(&mock_auths);
}
