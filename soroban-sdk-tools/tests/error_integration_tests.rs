//! Integration tests for #[scerr], including root enums and cross-contract calls.

use soroban_sdk::{contract, contractimpl, Address, Env};
use soroban_sdk_tools::{scerr, soroban_sdk, ContractError};

// -----------------------------------------------------------------------------
// Inner contract 1: math - used with both transparent and from_contract_client
// -----------------------------------------------------------------------------

#[scerr]
pub enum MathError {
    /// division by zero
    DivisionByZero,
    NegativeInput,
}

#[contract]
pub struct Math;

#[contractimpl]
impl Math {
    pub fn safe_div(num: i64, denom: i64) -> Result<i64, MathError> {
        if denom == 0 {
            return Err(MathError::DivisionByZero);
        }
        if denom < 0 {
            return Err(MathError::NegativeInput);
        }
        Ok(num / denom)
    }
}

// -----------------------------------------------------------------------------
// Inner contract 2: calc - used only with transparent
// -----------------------------------------------------------------------------

#[scerr]
pub enum CalcError {
    Overflow,
    Underflow,
}

#[contract]
pub struct Calc;

#[contractimpl]
impl Calc {
    pub fn safe_add(env: Env, a: i64, b: i64) -> Result<i64, CalcError> {
        env.logs().add("safe_add called", &[]);
        let result = a.checked_add(b).ok_or(CalcError::Overflow);

        if let Err(ref e) = result {
            use soroban_sdk::IntoVal;
            let code = (*e).into_code();
            env.logs()
                .add("Returning error with code", &[code.into_val(&env)]);
        }

        result
    }
}

// -----------------------------------------------------------------------------
// Inner contract 3: logic - used only with from_contract_client
// -----------------------------------------------------------------------------

#[scerr]
pub enum LogicError {
    InvalidState,
    Unauthorized,
}

#[contract]
pub struct Logic;

#[contractimpl]
impl Logic {
    pub fn check_positive(x: i64) -> Result<(), LogicError> {
        if x <= 0 {
            return Err(LogicError::InvalidState);
        }
        Ok(())
    }
}

// -----------------------------------------------------------------------------
// Outer contract with mixed usage
// -----------------------------------------------------------------------------

#[scerr]
pub enum OuterError {
    /// unauthorized
    Unauthorized,

    // Both for MathError
    #[transparent]
    Math(#[from] MathError),

    #[from_contract_client]
    MathClient(MathError),

    // Only transparent for CalcError
    #[transparent]
    Calc(#[from] CalcError),

    // Only from_contract_client for LogicError
    #[from_contract_client]
    LogicClient(#[from] LogicError),
}

#[contract]
pub struct Outer;

#[contractimpl]
impl Outer {
    pub fn div_through(
        env: Env,
        math_id: Address,
        num: i64,
        denom: i64,
    ) -> Result<i64, OuterError> {
        let client = MathClient::new(&env, &math_id);
        let result = client.try_safe_div(&num, &denom)??;
        Ok(result)
    }

    pub fn add_through(env: Env, calc_id: Address, a: i64, b: i64) -> Result<i64, OuterError> {
        // Use raw invoke_contract instead of client to see actual error codes
        use soroban_sdk::IntoVal;

        let result: Result<i64, soroban_sdk::Error> = env.invoke_contract(
            &calc_id,
            &soroban_sdk::symbol_short!("safe_add"),
            soroban_sdk::vec![&env, a.into_val(&env), b.into_val(&env)],
        );

        match result {
            Ok(val) => Ok(val),
            Err(e) => {
                let code = if e.is_type(soroban_sdk::xdr::ScErrorType::Contract) {
                    e.get_code()
                } else {
                    0
                };
                env.logs()
                    .add("Error from inner contract", &[code.into_val(&env)]);
                Err(OuterError::from(e))
            }
        }
    }

    pub fn check_through(env: Env, logic_id: Address, x: i64) -> Result<(), OuterError> {
        let client = LogicClient::new(&env, &logic_id);

        let result = client.try_check_positive(&x);

        match &result {
            Ok(_) => env.logs().add("check_positive returned Ok", &[]),
            Err(_) => env.logs().add("check_positive returned Err", &[]),
        }

        client.try_check_positive(&x)??;
        Ok(())
    }

    pub fn div_direct(num: i64, denom: i64) -> Result<i64, OuterError> {
        let result = Math::safe_div(num, denom)?;
        Ok(result)
    }

    pub fn add_direct(env: Env, a: i64, b: i64) -> Result<i64, OuterError> {
        let result = Calc::safe_add(env, a, b)?;
        Ok(result)
    }
}

fn setup_math_contract(env: &Env) -> Address {
    env.register(Math, ())
}

fn setup_logic_contract(env: &Env) -> Address {
    env.register(Logic, ())
}

fn setup_outer_contract(env: &Env) -> Address {
    env.register(Outer, ())
}

// -----------------------------------------------------------------------------
// Basic mode tests: sequential codes 1, 2, 3...
// -----------------------------------------------------------------------------

#[test]
fn basic_mode_sequential_codes() {
    assert_eq!(MathError::DivisionByZero.into_code(), 1);
    assert_eq!(MathError::NegativeInput.into_code(), 2);

    assert_eq!(CalcError::Overflow.into_code(), 1);
    assert_eq!(CalcError::Underflow.into_code(), 2);

    assert_eq!(LogicError::InvalidState.into_code(), 1);
    assert_eq!(LogicError::Unauthorized.into_code(), 2);
}

#[test]
fn basic_mode_roundtrip() {
    for code in 1..=2 {
        let err = MathError::from_code(code).expect("Should decode");
        assert_eq!(err.into_code(), code);
    }
    assert!(MathError::from_code(0).is_none());
    assert!(MathError::from_code(3).is_none());
}

// -----------------------------------------------------------------------------
// Root mode tests: const-chained sequential codes
// -----------------------------------------------------------------------------

#[test]
fn root_mode_sequential_codes() {
    // OuterError layout:
    // Unauthorized = 1
    // Math (transparent, 2 variants): offset=2, count=2 → codes 2,3
    // MathClient (FCC, 2 variants): offset=4, count=2 → codes 4,5
    // Calc (transparent, 2 variants): offset=6, count=2 → codes 6,7
    // LogicClient (FCC, 2 variants): offset=8, count=2 → codes 8,9
    // Aborted (auto) = 10
    // UnknownError (auto) = 11

    assert_eq!(OuterError::Unauthorized.into_code(), 1);
    assert_eq!(OuterError::Math(MathError::DivisionByZero).into_code(), 2);
    assert_eq!(OuterError::Math(MathError::NegativeInput).into_code(), 3);
    assert_eq!(
        OuterError::MathClient(MathError::DivisionByZero).into_code(),
        4
    );
    assert_eq!(
        OuterError::MathClient(MathError::NegativeInput).into_code(),
        5
    );
    assert_eq!(OuterError::Calc(CalcError::Overflow).into_code(), 6);
    assert_eq!(OuterError::Calc(CalcError::Underflow).into_code(), 7);
    assert_eq!(
        OuterError::LogicClient(LogicError::InvalidState).into_code(),
        8
    );
    assert_eq!(
        OuterError::LogicClient(LogicError::Unauthorized).into_code(),
        9
    );
    assert_eq!(OuterError::Aborted.into_code(), 10);
    assert_eq!(OuterError::UnknownError.into_code(), 11);
}

#[test]
fn root_mode_from_code_roundtrip() {
    let errors = [
        OuterError::Unauthorized,
        OuterError::Math(MathError::DivisionByZero),
        OuterError::Math(MathError::NegativeInput),
        OuterError::MathClient(MathError::DivisionByZero),
        OuterError::MathClient(MathError::NegativeInput),
        OuterError::Calc(CalcError::Overflow),
        OuterError::Calc(CalcError::Underflow),
        OuterError::LogicClient(LogicError::InvalidState),
        OuterError::LogicClient(LogicError::Unauthorized),
        OuterError::Aborted,
    ];

    for err in errors {
        let code = err.into_code();
        let decoded = OuterError::from_code(code).expect(&format!("Should decode code {}", code));
        assert_eq!(err, decoded, "Roundtrip failed for code {}", code);
    }

    // UnknownError sentinel decodes at its own code
    let unknown = OuterError::UnknownError;
    let code = unknown.into_code();
    let decoded = OuterError::from_code(code).expect("Should decode");
    assert_eq!(decoded, OuterError::UnknownError);
}

#[test]
fn root_mode_from_code_out_of_range() {
    assert!(OuterError::from_code(0).is_none());
    assert!(OuterError::from_code(12).is_none());
    assert!(OuterError::from_code(100).is_none());
    assert!(OuterError::from_code(u32::MAX).is_none());
}

// -----------------------------------------------------------------------------
// Cross-contract tests
// -----------------------------------------------------------------------------

#[test]
fn root_error_from_inner_error_via_div_through() {
    let env = Env::default();
    let math_id = setup_math_contract(&env);
    let outer_id = setup_outer_contract(&env);

    let outer_client = OuterClient::new(&env, &outer_id);

    let result = outer_client.try_div_through(&math_id, &10, &0);
    assert!(result.is_err());

    let err = result.err().unwrap();
    let decoded = err.expect("no invoke error");
    assert_eq!(
        decoded.into_code(),
        OuterError::MathClient(MathError::DivisionByZero).into_code()
    );
}

#[test]
fn transparent_only_for_calc() {
    let env = Env::default();
    let outer_id = setup_outer_contract(&env);

    let outer_client = OuterClient::new(&env, &outer_id);

    let result = outer_client.try_add_direct(&i64::MAX, &1);
    assert!(result.is_err());

    let err = result.unwrap_err();
    let decoded: OuterError = err.expect("no invoke error");

    assert_eq!(decoded, OuterError::Calc(CalcError::Overflow));
}

#[test]
fn from_contract_client_only_for_logic() {
    let env = Env::default();
    let logic_id = setup_logic_contract(&env);
    let outer_id = setup_outer_contract(&env);

    let outer_client = OuterClient::new(&env, &outer_id);

    let result = outer_client.try_check_through(&logic_id, &-1);
    assert!(result.is_err());

    let err = result.err().unwrap();
    let decoded: OuterError = err.expect("no invoke error");

    assert_eq!(decoded, OuterError::LogicClient(LogicError::InvalidState));
}

#[test]
fn mixed_usage_for_math() {
    let env = Env::default();
    let math_id = setup_math_contract(&env);
    let outer_id = setup_outer_contract(&env);

    let outer_client = OuterClient::new(&env, &outer_id);

    // Test transparent conversion
    let inner_err = MathError::NegativeInput;
    let outer_err: OuterError = inner_err.into();
    assert_eq!(outer_err, OuterError::Math(MathError::NegativeInput));

    // Test through client (should use MathClient variant)
    let result = outer_client.try_div_through(&math_id, &10, &-1);
    assert!(result.is_err());

    let err = result.err().unwrap();
    let decoded: OuterError = err.expect("no invoke error");

    assert_eq!(decoded, OuterError::MathClient(MathError::NegativeInput));
}

#[test]
fn transparent_and_from_impls_exist_and_work() {
    let inner = MathError::NegativeInput;
    let outer: OuterError = inner.into();

    let inner_code = MathError::NegativeInput.into_code();
    let outer_code = outer.into_code();

    // Sequential codes mean inner code 2 becomes outer code 3
    assert_ne!(inner_code, outer_code);

    let calc_inner = CalcError::Overflow;
    let calc_outer: OuterError = calc_inner.into();
    assert_eq!(calc_outer, OuterError::Calc(CalcError::Overflow));
}

#[test]
fn transparent_allows_question_mark_propagation() {
    let env = Env::default();
    let outer_id = setup_outer_contract(&env);

    let outer_client = OuterClient::new(&env, &outer_id);

    let result = outer_client.try_div_direct(&10, &0);
    assert!(result.is_err());

    let err = result.err().unwrap();
    let decoded: OuterError = err.expect("no invoke error");

    assert_eq!(decoded, OuterError::Math(MathError::DivisionByZero));
}

#[test]
fn transparent_propagates_with_question_mark() {
    let env = Env::default();
    let outer_id = setup_outer_contract(&env);

    let outer_client = OuterClient::new(&env, &outer_id);

    let result = outer_client.try_add_direct(&i64::MAX, &1);
    assert!(result.is_err());

    let err = result.err().unwrap();
    let decoded: OuterError = err.expect("no invoke error");

    assert_eq!(decoded, OuterError::Calc(CalcError::Overflow));
}

// -----------------------------------------------------------------------------
// Test case for root-to-root via from_contract_client
// -----------------------------------------------------------------------------

#[scerr]
pub enum InnerRootError {
    /// inner unauthorized
    InnerUnauthorized,
    /// inner invalid state
    InnerInvalidState,
}

#[contract]
pub struct InnerRoot;

#[contractimpl]
impl InnerRoot {
    pub fn fail_unauthorized() -> Result<(), InnerRootError> {
        Err(InnerRootError::InnerUnauthorized)
    }

    pub fn fail_invalid_state() -> Result<(), InnerRootError> {
        Err(InnerRootError::InnerInvalidState)
    }
}

#[scerr]
pub enum OuterRootError {
    /// outer unauthorized
    OuterUnauthorized,

    #[from_contract_client]
    Inner(#[from] InnerRootError),
}

#[contract]
pub struct OuterRoot;

#[contractimpl]
impl OuterRoot {
    pub fn call_inner(env: Env, inner_id: Address) -> Result<(), OuterRootError> {
        let client = InnerRootClient::new(&env, &inner_id);
        client.try_fail_unauthorized()??;
        Ok(())
    }

    pub fn call_inner_invalid(env: Env, inner_id: Address) -> Result<(), OuterRootError> {
        let client = InnerRootClient::new(&env, &inner_id);
        client.try_fail_invalid_state()??;
        Ok(())
    }

    pub fn fail_outer_unauthorized() -> Result<(), OuterRootError> {
        Err(OuterRootError::OuterUnauthorized)
    }
}

fn setup_inner_root_contract(env: &Env) -> Address {
    env.register(InnerRoot, ())
}

fn setup_outer_root_contract(env: &Env) -> Address {
    env.register(OuterRoot, ())
}

#[test]
fn root_to_root_sequential_codes() {
    // OuterRootError layout:
    // OuterUnauthorized = 1
    // Inner (FCC, 2 variants): offset=2, count=2 → codes 2,3
    // Aborted (auto) = 4
    // UnknownError (auto) = 5

    assert_eq!(OuterRootError::OuterUnauthorized.into_code(), 1);
    assert_eq!(
        OuterRootError::Inner(InnerRootError::InnerUnauthorized).into_code(),
        2
    );
    assert_eq!(
        OuterRootError::Inner(InnerRootError::InnerInvalidState).into_code(),
        3
    );
    assert_eq!(OuterRootError::Aborted.into_code(), 4);
    assert_eq!(OuterRootError::UnknownError.into_code(), 5);
}

#[test]
fn root_to_root_collision_via_from_contract_client() {
    let env = Env::default();
    let inner_id = setup_inner_root_contract(&env);
    let outer_id = setup_outer_root_contract(&env);

    let outer_client = OuterRootClient::new(&env, &outer_id);

    let result = outer_client.try_call_inner(&inner_id);
    assert!(result.is_err());

    let err = result.err().unwrap();
    let decoded: OuterRootError = err.expect("no invoke error");

    assert_eq!(
        decoded,
        OuterRootError::Inner(InnerRootError::InnerUnauthorized)
    );
}

#[test]
fn root_to_root_collision_second_variant() {
    let env = Env::default();
    let inner_id = setup_inner_root_contract(&env);
    let outer_id = setup_outer_root_contract(&env);

    let outer_client = OuterRootClient::new(&env, &outer_id);

    let result = outer_client.try_call_inner_invalid(&inner_id);
    assert!(result.is_err());

    let err = result.err().unwrap();
    let decoded: OuterRootError = err.expect("no invoke error");

    assert_eq!(
        decoded,
        OuterRootError::Inner(InnerRootError::InnerInvalidState)
    );
}

#[test]
fn show_sequential_codes_no_collision() {
    // Sequential codes guarantee no collision between unit variants and wrapped ranges
    let inner_code_1 = InnerRootError::InnerUnauthorized.into_code(); // 1
    let inner_code_2 = InnerRootError::InnerInvalidState.into_code(); // 2

    let outer_wrapped_1 = OuterRootError::Inner(InnerRootError::InnerUnauthorized).into_code(); // 2
    let outer_wrapped_2 = OuterRootError::Inner(InnerRootError::InnerInvalidState).into_code(); // 3

    let outer_own_code = OuterRootError::OuterUnauthorized.into_code(); // 1

    // The outer's own codes and wrapped codes are different
    assert_ne!(outer_own_code, outer_wrapped_1);
    assert_ne!(outer_own_code, outer_wrapped_2);

    // Wrapped codes use a different range than the raw inner codes
    assert_ne!(outer_wrapped_1, inner_code_1);
    assert_ne!(outer_wrapped_2, inner_code_2);
}

#[test]
fn outer_unit_variant_not_confused_with_inner() {
    let env = Env::default();
    let outer_id = setup_outer_root_contract(&env);

    let outer_client = OuterRootClient::new(&env, &outer_id);

    let result = outer_client.try_fail_outer_unauthorized();
    assert!(result.is_err());

    let err = result.err().unwrap();
    let decoded: OuterRootError = err.expect("no invoke error");

    assert_eq!(decoded, OuterRootError::OuterUnauthorized);

    assert!(
        !matches!(decoded, OuterRootError::Inner(_)),
        "OuterUnauthorized should not be decoded as Inner variant"
    );

    assert_eq!(decoded.into_code(), 1);
}

#[test]
fn bidirectional_code_mapping_integrity() {
    let outer_unit = OuterRootError::OuterUnauthorized;
    let code1 = outer_unit.into_code();
    let decoded1 = OuterRootError::from_code(code1).expect("Should decode");
    assert_eq!(outer_unit, decoded1);

    let inner_wrapped = OuterRootError::Inner(InnerRootError::InnerUnauthorized);
    let code2 = inner_wrapped.into_code();
    let decoded2 = OuterRootError::from_code(code2).expect("Should decode");
    assert_eq!(inner_wrapped, decoded2);

    assert_ne!(code1, code2);

    let inner_wrapped2 = OuterRootError::Inner(InnerRootError::InnerInvalidState);
    let code3 = inner_wrapped2.into_code();
    let decoded3 = OuterRootError::from_code(code3).expect("Should decode");
    assert_eq!(inner_wrapped2, decoded3);
}

// -----------------------------------------------------------------------------
// Test: Auto-generated abort and unknown handlers
// -----------------------------------------------------------------------------

#[scerr]
pub enum AutoError {
    InvalidInput,
    #[from_contract_client]
    Math(MathError),
}

#[contract]
pub struct AutoContract;

#[contractimpl]
impl AutoContract {
    pub fn call_math(env: Env, math_id: Address) -> Result<i64, AutoError> {
        let client = MathClient::new(&env, &math_id);
        client.try_safe_div(&10, &0)??;
        Ok(0)
    }
}

#[test]
fn test_auto_abort_handler() {
    let abort_err = AutoError::Aborted;
    assert_eq!(
        abort_err.into_code(),
        AutoError::from_code(abort_err.into_code())
            .unwrap()
            .into_code()
    );
}

#[test]
fn test_auto_unknown_handler() {
    let unknown = AutoError::UnknownError;
    assert_eq!(
        unknown.into_code(),
        AutoError::from_code(unknown.into_code())
            .unwrap()
            .into_code()
    );
}

#[test]
fn test_auto_variants_sequential_codes() {
    // AutoError layout:
    // InvalidInput = 1
    // Math (FCC, 2 variants): offset=2, count=2 → codes 2,3
    // Aborted (auto) = 4
    // UnknownError (auto) = 5

    assert_eq!(AutoError::InvalidInput.into_code(), 1);
    assert_eq!(AutoError::Math(MathError::DivisionByZero).into_code(), 2);
    assert_eq!(AutoError::Math(MathError::NegativeInput).into_code(), 3);
    assert_eq!(AutoError::Aborted.into_code(), 4);
    assert_eq!(AutoError::UnknownError.into_code(), 5);
}

// -----------------------------------------------------------------------------
// Test: Error logging with sentinel
// -----------------------------------------------------------------------------

#[scerr(handle_unknown = "auto", log_unknown_errors = true)]
pub enum LoggingError {
    InvalidInput,

    #[from_contract_client]
    Math(MathError),
}

#[contract]
pub struct LoggingContract;

#[contractimpl]
impl LoggingContract {
    pub fn call_with_logging(env: Env, math_id: Address) -> Result<i64, LoggingError> {
        let client = MathClient::new(&env, &math_id);
        let result = client.try_safe_div(&10, &0);

        match result {
            Ok(inner_result) => match inner_result {
                Ok(val) => Ok(val),
                Err(contract_err) => Err(LoggingError::from(contract_err)),
            },
            Err(inner_error) => match inner_error {
                Ok(math_error) => Err(LoggingError::Math(math_error)),
                Err(e) => Err(LoggingError::from_invoke_error(&env, e)),
            },
        }
    }
}

#[test]
fn test_logging_unknown_errors() {
    let env = Env::default();
    let contract_id = env.register(LoggingContract, ());
    let client = LoggingContractClient::new(&env, &contract_id);

    let _ = client.try_call_with_logging(&env.register(Math, ()));
}

// -----------------------------------------------------------------------------
// Test: Panic mode (default behavior)
// -----------------------------------------------------------------------------

#[scerr]
pub enum PanicError {
    InvalidInput,

    #[from_contract_client]
    Math(MathError),
}

#[contract]
pub struct PanicContract;

#[contractimpl]
impl PanicContract {
    pub fn call_math(env: Env, math_id: Address) -> Result<i64, PanicError> {
        let client = MathClient::new(&env, &math_id);
        client.try_safe_div(&10, &0)??;
        Ok(0)
    }
}

// Note: Can't easily test panic behavior in tests, but verifying compilation is valuable

// -----------------------------------------------------------------------------
// Test: Sentinel without code storage
// -----------------------------------------------------------------------------

#[scerr]
pub enum SimpleSentinelError {
    InvalidInput,

    #[sentinel]
    Unknown, // Unit variant - doesn't store code

    #[from_contract_client]
    Math(MathError),
}

#[test]
fn test_simple_sentinel() {
    let unknown = SimpleSentinelError::Unknown;
    assert!(matches!(unknown, SimpleSentinelError::Unknown));
}

// -----------------------------------------------------------------------------
// Test: Mixed transparent and from_contract_client
// -----------------------------------------------------------------------------

#[scerr]
pub enum MixedError {
    InvalidInput,

    #[transparent]
    MathDirect(#[from] MathError),

    #[from_contract_client]
    MathRemote(MathError),
}

#[contract]
pub struct MixedContract;

#[contractimpl]
impl MixedContract {
    pub fn direct_div(num: i64, denom: i64) -> Result<i64, MixedError> {
        Math::safe_div(num, denom)?;
        Ok(num / denom)
    }

    pub fn remote_div(env: Env, math_id: Address, num: i64, denom: i64) -> Result<i64, MixedError> {
        let client = MathClient::new(&env, &math_id);
        client.try_safe_div(&num, &denom)??;
        Ok(num / denom)
    }
}

#[test]
fn test_mixed_transparent() {
    let env = Env::default();
    let contract_id = env.register(MixedContract, ());
    let client = MixedContractClient::new(&env, &contract_id);

    let result = client.try_direct_div(&10, &0);
    assert!(result.is_err());

    let err: MixedError = result.err().unwrap().unwrap();
    assert!(matches!(err, MixedError::MathDirect(_)));
}

#[test]
fn test_mixed_from_contract_client() {
    let env = Env::default();
    let math_id = env.register(Math, ());
    let contract_id = env.register(MixedContract, ());
    let client = MixedContractClient::new(&env, &contract_id);

    let result = client.try_remote_div(&math_id, &10, &0);
    assert!(result.is_err());

    let err: MixedError = result.err().unwrap().unwrap();
    assert!(matches!(err, MixedError::MathRemote(_)));
}

// -----------------------------------------------------------------------------
// Test: Roundtrip encoding with auto variants
// -----------------------------------------------------------------------------

#[test]
fn test_auto_variants_roundtrip() {
    let abort = AutoError::Aborted;
    let code = abort.into_code();
    let decoded = AutoError::from_code(code).expect("Should decode");
    assert_eq!(abort, decoded);

    let unknown = AutoError::UnknownError;
    let code2 = unknown.into_code();
    let decoded2 = AutoError::from_code(code2).expect("Should decode");
    assert_eq!(unknown, decoded2);

    assert_ne!(code, code2);
}

// -----------------------------------------------------------------------------
// Test: Inner types using #[scerr] basic mode
// These tests verify that both #[transparent] and #[from_contract_client]
// work with #[scerr] basic mode types (which get ContractError + ContractErrorSpec).
// -----------------------------------------------------------------------------

#[scerr]
pub enum StandardError {
    NotFound,
    AlreadyExists,
    InvalidArgument,
}

#[contract]
pub struct StandardContract;

#[contractimpl]
impl StandardContract {
    pub fn fail_not_found() -> Result<(), StandardError> {
        Err(StandardError::NotFound)
    }

    pub fn fail_already_exists() -> Result<(), StandardError> {
        Err(StandardError::AlreadyExists)
    }

    pub fn succeed() -> Result<u32, StandardError> {
        Ok(42)
    }
}

// -----------------------------------------------------------------------------
// Test: #[from_contract_client] with standard #[contracterror]
// -----------------------------------------------------------------------------

#[scerr]
pub enum FccStandardError {
    /// unauthorized operation
    Unauthorized,

    #[from_contract_client]
    /// standard contract error
    Standard(StandardError),
}

#[contract]
pub struct FccStandardContract;

#[contractimpl]
impl FccStandardContract {
    pub fn call_standard_fail(env: Env, standard_id: Address) -> Result<(), FccStandardError> {
        let client = StandardContractClient::new(&env, &standard_id);
        client.try_fail_not_found()??;
        Ok(())
    }

    pub fn call_standard_success(env: Env, standard_id: Address) -> Result<u32, FccStandardError> {
        let client = StandardContractClient::new(&env, &standard_id);
        let result = client.try_succeed()??;
        Ok(result)
    }
}

fn setup_standard_contract(env: &Env) -> Address {
    env.register(StandardContract, ())
}

fn setup_fcc_standard_contract(env: &Env) -> Address {
    env.register(FccStandardContract, ())
}

#[test]
fn test_fcc_with_standard_contracterror_success() {
    let env = Env::default();
    let standard_id = setup_standard_contract(&env);
    let fcc_id = setup_fcc_standard_contract(&env);
    let client = FccStandardContractClient::new(&env, &fcc_id);

    let result = client.call_standard_success(&standard_id);
    assert_eq!(result, 42);
}

#[test]
fn test_fcc_with_standard_contracterror_error() {
    let env = Env::default();
    let standard_id = setup_standard_contract(&env);
    let fcc_id = setup_fcc_standard_contract(&env);
    let client = FccStandardContractClient::new(&env, &fcc_id);

    let result = client.try_call_standard_fail(&standard_id);
    assert!(result.is_err());

    let err = result.err().unwrap();
    let decoded: FccStandardError = err.expect("should be a contract error");

    assert_eq!(decoded, FccStandardError::Standard(StandardError::NotFound));
}

#[test]
fn test_fcc_standard_error_code_roundtrip() {
    let err = FccStandardError::Standard(StandardError::NotFound);
    let code = err.into_code();
    let decoded = FccStandardError::from_code(code).expect("Should decode");
    assert_eq!(err, decoded);

    let err2 = FccStandardError::Standard(StandardError::AlreadyExists);
    let code2 = err2.into_code();
    let decoded2 = FccStandardError::from_code(code2).expect("Should decode");
    assert_eq!(err2, decoded2);

    assert_ne!(code, code2);
}

// -----------------------------------------------------------------------------
// Test: #[transparent] with standard #[contracterror]
// -----------------------------------------------------------------------------

#[scerr]
pub enum TransparentStandardError {
    /// unauthorized operation
    Unauthorized,

    #[transparent]
    /// standard error
    Standard(#[from] StandardError),
}

#[contract]
pub struct TransparentStandardContract;

#[contractimpl]
impl TransparentStandardContract {
    pub fn direct_fail() -> Result<(), TransparentStandardError> {
        let inner_result: Result<(), StandardError> = Err(StandardError::InvalidArgument);
        inner_result?;
        Ok(())
    }

    pub fn direct_success() -> Result<u32, TransparentStandardError> {
        Ok(123)
    }
}

fn setup_transparent_standard_contract(env: &Env) -> Address {
    env.register(TransparentStandardContract, ())
}

#[test]
fn test_transparent_with_standard_contracterror_success() {
    let env = Env::default();
    let contract_id = setup_transparent_standard_contract(&env);
    let client = TransparentStandardContractClient::new(&env, &contract_id);

    let result = client.direct_success();
    assert_eq!(result, 123);
}

#[test]
fn test_transparent_with_standard_contracterror_error() {
    let env = Env::default();
    let contract_id = setup_transparent_standard_contract(&env);
    let client = TransparentStandardContractClient::new(&env, &contract_id);

    let result = client.try_direct_fail();
    assert!(result.is_err());

    let err = result.err().unwrap();
    let decoded: TransparentStandardError = err.expect("should be a contract error");

    assert_eq!(
        decoded,
        TransparentStandardError::Standard(StandardError::InvalidArgument)
    );
}

#[test]
fn test_transparent_standard_from_trait() {
    let inner = StandardError::NotFound;
    let outer: TransparentStandardError = inner.into();
    assert_eq!(
        outer,
        TransparentStandardError::Standard(StandardError::NotFound)
    );
}

#[test]
fn test_transparent_standard_error_code_roundtrip() {
    let err = TransparentStandardError::Standard(StandardError::NotFound);
    let code = err.into_code();
    let decoded = TransparentStandardError::from_code(code).expect("Should decode");
    assert_eq!(err, decoded);

    let err2 = TransparentStandardError::Standard(StandardError::InvalidArgument);
    let code2 = err2.into_code();
    let decoded2 = TransparentStandardError::from_code(code2).expect("Should decode");
    assert_eq!(err2, decoded2);

    assert_ne!(code, code2);
}

#[test]
fn test_transparent_standard_no_collision_with_unit_variants() {
    let unauthorized = TransparentStandardError::Unauthorized;
    let standard = TransparentStandardError::Standard(StandardError::NotFound);

    let code1 = unauthorized.into_code();
    let code2 = standard.into_code();

    assert_ne!(
        code1, code2,
        "Unit and wrapped variant codes must not collide"
    );

    let decoded1 = TransparentStandardError::from_code(code1).expect("Should decode");
    let decoded2 = TransparentStandardError::from_code(code2).expect("Should decode");

    assert_eq!(unauthorized, decoded1);
    assert_eq!(standard, decoded2);
}

// -----------------------------------------------------------------------------
// Test: Mixed scerr and non-scerr types with #[from_contract_client]
// -----------------------------------------------------------------------------

#[scerr]
pub enum StorageError {
    KeyNotFound,
    ValueTooLarge,
    QuotaExceeded,
}

#[contract]
pub struct StorageContract;

#[contractimpl]
impl StorageContract {
    pub fn get_value(_key: u32) -> Result<u32, StorageError> {
        Err(StorageError::KeyNotFound)
    }

    pub fn set_value(_key: u32, _value: u32) -> Result<(), StorageError> {
        Err(StorageError::ValueTooLarge)
    }

    pub fn check_quota() -> Result<(), StorageError> {
        Err(StorageError::QuotaExceeded)
    }
}

#[scerr]
pub enum MixedFccError {
    /// operation not permitted
    NotPermitted,

    /// invalid configuration
    InvalidConfig,

    #[from_contract_client]
    /// math operation error
    MathOp(MathError),

    #[from_contract_client]
    /// standard operation error
    StandardOp(StandardError),

    #[from_contract_client]
    /// storage operation error
    StorageOp(StorageError),

    #[from_contract_client]
    /// logic operation error
    LogicOp(LogicError),
}

#[contract]
pub struct MixedFccContract;

#[contractimpl]
impl MixedFccContract {
    pub fn call_math(
        env: Env,
        math_id: Address,
        num: i64,
        denom: i64,
    ) -> Result<i64, MixedFccError> {
        let client = MathClient::new(&env, &math_id);
        let result = client.try_safe_div(&num, &denom)??;
        Ok(result)
    }

    pub fn call_standard_fail(env: Env, standard_id: Address) -> Result<(), MixedFccError> {
        let client = StandardContractClient::new(&env, &standard_id);
        client.try_fail_not_found()??;
        Ok(())
    }

    pub fn call_storage_get(env: Env, storage_id: Address, key: u32) -> Result<u32, MixedFccError> {
        let client = StorageContractClient::new(&env, &storage_id);
        let result = client.try_get_value(&key)??;
        Ok(result)
    }

    pub fn call_logic(env: Env, logic_id: Address, x: i64) -> Result<(), MixedFccError> {
        let client = LogicClient::new(&env, &logic_id);
        client.try_check_positive(&x)??;
        Ok(())
    }

    pub fn chain_calls(
        env: Env,
        math_id: Address,
        storage_id: Address,
        logic_id: Address,
        num: i64,
        denom: i64,
    ) -> Result<u32, MixedFccError> {
        let math_client = MathClient::new(&env, &math_id);
        let div_result = math_client.try_safe_div(&num, &denom)??;

        let storage_client = StorageContractClient::new(&env, &storage_id);
        let storage_result = storage_client.try_get_value(&(div_result as u32))??;

        let logic_client = LogicClient::new(&env, &logic_id);
        logic_client.try_check_positive(&(storage_result as i64))??;

        Ok(storage_result)
    }
}

fn setup_storage_contract(env: &Env) -> Address {
    env.register(StorageContract, ())
}

fn setup_mixed_fcc_contract(env: &Env) -> Address {
    env.register(MixedFccContract, ())
}

#[test]
fn test_mixed_fcc_sequential_codes() {
    // MixedFccError layout:
    // NotPermitted = 1
    // InvalidConfig = 2
    // MathOp (FCC, 2 variants): offset=3, count=2 → codes 3,4
    // StandardOp (FCC, 3 variants): offset=5, count=3 → codes 5,6,7
    // StorageOp (FCC, 3 variants): offset=8, count=3 → codes 8,9,10
    // LogicOp (FCC, 2 variants): offset=11, count=2 → codes 11,12
    // Aborted (auto) = 13
    // UnknownError (auto) = 14

    assert_eq!(MixedFccError::NotPermitted.into_code(), 1);
    assert_eq!(MixedFccError::InvalidConfig.into_code(), 2);
    assert_eq!(
        MixedFccError::MathOp(MathError::DivisionByZero).into_code(),
        3
    );
    assert_eq!(
        MixedFccError::MathOp(MathError::NegativeInput).into_code(),
        4
    );
    assert_eq!(
        MixedFccError::StandardOp(StandardError::NotFound).into_code(),
        5
    );
    assert_eq!(
        MixedFccError::StandardOp(StandardError::AlreadyExists).into_code(),
        6
    );
    assert_eq!(
        MixedFccError::StandardOp(StandardError::InvalidArgument).into_code(),
        7
    );
    assert_eq!(
        MixedFccError::StorageOp(StorageError::KeyNotFound).into_code(),
        8
    );
    assert_eq!(
        MixedFccError::StorageOp(StorageError::ValueTooLarge).into_code(),
        9
    );
    assert_eq!(
        MixedFccError::StorageOp(StorageError::QuotaExceeded).into_code(),
        10
    );
    assert_eq!(
        MixedFccError::LogicOp(LogicError::InvalidState).into_code(),
        11
    );
    assert_eq!(
        MixedFccError::LogicOp(LogicError::Unauthorized).into_code(),
        12
    );
    assert_eq!(MixedFccError::Aborted.into_code(), 13);
    assert_eq!(MixedFccError::UnknownError.into_code(), 14);
}

#[test]
fn test_mixed_fcc_math_error_scerr() {
    let env = Env::default();
    let math_id = setup_math_contract(&env);
    let mixed_id = setup_mixed_fcc_contract(&env);
    let client = MixedFccContractClient::new(&env, &mixed_id);

    let result = client.try_call_math(&math_id, &10, &0);
    assert!(result.is_err());

    let err = result.err().unwrap();
    let decoded: MixedFccError = err.expect("should be contract error");
    assert_eq!(decoded, MixedFccError::MathOp(MathError::DivisionByZero));
}

#[test]
fn test_mixed_fcc_standard_error() {
    let env = Env::default();
    let standard_id = setup_standard_contract(&env);
    let mixed_id = setup_mixed_fcc_contract(&env);
    let client = MixedFccContractClient::new(&env, &mixed_id);

    let result = client.try_call_standard_fail(&standard_id);
    assert!(result.is_err());

    let err = result.err().unwrap();
    let decoded: MixedFccError = err.expect("should be contract error");
    assert_eq!(decoded, MixedFccError::StandardOp(StandardError::NotFound));
}

#[test]
fn test_mixed_fcc_storage_error() {
    let env = Env::default();
    let storage_id = setup_storage_contract(&env);
    let mixed_id = setup_mixed_fcc_contract(&env);
    let client = MixedFccContractClient::new(&env, &mixed_id);

    let result = client.try_call_storage_get(&storage_id, &42);
    assert!(result.is_err());

    let err = result.err().unwrap();
    let decoded: MixedFccError = err.expect("should be contract error");
    assert_eq!(decoded, MixedFccError::StorageOp(StorageError::KeyNotFound));
}

#[test]
fn test_mixed_fcc_logic_error_scerr() {
    let env = Env::default();
    let logic_id = setup_logic_contract(&env);
    let mixed_id = setup_mixed_fcc_contract(&env);
    let client = MixedFccContractClient::new(&env, &mixed_id);

    let result = client.try_call_logic(&logic_id, &-5);
    assert!(result.is_err());

    let err = result.err().unwrap();
    let decoded: MixedFccError = err.expect("should be contract error");
    assert_eq!(decoded, MixedFccError::LogicOp(LogicError::InvalidState));
}

#[test]
fn test_mixed_fcc_error_codes_no_collision() {
    let all_errors = [
        MixedFccError::NotPermitted,
        MixedFccError::InvalidConfig,
        MixedFccError::MathOp(MathError::DivisionByZero),
        MixedFccError::MathOp(MathError::NegativeInput),
        MixedFccError::StandardOp(StandardError::NotFound),
        MixedFccError::StandardOp(StandardError::AlreadyExists),
        MixedFccError::StorageOp(StorageError::KeyNotFound),
        MixedFccError::StorageOp(StorageError::ValueTooLarge),
        MixedFccError::LogicOp(LogicError::InvalidState),
        MixedFccError::LogicOp(LogicError::Unauthorized),
        MixedFccError::Aborted,
        MixedFccError::UnknownError,
    ];

    let codes: Vec<_> = all_errors.iter().map(|e| e.into_code()).collect();

    for i in 0..codes.len() {
        for j in (i + 1)..codes.len() {
            assert_ne!(
                codes[i], codes[j],
                "Code collision between error variants at indices {} and {}",
                i, j
            );
        }
    }
}

#[test]
fn test_mixed_fcc_error_code_roundtrip() {
    let errors = [
        MixedFccError::NotPermitted,
        MixedFccError::InvalidConfig,
        MixedFccError::MathOp(MathError::DivisionByZero),
        MixedFccError::MathOp(MathError::NegativeInput),
        MixedFccError::StandardOp(StandardError::NotFound),
        MixedFccError::StandardOp(StandardError::AlreadyExists),
        MixedFccError::StandardOp(StandardError::InvalidArgument),
        MixedFccError::StorageOp(StorageError::KeyNotFound),
        MixedFccError::StorageOp(StorageError::ValueTooLarge),
        MixedFccError::StorageOp(StorageError::QuotaExceeded),
        MixedFccError::LogicOp(LogicError::InvalidState),
        MixedFccError::LogicOp(LogicError::Unauthorized),
        MixedFccError::Aborted,
        MixedFccError::UnknownError,
    ];

    for err in errors {
        let code = err.into_code();
        let decoded = MixedFccError::from_code(code).expect("Should decode");
        assert_eq!(
            err, decoded,
            "Roundtrip failed for {:?} (code {})",
            err, code
        );
    }
}

#[test]
fn test_mixed_fcc_chain_calls_first_fails() {
    let env = Env::default();
    let math_id = setup_math_contract(&env);
    let storage_id = setup_storage_contract(&env);
    let logic_id = setup_logic_contract(&env);
    let mixed_id = setup_mixed_fcc_contract(&env);
    let client = MixedFccContractClient::new(&env, &mixed_id);

    let result = client.try_chain_calls(&math_id, &storage_id, &logic_id, &10, &0);
    assert!(result.is_err());

    let err = result.err().unwrap();
    let decoded: MixedFccError = err.expect("should be contract error");
    assert_eq!(decoded, MixedFccError::MathOp(MathError::DivisionByZero));
}

#[test]
fn test_mixed_fcc_chain_calls_second_fails() {
    let env = Env::default();
    let math_id = setup_math_contract(&env);
    let storage_id = setup_storage_contract(&env);
    let logic_id = setup_logic_contract(&env);
    let mixed_id = setup_mixed_fcc_contract(&env);
    let client = MixedFccContractClient::new(&env, &mixed_id);

    let result = client.try_chain_calls(&math_id, &storage_id, &logic_id, &10, &2);
    assert!(result.is_err());

    let err = result.err().unwrap();
    let decoded: MixedFccError = err.expect("should be contract error");
    assert_eq!(decoded, MixedFccError::StorageOp(StorageError::KeyNotFound));
}

// -----------------------------------------------------------------------------
// Tests: ContractErrorSpec, SPEC_TREE, and TOTAL_CODES
// -----------------------------------------------------------------------------

use soroban_sdk_tools::error::ContractErrorSpec;

#[test]
fn basic_mode_spec_entries() {
    // Basic-mode enums should have SPEC_ENTRIES matching their variants
    let math_entries = MathError::SPEC_ENTRIES;
    assert_eq!(math_entries.len(), 2);
    assert_eq!(math_entries[0].code, 1);
    assert_eq!(math_entries[0].name, "DivisionByZero");
    assert_eq!(math_entries[1].code, 2);
    assert_eq!(math_entries[1].name, "NegativeInput");

    let calc_entries = CalcError::SPEC_ENTRIES;
    assert_eq!(calc_entries.len(), 2);
    assert_eq!(calc_entries[0].code, 1);
    assert_eq!(calc_entries[0].name, "Overflow");
    assert_eq!(calc_entries[1].code, 2);
    assert_eq!(calc_entries[1].name, "Underflow");
}

#[test]
fn basic_mode_total_codes() {
    // For basic-mode enums, TOTAL_CODES equals variant count
    assert_eq!(MathError::TOTAL_CODES, 2);
    assert_eq!(CalcError::TOTAL_CODES, 2);
    assert_eq!(LogicError::TOTAL_CODES, 2);
    assert_eq!(StandardError::TOTAL_CODES, 3);
    assert_eq!(StorageError::TOTAL_CODES, 3);
}

#[test]
fn basic_mode_spec_tree() {
    // Basic-mode SPEC_TREE should have all leaves, no groups
    let tree = MathError::SPEC_TREE;
    assert_eq!(tree.len(), 2);
    assert!(tree[0].children.is_empty(), "leaf should have no children");
    assert!(tree[1].children.is_empty(), "leaf should have no children");
    assert_eq!(tree[0].code, 1);
    assert_eq!(tree[0].name, "DivisionByZero");
    assert_eq!(tree[1].code, 2);
    assert_eq!(tree[1].name, "NegativeInput");
}

#[test]
fn root_mode_spec_entries_only_unit_variants() {
    // Root-mode SPEC_ENTRIES should contain only unit-like variants (not wrapped)
    let entries = OuterError::SPEC_ENTRIES;

    // OuterError has: Unauthorized (unit), Math (wrapped), MathClient (wrapped),
    // Calc (wrapped), LogicClient (wrapped), Aborted (auto unit), UnknownError (auto unit)
    // → only 3 unit-like entries
    assert_eq!(entries.len(), 3);
    assert_eq!(entries[0].name, "Unauthorized");
    assert_eq!(entries[1].name, "Aborted");
    assert_eq!(entries[2].name, "UnknownError");
}

#[test]
fn root_mode_total_codes() {
    // OuterError: Unauthorized=1, Math[2]=2-3, MathClient[2]=4-5,
    // Calc[2]=6-7, LogicClient[2]=8-9, Aborted=10, UnknownError=11
    assert_eq!(OuterError::TOTAL_CODES, 11);

    // OuterRootError: OuterUnauthorized=1, Inner[2]=2-3, Aborted=4, UnknownError=5
    assert_eq!(OuterRootError::TOTAL_CODES, 5);

    // AutoError: InvalidInput=1, Math[2]=2-3, Aborted=4, UnknownError=5
    assert_eq!(AutoError::TOTAL_CODES, 5);

    // MixedFccError: NotPermitted=1, InvalidConfig=2, MathOp[2]=3-4,
    // StandardOp[3]=5-7, StorageOp[3]=8-10, LogicOp[2]=11-12,
    // Aborted=13, UnknownError=14
    assert_eq!(MixedFccError::TOTAL_CODES, 14);
}

#[test]
fn root_mode_spec_tree_structure() {
    // OuterRootError tree should have:
    // - OuterUnauthorized (leaf)
    // - Inner (group with InnerRootError's tree)
    // - Aborted (leaf)
    // - UnknownError (leaf)
    let tree = OuterRootError::SPEC_TREE;
    assert_eq!(tree.len(), 4);

    // Leaf: OuterUnauthorized
    assert!(tree[0].children.is_empty());
    assert_eq!(tree[0].name, "OuterUnauthorized");
    assert_eq!(tree[0].code, 1);

    // Group: Inner (wrapping InnerRootError)
    assert!(!tree[1].children.is_empty(), "Inner should be a group node");
    assert_eq!(tree[1].name, "Inner");
    assert_eq!(tree[1].code, 2); // offset
    assert_eq!(tree[1].children.len(), 2);
    assert_eq!(tree[1].children[0].name, "InnerUnauthorized");
    assert_eq!(tree[1].children[1].name, "InnerInvalidState");

    // Leaf: Aborted
    assert!(tree[2].children.is_empty());
    assert_eq!(tree[2].name, "Aborted");
    assert_eq!(tree[2].code, 4);

    // Leaf: UnknownError
    assert!(tree[3].children.is_empty());
    assert_eq!(tree[3].name, "UnknownError");
    assert_eq!(tree[3].code, 5);
}

#[test]
fn root_mode_spec_tree_with_multiple_fcc_variants() {
    // MixedFccError tree should have 8 nodes:
    // NotPermitted, InvalidConfig, MathOp(group), StandardOp(group),
    // StorageOp(group), LogicOp(group), Aborted, UnknownError
    let tree = MixedFccError::SPEC_TREE;
    assert_eq!(tree.len(), 8);

    // Unit leaves
    assert!(tree[0].children.is_empty());
    assert_eq!(tree[0].name, "NotPermitted");
    assert!(tree[1].children.is_empty());
    assert_eq!(tree[1].name, "InvalidConfig");

    // Group: MathOp
    assert!(!tree[2].children.is_empty());
    assert_eq!(tree[2].name, "MathOp");
    assert_eq!(tree[2].children.len(), 2);

    // Group: StandardOp
    assert!(!tree[3].children.is_empty());
    assert_eq!(tree[3].name, "StandardOp");
    assert_eq!(tree[3].children.len(), 3);

    // Group: StorageOp
    assert!(!tree[4].children.is_empty());
    assert_eq!(tree[4].name, "StorageOp");
    assert_eq!(tree[4].children.len(), 3);

    // Group: LogicOp
    assert!(!tree[5].children.is_empty());
    assert_eq!(tree[5].name, "LogicOp");
    assert_eq!(tree[5].children.len(), 2);

    // Auto-generated unit leaves
    assert!(tree[6].children.is_empty());
    assert_eq!(tree[6].name, "Aborted");
    assert!(tree[7].children.is_empty());
    assert_eq!(tree[7].name, "UnknownError");
}

// -----------------------------------------------------------------------------
// Tests: Cross-contract error code space overlap
// -----------------------------------------------------------------------------

#[scerr]
pub enum FiveCodeError {
    Fault1,
    Fault2,
    Fault3,
    Fault4,
    Fault5 = 5,
}

// OverlapCallerError layout:
// OwnFault1 = 1, OwnFault2 = 2, OwnFault3 = 3, OwnFault4 = 4, OwnFault5 = 5
// Imported (FCC, 5 variants): offset=6, count=5 -> codes 6-10
// Aborted (auto) = 11, UnknownError (auto) = 12
#[scerr]
pub enum OverlapCallerError {
    OwnFault1,
    OwnFault2,
    OwnFault3,
    OwnFault4,
    OwnFault5,

    #[from_contract_client]
    Imported(FiveCodeError),
}

#[test]
fn overlap_caller_error_layout() {
    assert_eq!(OverlapCallerError::OwnFault1.into_code(), 1);
    assert_eq!(OverlapCallerError::OwnFault2.into_code(), 2);
    assert_eq!(OverlapCallerError::OwnFault3.into_code(), 3);
    assert_eq!(OverlapCallerError::OwnFault4.into_code(), 4);
    assert_eq!(OverlapCallerError::OwnFault5.into_code(), 5);
    assert_eq!(
        OverlapCallerError::Imported(FiveCodeError::Fault1).into_code(),
        6
    );
    assert_eq!(
        OverlapCallerError::Imported(FiveCodeError::Fault2).into_code(),
        7
    );
    assert_eq!(
        OverlapCallerError::Imported(FiveCodeError::Fault3).into_code(),
        8
    );
    assert_eq!(
        OverlapCallerError::Imported(FiveCodeError::Fault4).into_code(),
        9
    );
    assert_eq!(
        OverlapCallerError::Imported(FiveCodeError::Fault5).into_code(),
        10
    );
    assert_eq!(OverlapCallerError::Aborted.into_code(), 11);
    assert_eq!(OverlapCallerError::UnknownError.into_code(), 12);
}

#[test]
fn unknown_error_catches_out_of_range_cross_contract_code() {
    let err = OverlapCallerError::from(soroban_sdk::InvokeError::Contract(42));
    assert_eq!(err, OverlapCallerError::UnknownError);
}

// From<InvokeError> uses from_code() which is needed for SDK client decoding of
// our own errors. It can't distinguish our codes from an alien contract's codes.
// Cross-contract decoding goes through From<Result<E, InvokeError>> (via ??)
// which does NOT use from_code().
#[test]
fn from_invoke_error_cannot_distinguish_own_codes_from_alien_codes() {
    // from_code(3) matches OwnFault3 in our code space
    let err = OverlapCallerError::from(soroban_sdk::InvokeError::Contract(3));
    assert_eq!(err, OverlapCallerError::OwnFault3);

    // codes beyond our space correctly fall through to UnknownError
    let err = OverlapCallerError::from(soroban_sdk::InvokeError::Contract(42));
    assert_eq!(err, OverlapCallerError::UnknownError);
}

#[test]
fn double_q_correctly_decodes_cross_contract_errors() {
    // Ok branch: SDK decoded the raw code as the expected inner type
    let decoded: OverlapCallerError =
        From::from(Ok(FiveCodeError::Fault5) as Result<FiveCodeError, soroban_sdk::InvokeError>);
    assert_eq!(decoded, OverlapCallerError::Imported(FiveCodeError::Fault5));

    let decoded: OverlapCallerError =
        From::from(Ok(FiveCodeError::Fault1) as Result<FiveCodeError, soroban_sdk::InvokeError>);
    assert_eq!(decoded, OverlapCallerError::Imported(FiveCodeError::Fault1));

    // Err branch: SDK could not decode (e.g. contract upgraded with new codes)
    let decoded: OverlapCallerError = From::from(Err(soroban_sdk::InvokeError::Contract(99))
        as Result<FiveCodeError, soroban_sdk::InvokeError>);
    assert_eq!(decoded, OverlapCallerError::UnknownError);

    // Err branch: abort
    let decoded: OverlapCallerError = From::from(
        Err(soroban_sdk::InvokeError::Abort) as Result<FiveCodeError, soroban_sdk::InvokeError>
    );
    assert_eq!(decoded, OverlapCallerError::Aborted);
}

// The ?? Err branch handles unknown codes directly instead of delegating to
// From<InvokeError>. Without this, from_code() would remap raw code 6 to
// inner_code 1 (6 - offset + 1 = 1), silently giving Imported(Fault1).
#[test]
fn double_q_err_branch_does_not_mismatch_via_from_code() {
    // code 6 is outside FiveCodeError's range but inside our FCC range [6, 11)
    let decoded: OverlapCallerError = From::from(Err(soroban_sdk::InvokeError::Contract(6))
        as Result<FiveCodeError, soroban_sdk::InvokeError>);
    assert_eq!(decoded, OverlapCallerError::UnknownError);
    assert_ne!(decoded, OverlapCallerError::Imported(FiveCodeError::Fault1));

    // code 5 matches our OwnFault5 via from_code but should be unknown here
    let decoded: OverlapCallerError = From::from(Err(soroban_sdk::InvokeError::Contract(5))
        as Result<FiveCodeError, soroban_sdk::InvokeError>);
    assert_eq!(decoded, OverlapCallerError::UnknownError);
    assert_ne!(decoded, OverlapCallerError::OwnFault5);
}
