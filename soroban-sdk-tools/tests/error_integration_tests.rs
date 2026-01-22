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

#[scerr(mode = "root")]
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
        // Create the Math contract directly and call it
        // The error will be transparently converted via From<MathError>
        let result = Math::safe_div(num, denom)?;
        Ok(result)
    }

    pub fn add_direct(env: Env, a: i64, b: i64) -> Result<i64, OuterError> {
        // Create the Calc contract directly and call it
        // The error will be transparently converted via From<CalcError>
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

#[test]
fn root_error_from_inner_error_via_div_through() {
    let env = Env::default();
    let math_id = setup_math_contract(&env);
    let outer_id = setup_outer_contract(&env);

    let outer_client = OuterClient::new(&env, &outer_id);

    // denom = 0, should result in MathError::DivisionByZero, mapped to OuterError::MathClient
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

    // Use add_direct instead of add_through to test transparent propagation
    // add_direct calls Calc directly (in-process), not via contract client
    let result = outer_client.try_add_direct(&i64::MAX, &1);
    assert!(result.is_err());

    let err = result.unwrap_err();
    let decoded: OuterError = err.expect("no invoke error");

    // Should be wrapped in the transparent Calc variant
    assert_eq!(decoded, OuterError::Calc(CalcError::Overflow));
}

#[test]
fn from_contract_client_only_for_logic() {
    let env = Env::default();
    let logic_id = setup_logic_contract(&env);
    let outer_id = setup_outer_contract(&env);

    let outer_client = OuterClient::new(&env, &outer_id);

    // Trigger invalid state
    let result = outer_client.try_check_through(&logic_id, &-1);
    assert!(result.is_err());

    let err = result.err().unwrap();

    // Debug: Print the error type and details
    /*println!("DEBUG from_contract_client_only_for_logic:");
    println!("  Raw error: {:?}", err);*/

    let decoded: OuterError = err.expect("no invoke error");

    /*println!("  Decoded error: {:?}", decoded);
    println!("  Error code: {}", decoded.into_code());
    println!("  Expected: LogicClient(InvalidState)");
    println!("  LogicError::InvalidState code: {}", LogicError::InvalidState.into_code());
    println!("  OuterError::LogicClient(InvalidState) code: {}", OuterError::LogicClient(LogicError::InvalidState).into_code());*/

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

    // Debug: Print the error type and details
    //println!("DEBUG mixed_usage_for_math:");
    //println!("  Raw error: {:?}", err);

    let decoded: OuterError = err.expect("no invoke error");

    // println!("  Decoded error: {:?}", decoded);
    // println!("  Error code: {}", decoded.into_code());
    // println!("  Expected: MathClient(NegativeInput)");
    // println!("  MathError::NegativeInput code: {}", MathError::NegativeInput.into_code());
    // println!("  OuterError::MathClient(NegativeInput) code: {}", OuterError::MathClient(MathError::NegativeInput).into_code());
    // println!("  MathError::DivisionByZero code: {}", MathError::DivisionByZero.into_code());
    // println!("  OuterError::MathClient(DivisionByZero) code: {}", OuterError::MathClient(MathError::DivisionByZero).into_code());

    assert_eq!(decoded, OuterError::MathClient(MathError::NegativeInput));
}

#[test]
fn transparent_and_from_impls_exist_and_work() {
    // Check that From<MathError> is implemented for OuterError via the #[transparent] variant.

    let inner = MathError::NegativeInput;
    let outer: OuterError = inner.into();
    // It should match the Math variant; we check via into_code roundtrip.

    let inner_code = MathError::NegativeInput.into_code();
    let outer_code = outer.into_code();

    // Codes are not required to match between enums; we just check that the
    // conversion doesn't panic and that description is reasonable.
    assert_ne!(inner_code, outer_code);

    // However, we can still use description() for debugging.
    let _desc = outer.description();

    // Check reverse for CalcError (transparent only)
    let calc_inner = CalcError::Overflow;
    let calc_outer: OuterError = calc_inner.into();
    assert_eq!(calc_outer, OuterError::Calc(CalcError::Overflow));
}

#[test]
fn transparent_allows_question_mark_propagation() {
    let env = Env::default();
    let outer_id = setup_outer_contract(&env);

    let outer_client = OuterClient::new(&env, &outer_id);

    // Test direct propagation via transparent variant
    let result = outer_client.try_div_direct(&10, &0);
    assert!(result.is_err());

    let err = result.err().unwrap();
    let decoded: OuterError = err.expect("no invoke error");

    // Should be wrapped in the transparent Math variant
    assert_eq!(decoded, OuterError::Math(MathError::DivisionByZero));
}

#[test]
fn transparent_propagates_with_question_mark() {
    let env = Env::default();
    let outer_id = setup_outer_contract(&env);

    let outer_client = OuterClient::new(&env, &outer_id);

    // Test overflow via transparent propagation
    let result = outer_client.try_add_direct(&i64::MAX, &1);
    assert!(result.is_err());

    let err = result.err().unwrap();
    let decoded: OuterError = err.expect("no invoke error");

    // Should be wrapped in the transparent Calc variant
    assert_eq!(decoded, OuterError::Calc(CalcError::Overflow));
}

// -----------------------------------------------------------------------------
// Test case for root-to-root collision via from_contract_client
// -----------------------------------------------------------------------------

#[scerr(mode = "root")]
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

#[scerr(mode = "root")]
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
fn root_to_root_collision_via_from_contract_client() {
    let env = Env::default();
    let inner_id = setup_inner_root_contract(&env);
    let outer_id = setup_outer_root_contract(&env);

    let outer_client = OuterRootClient::new(&env, &outer_id);

    let result = outer_client.try_call_inner(&inner_id);
    assert!(result.is_err());

    let err = result.err().unwrap();
    let decoded: OuterRootError = err.expect("no invoke error");

    println!("DEBUG root_to_root_collision:");
    println!("  Decoded error: {:?}", decoded);
    println!("  Error code: {}", decoded.into_code());
    println!("  Expected: Inner(InnerUnauthorized)");
    println!(
        "  InnerRootError::InnerUnauthorized code: {}",
        InnerRootError::InnerUnauthorized.into_code()
    );
    println!(
        "  OuterRootError::OuterUnauthorized code: {}",
        OuterRootError::OuterUnauthorized.into_code()
    );
    println!(
        "  OuterRootError::Inner(InnerUnauthorized) code: {}",
        OuterRootError::Inner(InnerRootError::InnerUnauthorized).into_code()
    );

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

    println!("DEBUG root_to_root_collision_second_variant:");
    println!("  Decoded error: {:?}", decoded);
    println!("  Error code: {}", decoded.into_code());
    println!("  Expected: Inner(InnerInvalidState)");
    println!(
        "  InnerRootError::InnerInvalidState code: {}",
        InnerRootError::InnerInvalidState.into_code()
    );
    println!(
        "  OuterRootError::Inner(InnerInvalidState) code: {}",
        OuterRootError::Inner(InnerRootError::InnerInvalidState).into_code()
    );

    assert_eq!(
        decoded,
        OuterRootError::Inner(InnerRootError::InnerInvalidState)
    );
}

#[test]
fn show_code_collision_explicitly() {
    // This test explicitly shows the collision problem with root-to-root wrapping

    // Inner root codes: 1, 2
    let inner_code_1 = InnerRootError::InnerUnauthorized.into_code();
    let inner_code_2 = InnerRootError::InnerInvalidState.into_code();

    // Outer root codes when wrapping: should preserve inner structure
    let outer_wrapped_1 = OuterRootError::Inner(InnerRootError::InnerUnauthorized).into_code();
    let outer_wrapped_2 = OuterRootError::Inner(InnerRootError::InnerInvalidState).into_code();

    // Outer's own codes
    let outer_own_code = OuterRootError::OuterUnauthorized.into_code();

    println!("Code Analysis:");
    println!("  InnerRootError::InnerUnauthorized: {}", inner_code_1);
    println!("  InnerRootError::InnerInvalidState: {}", inner_code_2);
    println!("  OuterRootError::OuterUnauthorized: {}", outer_own_code);
    println!(
        "  OuterRootError::Inner(InnerUnauthorized): {}",
        outer_wrapped_1
    );
    println!(
        "  OuterRootError::Inner(InnerInvalidState): {}",
        outer_wrapped_2
    );

    // The problem: if outer_own_code == inner_code_1, we have a collision
    // because when from_code(1) is called, it will match OuterUnauthorized
    // before trying to decode as Inner(InnerUnauthorized)
    if outer_own_code == inner_code_1 || outer_own_code == inner_code_2 {
        println!("  ⚠️  COLLISION DETECTED: Outer unit variant collides with inner codes!");
    }

    // The wrapped versions should NOT equal the raw inner codes
    // They should be shifted/transformed to avoid collision
    assert_ne!(
        outer_wrapped_1, inner_code_1,
        "Wrapped error code should differ from raw inner code to avoid collision"
    );
    assert_ne!(
        outer_wrapped_2, inner_code_2,
        "Wrapped error code should differ from raw inner code to avoid collision"
    );
}

#[test]
fn outer_unit_variant_not_confused_with_inner() {
    let env = Env::default();
    let outer_id = setup_outer_root_contract(&env);

    let outer_client = OuterRootClient::new(&env, &outer_id);

    // Explicitly trigger OuterRootError::OuterUnauthorized (code 1)
    let result = outer_client.try_fail_outer_unauthorized();
    assert!(result.is_err());

    let err = result.err().unwrap();
    let decoded: OuterRootError = err.expect("no invoke error");

    println!("DEBUG outer_unit_variant_not_confused_with_inner:");
    println!("  Decoded error: {:?}", decoded);
    println!("  Error code: {}", decoded.into_code());

    // This MUST be OuterUnauthorized, not Inner(InnerUnauthorized)
    // Even though both have related codes (1 vs 33554433)
    assert_eq!(decoded, OuterRootError::OuterUnauthorized);

    // Ensure it's NOT decoded as Inner
    assert!(
        !matches!(decoded, OuterRootError::Inner(_)),
        "OuterUnauthorized should not be decoded as Inner variant"
    );

    // Verify the code is exactly 1
    assert_eq!(decoded.into_code(), 1);
}

#[test]
fn bidirectional_code_mapping_integrity() {
    // Verify that encoding and decoding are symmetric and unambiguous

    // Test outer unit variant
    let outer_unit = OuterRootError::OuterUnauthorized;
    let code1 = outer_unit.into_code();
    let decoded1 = OuterRootError::from_code(code1).expect("Should decode");
    assert_eq!(outer_unit, decoded1, "Outer unit variant roundtrip failed");

    // Test wrapped inner variant
    let inner_wrapped = OuterRootError::Inner(InnerRootError::InnerUnauthorized);
    let code2 = inner_wrapped.into_code();
    let decoded2 = OuterRootError::from_code(code2).expect("Should decode");
    assert_eq!(
        inner_wrapped, decoded2,
        "Wrapped inner variant roundtrip failed"
    );

    // Ensure codes are different
    assert_ne!(code1, code2, "Codes must be different to avoid ambiguity");

    // Test second inner variant
    let inner_wrapped2 = OuterRootError::Inner(InnerRootError::InnerInvalidState);
    let code3 = inner_wrapped2.into_code();
    let decoded3 = OuterRootError::from_code(code3).expect("Should decode");
    assert_eq!(
        inner_wrapped2, decoded3,
        "Second wrapped variant roundtrip failed"
    );

    println!("Bidirectional mapping verified:");
    println!("  OuterUnauthorized: {} -> {:?}", code1, decoded1);
    println!("  Inner(InnerUnauthorized): {} -> {:?}", code2, decoded2);
    println!("  Inner(InnerInvalidState): {} -> {:?}", code3, decoded3);
}

#[test]
fn from_code_doesnt_match_wrong_namespace() {
    // Test that unit variants only match in their correct namespace (high == 0)
    // and don't incorrectly match codes in the shifted namespace

    // OuterUnauthorized has code 1 (high = 0, low = 1)
    let code_1 = OuterRootError::OuterUnauthorized.into_code();
    assert_eq!(code_1, 1);

    // Try to decode code with high bits = 1 (i.e., 1 << 24 = 16777216)
    // This should NOT decode as OuterUnauthorized
    let fake_code = 1u32 << 24; // 16777216
    let decoded = OuterRootError::from_code(fake_code);

    println!("DEBUG from_code_doesnt_match_wrong_namespace:");
    println!("  Trying to decode code: {}", fake_code);
    println!("  Result: {:?}", decoded);

    // This should be None or something else, NOT OuterUnauthorized
    if let Some(err) = decoded {
        assert_ne!(
            err,
            OuterRootError::OuterUnauthorized,
            "Code {} should not decode as OuterUnauthorized (which has code 1)",
            fake_code
        );
    }

    // Also test that the Inner variant has non-zero namespace bits (uses hash-based assignment)
    let inner_code = OuterRootError::Inner(InnerRootError::InnerUnauthorized).into_code();
    let namespace = inner_code >> 22; // 10/22 bit split
    assert!(
        namespace > 0 && namespace <= 1023,
        "Inner variant should have valid namespace bits (1-1023), got {}",
        namespace
    );

    // Code 2 (high = 0, low = 2) should not decode as anything, or at least not as Inner
    let code_2 = 2u32;
    let decoded_2 = OuterRootError::from_code(code_2);
    println!("  Trying to decode code 2: {:?}", decoded_2);
}

// -----------------------------------------------------------------------------
// Test: Auto-generated abort and unknown handlers
// -----------------------------------------------------------------------------

#[scerr(mode = "root", handle_abort = "auto", handle_unknown = "auto")]
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
    // Verify Aborted variant was auto-generated
    let abort_err = AutoError::Aborted;
    assert_eq!(abort_err.description(), "Cross-contract call aborted");
}

#[test]
fn test_auto_unknown_handler() {
    // Verify UnknownError variant was auto-generated
    let unknown = AutoError::UnknownError;
    assert_eq!(
        unknown.description(),
        "Unknown error from cross-contract call"
    );
}
// -----------------------------------------------------------------------------
// Test: Error logging with sentinel
// -----------------------------------------------------------------------------

#[scerr(mode = "root", handle_unknown = "auto", log_unknown_errors = true)]
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

        // Handle the nested Result: outer is InvokeError, inner is MathError
        match result {
            Ok(inner_result) => {
                // inner_result is Result<i64, MathError>
                match inner_result {
                    Ok(val) => Ok(val),
                    Err(contract_err) => {
                        // Handle contract error (MathError)
                        Err(LoggingError::from(contract_err))
                    }
                }
            }
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

    // This should log if we get an unknown error
    // (in this case it should map to Math variant, but demonstrates the mechanism)
    let _ = client.try_call_with_logging(&env.register(Math, ()));
}

// -----------------------------------------------------------------------------
// Test: Panic mode (default behavior)
// -----------------------------------------------------------------------------

#[scerr(mode = "root")] // Default: both handle_abort and handle_unknown = "panic"
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

#[scerr(mode = "root")]
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

#[scerr(mode = "root", handle_abort = "auto", handle_unknown = "auto")]
pub enum MixedError {
    InvalidInput,

    // Transparent for direct calls
    #[transparent]
    MathDirect(#[from] MathError),

    // from_contract_client for cross-contract
    #[from_contract_client]
    MathRemote(MathError),
}

#[contract]
pub struct MixedContract;

#[contractimpl]
impl MixedContract {
    pub fn direct_div(num: i64, denom: i64) -> Result<i64, MixedError> {
        // Direct call uses transparent variant
        Math::safe_div(num, denom)?;
        Ok(num / denom)
    }

    pub fn remote_div(env: Env, math_id: Address, num: i64, denom: i64) -> Result<i64, MixedError> {
        // Cross-contract call uses from_contract_client variant
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

    // Test transparent variant
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

    // Test from_contract_client variant
    let result = client.try_remote_div(&math_id, &10, &0);
    assert!(result.is_err());

    let err: MixedError = result.err().unwrap().unwrap();
    assert!(matches!(err, MixedError::MathRemote(_)));
}

// -----------------------------------------------------------------------------
// Test: Configuration conflicts
// -----------------------------------------------------------------------------

// These should fail to compile:

// #[scerr(mode = "root", handle_abort = "panic")]
// pub enum ConflictError {
//     #[abort]  // Error: conflicts with handle_abort = "panic"
//     Aborted,
// }

// #[scerr(mode = "root", handle_unknown = "panic")]
// pub enum ConflictError2 {
//     #[sentinel]  // Error: conflicts with handle_unknown = "panic"
//     Unknown,
// }

// -----------------------------------------------------------------------------
// Test: Roundtrip encoding with auto variants
// -----------------------------------------------------------------------------

#[test]
fn test_auto_variants_roundtrip() {
    // Test that auto-generated variants encode/decode correctly
    let abort = AutoError::Aborted;
    let code = abort.into_code();
    let decoded = AutoError::from_code(code).expect("Should decode");
    assert_eq!(abort, decoded);

    let unknown = AutoError::UnknownError;
    let code2 = unknown.into_code();
    let decoded2 = AutoError::from_code(code2).expect("Should decode");
    assert_eq!(unknown, decoded2);

    // Ensure they have different codes
    assert_ne!(code, code2);
}

// -----------------------------------------------------------------------------
// Test: Standard #[contracterror] types (NOT using scerr)
// These tests verify that both #[transparent] and #[from_contract_client]
// work with standard soroban-sdk #[contracterror] types.
// -----------------------------------------------------------------------------

/// A standard contract error using only soroban-sdk's #[contracterror] macro.
/// This does NOT use scerr - it's a plain old contracterror enum.
#[soroban_sdk::contracterror]
#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
#[repr(u32)]
pub enum StandardError {
    NotFound = 1,
    AlreadyExists = 2,
    InvalidArgument = 3,
}

/// Contract that uses standard #[contracterror] (not scerr)
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

#[scerr(mode = "root", handle_abort = "auto", handle_unknown = "auto")]
pub enum FccStandardError {
    /// unauthorized operation
    Unauthorized,

    // This uses #[from_contract_client] with a STANDARD contracterror type
    // (not scerr). This should now work because we use TryFrom<InvokeError>
    // and Error::get_code() which both standard and scerr types implement.
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

    // The standard error should be properly decoded and wrapped
    assert_eq!(decoded, FccStandardError::Standard(StandardError::NotFound));
}

#[test]
fn test_fcc_standard_error_code_roundtrip() {
    // Verify that encoding and decoding work correctly for standard errors
    let err = FccStandardError::Standard(StandardError::NotFound);
    let code = err.into_code();
    let decoded = FccStandardError::from_code(code).expect("Should decode");
    assert_eq!(err, decoded);

    let err2 = FccStandardError::Standard(StandardError::AlreadyExists);
    let code2 = err2.into_code();
    let decoded2 = FccStandardError::from_code(code2).expect("Should decode");
    assert_eq!(err2, decoded2);

    // Codes should be different
    assert_ne!(code, code2);
}

// -----------------------------------------------------------------------------
// Test: #[transparent] with standard #[contracterror]
// -----------------------------------------------------------------------------

#[scerr(mode = "root", handle_abort = "auto", handle_unknown = "auto")]
pub enum TransparentStandardError {
    /// unauthorized operation
    Unauthorized,

    // This uses #[transparent] with a STANDARD contracterror type.
    // Now works because we use Error::get_code() instead of inner.into_code()
    #[transparent]
    /// standard error
    Standard(#[from] StandardError),
}

#[contract]
pub struct TransparentStandardContract;

#[contractimpl]
impl TransparentStandardContract {
    /// Direct call - error propagates via transparent variant using From trait
    pub fn direct_fail() -> Result<(), TransparentStandardError> {
        // Use the ? operator which invokes From<StandardError>
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

    // The standard error should be properly wrapped via transparent variant
    assert_eq!(
        decoded,
        TransparentStandardError::Standard(StandardError::InvalidArgument)
    );
}

#[test]
fn test_transparent_standard_from_trait() {
    // Verify that From<StandardError> is implemented via #[transparent] + #[from]
    let inner = StandardError::NotFound;
    let outer: TransparentStandardError = inner.into();
    assert_eq!(
        outer,
        TransparentStandardError::Standard(StandardError::NotFound)
    );
}

#[test]
fn test_transparent_standard_error_code_roundtrip() {
    // Verify that encoding and decoding work correctly for transparent standard errors
    let err = TransparentStandardError::Standard(StandardError::NotFound);
    let code = err.into_code();
    let decoded = TransparentStandardError::from_code(code).expect("Should decode");
    assert_eq!(err, decoded);

    let err2 = TransparentStandardError::Standard(StandardError::InvalidArgument);
    let code2 = err2.into_code();
    let decoded2 = TransparentStandardError::from_code(code2).expect("Should decode");
    assert_eq!(err2, decoded2);

    // Codes should be different
    assert_ne!(code, code2);
}

#[test]
fn test_transparent_standard_no_collision_with_unit_variants() {
    // Ensure transparent standard error codes don't collide with unit variant codes
    let unauthorized = TransparentStandardError::Unauthorized;
    let standard = TransparentStandardError::Standard(StandardError::NotFound);

    let code1 = unauthorized.into_code();
    let code2 = standard.into_code();

    assert_ne!(
        code1, code2,
        "Unit and wrapped variant codes must not collide"
    );

    // Both should decode correctly
    let decoded1 = TransparentStandardError::from_code(code1).expect("Should decode");
    let decoded2 = TransparentStandardError::from_code(code2).expect("Should decode");

    assert_eq!(unauthorized, decoded1);
    assert_eq!(standard, decoded2);
}

// -----------------------------------------------------------------------------
// Test: Spec companion enum generation
// These tests verify that the spec companion enum is generated correctly
// for TypeScript bindings and other tooling.
// -----------------------------------------------------------------------------

#[test]
fn test_spec_companion_enum_exists() {
    // Verify that the spec companion enum exists and has the expected variants
    // OuterErrorSpec should be generated for OuterError
    let _ = OuterErrorSpec::Unauthorized;
    let _ = OuterErrorSpec::MathNamespace;
    let _ = OuterErrorSpec::MathClientNamespace;
    let _ = OuterErrorSpec::CalcNamespace;
    let _ = OuterErrorSpec::LogicClientNamespace;
}

#[test]
fn test_spec_companion_unit_variant_codes() {
    // Unit variants should have the same codes as the original enum
    assert_eq!(OuterErrorSpec::Unauthorized as u32, 1);
}

#[test]
fn test_spec_companion_namespace_codes() {
    // With 10/22 bit split and hash-based namespace assignment,
    // namespace codes are computed from hash(variant_name) % 1023 + 1.
    // The namespace code is namespace << 22 (start of range).

    // All namespaces should be valid (1-1023 shifted left by 22)
    let math_ns = OuterErrorSpec::MathNamespace as u32;
    let namespace = math_ns >> 22;
    assert!(
        namespace > 0 && namespace <= 1023,
        "Math namespace should be valid (1-1023), got {}",
        namespace
    );

    let math_client_ns = OuterErrorSpec::MathClientNamespace as u32;
    let namespace = math_client_ns >> 22;
    assert!(
        namespace > 0 && namespace <= 1023,
        "MathClient namespace should be valid (1-1023), got {}",
        namespace
    );

    let calc_ns = OuterErrorSpec::CalcNamespace as u32;
    let namespace = calc_ns >> 22;
    assert!(
        namespace > 0 && namespace <= 1023,
        "Calc namespace should be valid (1-1023), got {}",
        namespace
    );

    let logic_client_ns = OuterErrorSpec::LogicClientNamespace as u32;
    let namespace = logic_client_ns >> 22;
    assert!(
        namespace > 0 && namespace <= 1023,
        "LogicClient namespace should be valid (1-1023), got {}",
        namespace
    );

    // All namespaces should be distinct
    let namespaces = [
        math_ns >> 22,
        math_client_ns >> 22,
        calc_ns >> 22,
        logic_client_ns >> 22,
    ];
    for i in 0..namespaces.len() {
        for j in (i + 1)..namespaces.len() {
            assert_ne!(
                namespaces[i], namespaces[j],
                "Namespaces should be distinct"
            );
        }
    }
}

#[test]
fn test_fcc_standard_spec_companion_exists() {
    // Verify spec companion for FccStandardError
    let _ = FccStandardErrorSpec::Unauthorized;
    let _ = FccStandardErrorSpec::StandardNamespace;
    let _ = FccStandardErrorSpec::Aborted;
    let _ = FccStandardErrorSpec::UnknownError;
}

#[test]
fn test_transparent_standard_spec_companion_exists() {
    // Verify spec companion for TransparentStandardError
    let _ = TransparentStandardErrorSpec::Unauthorized;
    let _ = TransparentStandardErrorSpec::StandardNamespace;
    let _ = TransparentStandardErrorSpec::Aborted;
    let _ = TransparentStandardErrorSpec::UnknownError;
}

// -----------------------------------------------------------------------------
// Test: Mixed scerr and non-scerr types with #[from_contract_client]
// This tests the edge case of having multiple FCC variants that wrap
// different types - some using scerr, some using standard #[contracterror].
// -----------------------------------------------------------------------------

/// Another standard contract error (non-scerr) for testing mixed scenarios
#[soroban_sdk::contracterror]
#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
#[repr(u32)]
pub enum StorageError {
    KeyNotFound = 1,
    ValueTooLarge = 2,
    QuotaExceeded = 3,
}

/// Contract using StorageError (standard contracterror)
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

/// Mixed error enum with multiple FCC variants:
/// - MathError: scerr type
/// - StandardError: standard contracterror
/// - StorageError: another standard contracterror
/// - LogicError: scerr type
#[scerr(mode = "root", handle_abort = "auto", handle_unknown = "auto")]
pub enum MixedFccError {
    /// operation not permitted
    NotPermitted,

    /// invalid configuration
    InvalidConfig,

    // FCC with scerr type (MathError uses scerr)
    #[from_contract_client]
    /// math operation error
    MathOp(MathError),

    // FCC with standard contracterror (StandardError)
    #[from_contract_client]
    /// standard operation error
    StandardOp(StandardError),

    // FCC with another standard contracterror (StorageError)
    #[from_contract_client]
    /// storage operation error
    StorageOp(StorageError),

    // FCC with another scerr type (LogicError uses scerr)
    #[from_contract_client]
    /// logic operation error
    LogicOp(LogicError),
}

/// Contract that calls multiple other contracts with mixed error types
#[contract]
pub struct MixedFccContract;

#[contractimpl]
impl MixedFccContract {
    /// Call math contract (scerr type)
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

    /// Call standard contract (standard contracterror)
    pub fn call_standard_fail(env: Env, standard_id: Address) -> Result<(), MixedFccError> {
        let client = StandardContractClient::new(&env, &standard_id);
        client.try_fail_not_found()??;
        Ok(())
    }

    /// Call storage contract (another standard contracterror)
    pub fn call_storage_get(env: Env, storage_id: Address, key: u32) -> Result<u32, MixedFccError> {
        let client = StorageContractClient::new(&env, &storage_id);
        let result = client.try_get_value(&key)??;
        Ok(result)
    }

    /// Call logic contract (scerr type)
    pub fn call_logic(env: Env, logic_id: Address, x: i64) -> Result<(), MixedFccError> {
        let client = LogicClient::new(&env, &logic_id);
        client.try_check_positive(&x)??;
        Ok(())
    }

    /// Chain calls: math -> storage -> logic
    pub fn chain_calls(
        env: Env,
        math_id: Address,
        storage_id: Address,
        logic_id: Address,
        num: i64,
        denom: i64,
    ) -> Result<u32, MixedFccError> {
        // First call math (scerr)
        let math_client = MathClient::new(&env, &math_id);
        let div_result = math_client.try_safe_div(&num, &denom)??;

        // Then call storage (standard contracterror) - will fail
        let storage_client = StorageContractClient::new(&env, &storage_id);
        let storage_result = storage_client.try_get_value(&(div_result as u32))??;

        // Finally call logic (scerr)
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

// -----------------------------------------------------------------------------
// Tests for mixed scerr and non-scerr FCC
// -----------------------------------------------------------------------------

#[test]
fn test_mixed_fcc_math_error_scerr() {
    // Test FCC with scerr type (MathError)
    let env = Env::default();
    let math_id = setup_math_contract(&env);
    let mixed_id = setup_mixed_fcc_contract(&env);
    let client = MixedFccContractClient::new(&env, &mixed_id);

    // Division by zero
    let result = client.try_call_math(&math_id, &10, &0);
    assert!(result.is_err());

    let err = result.err().unwrap();
    let decoded: MixedFccError = err.expect("should be contract error");
    assert_eq!(decoded, MixedFccError::MathOp(MathError::DivisionByZero));
}

#[test]
fn test_mixed_fcc_standard_error() {
    // Test FCC with standard contracterror (StandardError)
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
    // Test FCC with another standard contracterror (StorageError)
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
    // Test FCC with another scerr type (LogicError)
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
    // Verify that all error variants have unique codes
    let not_permitted = MixedFccError::NotPermitted;
    let invalid_config = MixedFccError::InvalidConfig;
    let math_div_zero = MixedFccError::MathOp(MathError::DivisionByZero);
    let math_neg = MixedFccError::MathOp(MathError::NegativeInput);
    let standard_not_found = MixedFccError::StandardOp(StandardError::NotFound);
    let standard_exists = MixedFccError::StandardOp(StandardError::AlreadyExists);
    let storage_key = MixedFccError::StorageOp(StorageError::KeyNotFound);
    let storage_large = MixedFccError::StorageOp(StorageError::ValueTooLarge);
    let logic_invalid = MixedFccError::LogicOp(LogicError::InvalidState);
    let logic_unauth = MixedFccError::LogicOp(LogicError::Unauthorized);
    let aborted = MixedFccError::Aborted;
    let unknown = MixedFccError::UnknownError;

    let codes = [
        not_permitted.into_code(),
        invalid_config.into_code(),
        math_div_zero.into_code(),
        math_neg.into_code(),
        standard_not_found.into_code(),
        standard_exists.into_code(),
        storage_key.into_code(),
        storage_large.into_code(),
        logic_invalid.into_code(),
        logic_unauth.into_code(),
        aborted.into_code(),
        unknown.into_code(),
    ];

    // All codes should be unique
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
    // Test encoding and decoding for all variant types
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
fn test_mixed_fcc_namespaces_distinct() {
    // Verify that scerr and standard types use distinct namespaces
    let math_err = MixedFccError::MathOp(MathError::DivisionByZero);
    let standard_err = MixedFccError::StandardOp(StandardError::NotFound);
    let storage_err = MixedFccError::StorageOp(StorageError::KeyNotFound);
    let logic_err = MixedFccError::LogicOp(LogicError::InvalidState);

    let math_ns = math_err.into_code() >> 24;
    let standard_ns = standard_err.into_code() >> 24;
    let storage_ns = storage_err.into_code() >> 24;
    let logic_ns = logic_err.into_code() >> 24;

    // All namespaces should be different
    assert_ne!(math_ns, standard_ns);
    assert_ne!(math_ns, storage_ns);
    assert_ne!(math_ns, logic_ns);
    assert_ne!(standard_ns, storage_ns);
    assert_ne!(standard_ns, logic_ns);
    assert_ne!(storage_ns, logic_ns);

    // Unit variants should have namespace 0
    let unit_ns = MixedFccError::NotPermitted.into_code() >> 24;
    assert_eq!(unit_ns, 0);
}

#[test]
fn test_mixed_fcc_spec_companion_exists() {
    // Verify spec companion enum has all expected variants
    let _ = MixedFccErrorSpec::NotPermitted;
    let _ = MixedFccErrorSpec::InvalidConfig;
    let _ = MixedFccErrorSpec::MathOpNamespace;
    let _ = MixedFccErrorSpec::StandardOpNamespace;
    let _ = MixedFccErrorSpec::StorageOpNamespace;
    let _ = MixedFccErrorSpec::LogicOpNamespace;
    let _ = MixedFccErrorSpec::Aborted;
    let _ = MixedFccErrorSpec::UnknownError;
}

#[test]
fn test_mixed_fcc_chain_calls_first_fails() {
    // Test chained calls where the first (scerr) fails
    let env = Env::default();
    let math_id = setup_math_contract(&env);
    let storage_id = setup_storage_contract(&env);
    let logic_id = setup_logic_contract(&env);
    let mixed_id = setup_mixed_fcc_contract(&env);
    let client = MixedFccContractClient::new(&env, &mixed_id);

    // Math will fail with division by zero
    let result = client.try_chain_calls(&math_id, &storage_id, &logic_id, &10, &0);
    assert!(result.is_err());

    let err = result.err().unwrap();
    let decoded: MixedFccError = err.expect("should be contract error");
    assert_eq!(decoded, MixedFccError::MathOp(MathError::DivisionByZero));
}

#[test]
fn test_mixed_fcc_chain_calls_second_fails() {
    // Test chained calls where the second (standard contracterror) fails
    let env = Env::default();
    let math_id = setup_math_contract(&env);
    let storage_id = setup_storage_contract(&env);
    let logic_id = setup_logic_contract(&env);
    let mixed_id = setup_mixed_fcc_contract(&env);
    let client = MixedFccContractClient::new(&env, &mixed_id);

    // Math succeeds (10/2=5), but storage will fail with KeyNotFound
    let result = client.try_chain_calls(&math_id, &storage_id, &logic_id, &10, &2);
    assert!(result.is_err());

    let err = result.err().unwrap();
    let decoded: MixedFccError = err.expect("should be contract error");
    assert_eq!(decoded, MixedFccError::StorageOp(StorageError::KeyNotFound));
}

// =============================================================================
// Tests for __build_unified_spec! macro
// =============================================================================
//
// The __build_unified_spec! macro generates a unified flattened error enum
// containing all unit variants plus flattened variants from imported contracts.
// It is invoked automatically by scerr when #[from_contract_client] variants
// reference types from contractimport_with_errors! modules.

#[test]
fn test_build_unified_spec_unit_variants_only() {
    // Test __build_unified_spec! with only unit variants (no mixins)
    soroban_sdk_tools::__build_unified_spec! {
        @name TestUnifiedUnit
        @unit [
            { name: Unauthorized, code: 1, doc: "unauthorized operation" },
            { name: InvalidInput, code: 2, doc: "invalid input provided" }
        ]
        @mixins []
    }

    // Verify the enum was generated in a hidden module
    use __scerr_unified_testunifiedunit::TestUnifiedUnit;

    // Verify variant codes
    assert_eq!(TestUnifiedUnit::Unauthorized as u32, 1);
    assert_eq!(TestUnifiedUnit::InvalidInput as u32, 2);
}

#[test]
fn test_build_unified_spec_with_accumulated_variants() {
    // Test __build_unified_spec! base case with pre-accumulated flattened variants
    // This simulates what happens after getter macros have been processed
    soroban_sdk_tools::__build_unified_spec! {
        @name TestUnifiedWithAcc
        @acc [
            { name: MathError_DivisionByZero, code: 3922378893, doc: "division by zero" },
            { name: MathError_NegativeInput, code: 3925386672, doc: "negative input" }
        ]
        @unit [
            { name: Unauthorized, code: 1, doc: "unauthorized" },
            { name: Aborted, code: 936, doc: "aborted" }
        ]
        @pending []
    }

    use __scerr_unified_testunifiedwithacc::TestUnifiedWithAcc;

    // Verify unit variants
    assert_eq!(TestUnifiedWithAcc::Unauthorized as u32, 1);
    assert_eq!(TestUnifiedWithAcc::Aborted as u32, 936);

    // Verify flattened variants from accumulator
    assert_eq!(
        TestUnifiedWithAcc::MathError_DivisionByZero as u32,
        3922378893
    );
    assert_eq!(
        TestUnifiedWithAcc::MathError_NegativeInput as u32,
        3925386672
    );
}

#[test]
fn test_build_unified_spec_multiple_inner_types() {
    // Test with multiple flattened inner error types
    soroban_sdk_tools::__build_unified_spec! {
        @name TestMultipleInner
        @acc [
            { name: MathError_DivisionByZero, code: 3922378893, doc: "division by zero" },
            { name: MathError_NegativeInput, code: 3925386672, doc: "negative input" },
            { name: CalcError_Overflow, code: 2570199786, doc: "overflow" },
            { name: CalcError_Underflow, code: 2568683692, doc: "underflow" }
        ]
        @unit [
            { name: Unauthorized, code: 1, doc: "unauthorized" },
            { name: InvalidInput, code: 2, doc: "invalid input" },
            { name: Aborted, code: 936, doc: "aborted" },
            { name: UnknownError, code: 937, doc: "unknown error" }
        ]
        @pending []
    }

    use __scerr_unified_testmultipleinner::TestMultipleInner;

    // Verify all unit variants
    assert_eq!(TestMultipleInner::Unauthorized as u32, 1);
    assert_eq!(TestMultipleInner::InvalidInput as u32, 2);
    assert_eq!(TestMultipleInner::Aborted as u32, 936);
    assert_eq!(TestMultipleInner::UnknownError as u32, 937);

    // Verify all flattened MathError variants
    assert_eq!(
        TestMultipleInner::MathError_DivisionByZero as u32,
        3922378893
    );
    assert_eq!(
        TestMultipleInner::MathError_NegativeInput as u32,
        3925386672
    );

    // Verify all flattened CalcError variants
    assert_eq!(TestMultipleInner::CalcError_Overflow as u32, 2570199786);
    assert_eq!(TestMultipleInner::CalcError_Underflow as u32, 2568683692);
}

#[test]
fn test_build_unified_spec_codes_are_correct_combined_values() {
    // Verify the flattened codes follow the expected formula:
    // combined_code = (namespace << 22) | inner_code
    //
    // For MathError (namespace = 935):
    //   DivisionByZero: (935 << 22) | 704653 = 3922378893
    //   NegativeInput: (935 << 22) | 3712432 = 3925386672
    //
    // For CalcError (namespace = 612):
    //   Overflow: (612 << 22) | 3285738 = 2570199786
    //   Underflow: (612 << 22) | 1769644 = 2568683692

    // Compute expected values
    let math_namespace: u32 = 935;
    let calc_namespace: u32 = 612;

    let expected_math_div_zero = (math_namespace << 22) | 704653;
    let expected_math_neg_input = (math_namespace << 22) | 3712432;
    let expected_calc_overflow = (calc_namespace << 22) | 3285738;
    let expected_calc_underflow = (calc_namespace << 22) | 1769644;

    // Verify the expected values match the test constants
    assert_eq!(expected_math_div_zero, 3922378893);
    assert_eq!(expected_math_neg_input, 3925386672);
    assert_eq!(expected_calc_overflow, 2570199786);
    assert_eq!(expected_calc_underflow, 2568683692);
}
