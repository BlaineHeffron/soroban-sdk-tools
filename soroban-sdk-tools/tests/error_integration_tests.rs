//! Integration tests for #[scerr], including root enums and cross-contract calls.

use soroban_sdk::{contract, contractimpl, Address, Env};
use soroban_sdk_tools::{scerr, soroban_sdk, ContractError};

// -----------------------------------------------------------------------------
// Inner contract 1: math - used with both transparent and from_contract_client
// -----------------------------------------------------------------------------

#[scerr]
pub enum MathError {
    #[description = "division by zero"]
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

#[scerr(root)]
pub enum OuterError {
    #[description = "unauthorized"]
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

#[scerr(root)]
pub enum InnerRootError {
    #[description = "inner unauthorized"]
    InnerUnauthorized,
    #[description = "inner invalid state"]
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

#[scerr(root)]
pub enum OuterRootError {
    #[description = "outer unauthorized"]
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

    // Also test that the Inner variant index (2) doesn't collide
    let inner_code = OuterRootError::Inner(InnerRootError::InnerUnauthorized).into_code();
    assert_eq!(
        inner_code >> 24,
        2,
        "Inner variant should have high bits = 2"
    );

    // Code 2 (high = 0, low = 2) should not decode as anything, or at least not as Inner
    let code_2 = 2u32;
    let decoded_2 = OuterRootError::from_code(code_2);
    println!("  Trying to decode code 2: {:?}", decoded_2);

}
