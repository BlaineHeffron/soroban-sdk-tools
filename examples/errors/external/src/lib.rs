#![no_std]
use soroban_sdk::{contract, contracterror, contractimpl, Env};
use soroban_sdk_tools::scerr;

#[scerr]
pub enum MathError {
    ///division by zero
    DivisionByZero,
    NegativeInput,
}

#[contracterror]
#[derive(Debug)]
enum OtherError {
    ///division by zero
    Other = 3,
}

#[contract]
pub struct Contract;

#[contractimpl]
impl Contract {
    pub fn safe_div(num: i64, denom: i64) -> Result<i64, MathError> {
        if denom == 0 {
            return Err(MathError::DivisionByZero);
        }
        if denom < 0 {
            return Err(MathError::NegativeInput);
        }
        Ok(num / denom)
    }

    pub fn panic_error(e: &Env) -> Result<i64, MathError> {
        e.panic_with_error(OtherError::Other);
    }
}

mod test;
