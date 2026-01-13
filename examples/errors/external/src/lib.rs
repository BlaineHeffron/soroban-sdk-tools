#![no_std]
use soroban_sdk::{contract, contractimpl};
use soroban_sdk_tools::scerr;

#[scerr]
pub enum MathError {
    ///division by zero
    DivisionByZero,
    NegativeInput,
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
}

mod test;
