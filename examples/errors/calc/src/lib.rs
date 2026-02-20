#![no_std]
use soroban_sdk::{contract, contractimpl};

#[soroban_sdk::contracterror]
#[derive(Debug, PartialEq)]
#[repr(u32)]
pub enum CalcError {
    ///division by zero
    DivisionByZero = 100,
    NegativeInput = 200,
}

#[contract]
pub struct Contract;

#[contractimpl]
impl Contract {
    pub fn safe_div(num: i64, denom: i64) -> Result<i64, CalcError> {
        if denom == 0 {
            return Err(CalcError::DivisionByZero);
        }
        if denom < 0 {
            return Err(CalcError::NegativeInput);
        }
        Ok(num / denom)
    }
}

mod test;
