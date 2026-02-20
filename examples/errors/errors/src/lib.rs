#![no_std]
use soroban_sdk::{contract, contractimpl, Address, Env};
use soroban_sdk_tools::scerr;

mod external {
    soroban_sdk_tools::contractimport!(
        file = "../../../target/stellar/soroban_errors_external_contract.wasm"
    );
}
mod calc {
    soroban_sdk_tools::contractimport!(
        file = "../../../target/stellar/soroban_errors_calc_contract.wasm"
    );
}

#[scerr]
pub enum Error {
    /// unauthorized
    Unauthorized,

    #[from_contract_client]
    Math(#[from] external::MathError),
    #[from_contract_client]
    Calc(#[from] calc::CalcError),
}

#[contract]
pub struct Contract;

#[contractimpl]
impl Contract {
    pub fn safe_div_with(
        env: &Env,
        address: &Address,
        num: &i64,
        denom: &i64,
    ) -> Result<i64, Error> {
        Ok(external::Client::new(env, address).try_safe_div(num, denom)??)
    }
    pub fn safe_div_with_calc(
        env: &Env,
        address: &Address,
        num: &i64,
        denom: &i64,
    ) -> Result<i64, Error> {
        Ok(calc::Client::new(env, address).try_safe_div(num, denom)??)
    }

    pub fn error_panic(env: &Env, address: &Address) -> Result<i64, Error> {
        Ok(external::Client::new(env, address).try_panic_error()??)
    }
}

mod test;
