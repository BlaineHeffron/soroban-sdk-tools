#![no_std]
use soroban_sdk::{contract, contractimpl, Address, Env};
use soroban_sdk_tools::{scerr, ContractError};

mod external {
    soroban_sdk_tools::contractimport!(
        file = "../../../target/stellar/soroban_errors_external_contract.wasm"
    );
}
// Mode is auto-detected due to #[from_contract_client] attribute
#[scerr]
pub enum Error {
    /// unauthorized
    Unauthorized,

    #[from_contract_client]
    Math(#[from] external::MathError),
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
}

mod test;
