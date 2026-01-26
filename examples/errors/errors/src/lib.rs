#![no_std]
use soroban_sdk::{contract, contractimpl, Address, Env};
use soroban_sdk_tools::{scerr, ContractError};

mod external {
    use soroban_sdk::InvokeError;
    use soroban_sdk_tools::ContractError;

    soroban_sdk_tools::contractimport_with_errors!(
        file = "../../../target/stellar/soroban_errors_external_contract.wasm"
    );
    impl ContractError for MathError {
        fn into_code(self) -> u32 {
            self as u32
        }

        fn from_code(code: u32) -> Option<Self> {
            InvokeError::Contract(code).try_into().ok()
        }

        fn description(&self) -> &'static str {
            "bla bla bla"
        }
    }
}
// Mode is auto-detected due to #[from_contract_client] attribute
#[scerr]
pub enum Error {
    /// unauthorized
    Unauthorized,

    // Use full path (external::MathError) so scerr can derive the getter macro path
    // for unified flattened error generation
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
