#![cfg(test)]
use crate::{Contract, ContractClient};
use soroban_sdk::Env;

#[test]
fn contract_errors() {
    let env = &Env::default();
    env.mock_all_auths();

    let contract_id = env.register(Contract, ());
    let client = ContractClient::new(env, &contract_id);
    assert_eq!(client.try_safe_div(&10, &1), Ok(Ok(10)));
    assert_eq!(
        client.try_safe_div(&10, &0),
        Err(Ok(crate::CalcError::DivisionByZero))
    );
}
