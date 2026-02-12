#![cfg(test)]
use crate::{calc, external, Contract, ContractClient};
use soroban_sdk::Env;

#[test]
fn external_errors() {
    let env = &Env::default();
    env.mock_all_auths();

    let external_contract_id = &env.register(external::WASM, ());
    let contract_id = env.register(Contract, ());
    let client = ContractClient::new(env, &contract_id);
    assert_eq!(
        client.try_safe_div_with(external_contract_id, &10, &1),
        Ok(Ok(10))
    );
    assert_eq!(
        client.try_safe_div_with(external_contract_id, &10, &0),
        Err(Ok(crate::Error::Math(external::MathError::DivisionByZero)))
    );
}

#[test]
fn calc_errors() {
    let env = &Env::default();
    env.mock_all_auths();

    let external_contract_id = &env.register(external::WASM, ());
    let calc_contract_id = &env.register(calc::WASM, ());
    let contract_id = env.register(Contract, ());
    let client = ContractClient::new(env, &contract_id);
    assert_eq!(
        client.try_safe_div_with_calc(external_contract_id, &10, &1),
        Ok(Ok(10))
    );
    assert_eq!(
        client.try_safe_div_with_calc(calc_contract_id, &10, &0),
        Err(Ok(crate::Error::Calc(calc::CalcError::DivisionByZero)))
    );
}
