#![cfg(test)]
extern crate alloc;
extern crate std;

use crate::{Deployer, DeployerClient};
use alloc::vec;
use soroban_sdk::{
    symbol_short,
    testutils::{Address as _, AuthorizedFunction, AuthorizedInvocation, MockAuthInvoke},
    Address, BytesN, Env, IntoVal, Val, Vec,
};
use soroban_deployer_test_contract::ContractClient;

const DEPLOYED_CONTRACT_WASM: &[u8] =
    include_bytes!("../../../fixtures/soroban_deployer_test_contract.wasm");

#[test]
fn test() {
    let env = Env::default();
    let admin = Address::generate(&env);
    let deployer_client = DeployerClient::new(&env, &env.register(Deployer, (&admin,)));

    // Upload the Wasm to be deployed from the deployer contract.
    // This can also be called from within a contract if needed.
    let wasm_hash = env.deployer().upload_contract_wasm(DEPLOYED_CONTRACT_WASM);

    // Deploy contract using deployer, and include an init function to call.
    let salt = BytesN::from_array(&env, &[0; 32]);
    let constructor_args: Vec<Val> = (5u32,).into_val(&env);
    soroban_sdk_tools::setup_mock_auth(
        &env,
        &[&admin],
        MockAuthInvoke {
            contract: &deployer_client.address,
            fn_name: "deploy",
            args: (wasm_hash.clone(), salt.clone(), constructor_args.clone()).into_val(&env),
            sub_invokes: &[],
        },
    );
    let contract_id = deployer_client.deploy(&wasm_hash, &salt, &constructor_args);

    // An authorization from the admin is required.
    let expected_auth = AuthorizedInvocation {
        // Top-level authorized function is `deploy` with all the arguments.
        function: AuthorizedFunction::Contract((
            deployer_client.address,
            symbol_short!("deploy"),
            (wasm_hash.clone(), salt, constructor_args).into_val(&env),
        )),
        sub_invocations: vec![],
    };
    assert_eq!(env.auths(), vec![(admin, expected_auth)]);

    // Invoke contract to check that it is initialized.
    let client = ContractClient::new(&env, &contract_id);
    let sum = client.value();
    assert_eq!(sum, 5);
}
