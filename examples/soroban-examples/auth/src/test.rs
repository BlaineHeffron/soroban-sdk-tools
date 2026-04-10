#![cfg(test)]
extern crate std;

use soroban_sdk::{
    symbol_short,
    testutils::{Address as _, AuthorizedFunction, AuthorizedInvocation, MockAuthInvoke},
    Address, Env, IntoVal,
};

use crate::{IncrementContract, IncrementContractClient};

#[test]
fn test() {
    let env = Env::default();

    let contract_id = env.register(IncrementContract, {});
    let client = IncrementContractClient::new(&env, &contract_id);

    let user_1 = Address::generate(&env);
    let user_2 = Address::generate(&env);

    soroban_sdk_tools::setup_mock_auth(
        &env,
        &[&user_1],
        MockAuthInvoke {
            contract: &contract_id,
            fn_name: "increment",
            args: (user_1.clone(), 5_u32).into_val(&env),
            sub_invokes: &[],
        },
    );
    assert_eq!(client.increment(&user_1, &5), 5);
    // Verify that the user indeed had to authorize a call of `increment` with
    // the expected arguments:
    assert_eq!(
        env.auths(),
        std::vec![(
            // Address for which authorization check is performed
            user_1.clone(),
            // Invocation tree that needs to be authorized
            AuthorizedInvocation {
                // Function that is authorized. Can be a contract function or
                // a host function that requires authorization.
                function: AuthorizedFunction::Contract((
                    // Address of the called contract
                    contract_id.clone(),
                    // Name of the called function
                    symbol_short!("increment"),
                    // Arguments used to call `increment` (converted to the env-managed vector via `into_val`)
                    (user_1.clone(), 5_u32).into_val(&env),
                )),
                // The contract doesn't call any other contracts that require
                // authorization,
                sub_invocations: std::vec![]
            }
        )]
    );

    // Do more `increment` calls. It's not necessary to verify authorizations
    // for every one of them as we don't expect the auth logic to change from
    // call to call.
    soroban_sdk_tools::setup_mock_auth(
        &env,
        &[&user_1],
        MockAuthInvoke {
            contract: &contract_id,
            fn_name: "increment",
            args: (user_1.clone(), 2_u32).into_val(&env),
            sub_invokes: &[],
        },
    );
    assert_eq!(client.increment(&user_1, &2), 7);
    soroban_sdk_tools::setup_mock_auth(
        &env,
        &[&user_2],
        MockAuthInvoke {
            contract: &contract_id,
            fn_name: "increment",
            args: (user_2.clone(), 1_u32).into_val(&env),
            sub_invokes: &[],
        },
    );
    assert_eq!(client.increment(&user_2, &1), 1);
    soroban_sdk_tools::setup_mock_auth(
        &env,
        &[&user_1],
        MockAuthInvoke {
            contract: &contract_id,
            fn_name: "increment",
            args: (user_1.clone(), 3_u32).into_val(&env),
            sub_invokes: &[],
        },
    );
    assert_eq!(client.increment(&user_1, &3), 10);
    soroban_sdk_tools::setup_mock_auth(
        &env,
        &[&user_2],
        MockAuthInvoke {
            contract: &contract_id,
            fn_name: "increment",
            args: (user_2.clone(), 4_u32).into_val(&env),
            sub_invokes: &[],
        },
    );
    assert_eq!(client.increment(&user_2, &4), 5);
}
