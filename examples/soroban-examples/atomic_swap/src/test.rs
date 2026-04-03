#![cfg(test)]
extern crate std;

use super::*;
use soroban_sdk::{
    symbol_short,
    testutils::{Address as _, AuthorizedFunction, AuthorizedInvocation, MockAuth, MockAuthInvoke},
    token, Address, Env, IntoVal,
};
use soroban_sdk_tools::auth::setup_mock_auth;
use token::Client as TokenClient;
use token::StellarAssetClient as TokenAdminClient;

fn create_token_contract<'a>(e: &Env, admin: &Address) -> (TokenClient<'a>, TokenAdminClient<'a>) {
    let sac = e.register_stellar_asset_contract_v2(admin.clone());
    (
        token::Client::new(e, &sac.address()),
        token::StellarAssetClient::new(e, &sac.address()),
    )
}

fn create_atomic_swap_contract(e: &Env) -> AtomicSwapContractClient<'_> {
    AtomicSwapContractClient::new(e, &e.register(AtomicSwapContract {}, ()))
}

fn setup_swap_auth(
    env: &Env,
    contract_id: &Address,
    a: &Address,
    b: &Address,
    token_a: &Address,
    token_b: &Address,
    amount_a: i128,
    min_b_for_a: i128,
    amount_b: i128,
    min_a_for_b: i128,
) {
    let a_transfer = MockAuthInvoke {
        contract: token_a,
        fn_name: "transfer",
        args: (a.clone(), contract_id.clone(), amount_a).into_val(env),
        sub_invokes: &[],
    };
    let a_invoke = MockAuthInvoke {
        contract: contract_id,
        fn_name: "swap",
        args: (token_a.clone(), token_b.clone(), amount_a, min_b_for_a).into_val(env),
        sub_invokes: &[a_transfer],
    };

    let b_transfer = MockAuthInvoke {
        contract: token_b,
        fn_name: "transfer",
        args: (b.clone(), contract_id.clone(), amount_b).into_val(env),
        sub_invokes: &[],
    };
    let b_invoke = MockAuthInvoke {
        contract: contract_id,
        fn_name: "swap",
        args: (token_b.clone(), token_a.clone(), amount_b, min_a_for_b).into_val(env),
        sub_invokes: &[b_transfer],
    };

    env.mock_auths(&[
        MockAuth {
            address: a,
            invoke: &a_invoke,
        },
        MockAuth {
            address: b,
            invoke: &b_invoke,
        },
    ]);
}

#[test]
fn test_atomic_swap() {
    let env = Env::default();

    let a = Address::generate(&env);
    let b = Address::generate(&env);

    let token_admin = Address::generate(&env);

    let (token_a, token_a_admin) = create_token_contract(&env, &token_admin);
    let (token_b, token_b_admin) = create_token_contract(&env, &token_admin);

    setup_mock_auth(
        &env,
        &token_a.address,
        "mint",
        (a.clone(), 1000_i128),
        &[&token_admin],
    );
    token_a_admin.mint(&a, &1000);

    setup_mock_auth(
        &env,
        &token_b.address,
        "mint",
        (b.clone(), 5000_i128),
        &[&token_admin],
    );
    token_b_admin.mint(&b, &5000);

    let contract = create_atomic_swap_contract(&env);

    setup_swap_auth(
        &env,
        &contract.address,
        &a,
        &b,
        &token_a.address,
        &token_b.address,
        1000,
        4500,
        5000,
        950,
    );

    contract.swap(
        &a,
        &b,
        &token_a.address,
        &token_b.address,
        &1000,
        &4500,
        &5000,
        &950,
    );

    assert_eq!(
        env.auths(),
        std::vec![
            (
                a.clone(),
                AuthorizedInvocation {
                    function: AuthorizedFunction::Contract((
                        contract.address.clone(),
                        symbol_short!("swap"),
                        (
                            token_a.address.clone(),
                            token_b.address.clone(),
                            1000_i128,
                            4500_i128
                        )
                            .into_val(&env),
                    )),
                    sub_invocations: std::vec![AuthorizedInvocation {
                        function: AuthorizedFunction::Contract((
                            token_a.address.clone(),
                            symbol_short!("transfer"),
                            (a.clone(), contract.address.clone(), 1000_i128,).into_val(&env),
                        )),
                        sub_invocations: std::vec![]
                    }]
                }
            ),
            (
                b.clone(),
                AuthorizedInvocation {
                    function: AuthorizedFunction::Contract((
                        contract.address.clone(),
                        symbol_short!("swap"),
                        (
                            token_b.address.clone(),
                            token_a.address.clone(),
                            5000_i128,
                            950_i128
                        )
                            .into_val(&env),
                    )),
                    sub_invocations: std::vec![AuthorizedInvocation {
                        function: AuthorizedFunction::Contract((
                            token_b.address.clone(),
                            symbol_short!("transfer"),
                            (b.clone(), contract.address.clone(), 5000_i128,).into_val(&env),
                        )),
                        sub_invocations: std::vec![]
                    }]
                }
            ),
        ]
    );

    assert_eq!(token_a.balance(&a), 50);
    assert_eq!(token_a.balance(&b), 950);

    assert_eq!(token_b.balance(&a), 4500);
    assert_eq!(token_b.balance(&b), 500);
}
