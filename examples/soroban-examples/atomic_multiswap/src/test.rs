#![cfg(test)]
extern crate std;

use super::*;
use assert_unordered::assert_eq_unordered;
use soroban_sdk::{
    symbol_short,
    testutils::{Address as _, AuthorizedFunction, AuthorizedInvocation, MockAuth, MockAuthInvoke},
    token, Address, Env, IntoVal,
};
use soroban_atomic_swap_contract::AtomicSwapContract;
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

fn create_atomic_multiswap_contract(e: &Env) -> AtomicMultiSwapContractClient<'_> {
    AtomicMultiSwapContractClient::new(e, &e.register(AtomicMultiSwapContract {}, ()))
}

fn mint_with_auth(
    env: &Env,
    token_address: &Address,
    admin: &Address,
    token_admin: &TokenAdminClient<'_>,
    to: &Address,
    amount: i128,
) {
    setup_mock_auth(
        env,
        token_address,
        "mint",
        (to.clone(), amount),
        &[admin],
    );
    token_admin.mint(to, &amount);
}

fn setup_two_swap_pair_auth(
    env: &Env,
    swap_contract_id: &Address,
    token_a: &Address,
    token_b: &Address,
    pair1_a: (&Address, i128, i128),
    pair1_b: (&Address, i128, i128),
    pair2_a: (&Address, i128, i128),
    pair2_b: (&Address, i128, i128),
) {
    let p1a_transfer = MockAuthInvoke {
        contract: token_a,
        fn_name: "transfer",
        args: (pair1_a.0.clone(), swap_contract_id.clone(), pair1_a.1).into_val(env),
        sub_invokes: &[],
    };
    let p1a_invoke = MockAuthInvoke {
        contract: swap_contract_id,
        fn_name: "swap",
        args: (token_a.clone(), token_b.clone(), pair1_a.1, pair1_a.2).into_val(env),
        sub_invokes: &[p1a_transfer],
    };

    let p1b_transfer = MockAuthInvoke {
        contract: token_b,
        fn_name: "transfer",
        args: (pair1_b.0.clone(), swap_contract_id.clone(), pair1_b.1).into_val(env),
        sub_invokes: &[],
    };
    let p1b_invoke = MockAuthInvoke {
        contract: swap_contract_id,
        fn_name: "swap",
        args: (token_b.clone(), token_a.clone(), pair1_b.1, pair1_b.2).into_val(env),
        sub_invokes: &[p1b_transfer],
    };

    let p2a_transfer = MockAuthInvoke {
        contract: token_a,
        fn_name: "transfer",
        args: (pair2_a.0.clone(), swap_contract_id.clone(), pair2_a.1).into_val(env),
        sub_invokes: &[],
    };
    let p2a_invoke = MockAuthInvoke {
        contract: swap_contract_id,
        fn_name: "swap",
        args: (token_a.clone(), token_b.clone(), pair2_a.1, pair2_a.2).into_val(env),
        sub_invokes: &[p2a_transfer],
    };

    let p2b_transfer = MockAuthInvoke {
        contract: token_b,
        fn_name: "transfer",
        args: (pair2_b.0.clone(), swap_contract_id.clone(), pair2_b.1).into_val(env),
        sub_invokes: &[],
    };
    let p2b_invoke = MockAuthInvoke {
        contract: swap_contract_id,
        fn_name: "swap",
        args: (token_b.clone(), token_a.clone(), pair2_b.1, pair2_b.2).into_val(env),
        sub_invokes: &[p2b_transfer],
    };

    env.mock_auths(&[
        MockAuth {
            address: pair1_a.0,
            invoke: &p1a_invoke,
        },
        MockAuth {
            address: pair1_b.0,
            invoke: &p1b_invoke,
        },
        MockAuth {
            address: pair2_a.0,
            invoke: &p2a_invoke,
        },
        MockAuth {
            address: pair2_b.0,
            invoke: &p2b_invoke,
        },
    ]);
}

#[test]
fn test_atomic_multi_swap() {
    let env = Env::default();

    let swaps_a = [
        SwapSpec {
            address: Address::generate(&env),
            amount: 2000,
            min_recv: 290,
        },
        SwapSpec {
            address: Address::generate(&env),
            amount: 3000,
            min_recv: 350,
        },
        SwapSpec {
            address: Address::generate(&env),
            amount: 4000,
            min_recv: 301,
        },
    ];
    let swaps_b = [
        SwapSpec {
            address: Address::generate(&env),
            amount: 300,
            min_recv: 2100,
        },
        SwapSpec {
            address: Address::generate(&env),
            amount: 295,
            min_recv: 1950,
        },
        SwapSpec {
            address: Address::generate(&env),
            amount: 400,
            min_recv: 2900,
        },
    ];

    let token_admin = Address::generate(&env);

    let (token_a, token_a_admin) = create_token_contract(&env, &token_admin);
    let (token_b, token_b_admin) = create_token_contract(&env, &token_admin);
    mint_with_auth(
        &env,
        &token_a.address,
        &token_admin,
        &token_a_admin,
        &swaps_a[0].address,
        2000,
    );
    mint_with_auth(
        &env,
        &token_a.address,
        &token_admin,
        &token_a_admin,
        &swaps_a[1].address,
        3000,
    );
    mint_with_auth(
        &env,
        &token_a.address,
        &token_admin,
        &token_a_admin,
        &swaps_a[2].address,
        4000,
    );

    mint_with_auth(
        &env,
        &token_b.address,
        &token_admin,
        &token_b_admin,
        &swaps_b[0].address,
        300,
    );
    mint_with_auth(
        &env,
        &token_b.address,
        &token_admin,
        &token_b_admin,
        &swaps_b[1].address,
        295,
    );
    mint_with_auth(
        &env,
        &token_b.address,
        &token_admin,
        &token_b_admin,
        &swaps_b[2].address,
        400,
    );

    let contract = create_atomic_multiswap_contract(&env);

    let swap_contract_id = env.register(AtomicSwapContract {}, ());

    setup_two_swap_pair_auth(
        &env,
        &swap_contract_id,
        &token_a.address,
        &token_b.address,
        (&swaps_a[0].address, 2000, 290),
        (&swaps_b[1].address, 295, 1950),
        (&swaps_a[1].address, 3000, 350),
        (&swaps_b[2].address, 400, 2900),
    );

    contract.multi_swap(
        &swap_contract_id,
        &token_a.address,
        &token_b.address,
        &Vec::from_array(&env, swaps_a.clone()),
        &Vec::from_array(&env, swaps_b.clone()),
    );

    // Check that only 4 swaps were authorized and accounts A[0] and B[1] didn't
    // authorize anything. Their swaps still can be cleared via a new contract
    // call with the correct arguments.
    // Notice, that `swap` authorizations are recorded - they're the top-level
    // authorized calls, even though `multi_swap` was the overall top-level
    // invocation.
    assert_eq_unordered!(
        env.auths(),
        std::vec![
            (
                swaps_a[0].address.clone(),
                AuthorizedInvocation {
                    function: AuthorizedFunction::Contract((
                        swap_contract_id.clone(),
                        symbol_short!("swap"),
                        (
                            token_a.address.clone(),
                            token_b.address.clone(),
                            2000_i128,
                            290_i128
                        )
                            .into_val(&env),
                    )),
                    sub_invocations: std::vec![AuthorizedInvocation {
                        function: AuthorizedFunction::Contract((
                            token_a.address.clone(),
                            symbol_short!("transfer"),
                            (
                                swaps_a[0].address.clone(),
                                swap_contract_id.clone(),
                                2000_i128,
                            )
                                .into_val(&env),
                        )),
                        sub_invocations: std::vec![]
                    }]
                }
            ),
            (
                swaps_a[1].address.clone(),
                AuthorizedInvocation {
                    function: AuthorizedFunction::Contract((
                        swap_contract_id.clone(),
                        symbol_short!("swap"),
                        (
                            token_a.address.clone(),
                            token_b.address.clone(),
                            3000_i128,
                            350_i128
                        )
                            .into_val(&env),
                    )),
                    sub_invocations: std::vec![AuthorizedInvocation {
                        function: AuthorizedFunction::Contract((
                            token_a.address.clone(),
                            symbol_short!("transfer"),
                            (
                                swaps_a[1].address.clone(),
                                swap_contract_id.clone(),
                                3000_i128,
                            )
                                .into_val(&env),
                        )),
                        sub_invocations: std::vec![]
                    }]
                }
            ),
            (
                swaps_b[1].address.clone(),
                AuthorizedInvocation {
                    function: AuthorizedFunction::Contract((
                        swap_contract_id.clone(),
                        symbol_short!("swap"),
                        (
                            token_b.address.clone(),
                            token_a.address.clone(),
                            295_i128,
                            1950_i128,
                        )
                            .into_val(&env),
                    )),
                    sub_invocations: std::vec![AuthorizedInvocation {
                        function: AuthorizedFunction::Contract((
                            token_b.address.clone(),
                            symbol_short!("transfer"),
                            (
                                swaps_b[1].address.clone(),
                                swap_contract_id.clone(),
                                295_i128,
                            )
                                .into_val(&env),
                        )),
                        sub_invocations: std::vec![]
                    }]
                }
            ),
            (
                swaps_b[2].address.clone(),
                AuthorizedInvocation {
                    function: AuthorizedFunction::Contract((
                        swap_contract_id.clone(),
                        symbol_short!("swap"),
                        (
                            token_b.address.clone(),
                            token_a.address.clone(),
                            400_i128,
                            2900_i128,
                        )
                            .into_val(&env),
                    )),
                    sub_invocations: std::vec![AuthorizedInvocation {
                        function: AuthorizedFunction::Contract((
                            token_b.address.clone(),
                            symbol_short!("transfer"),
                            (
                                swaps_b[2].address.clone(),
                                swap_contract_id.clone(),
                                400_i128,
                            )
                                .into_val(&env),
                        )),
                        sub_invocations: std::vec![]
                    }]
                }
            ),
        ]
    );
    // Balance has to be checked after the auth checks because auth is only
    // stored for the last invocation currently.
    assert_eq!(token_a.balance(&swaps_a[0].address), 50);
    assert_eq!(token_a.balance(&swaps_a[1].address), 100);
    assert_eq!(token_a.balance(&swaps_a[2].address), 4000);

    assert_eq!(token_a.balance(&swaps_b[0].address), 0);
    assert_eq!(token_a.balance(&swaps_b[1].address), 1950);
    assert_eq!(token_a.balance(&swaps_b[2].address), 2900);

    assert_eq!(token_b.balance(&swaps_a[0].address), 290);
    assert_eq!(token_b.balance(&swaps_a[1].address), 350);
    assert_eq!(token_b.balance(&swaps_a[2].address), 0);

    assert_eq!(token_b.balance(&swaps_b[0].address), 300);
    assert_eq!(token_b.balance(&swaps_b[1].address), 5);
    assert_eq!(token_b.balance(&swaps_b[2].address), 50);
}

#[test]
fn test_multi_swap_with_duplicate_account() {
    let env = Env::default();

    let address_a = Address::generate(&env);
    let address_b = Address::generate(&env);
    let swaps_a = [
        SwapSpec {
            address: address_a.clone(),
            amount: 1000,
            min_recv: 100,
        },
        SwapSpec {
            address: address_a.clone(),
            amount: 2000,
            min_recv: 190,
        },
    ];
    let swaps_b = [
        SwapSpec {
            address: address_b.clone(),
            amount: 101,
            min_recv: 1000,
        },
        SwapSpec {
            address: address_b.clone(),
            amount: 190,
            min_recv: 2000,
        },
    ];

    let token_admin = Address::generate(&env);

    let (token_a, token_a_admin) = create_token_contract(&env, &token_admin);
    let (token_b, token_b_admin) = create_token_contract(&env, &token_admin);
    mint_with_auth(
        &env,
        &token_a.address,
        &token_admin,
        &token_a_admin,
        &address_a,
        3000,
    );
    mint_with_auth(
        &env,
        &token_b.address,
        &token_admin,
        &token_b_admin,
        &address_b,
        291,
    );

    let contract = create_atomic_multiswap_contract(&env);

    let swap_contract_id = env.register(AtomicSwapContract {}, ());

    setup_two_swap_pair_auth(
        &env,
        &swap_contract_id,
        &token_a.address,
        &token_b.address,
        (&address_a, 1000, 100),
        (&address_b, 101, 1000),
        (&address_a, 2000, 190),
        (&address_b, 190, 2000),
    );

    contract.multi_swap(
        &swap_contract_id,
        &token_a.address,
        &token_b.address,
        &Vec::from_array(&env, swaps_a.clone()),
        &Vec::from_array(&env, swaps_b.clone()),
    );

    // Notice that the same address may participate in multiple swaps. Separate
    // authorizations are recorded (and required on-chain) for every swap.
    assert_eq_unordered!(
        env.auths(),
        std::vec![
            (
                address_a.clone(),
                AuthorizedInvocation {
                    function: AuthorizedFunction::Contract((
                        swap_contract_id.clone(),
                        symbol_short!("swap"),
                        (
                            token_a.address.clone(),
                            token_b.address.clone(),
                            1000_i128,
                            100_i128
                        )
                            .into_val(&env),
                    )),
                    sub_invocations: std::vec![AuthorizedInvocation {
                        function: AuthorizedFunction::Contract((
                            token_a.address.clone(),
                            symbol_short!("transfer"),
                            (address_a.clone(), swap_contract_id.clone(), 1000_i128,)
                                .into_val(&env),
                        )),
                        sub_invocations: std::vec![]
                    }]
                }
            ),
            (
                address_a.clone(),
                AuthorizedInvocation {
                    function: AuthorizedFunction::Contract((
                        swap_contract_id.clone(),
                        symbol_short!("swap"),
                        (
                            token_a.address.clone(),
                            token_b.address.clone(),
                            2000_i128,
                            190_i128
                        )
                            .into_val(&env),
                    )),
                    sub_invocations: std::vec![AuthorizedInvocation {
                        function: AuthorizedFunction::Contract((
                            token_a.address.clone(),
                            symbol_short!("transfer"),
                            (address_a.clone(), swap_contract_id.clone(), 2000_i128,)
                                .into_val(&env),
                        )),
                        sub_invocations: std::vec![]
                    }]
                }
            ),
            (
                address_b.clone(),
                AuthorizedInvocation {
                    function: AuthorizedFunction::Contract((
                        swap_contract_id.clone(),
                        symbol_short!("swap"),
                        (
                            token_b.address.clone(),
                            token_a.address.clone(),
                            101_i128,
                            1000_i128
                        )
                            .into_val(&env),
                    )),
                    sub_invocations: std::vec![AuthorizedInvocation {
                        function: AuthorizedFunction::Contract((
                            token_b.address.clone(),
                            symbol_short!("transfer"),
                            (address_b.clone(), swap_contract_id.clone(), 101_i128,).into_val(&env),
                        )),
                        sub_invocations: std::vec![]
                    }]
                }
            ),
            (
                address_b.clone(),
                AuthorizedInvocation {
                    function: AuthorizedFunction::Contract((
                        swap_contract_id.clone(),
                        symbol_short!("swap"),
                        (
                            token_b.address.clone(),
                            token_a.address.clone(),
                            190_i128,
                            2000_i128
                        )
                            .into_val(&env),
                    )),
                    sub_invocations: std::vec![AuthorizedInvocation {
                        function: AuthorizedFunction::Contract((
                            token_b.address.clone(),
                            symbol_short!("transfer"),
                            (address_b.clone(), swap_contract_id.clone(), 190_i128,).into_val(&env),
                        )),
                        sub_invocations: std::vec![]
                    }]
                }
            ),
        ]
    );

    // Balance has to be checked after the auth checks because auth is only
    // stored for the last invocation currently.
    assert_eq!(token_a.balance(&address_a), 0);
    assert_eq!(token_a.balance(&address_b), 3000);

    assert_eq!(token_b.balance(&address_a), 290);
    assert_eq!(token_b.balance(&address_b), 1);
}
