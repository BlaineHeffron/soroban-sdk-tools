#![cfg(test)]
extern crate std;

use crate::LiquidityPoolClient;

use soroban_sdk::{
    symbol_short,
    testutils::{Address as _, AuthorizedFunction, AuthorizedInvocation},
    token, Address, Env, IntoVal,
};
use soroban_sdk::{testutils::MockAuth, testutils::MockAuthInvoke};
use soroban_sdk_tools::setup_mock_auth;

fn create_token_contract<'a>(
    e: &Env,
    admin: &Address,
) -> (token::Client<'a>, token::StellarAssetClient<'a>) {
    let sac = e.register_stellar_asset_contract_v2(admin.clone());
    (
        token::Client::new(e, &sac.address()),
        token::StellarAssetClient::new(e, &sac.address()),
    )
}

fn create_liqpool_contract<'a>(
    e: &Env,
    token_a: &Address,
    token_b: &Address,
) -> LiquidityPoolClient<'a> {
    LiquidityPoolClient::new(e, &e.register(crate::LiquidityPool {}, (token_a, token_b)))
}

fn mock_mint_auth(e: &Env, token: &Address, admin: &Address, to: &Address, amount: i128) {
    setup_mock_auth(e, token, "mint", (to.clone(), amount), &[admin]);
}

fn mock_deposit_auth(
    e: &Env,
    liqpool: &Address,
    token_a: &Address,
    token_b: &Address,
    user: &Address,
    desired_a: i128,
    min_a: i128,
    desired_b: i128,
    min_b: i128,
    amount_a: i128,
    amount_b: i128,
) {
    let sub_invokes = [
        MockAuthInvoke {
            contract: token_a,
            fn_name: "transfer",
            args: (user.clone(), liqpool.clone(), amount_a).into_val(e),
            sub_invokes: &[],
        },
        MockAuthInvoke {
            contract: token_b,
            fn_name: "transfer",
            args: (user.clone(), liqpool.clone(), amount_b).into_val(e),
            sub_invokes: &[],
        },
    ];
    let root = MockAuthInvoke {
        contract: liqpool,
        fn_name: "deposit",
        args: (user.clone(), desired_a, min_a, desired_b, min_b).into_val(e),
        sub_invokes: &sub_invokes,
    };
    e.mock_auths(&[MockAuth {
        address: user,
        invoke: &root,
    }]);
}

fn mock_swap_auth(
    e: &Env,
    liqpool: &Address,
    sell_token: &Address,
    user: &Address,
    buy_a: bool,
    out: i128,
    in_max: i128,
    sell_amount: i128,
) {
    let sub_invokes = [MockAuthInvoke {
        contract: sell_token,
        fn_name: "transfer",
        args: (user.clone(), liqpool.clone(), sell_amount).into_val(e),
        sub_invokes: &[],
    }];
    let root = MockAuthInvoke {
        contract: liqpool,
        fn_name: "swap",
        args: (user.clone(), buy_a, out, in_max).into_val(e),
        sub_invokes: &sub_invokes,
    };
    e.mock_auths(&[MockAuth {
        address: user,
        invoke: &root,
    }]);
}

#[test]
fn test() {
    let e = Env::default();

    let admin1 = Address::generate(&e);
    let admin2 = Address::generate(&e);

    let (token1, token1_admin) = create_token_contract(&e, &admin1);
    let (token2, token2_admin) = create_token_contract(&e, &admin2);
    let user1 = Address::generate(&e);

    let liqpool = create_liqpool_contract(&e, &token1.address, &token2.address);

    mock_mint_auth(&e, &token1.address, &admin1, &user1, 1000);
    token1_admin.mint(&user1, &1000);
    assert_eq!(token1.balance(&user1), 1000);

    mock_mint_auth(&e, &token2.address, &admin2, &user1, 1000);
    token2_admin.mint(&user1, &1000);
    assert_eq!(token2.balance(&user1), 1000);

    mock_deposit_auth(
        &e,
        &liqpool.address,
        &token1.address,
        &token2.address,
        &user1,
        100,
        100,
        100,
        100,
        100,
        100,
    );
    liqpool.deposit(&user1, &100, &100, &100, &100);
    assert_eq!(
        e.auths(),
        std::vec![(
            user1.clone(),
            AuthorizedInvocation {
                function: AuthorizedFunction::Contract((
                    liqpool.address.clone(),
                    symbol_short!("deposit"),
                    (&user1, 100_i128, 100_i128, 100_i128, 100_i128).into_val(&e)
                )),
                sub_invocations: std::vec![
                    AuthorizedInvocation {
                        function: AuthorizedFunction::Contract((
                            token1.address.clone(),
                            symbol_short!("transfer"),
                            (&user1, &liqpool.address, 100_i128).into_val(&e)
                        )),
                        sub_invocations: std::vec![]
                    },
                    AuthorizedInvocation {
                        function: AuthorizedFunction::Contract((
                            token2.address.clone(),
                            symbol_short!("transfer"),
                            (&user1, &liqpool.address, 100_i128).into_val(&e)
                        )),
                        sub_invocations: std::vec![]
                    }
                ]
            }
        )]
    );

    assert_eq!(liqpool.balance_shares(&user1), 100);
    assert_eq!(token1.balance(&user1), 900);
    assert_eq!(token1.balance(&liqpool.address), 100);
    assert_eq!(token2.balance(&user1), 900);
    assert_eq!(token2.balance(&liqpool.address), 100);

    mock_swap_auth(
        &e,
        &liqpool.address,
        &token1.address,
        &user1,
        false,
        49,
        100,
        97,
    );
    liqpool.swap(&user1, &false, &49, &100);
    assert_eq!(
        e.auths(),
        std::vec![(
            user1.clone(),
            AuthorizedInvocation {
                function: AuthorizedFunction::Contract((
                    liqpool.address.clone(),
                    symbol_short!("swap"),
                    (&user1, false, 49_i128, 100_i128).into_val(&e)
                )),
                sub_invocations: std::vec![AuthorizedInvocation {
                    function: AuthorizedFunction::Contract((
                        token1.address.clone(),
                        symbol_short!("transfer"),
                        (&user1, &liqpool.address, 97_i128).into_val(&e)
                    )),
                    sub_invocations: std::vec![]
                }]
            }
        )]
    );

    assert_eq!(token1.balance(&user1), 803);
    assert_eq!(token1.balance(&liqpool.address), 197);
    assert_eq!(token2.balance(&user1), 949);
    assert_eq!(token2.balance(&liqpool.address), 51);

    e.cost_estimate().budget().reset_unlimited();
    setup_mock_auth(
        &e,
        &liqpool.address,
        "withdraw",
        (user1.clone(), 100_i128, 197_i128, 51_i128),
        &[&user1],
    );
    liqpool.withdraw(&user1, &100, &197, &51);

    assert_eq!(
        e.auths(),
        std::vec![(
            user1.clone(),
            AuthorizedInvocation {
                function: AuthorizedFunction::Contract((
                    liqpool.address.clone(),
                    symbol_short!("withdraw"),
                    (&user1, 100_i128, 197_i128, 51_i128).into_val(&e)
                )),
                sub_invocations: std::vec![]
            }
        )]
    );

    assert_eq!(token1.balance(&user1), 1000);
    assert_eq!(token2.balance(&user1), 1000);
    assert_eq!(liqpool.balance_shares(&user1), 0);
    assert_eq!(token1.balance(&liqpool.address), 0);
    assert_eq!(token2.balance(&liqpool.address), 0);
}

#[test]
#[should_panic]
fn deposit_amount_zero_should_panic() {
    let e = Env::default();

    // Create contracts
    let admin1 = Address::generate(&e);
    let admin2 = Address::generate(&e);

    let (token1, token1_admin) = create_token_contract(&e, &admin1);
    let (token2, token2_admin) = create_token_contract(&e, &admin2);
    let liqpool = create_liqpool_contract(&e, &token1.address, &token2.address);

    // Create a user
    let user1 = Address::generate(&e);

    mock_mint_auth(&e, &token1.address, &admin1, &user1, 1000);
    token1_admin.mint(&user1, &1000);
    assert_eq!(token1.balance(&user1), 1000);

    mock_mint_auth(&e, &token2.address, &admin2, &user1, 1000);
    token2_admin.mint(&user1, &1000);
    assert_eq!(token2.balance(&user1), 1000);

    mock_deposit_auth(
        &e,
        &liqpool.address,
        &token1.address,
        &token2.address,
        &user1,
        1,
        0,
        0,
        0,
        1,
        0,
    );
    liqpool.deposit(&user1, &1, &0, &0, &0);
}

#[test]
#[should_panic]
fn swap_reserve_one_nonzero_other_zero() {
    let e = Env::default();

    // Create contracts
    let admin1 = Address::generate(&e);
    let admin2 = Address::generate(&e);

    let (token1, token1_admin) = create_token_contract(&e, &admin1);
    let (token2, token2_admin) = create_token_contract(&e, &admin2);

    let liqpool = create_liqpool_contract(&e, &token1.address, &token2.address);

    // Create a user
    let user1 = Address::generate(&e);

    mock_mint_auth(&e, &token1.address, &admin1, &user1, 1000);
    token1_admin.mint(&user1, &1000);
    assert_eq!(token1.balance(&user1), 1000);

    mock_mint_auth(&e, &token2.address, &admin2, &user1, 1000);
    token2_admin.mint(&user1, &1000);
    assert_eq!(token2.balance(&user1), 1000);

    // Try to get to a situation where the reserves are 1 and 0.
    // It shouldn't be possible.
    setup_mock_auth(
        &e,
        &token2.address,
        "transfer",
        (user1.clone(), liqpool.address.clone(), 1_i128),
        &[&user1],
    );
    token2.transfer(&user1, &liqpool.address, &1);
    setup_mock_auth(
        &e,
        &liqpool.address,
        "swap",
        (user1.clone(), false, 1_i128, 1_i128),
        &[&user1],
    );
    liqpool.swap(&user1, &false, &1, &1);
}
