#![cfg(test)]
extern crate std;

use crate::{contract::Token, TokenClient};
use soroban_sdk::{
    symbol_short,
    testutils::{Address as _, AuthorizedFunction, AuthorizedInvocation, MockAuthInvoke},
    Address, Env, FromVal, IntoVal, String, Symbol,
};

fn create_token<'a>(e: &Env, admin: &Address) -> TokenClient<'a> {
    let token_contract = e.register(
        Token,
        (
            admin,
            7_u32,
            String::from_val(e, &"name"),
            String::from_val(e, &"symbol"),
        ),
    );
    TokenClient::new(e, &token_contract)
}

#[test]
fn test() {
    let e = Env::default();

    let admin1 = Address::generate(&e);
    let admin2 = Address::generate(&e);
    let user1 = Address::generate(&e);
    let user2 = Address::generate(&e);
    let user3 = Address::generate(&e);
    let token = create_token(&e, &admin1);

    soroban_sdk_tools::setup_mock_auth(&e, &[&admin1], MockAuthInvoke { contract: &token.address, fn_name: "mint", args: (user1.clone(), 1000_i128).into_val(&e), sub_invokes: &[] });
    token.mint(&user1, &1000);
    assert_eq!(
        e.auths(),
        std::vec![(
            admin1.clone(),
            AuthorizedInvocation {
                function: AuthorizedFunction::Contract((
                    token.address.clone(),
                    symbol_short!("mint"),
                    (&user1, 1000_i128).into_val(&e),
                )),
                sub_invocations: std::vec![]
            }
        )]
    );
    assert_eq!(token.balance(&user1), 1000);

    soroban_sdk_tools::setup_mock_auth(&e, &[&user2], MockAuthInvoke { contract: &token.address, fn_name: "approve", args: (user2.clone(), user3.clone(), 500_i128, 200_u32).into_val(&e), sub_invokes: &[] });
    token.approve(&user2, &user3, &500, &200);
    assert_eq!(
        e.auths(),
        std::vec![(
            user2.clone(),
            AuthorizedInvocation {
                function: AuthorizedFunction::Contract((
                    token.address.clone(),
                    symbol_short!("approve"),
                    (&user2, &user3, 500_i128, 200_u32).into_val(&e),
                )),
                sub_invocations: std::vec![]
            }
        )]
    );
    assert_eq!(token.allowance(&user2, &user3), 500);

    soroban_sdk_tools::setup_mock_auth(&e, &[&user1], MockAuthInvoke { contract: &token.address, fn_name: "transfer", args: (user1.clone(), user2.clone(), 600_i128).into_val(&e), sub_invokes: &[] });
    token.transfer(&user1, &user2, &600);
    assert_eq!(
        e.auths(),
        std::vec![(
            user1.clone(),
            AuthorizedInvocation {
                function: AuthorizedFunction::Contract((
                    token.address.clone(),
                    symbol_short!("transfer"),
                    (&user1, &user2, 600_i128).into_val(&e),
                )),
                sub_invocations: std::vec![]
            }
        )]
    );
    assert_eq!(token.balance(&user1), 400);
    assert_eq!(token.balance(&user2), 600);

    soroban_sdk_tools::setup_mock_auth(&e, &[&user3], MockAuthInvoke { contract: &token.address, fn_name: "transfer_from", args: (user3.clone(), user2.clone(), user1.clone(), 400_i128).into_val(&e), sub_invokes: &[] });
    token.transfer_from(&user3, &user2, &user1, &400);
    assert_eq!(
        e.auths(),
        std::vec![(
            user3.clone(),
            AuthorizedInvocation {
                function: AuthorizedFunction::Contract((
                    token.address.clone(),
                    Symbol::new(&e, "transfer_from"),
                    (&user3, &user2, &user1, 400_i128).into_val(&e),
                )),
                sub_invocations: std::vec![]
            }
        )]
    );
    assert_eq!(token.balance(&user1), 800);
    assert_eq!(token.balance(&user2), 200);

    soroban_sdk_tools::setup_mock_auth(&e, &[&user1], MockAuthInvoke { contract: &token.address, fn_name: "transfer", args: (user1.clone(), user3.clone(), 300_i128).into_val(&e), sub_invokes: &[] });
    token.transfer(&user1, &user3, &300);
    assert_eq!(token.balance(&user1), 500);
    assert_eq!(token.balance(&user3), 300);

    soroban_sdk_tools::setup_mock_auth(&e, &[&admin1], MockAuthInvoke { contract: &token.address, fn_name: "set_admin", args: (admin2.clone(),).into_val(&e), sub_invokes: &[] });
    token.set_admin(&admin2);
    assert_eq!(
        e.auths(),
        std::vec![(
            admin1.clone(),
            AuthorizedInvocation {
                function: AuthorizedFunction::Contract((
                    token.address.clone(),
                    symbol_short!("set_admin"),
                    (&admin2,).into_val(&e),
                )),
                sub_invocations: std::vec![]
            }
        )]
    );

    // Increase to 500
    soroban_sdk_tools::setup_mock_auth(&e, &[&user2], MockAuthInvoke { contract: &token.address, fn_name: "approve", args: (user2.clone(), user3.clone(), 500_i128, 200_u32).into_val(&e), sub_invokes: &[] });
    token.approve(&user2, &user3, &500, &200);
    assert_eq!(token.allowance(&user2, &user3), 500);
    soroban_sdk_tools::setup_mock_auth(&e, &[&user2], MockAuthInvoke { contract: &token.address, fn_name: "approve", args: (user2.clone(), user3.clone(), 0_i128, 200_u32).into_val(&e), sub_invokes: &[] });
    token.approve(&user2, &user3, &0, &200);
    assert_eq!(
        e.auths(),
        std::vec![(
            user2.clone(),
            AuthorizedInvocation {
                function: AuthorizedFunction::Contract((
                    token.address.clone(),
                    symbol_short!("approve"),
                    (&user2, &user3, 0_i128, 200_u32).into_val(&e),
                )),
                sub_invocations: std::vec![]
            }
        )]
    );
    assert_eq!(token.allowance(&user2, &user3), 0);
}

#[test]
fn test_burn() {
    let e = Env::default();

    let admin = Address::generate(&e);
    let user1 = Address::generate(&e);
    let user2 = Address::generate(&e);
    let token = create_token(&e, &admin);

    soroban_sdk_tools::setup_mock_auth(&e, &[&admin], MockAuthInvoke { contract: &token.address, fn_name: "mint", args: (user1.clone(), 1000_i128).into_val(&e), sub_invokes: &[] });
    token.mint(&user1, &1000);
    assert_eq!(token.balance(&user1), 1000);

    soroban_sdk_tools::setup_mock_auth(&e, &[&user1], MockAuthInvoke { contract: &token.address, fn_name: "approve", args: (user1.clone(), user2.clone(), 500_i128, 200_u32).into_val(&e), sub_invokes: &[] });
    token.approve(&user1, &user2, &500, &200);
    assert_eq!(token.allowance(&user1, &user2), 500);

    soroban_sdk_tools::setup_mock_auth(&e, &[&user2], MockAuthInvoke { contract: &token.address, fn_name: "burn_from", args: (user2.clone(), user1.clone(), 500_i128).into_val(&e), sub_invokes: &[] });
    token.burn_from(&user2, &user1, &500);
    assert_eq!(
        e.auths(),
        std::vec![(
            user2.clone(),
            AuthorizedInvocation {
                function: AuthorizedFunction::Contract((
                    token.address.clone(),
                    symbol_short!("burn_from"),
                    (&user2, &user1, 500_i128).into_val(&e),
                )),
                sub_invocations: std::vec![]
            }
        )]
    );

    assert_eq!(token.allowance(&user1, &user2), 0);
    assert_eq!(token.balance(&user1), 500);
    assert_eq!(token.balance(&user2), 0);

    soroban_sdk_tools::setup_mock_auth(&e, &[&user1], MockAuthInvoke { contract: &token.address, fn_name: "burn", args: (user1.clone(), 500_i128).into_val(&e), sub_invokes: &[] });
    token.burn(&user1, &500);
    assert_eq!(
        e.auths(),
        std::vec![(
            user1.clone(),
            AuthorizedInvocation {
                function: AuthorizedFunction::Contract((
                    token.address.clone(),
                    symbol_short!("burn"),
                    (&user1, 500_i128).into_val(&e),
                )),
                sub_invocations: std::vec![]
            }
        )]
    );

    assert_eq!(token.balance(&user1), 0);
    assert_eq!(token.balance(&user2), 0);
}

#[test]
#[should_panic(expected = "insufficient balance")]
fn transfer_insufficient_balance() {
    let e = Env::default();

    let admin = Address::generate(&e);
    let user1 = Address::generate(&e);
    let user2 = Address::generate(&e);
    let token = create_token(&e, &admin);

    soroban_sdk_tools::setup_mock_auth(&e, &[&admin], MockAuthInvoke { contract: &token.address, fn_name: "mint", args: (user1.clone(), 1000_i128).into_val(&e), sub_invokes: &[] });
    token.mint(&user1, &1000);
    assert_eq!(token.balance(&user1), 1000);

    soroban_sdk_tools::setup_mock_auth(&e, &[&user1], MockAuthInvoke { contract: &token.address, fn_name: "transfer", args: (user1.clone(), user2.clone(), 1001_i128).into_val(&e), sub_invokes: &[] });
    token.transfer(&user1, &user2, &1001);
}

#[test]
#[should_panic(expected = "insufficient allowance")]
fn transfer_from_insufficient_allowance() {
    let e = Env::default();

    let admin = Address::generate(&e);
    let user1 = Address::generate(&e);
    let user2 = Address::generate(&e);
    let user3 = Address::generate(&e);
    let token = create_token(&e, &admin);

    soroban_sdk_tools::setup_mock_auth(&e, &[&admin], MockAuthInvoke { contract: &token.address, fn_name: "mint", args: (user1.clone(), 1000_i128).into_val(&e), sub_invokes: &[] });
    token.mint(&user1, &1000);
    assert_eq!(token.balance(&user1), 1000);

    soroban_sdk_tools::setup_mock_auth(&e, &[&user1], MockAuthInvoke { contract: &token.address, fn_name: "approve", args: (user1.clone(), user3.clone(), 100_i128, 200_u32).into_val(&e), sub_invokes: &[] });
    token.approve(&user1, &user3, &100, &200);
    assert_eq!(token.allowance(&user1, &user3), 100);

    soroban_sdk_tools::setup_mock_auth(&e, &[&user3], MockAuthInvoke { contract: &token.address, fn_name: "transfer_from", args: (user3.clone(), user1.clone(), user2.clone(), 101_i128).into_val(&e), sub_invokes: &[] });
    token.transfer_from(&user3, &user1, &user2, &101);
}

#[test]
#[should_panic(expected = "Decimal must not be greater than 18")]
fn decimal_is_over_eighteen() {
    let e = Env::default();
    let admin = Address::generate(&e);
    let _ = TokenClient::new(
        &e,
        &e.register(
            Token,
            (
                admin,
                19_u32,
                String::from_val(&e, &"name"),
                String::from_val(&e, &"symbol"),
            ),
        ),
    );
}

#[test]
fn test_zero_allowance() {
    // Here we test that transfer_from with a 0 amount does not create an empty allowance
    let e = Env::default();

    let admin = Address::generate(&e);
    let spender = Address::generate(&e);
    let from = Address::generate(&e);
    let token = create_token(&e, &admin);

    soroban_sdk_tools::setup_mock_auth(&e, &[&spender], MockAuthInvoke { contract: &token.address, fn_name: "transfer_from", args: (spender.clone(), from.clone(), spender.clone(), 0_i128).into_val(&e), sub_invokes: &[] });
    token.transfer_from(&spender, &from, &spender, &0);
    assert!(token.get_allowance(&from, &spender).is_none());
}
