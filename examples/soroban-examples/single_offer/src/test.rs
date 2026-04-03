#![cfg(test)]
extern crate std;

use crate::{token, SingleOfferClient};
use soroban_sdk::{
    symbol_short,
    testutils::{Address as _, AuthorizedFunction, AuthorizedInvocation, MockAuth, MockAuthInvoke},
    Address, Env, IntoVal, Symbol,
};

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

fn create_single_offer_contract<'a>(
    e: &Env,
    seller: &Address,
    sell_token: &Address,
    buy_token: &Address,
    sell_price: u32,
    buy_price: u32,
) -> SingleOfferClient<'a> {
    let offer = SingleOfferClient::new(e, &e.register(crate::SingleOffer, ()));
    let create_invoke = MockAuthInvoke {
        contract: &offer.address,
        fn_name: "create",
        args: (
            seller.clone(),
            sell_token.clone(),
            buy_token.clone(),
            sell_price,
            buy_price,
        )
            .into_val(e),
        sub_invokes: &[],
    };
    e.mock_auths(&[MockAuth {
        address: seller,
        invoke: &create_invoke,
    }]);
    offer.create(seller, sell_token, buy_token, &sell_price, &buy_price);

    // Verify that authorization is required for the seller.
    assert_eq!(
        e.auths(),
        std::vec![(
            seller.clone(),
            AuthorizedInvocation {
                function: AuthorizedFunction::Contract((
                    offer.address.clone(),
                    symbol_short!("create"),
                    (
                        seller,
                        sell_token.clone(),
                        buy_token.clone(),
                        sell_price,
                        buy_price
                    )
                        .into_val(e)
                )),
                sub_invocations: std::vec![]
            }
        )]
    );

    offer
}

fn mock_mint_auth(e: &Env, token: &Address, admin: &Address, to: &Address, amount: i128) {
    let mint_invoke = MockAuthInvoke {
        contract: token,
        fn_name: "mint",
        args: (to.clone(), amount).into_val(e),
        sub_invokes: &[],
    };
    e.mock_auths(&[MockAuth {
        address: admin,
        invoke: &mint_invoke,
    }]);
}

fn mock_transfer_auth(e: &Env, token: &Address, from: &Address, to: &Address, amount: i128) {
    let transfer_invoke = MockAuthInvoke {
        contract: token,
        fn_name: "transfer",
        args: (from.clone(), to.clone(), amount).into_val(e),
        sub_invokes: &[],
    };
    e.mock_auths(&[MockAuth {
        address: from,
        invoke: &transfer_invoke,
    }]);
}

fn mock_trade_auth(
    e: &Env,
    offer: &Address,
    buyer: &Address,
    buy_token: &Address,
    buy_token_amount: i128,
    min_sell_token_amount: i128,
) {
    let buy_transfer_invoke = MockAuthInvoke {
        contract: buy_token,
        fn_name: "transfer",
        args: (buyer.clone(), offer.clone(), buy_token_amount).into_val(e),
        sub_invokes: &[],
    };
    let trade_invoke = MockAuthInvoke {
        contract: offer,
        fn_name: "trade",
        args: (
            buyer.clone(),
            buy_token_amount,
            min_sell_token_amount,
        )
            .into_val(e),
        sub_invokes: &[buy_transfer_invoke],
    };
    e.mock_auths(&[MockAuth {
        address: buyer,
        invoke: &trade_invoke,
    }]);
}

fn mock_withdraw_auth(e: &Env, offer: &Address, seller: &Address, token: &Address, amount: i128) {
    let withdraw_invoke = MockAuthInvoke {
        contract: offer,
        fn_name: "withdraw",
        args: (token.clone(), amount).into_val(e),
        sub_invokes: &[],
    };
    e.mock_auths(&[MockAuth {
        address: seller,
        invoke: &withdraw_invoke,
    }]);
}

fn mock_update_price_auth(e: &Env, offer: &Address, seller: &Address, sell_price: u32, buy_price: u32) {
    let updt_price_invoke = MockAuthInvoke {
        contract: offer,
        fn_name: "updt_price",
        args: (sell_price, buy_price).into_val(e),
        sub_invokes: &[],
    };
    e.mock_auths(&[MockAuth {
        address: seller,
        invoke: &updt_price_invoke,
    }]);
}

#[test]
fn test() {
    let e = Env::default();

    let token_admin = Address::generate(&e);
    let seller = Address::generate(&e);
    let buyer = Address::generate(&e);

    let sell_token = create_token_contract(&e, &token_admin);
    let sell_token_client = sell_token.0;
    let sell_token_admin_client = sell_token.1;

    let buy_token = create_token_contract(&e, &token_admin);
    let buy_token_client = buy_token.0;
    let buy_token_admin_client = buy_token.1;

    // The price here is 1 sell_token for 2 buy_token.
    let offer = create_single_offer_contract(
        &e,
        &seller,
        &sell_token_client.address,
        &buy_token_client.address,
        1,
        2,
    );
    // Give some sell_token to seller and buy_token to buyer.
    mock_mint_auth(&e, &sell_token_client.address, &token_admin, &seller, 1000);
    sell_token_admin_client.mint(&seller, &1000);
    mock_mint_auth(&e, &buy_token_client.address, &token_admin, &buyer, 1000);
    buy_token_admin_client.mint(&buyer, &1000);
    // Deposit 100 sell_token from seller into offer.
    mock_transfer_auth(&e, &sell_token_client.address, &seller, &offer.address, 100);
    sell_token_client.transfer(&seller, &offer.address, &100);

    // Try trading 20 buy_token for at least 11 sell_token - that wouldn't
    // succeed because the offer price would result in 10 sell_token.
    mock_trade_auth(&e, &offer.address, &buyer, &buy_token_client.address, 20, 11);
    assert!(offer.try_trade(&buyer, &20_i128, &11_i128).is_err());
    // Buyer trades 20 buy_token for 10 sell_token.
    mock_trade_auth(&e, &offer.address, &buyer, &buy_token_client.address, 20, 10);
    offer.trade(&buyer, &20_i128, &10_i128);
    // Verify that authorization is required for the buyer.
    assert_eq!(
        e.auths(),
        std::vec![(
            buyer.clone(),
            AuthorizedInvocation {
                function: AuthorizedFunction::Contract((
                    offer.address.clone(),
                    symbol_short!("trade"),
                    (&buyer, 20_i128, 10_i128).into_val(&e)
                )),
                sub_invocations: std::vec![AuthorizedInvocation {
                    function: AuthorizedFunction::Contract((
                        buy_token_client.address.clone(),
                        symbol_short!("transfer"),
                        (buyer.clone(), &offer.address, 20_i128).into_val(&e)
                    )),
                    sub_invocations: std::vec![]
                }]
            }
        )]
    );

    assert_eq!(sell_token_client.balance(&seller), 900);
    assert_eq!(sell_token_client.balance(&buyer), 10);
    assert_eq!(sell_token_client.balance(&offer.address), 90);
    assert_eq!(buy_token_client.balance(&seller), 20);
    assert_eq!(buy_token_client.balance(&buyer), 980);
    assert_eq!(buy_token_client.balance(&offer.address), 0);

    // Withdraw 70 sell_token from offer.
    mock_withdraw_auth(&e, &offer.address, &seller, &sell_token_client.address, 70);
    offer.withdraw(&sell_token_client.address, &70);
    // Verify that the seller has to authorize this.
    assert_eq!(
        e.auths(),
        std::vec![(
            seller.clone(),
            AuthorizedInvocation {
                function: AuthorizedFunction::Contract((
                    offer.address.clone(),
                    symbol_short!("withdraw"),
                    (sell_token_client.address.clone(), 70_i128).into_val(&e)
                )),
                sub_invocations: std::vec![]
            }
        )]
    );

    assert_eq!(sell_token_client.balance(&seller), 970);
    assert_eq!(sell_token_client.balance(&offer.address), 20);

    // The price here is 1 sell_token = 1 buy_token.
    mock_update_price_auth(&e, &offer.address, &seller, 1, 1);
    offer.updt_price(&1, &1);
    // Verify that the seller has to authorize this.
    assert_eq!(
        e.auths(),
        std::vec![(
            seller.clone(),
            AuthorizedInvocation {
                function: AuthorizedFunction::Contract((
                    offer.address.clone(),
                    Symbol::new(&e, "updt_price"),
                    (1_u32, 1_u32).into_val(&e)
                )),
                sub_invocations: std::vec![]
            }
        )]
    );

    // Buyer trades 10 buy_token for 10 sell_token.
    mock_trade_auth(&e, &offer.address, &buyer, &buy_token_client.address, 10, 9);
    offer.trade(&buyer, &10_i128, &9_i128);
    assert_eq!(sell_token_client.balance(&seller), 970);
    assert_eq!(sell_token_client.balance(&buyer), 20);
    assert_eq!(sell_token_client.balance(&offer.address), 10);
    assert_eq!(buy_token_client.balance(&seller), 30);
    assert_eq!(buy_token_client.balance(&buyer), 970);
    assert_eq!(buy_token_client.balance(&offer.address), 0);
}
