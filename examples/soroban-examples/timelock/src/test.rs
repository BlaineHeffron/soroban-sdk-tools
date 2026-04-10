#![cfg(test)]
extern crate std;

use super::*;
use soroban_sdk::testutils::{
    Address as _, AuthorizedFunction, AuthorizedInvocation, Ledger, MockAuth, MockAuthInvoke,
};
use soroban_sdk::{symbol_short, token, vec, Address, Env, IntoVal};
use token::Client as TokenClient;
use token::StellarAssetClient as TokenAdminClient;

fn create_token_contract<'a>(e: &Env, admin: &Address) -> (TokenClient<'a>, TokenAdminClient<'a>) {
    let sac = e.register_stellar_asset_contract_v2(admin.clone());
    (
        token::Client::new(e, &sac.address()),
        token::StellarAssetClient::new(e, &sac.address()),
    )
}

fn create_claimable_balance_contract<'a>(e: &Env) -> ClaimableBalanceContractClient<'a> {
    ClaimableBalanceContractClient::new(e, &e.register(ClaimableBalanceContract, ()))
}

struct ClaimableBalanceTest<'a> {
    env: Env,
    deposit_address: Address,
    claim_addresses: [Address; 3],
    token: TokenClient<'a>,
    contract: ClaimableBalanceContractClient<'a>,
}

impl<'a> ClaimableBalanceTest<'a> {
    fn setup() -> Self {
        let env = Env::default();

        env.ledger().with_mut(|li| {
            li.timestamp = 12345;
        });

        let deposit_address = Address::generate(&env);

        let claim_addresses = [
            Address::generate(&env),
            Address::generate(&env),
            Address::generate(&env),
        ];

        let token_admin = Address::generate(&env);

        let (token, token_admin_client) = create_token_contract(&env, &token_admin);
        let mint_invoke = MockAuthInvoke {
            contract: &token.address,
            fn_name: "mint",
            args: (deposit_address.clone(), 1000_i128).into_val(&env),
            sub_invokes: &[],
        };
        env.mock_auths(&[MockAuth {
            address: &token_admin,
            invoke: &mint_invoke,
        }]);
        token_admin_client.mint(&deposit_address, &1000);

        let contract = create_claimable_balance_contract(&env);
        ClaimableBalanceTest {
            env,
            deposit_address,
            claim_addresses,
            token,
            contract,
        }
    }

    fn setup_deposit_auth(&self, amount: i128, claimants: Vec<Address>, time_bound: TimeBound) {
        let transfer_invoke = MockAuthInvoke {
            contract: &self.token.address,
            fn_name: "transfer",
            args: (
                self.deposit_address.clone(),
                self.contract.address.clone(),
                amount,
            )
                .into_val(&self.env),
            sub_invokes: &[],
        };
        let deposit_invoke = MockAuthInvoke {
            contract: &self.contract.address,
            fn_name: "deposit",
            args: (
                self.deposit_address.clone(),
                self.token.address.clone(),
                amount,
                claimants,
                time_bound,
            )
                .into_val(&self.env),
            sub_invokes: &[transfer_invoke],
        };
        self.env.mock_auths(&[MockAuth {
            address: &self.deposit_address,
            invoke: &deposit_invoke,
        }]);
    }

    fn setup_claim_auth(&self, claimant: &Address) {
        let claim_invoke = MockAuthInvoke {
            contract: &self.contract.address,
            fn_name: "claim",
            args: (claimant.clone(),).into_val(&self.env),
            sub_invokes: &[],
        };
        self.env.mock_auths(&[MockAuth {
            address: claimant,
            invoke: &claim_invoke,
        }]);
    }
}

#[test]
fn test_deposit_and_claim() {
    let test = ClaimableBalanceTest::setup();
    let claimants = vec![
        &test.env,
        test.claim_addresses[0].clone(),
        test.claim_addresses[1].clone(),
    ];
    let time_bound = TimeBound {
        kind: TimeBoundKind::Before,
        timestamp: 12346,
    };
    test.setup_deposit_auth(800, claimants.clone(), time_bound.clone());
    test.contract.deposit(
        &test.deposit_address,
        &test.token.address,
        &800,
        &claimants,
        &time_bound,
    );

    assert_eq!(
        test.env.auths(),
        [(
            test.deposit_address.clone(),
            AuthorizedInvocation {
                function: AuthorizedFunction::Contract((
                    test.contract.address.clone(),
                    symbol_short!("deposit"),
                    (
                        test.deposit_address.clone(),
                        test.token.address.clone(),
                        800_i128,
                        vec![
                            &test.env,
                            test.claim_addresses[0].clone(),
                            test.claim_addresses[1].clone()
                        ],
                        TimeBound {
                            kind: TimeBoundKind::Before,
                            timestamp: 12346,
                        },
                    )
                        .into_val(&test.env),
                )),
                sub_invocations: std::vec![AuthorizedInvocation {
                    function: AuthorizedFunction::Contract((
                        test.token.address.clone(),
                        symbol_short!("transfer"),
                        (
                            test.deposit_address.clone(),
                            &test.contract.address,
                            800_i128,
                        )
                            .into_val(&test.env),
                    )),
                    sub_invocations: std::vec![]
                }]
            }
        ),]
    );

    assert_eq!(test.token.balance(&test.deposit_address), 200);
    assert_eq!(test.token.balance(&test.contract.address), 800);
    assert_eq!(test.token.balance(&test.claim_addresses[1]), 0);

    test.setup_claim_auth(&test.claim_addresses[1]);
    test.contract.claim(&test.claim_addresses[1]);
    assert_eq!(
        test.env.auths(),
        [(
            test.claim_addresses[1].clone(),
            AuthorizedInvocation {
                function: AuthorizedFunction::Contract((
                    test.contract.address.clone(),
                    symbol_short!("claim"),
                    (test.claim_addresses[1].clone(),).into_val(&test.env),
                )),
                sub_invocations: std::vec![]
            }
        ),]
    );

    assert_eq!(test.token.balance(&test.deposit_address), 200);
    assert_eq!(test.token.balance(&test.contract.address), 0);
    assert_eq!(test.token.balance(&test.claim_addresses[1]), 800);
}

#[test]
#[should_panic(expected = "contract has been already initialized")]
fn test_double_deposit_not_possible() {
    let test = ClaimableBalanceTest::setup();
    let claimants = vec![&test.env, test.claim_addresses[0].clone()];
    let time_bound = TimeBound {
        kind: TimeBoundKind::Before,
        timestamp: 12346,
    };
    test.setup_deposit_auth(1, claimants.clone(), time_bound.clone());
    test.contract.deposit(
        &test.deposit_address,
        &test.token.address,
        &1,
        &claimants,
        &time_bound,
    );
    test.setup_deposit_auth(1, claimants.clone(), time_bound.clone());
    test.contract.deposit(
        &test.deposit_address,
        &test.token.address,
        &1,
        &claimants,
        &time_bound,
    );
}

#[test]
#[should_panic(expected = "claimant is not allowed to claim this balance")]
fn test_unauthorized_claim_not_possible() {
    let test = ClaimableBalanceTest::setup();
    let claimants = vec![
        &test.env,
        test.claim_addresses[0].clone(),
        test.claim_addresses[1].clone(),
    ];
    let time_bound = TimeBound {
        kind: TimeBoundKind::Before,
        timestamp: 12346,
    };
    test.setup_deposit_auth(800, claimants.clone(), time_bound.clone());
    test.contract.deposit(
        &test.deposit_address,
        &test.token.address,
        &800,
        &claimants,
        &time_bound,
    );

    test.setup_claim_auth(&test.claim_addresses[2]);
    test.contract.claim(&test.claim_addresses[2]);
}

#[test]
#[should_panic(expected = "time predicate is not fulfilled")]
fn test_out_of_time_bound_claim_not_possible() {
    let test = ClaimableBalanceTest::setup();
    let claimants = vec![&test.env, test.claim_addresses[0].clone()];
    let time_bound = TimeBound {
        kind: TimeBoundKind::After,
        timestamp: 12346,
    };
    test.setup_deposit_auth(800, claimants.clone(), time_bound.clone());
    test.contract.deposit(
        &test.deposit_address,
        &test.token.address,
        &800,
        &claimants,
        &time_bound,
    );

    test.setup_claim_auth(&test.claim_addresses[0]);
    test.contract.claim(&test.claim_addresses[0]);
}

#[test]
#[should_panic]
fn test_double_claim_not_possible() {
    let test = ClaimableBalanceTest::setup();
    let claimants = vec![&test.env, test.claim_addresses[0].clone()];
    let time_bound = TimeBound {
        kind: TimeBoundKind::Before,
        timestamp: 12346,
    };
    test.setup_deposit_auth(800, claimants.clone(), time_bound.clone());
    test.contract.deposit(
        &test.deposit_address,
        &test.token.address,
        &800,
        &claimants,
        &time_bound,
    );

    test.setup_claim_auth(&test.claim_addresses[0]);
    test.contract.claim(&test.claim_addresses[0]);
    assert_eq!(test.token.balance(&test.claim_addresses[0]), 800);
    test.setup_claim_auth(&test.claim_addresses[0]);
    test.contract.claim(&test.claim_addresses[0]);
}

#[test]
#[should_panic(expected = "contract has been already initialized")]
fn test_deposit_after_claim_not_possible() {
    let test = ClaimableBalanceTest::setup();
    let claimants = vec![&test.env, test.claim_addresses[0].clone()];
    let time_bound = TimeBound {
        kind: TimeBoundKind::After,
        timestamp: 12344,
    };
    test.setup_deposit_auth(800, claimants.clone(), time_bound.clone());
    test.contract.deposit(
        &test.deposit_address,
        &test.token.address,
        &800,
        &claimants,
        &time_bound,
    );

    test.setup_claim_auth(&test.claim_addresses[0]);
    test.contract.claim(&test.claim_addresses[0]);
    assert_eq!(test.token.balance(&test.claim_addresses[0]), 800);
    test.setup_deposit_auth(200, claimants.clone(), time_bound.clone());
    test.contract.deposit(
        &test.deposit_address,
        &test.token.address,
        &200,
        &claimants,
        &time_bound,
    );
}
