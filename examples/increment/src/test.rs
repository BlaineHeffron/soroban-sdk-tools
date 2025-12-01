#![cfg(test)]
use crate::{IncrementContract, IncrementContractClient};
use soroban_sdk::{testutils::Address as _, Address, Env};

#[test]
fn increment() {
    let env = &Env::default();
    let contract_id = env.register(IncrementContract, ());
    let client = IncrementContractClient::new(env, &contract_id);

    assert_eq!(client.increment(), 1);
    assert_eq!(client.increment(), 2);
    assert_eq!(client.increment(), 3);
}

#[test]
fn increment_by() {
    let env = &Env::default();
    let contract_id = env.register(IncrementContract, ());
    let client = IncrementContractClient::new(env, &contract_id);

    assert_eq!(client.increment_by(&5), 5);
    assert_eq!(client.increment_by(&10), 15);
    assert_eq!(client.increment_by(&20), 35);
}

#[test]
fn increment_for() {
    let env = &Env::default();
    let contract_id = env.register(IncrementContract, ());
    let client = IncrementContractClient::new(env, &contract_id);
    let addr1 = Address::generate(env);
    let addr2 = Address::generate(env);
    assert_eq!(client.increment_for(&addr1), 1);
    assert_eq!(client.increment_for(&addr1), 2);
    assert_eq!(client.increment_for(&addr2), 1);
    assert_eq!(client.increment_for(&addr1), 3);
}
