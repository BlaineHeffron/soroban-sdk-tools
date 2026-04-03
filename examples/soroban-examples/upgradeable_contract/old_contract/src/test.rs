#![cfg(test)]

extern crate std;

use soroban_sdk::{
    testutils::Address as _,
    Address, BytesN, Env, Executable,
};
use soroban_sdk_tools::setup_mock_auth;

use crate::{UpgradeableContract, UpgradeableContractClient};

fn current_wasm_hash(contract_id: &Address) -> BytesN<32> {
    match contract_id.executable() {
        Some(Executable::Wasm(hash)) => hash,
        _ => panic!("expected registered contract address to contain wasm executable"),
    }
}

#[test]
fn test() {
    let env = Env::default();

    let admin = Address::generate(&env);
    let contract_id = env.register(UpgradeableContract, (&admin,));

    let client = UpgradeableContractClient::new(&env, &contract_id);

    assert_eq!(1, client.version());

    let new_wasm_hash = current_wasm_hash(&contract_id);

    assert!(client.try_upgrade(&new_wasm_hash).is_err());

    setup_mock_auth(&env, &contract_id, "upgrade", (new_wasm_hash.clone(),), &[&admin]);
    client.upgrade(&new_wasm_hash);
    assert_eq!(1, client.version());
}
