#![cfg(test)]

extern crate std;

use soroban_sdk::{
    testutils::{Address as _, MockAuthInvoke},
    Address, BytesN, Env, Executable,
};

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

    soroban_sdk_tools::setup_mock_auth(
        &env,
        &[&admin],
        MockAuthInvoke {
            contract: &contract_id,
            fn_name: "upgrade",
            args: (new_wasm_hash.clone(),).into_val(&env),
            sub_invokes: &[],
        },
    );
    client.upgrade(&new_wasm_hash);
    assert_eq!(1, client.version());
}
