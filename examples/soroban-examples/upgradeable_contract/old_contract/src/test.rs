#![cfg(test)]

extern crate std;

use soroban_sdk::{
    testutils::{Address as _, MockAuthInvoke},
    Address, Env, IntoVal,
};

use crate::{UpgradeableContract, UpgradeableContractClient};

// Pre-built wasm of the new contract version, used to obtain a fresh wasm
// hash to upgrade to.
mod new_contract {
    soroban_sdk::contractimport!(
        file = "../../fixtures/soroban_upgradeable_contract_new_contract.wasm"
    );
}

#[test]
fn test() {
    let env = Env::default();

    let admin = Address::generate(&env);
    let contract_id = env.register(UpgradeableContract, (&admin,));

    let client = UpgradeableContractClient::new(&env, &contract_id);

    assert_eq!(1, client.version());

    // Upload the new contract wasm and use its hash to upgrade.
    let new_wasm_hash = env.deployer().upload_contract_wasm(new_contract::WASM);

    // Without auth, the upgrade should fail.
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

    // After upgrade, version() now comes from new_contract and returns 2.
    assert_eq!(2, client.version());
}
