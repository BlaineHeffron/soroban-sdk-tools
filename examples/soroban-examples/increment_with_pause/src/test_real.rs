#![cfg(test)]
use crate::{Error, IncrementContract, IncrementContractArgs, IncrementContractClient};
use soroban_sdk::Env;
use soroban_pause_contract::{Pause, PauseClient};

#[test]
fn test() {
    let env = Env::default();

    let pause_id = env.register(Pause, ());
    let pause_client = PauseClient::new(&env, &pause_id);

    let contract_id = env.register(
        IncrementContract,
        IncrementContractArgs::__constructor(&pause_id),
    );
    let client = IncrementContractClient::new(&env, &contract_id);

    pause_client.set(&false);
    assert_eq!(client.increment(), 1);

    pause_client.set(&true);
    assert_eq!(client.try_increment(), Err(Ok(Error::Paused)));

    pause_client.set(&false);
    assert_eq!(client.increment(), 2);
}
