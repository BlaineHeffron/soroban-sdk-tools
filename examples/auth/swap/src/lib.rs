//! Atomic Swap Contract
//!
//! This contract demonstrates complex multi-party authorization:
//!
//! - **Atomic two-party swap**: Alice and Bob swap atomically
//! - **Multi-party batch swap**: Multiple parties exchange in a single tx
//! - **Escrow pattern**: One party deposits, other completes
//!
//! # Authorization Patterns Demonstrated
//!
//! 1. Two-party atomic swap (both parties must authorize)
//! 2. N-party batch swap (all parties must authorize)
//! 3. Escrow with timeout (initiator then counterparty)

#![no_std]

use soroban_sdk::{contract, contractimpl, contracttype, Address, Env, Vec};

/// Represents one side of a swap
#[derive(Clone)]
#[contracttype]
pub struct SwapParty {
    /// The address participating in the swap
    pub address: Address,
    /// The amount they are offering
    pub offer_amount: i128,
    /// The minimum amount they want to receive
    pub want_min_amount: i128,
}

/// Storage key for pending swaps
#[derive(Clone)]
#[contracttype]
pub enum DataKey {
    /// A pending escrow swap by ID
    PendingSwap(u64),
    /// Next swap ID counter
    SwapCounter,
}

/// A pending escrow swap
#[derive(Clone)]
#[contracttype]
pub struct PendingSwap {
    pub initiator: Address,
    pub counterparty: Address,
    pub initiator_amount: i128,
    pub counterparty_amount: i128,
    pub deadline: u32,
    pub completed: bool,
}

#[contract]
pub struct SwapContract;

#[contractimpl]
impl SwapContract {
    /// Atomic two-party swap
    ///
    /// **Both parties must authorize in the same transaction!**
    ///
    /// Alice gives `alice_amount` to Bob.
    /// Bob gives `bob_amount` to Alice.
    /// If either party doesn't authorize, the entire transaction fails.
    ///
    /// # Arguments
    /// * `alice` - First party address
    /// * `alice_amount` - Amount Alice is offering
    /// * `bob` - Second party address
    /// * `bob_amount` - Amount Bob is offering
    pub fn atomic_swap(
        _env: Env,
        alice: Address,
        alice_amount: i128,
        bob: Address,
        bob_amount: i128,
    ) {
        // Both parties must authorize this swap
        // This is the key multi-party auth pattern!
        alice.require_auth();
        bob.require_auth();

        // Verify amounts are valid
        if alice_amount <= 0 || bob_amount <= 0 {
            panic!("invalid amounts");
        }

        // In production, we'd transfer tokens here
        // For this demo, we just verify the auth requirements
    }

    /// Multi-party batch swap
    ///
    /// **All parties must authorize in the same transaction!**
    ///
    /// Each party in the list must authorize for the swap to proceed.
    /// This demonstrates N-party authorization.
    ///
    /// # Arguments
    /// * `parties` - List of swap parties
    pub fn batch_swap(_env: Env, parties: Vec<SwapParty>) {
        if parties.len() < 2 {
            panic!("need at least 2 parties");
        }

        // Require auth from ALL parties upfront
        // This is the N-party auth pattern!
        for i in 0..parties.len() {
            let party = parties.get(i).unwrap();
            party.address.require_auth();

            if party.offer_amount <= 0 {
                panic!("invalid offer amount");
            }
        }

        // In production, we'd match and execute swaps here
    }

    /// Three-way swap
    ///
    /// **All three parties must authorize!**
    ///
    /// A circular swap: A -> B -> C -> A
    pub fn three_way_swap(
        _env: Env,
        party_a: Address,
        party_b: Address,
        party_c: Address,
        amount: i128,
    ) {
        // All three must authorize
        party_a.require_auth();
        party_b.require_auth();
        party_c.require_auth();

        if amount <= 0 {
            panic!("invalid amount");
        }

        // In production, we'd execute A->B->C->A transfers
    }

    /// Create a pending escrow swap (initiator deposits first)
    ///
    /// The initiator authorizes and commits to a swap. The counterparty
    /// can then complete the swap before the deadline.
    ///
    /// # Arguments
    /// * `initiator` - The initiator's address
    /// * `counterparty` - The intended counterparty
    /// * `initiator_amount` - Amount the initiator is offering
    /// * `counterparty_amount` - Amount expected from counterparty
    /// * `deadline` - Ledger sequence deadline for completion
    pub fn create_escrow(
        env: Env,
        initiator: Address,
        counterparty: Address,
        initiator_amount: i128,
        counterparty_amount: i128,
        deadline: u32,
    ) -> u64 {
        // Initiator must authorize
        initiator.require_auth();

        if initiator_amount <= 0 || counterparty_amount <= 0 {
            panic!("invalid amounts");
        }

        // Get next swap ID
        let swap_id: u64 = env
            .storage()
            .instance()
            .get(&DataKey::SwapCounter)
            .unwrap_or(0);
        env.storage()
            .instance()
            .set(&DataKey::SwapCounter, &(swap_id + 1));

        // Store pending swap
        let pending = PendingSwap {
            initiator,
            counterparty,
            initiator_amount,
            counterparty_amount,
            deadline,
            completed: false,
        };
        env.storage()
            .persistent()
            .set(&DataKey::PendingSwap(swap_id), &pending);

        swap_id
    }

    /// Complete an escrow swap (counterparty fulfills)
    ///
    /// The counterparty authorizes and completes the swap.
    ///
    /// # Arguments
    /// * `swap_id` - The swap ID to complete
    pub fn complete_escrow(env: Env, swap_id: u64) {
        let mut pending: PendingSwap = env
            .storage()
            .persistent()
            .get(&DataKey::PendingSwap(swap_id))
            .expect("swap not found");

        if pending.completed {
            panic!("swap already completed");
        }

        // Check deadline
        if env.ledger().sequence() > pending.deadline {
            panic!("swap expired");
        }

        // Counterparty must authorize
        pending.counterparty.require_auth();

        // Mark as completed
        pending.completed = true;
        env.storage()
            .persistent()
            .set(&DataKey::PendingSwap(swap_id), &pending);

        // In production, we'd execute the token transfers here
    }

    /// Cancel an escrow swap (initiator reclaims after deadline)
    ///
    /// If the counterparty doesn't complete by the deadline, the initiator
    /// can reclaim.
    ///
    /// # Arguments
    /// * `swap_id` - The swap ID to cancel
    pub fn cancel_escrow(env: Env, swap_id: u64) {
        let pending: PendingSwap = env
            .storage()
            .persistent()
            .get(&DataKey::PendingSwap(swap_id))
            .expect("swap not found");

        if pending.completed {
            panic!("swap already completed");
        }

        // Can only cancel after deadline
        if env.ledger().sequence() <= pending.deadline {
            panic!("swap not yet expired");
        }

        // Initiator must authorize the cancellation
        pending.initiator.require_auth();

        // Clean up
        env.storage()
            .persistent()
            .remove(&DataKey::PendingSwap(swap_id));

        // In production, we'd return escrowed tokens here
    }

    /// Get a pending swap's details
    pub fn get_pending_swap(env: Env, swap_id: u64) -> PendingSwap {
        env.storage()
            .persistent()
            .get(&DataKey::PendingSwap(swap_id))
            .expect("swap not found")
    }
}

#[cfg(test)]
mod test {
    use soroban_sdk::testutils::Address as _;
    use soroban_sdk::xdr::{ScError, ScErrorCode};
    use soroban_sdk::{vec, Address, Env};

    mod swap {
        soroban_sdk_tools::contractimport!(file = "../../../target/stellar/soroban_auth_swap.wasm");
    }

    /// Demonstrates two-party atomic swap authorization.
    ///
    /// BOTH Alice AND Bob must authorize for the swap to proceed.
    /// Without authorization the call fails; auth is not sticky across calls.
    #[test]
    fn test_atomic_swap() {
        let env = &Env::default();
        let contract_id = &env.register(swap::WASM, ());
        let client = swap::AuthClient::new(env, contract_id);

        let alice = &Address::generate(env);
        let bob = &Address::generate(env);

        // Without auth -> fails
        let error: ScError = client
            .atomic_swap(alice, &100, bob, &200)
            .try_invoke()
            .unwrap_err()
            .unwrap()
            .try_into()
            .unwrap();
        assert_eq!(error, ScError::Context(ScErrorCode::InvalidAction));

        // With auth -> succeeds
        client
            .atomic_swap(alice, &100, bob, &200)
            .authorize_all(&[alice, bob])
            .invoke();

        // Auth is not sticky — next call without auth fails again
        let error: ScError = client
            .atomic_swap(alice, &100, bob, &200)
            .try_invoke()
            .unwrap_err()
            .unwrap()
            .try_into()
            .unwrap();
        assert_eq!(error, ScError::Context(ScErrorCode::InvalidAction));
    }

    /// Demonstrates three-party swap authorization.
    ///
    /// ALL THREE parties must authorize for the swap to proceed.
    #[test]
    fn test_three_way_swap() {
        let env = &Env::default();
        let contract_id = &env.register(swap::WASM, ());
        let client = swap::AuthClient::new(env, contract_id);

        let a = &Address::generate(env);
        let b = &Address::generate(env);
        let c = &Address::generate(env);

        client
            .three_way_swap(a, b, c, &100)
            .authorize_all(&[a, b, c])
            .invoke();
    }

    /// Demonstrates N-party batch swap authorization.
    ///
    /// Every party in the batch must authorize.
    #[test]
    fn test_batch_swap() {
        let env = &Env::default();
        let contract_id = &env.register(swap::WASM, ());
        let client = swap::AuthClient::new(env, contract_id);

        let alice = &Address::generate(env);
        let bob = &Address::generate(env);
        let charlie = &Address::generate(env);

        let parties = vec![
            env,
            swap::SwapParty {
                address: alice.clone(),
                offer_amount: 100,
                want_min_amount: 90,
            },
            swap::SwapParty {
                address: bob.clone(),
                offer_amount: 200,
                want_min_amount: 180,
            },
            swap::SwapParty {
                address: charlie.clone(),
                offer_amount: 150,
                want_min_amount: 140,
            },
        ];

        client
            .batch_swap(&parties)
            .authorize_all(&[alice, bob, charlie])
            .invoke();
    }

    /// Demonstrates escrow flow with sequential single-party auth.
    ///
    /// First the initiator authorizes to create the escrow,
    /// then the counterparty authorizes to complete it.
    #[test]
    fn test_escrow_flow() {
        let env = &Env::default();
        let contract_id = &env.register(swap::WASM, ());
        let client = swap::AuthClient::new(env, contract_id);

        let initiator = &Address::generate(env);
        let counterparty = &Address::generate(env);

        // Step 1: Initiator creates escrow
        let swap_id = client
            .create_escrow(initiator, counterparty, &100, &200, &1000)
            .authorize(initiator)
            .invoke();

        // Verify pending swap
        let pending = client.get_pending_swap(&swap_id).invoke();
        assert_eq!(&pending.initiator, initiator);
        assert_eq!(&pending.counterparty, counterparty);
        assert!(!pending.completed);

        // Step 2: Counterparty completes escrow
        client
            .complete_escrow(&swap_id)
            .authorize(counterparty)
            .invoke();

        // Verify completed
        let completed = client.get_pending_swap(&swap_id).invoke();
        assert!(completed.completed);
    }
}
