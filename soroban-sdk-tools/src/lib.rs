//! # soroban-sdk-tools
//!
//! Enhanced tools for Soroban smart contract development including:
//! - Advanced storage management with automatic key optimization
//! - Composable error handling with the #[scerr] macro
//! - Improved authorization testing utilities

#![no_std]

// Re-export soroban-sdk types for convenience
pub use soroban_sdk;

// Re-export procedural macros
pub use soroban_sdk_tools_macro::{contractimport, contractstorage, scerr};

// Public modules
pub mod error;
pub mod key;
pub mod storage;

// Auth module is always exported but its contents are gated by testutils
pub mod auth;

// Re-export commonly used types
pub use error::{ContractError, ContractErrorSpec, ErrorSpecEntry, SequentialError, SpecNode};
pub use key::StorageKey;
pub use storage::{
    InstanceItem, InstanceMap, PersistentItem, PersistentMap, TemporaryItem, TemporaryMap,
};

#[cfg(any(test, feature = "testutils"))]
pub use auth::setup_mock_auth;
