//! Storage abstractions for Soroban contracts
//!
//! Provides typed wrappers around Soroban's storage interfaces:
//! - `PersistentMap<K, V>`: Long-term key-value storage
//! - `InstanceMap<K, V>`: Per-instance configuration storage
//! - `TemporaryMap<K, V>`: Ephemeral storage with automatic expiry
//! - Single-value variants: `PersistentItem`, `InstanceItem`, `TemporaryItem`

use soroban_sdk::storage::{Instance, Persistent, Temporary};

pub mod types;

use types::{StorageItem, StorageMap};

/// A persistent key-value map with automatic key optimization
pub type PersistentMap<K, V, W = K> = StorageMap<Persistent, K, V, W>;
/// A persistent key-value map with automatic key optimization
pub type InstanceMap<K, V, W = K> = StorageMap<Instance, K, V, W>;
/// A temporary key-value map
pub type TemporaryMap<K, V, W = K> = StorageMap<Temporary, K, V, W>;


/// A single persistent value
pub type PersistentItem<V> = StorageItem<Persistent, V>;
/// A single instance value
pub type InstanceItem<V> = StorageItem<Instance, V>;
/// A single temporary value
pub type TemporaryItem<V> = StorageItem<Temporary, V>;
