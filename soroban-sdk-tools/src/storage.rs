//! Storage abstractions for Soroban contracts
//!
//! Provides typed wrappers around Soroban's storage interfaces:
//! - `PersistentMap<K, V>`: Long-term key-value storage
//! - `InstanceMap<K, V>`: Per-instance configuration storage
//! - `TemporaryMap<K, V>`: Ephemeral storage with automatic expiry
//! - Single-value variants: `PersistentItem`, `InstanceItem`, `TemporaryItem`

use crate::key::{hash_key, make_map_key, StorageKey};
use soroban_sdk::{Env, IntoVal, TryFromVal, Val};

#[derive(Clone)]
enum MapKeyMode {
    Hashed(soroban_sdk::Bytes), // hash(prefix [+ key])
    Raw,                        // store key.to_key(env) directly (DataKey-style)
}

#[derive(Clone)]
enum ItemKey {
    Hashed(soroban_sdk::BytesN<32>),
    Bytes(soroban_sdk::Bytes), // short key <= 32 bytes, not hashed
    Val(Val),                  // fully specified key Val (DataKey-style)
}

/// A persistent key-value map with automatic key optimization
#[derive(Clone)]
pub struct PersistentMap<K, V, W = K>
where
    K: Into<W>,
    W: StorageKey,
    V: IntoVal<Env, Val> + TryFromVal<Env, Val>,
{
    env: Env,
    mode: MapKeyMode,
    _phantom: core::marker::PhantomData<(K, V, W)>,
}

impl<K, V, W> PersistentMap<K, V, W>
where
    K: Into<W>,
    W: StorageKey,
    V: IntoVal<Env, Val> + TryFromVal<Env, Val>,
{
    /// Create a new persistent map that hashes the final key
    pub fn new_hashed(env: &Env, prefix: &str) -> Self {
        Self {
            env: env.clone(),
            mode: MapKeyMode::Hashed(soroban_sdk::Bytes::from_slice(env, prefix.as_bytes())),
            _phantom: core::marker::PhantomData,
        }
    }

    /// Create a new persistent map that stores fully formed keys directly (DataKey-style)
    pub fn new_raw(env: &Env) -> Self {
        Self {
            env: env.clone(),
            mode: MapKeyMode::Raw,
            _phantom: core::marker::PhantomData,
        }
    }

    fn composite_key_val(&self, key: K) -> Val {
        match &self.mode {
            MapKeyMode::Hashed(prefix) => {
                let w: W = key.into();
                make_map_key(&self.env, prefix, &w)
            }
            MapKeyMode::Raw => {
                let w: W = key.into();
                w.to_key(&self.env)
            }
        }
    }

    /// Get a value from the map
    pub fn get(&self, key: K) -> Option<V> {
        let k = self.composite_key_val(key);
        self.env.storage().persistent().get(&k)
    }

    /// Set a value in the map
    pub fn set(&self, key: K, value: &V) {
        let k = self.composite_key_val(key);
        self.env.storage().persistent().set(&k, value);
    }

    /// Check if a key exists
    pub fn has(&self, key: K) -> bool {
        let k = self.composite_key_val(key);
        self.env.storage().persistent().has(&k)
    }

    /// Remove a key from the map
    pub fn remove(&self, key: K) {
        let k = self.composite_key_val(key);
        self.env.storage().persistent().remove(&k);
    }

    /// Extend the TTL for a key
    pub fn extend_ttl(&self, key: K, threshold: u32, extend_to: u32) {
        let k = self.composite_key_val(key);
        self.env
            .storage()
            .persistent()
            .extend_ttl(&k, threshold, extend_to);
    }
}

/// An instance-scoped key-value map
#[derive(Clone)]
pub struct InstanceMap<K, V, W = K>
where
    K: Into<W>,
    W: StorageKey,
    V: IntoVal<Env, Val> + TryFromVal<Env, Val>,
{
    env: Env,
    mode: MapKeyMode,
    _phantom: core::marker::PhantomData<(K, V, W)>,
}

impl<K, V, W> InstanceMap<K, V, W>
where
    K: Into<W>,
    W: StorageKey,
    V: IntoVal<Env, Val> + TryFromVal<Env, Val>,
{
    /// Create a new instance map that hashes the final key
    pub fn new_hashed(env: &Env, prefix: &str) -> Self {
        Self {
            env: env.clone(),
            mode: MapKeyMode::Hashed(soroban_sdk::Bytes::from_slice(env, prefix.as_bytes())),
            _phantom: core::marker::PhantomData,
        }
    }

    /// Create a new instance map that stores fully formed keys directly (DataKey-style)
    pub fn new_raw(env: &Env) -> Self {
        Self {
            env: env.clone(),
            mode: MapKeyMode::Raw,
            _phantom: core::marker::PhantomData,
        }
    }

    fn composite_key_val(&self, key: K) -> Val {
        match &self.mode {
            MapKeyMode::Hashed(prefix) => {
                let w: W = key.into();
                make_map_key(&self.env, prefix, &w)
            }
            MapKeyMode::Raw => {
                let w: W = key.into();
                w.to_key(&self.env)
            }
        }
    }

    /// Get a value from the map
    pub fn get(&self, key: K) -> Option<V> {
        let k = self.composite_key_val(key);
        self.env.storage().instance().get(&k)
    }

    /// Set a value in the map
    pub fn set(&self, key: K, value: &V) {
        let k = self.composite_key_val(key);
        self.env.storage().instance().set(&k, value);
    }

    /// Check if a key exists
    pub fn has(&self, key: K) -> bool {
        let k = self.composite_key_val(key);
        self.env.storage().instance().has(&k)
    }

    /// Remove a key from the map
    pub fn remove(&self, key: K) {
        let k = self.composite_key_val(key);
        self.env.storage().instance().remove(&k);
    }
}

/// A temporary key-value map
#[derive(Clone)]
pub struct TemporaryMap<K, V, W = K>
where
    K: Into<W>,
    W: StorageKey,
    V: IntoVal<Env, Val> + TryFromVal<Env, Val>,
{
    env: Env,
    mode: MapKeyMode,
    _phantom: core::marker::PhantomData<(K, V, W)>,
}

impl<K, V, W> TemporaryMap<K, V, W>
where
    K: Into<W>,
    W: StorageKey,
    V: IntoVal<Env, Val> + TryFromVal<Env, Val>,
{
    /// Create a new temporary map that hashes the final key
    pub fn new_hashed(env: &Env, prefix: &str) -> Self {
        Self {
            env: env.clone(),
            mode: MapKeyMode::Hashed(soroban_sdk::Bytes::from_slice(env, prefix.as_bytes())),
            _phantom: core::marker::PhantomData,
        }
    }

    /// Create a new temporary map that stores fully formed keys directly (DataKey-style)
    pub fn new_raw(env: &Env) -> Self {
        Self {
            env: env.clone(),
            mode: MapKeyMode::Raw,
            _phantom: core::marker::PhantomData,
        }
    }

    fn composite_key_val(&self, key: K) -> Val {
        match &self.mode {
            MapKeyMode::Hashed(prefix) => {
                let w: W = key.into();
                make_map_key(&self.env, prefix, &w)
            }
            MapKeyMode::Raw => {
                let w: W = key.into();
                w.to_key(&self.env)
            }
        }
    }

    /// Get a value from the map
    pub fn get(&self, key: K) -> Option<V> {
        let k = self.composite_key_val(key);
        self.env.storage().temporary().get(&k)
    }

    /// Set a value in the map with default TTL
    /// Note: Temporary storage entries expire automatically after their TTL
    pub fn set(&self, key: K, value: &V) {
        let k = self.composite_key_val(key);
        self.env.storage().temporary().set(&k, value);
    }

    /// Check if a key exists
    pub fn has(&self, key: K) -> bool {
        let k = self.composite_key_val(key);
        self.env.storage().temporary().has(&k)
    }

    /// Remove a key from the map
    pub fn remove(&self, key: K) {
        let k = self.composite_key_val(key);
        self.env.storage().temporary().remove(&k);
    }

    /// Extend the TTL for a key
    ///
    /// # Arguments
    /// * `key` - The key to extend TTL for
    /// * `threshold` - Only extend if TTL is below this threshold
    /// * `extend_to` - Extend TTL to this many ledgers
    pub fn extend_ttl(&self, key: K, threshold: u32, extend_to: u32) {
        let k = self.composite_key_val(key);
        self.env
            .storage()
            .temporary()
            .extend_ttl(&k, threshold, extend_to);
    }
}

/// A single persistent value
#[derive(Clone)]
pub struct PersistentItem<V> {
    env: Env,
    key: ItemKey,
    _phantom: core::marker::PhantomData<V>,
}

impl<V> PersistentItem<V>
where
    V: IntoVal<Env, Val> + TryFromVal<Env, Val>,
{
    /// Create a new persistent item with hashed or short key:
    /// - If prefix is <= 32 bytes, use raw Bytes directly (no hash).
    /// - If longer, store sha256(prefix) as 32B BytesN.
    pub fn new_hashed(env: &Env, prefix: &str) -> Self {
        let prefix_bytes = soroban_sdk::Bytes::from_slice(env, prefix.as_bytes());
        let key = if prefix_bytes.len() <= 32 {
            ItemKey::Bytes(prefix_bytes)
        } else {
            let storage_key = hash_key(env, &prefix_bytes, None);
            ItemKey::Hashed(storage_key)
        };
        Self {
            env: env.clone(),
            key,
            _phantom: core::marker::PhantomData,
        }
    }

    /// Create a new persistent item that uses a fully-specified Val (DataKey-style)
    pub fn new_raw(env: &Env, key_val: Val) -> Self {
        Self {
            env: env.clone(),
            key: ItemKey::Val(key_val),
            _phantom: core::marker::PhantomData,
        }
    }

    pub fn get(&self) -> Option<V> {
        match &self.key {
            ItemKey::Hashed(k) => self.env.storage().persistent().get(k),
            ItemKey::Bytes(b) => self.env.storage().persistent().get(b),
            ItemKey::Val(v) => self.env.storage().persistent().get(v),
        }
    }

    pub fn set(&self, value: &V) {
        match &self.key {
            ItemKey::Hashed(k) => self.env.storage().persistent().set(k, value),
            ItemKey::Bytes(b) => self.env.storage().persistent().set(b, value),
            ItemKey::Val(v) => self.env.storage().persistent().set(v, value),
        }
    }

    pub fn has(&self) -> bool {
        match &self.key {
            ItemKey::Hashed(k) => self.env.storage().persistent().has(k),
            ItemKey::Bytes(b) => self.env.storage().persistent().has(b),
            ItemKey::Val(v) => self.env.storage().persistent().has(v),
        }
    }

    pub fn remove(&self) {
        match &self.key {
            ItemKey::Hashed(k) => self.env.storage().persistent().remove(k),
            ItemKey::Bytes(b) => self.env.storage().persistent().remove(b),
            ItemKey::Val(v) => self.env.storage().persistent().remove(v),
        }
    }

    pub fn extend_ttl(&self, threshold: u32, extend_to: u32) {
        match &self.key {
            ItemKey::Hashed(k) => self
                .env
                .storage()
                .persistent()
                .extend_ttl(k, threshold, extend_to),
            ItemKey::Bytes(b) => self
                .env
                .storage()
                .persistent()
                .extend_ttl(b, threshold, extend_to),
            ItemKey::Val(v) => self
                .env
                .storage()
                .persistent()
                .extend_ttl(v, threshold, extend_to),
        }
    }
}

/// A single instance value
#[derive(Clone)]
pub struct InstanceItem<V> {
    env: Env,
    key: ItemKey,
    _phantom: core::marker::PhantomData<V>,
}

impl<V> InstanceItem<V>
where
    V: IntoVal<Env, Val> + TryFromVal<Env, Val>,
{
    /// Create a new instance item with hashed or short key:
    /// - If prefix is <= 32 bytes, use raw Bytes directly (no hash).
    /// - If longer, store sha256(prefix) as 32B BytesN.
    pub fn new_hashed(env: &Env, prefix: &str) -> Self {
        let prefix_bytes = soroban_sdk::Bytes::from_slice(env, prefix.as_bytes());
        let key = if prefix_bytes.len() <= 32 {
            ItemKey::Bytes(prefix_bytes)
        } else {
            let storage_key = hash_key(env, &prefix_bytes, None);
            ItemKey::Hashed(storage_key)
        };
        Self {
            env: env.clone(),
            key,
            _phantom: core::marker::PhantomData,
        }
    }

    /// Create a new instance item that uses a fully-specified Val (DataKey-style)
    pub fn new_raw(env: &Env, key_val: Val) -> Self {
        Self {
            env: env.clone(),
            key: ItemKey::Val(key_val),
            _phantom: core::marker::PhantomData,
        }
    }

    /// Get the value
    pub fn get(&self) -> Option<V> {
        match &self.key {
            ItemKey::Hashed(k) => self.env.storage().instance().get(k),
            ItemKey::Bytes(b) => self.env.storage().instance().get(b),
            ItemKey::Val(v) => self.env.storage().instance().get(v),
        }
    }

    /// Set the value
    pub fn set(&self, value: &V) {
        match &self.key {
            ItemKey::Hashed(k) => self.env.storage().instance().set(k, value),
            ItemKey::Bytes(b) => self.env.storage().instance().set(b, value),
            ItemKey::Val(v) => self.env.storage().instance().set(v, value),
        }
    }

    /// Check if the value exists
    pub fn has(&self) -> bool {
        match &self.key {
            ItemKey::Hashed(k) => self.env.storage().instance().has(k),
            ItemKey::Bytes(b) => self.env.storage().instance().has(b),
            ItemKey::Val(v) => self.env.storage().instance().has(v),
        }
    }

    /// Remove the value
    pub fn remove(&self) {
        match &self.key {
            ItemKey::Hashed(k) => self.env.storage().instance().remove(k),
            ItemKey::Bytes(b) => self.env.storage().instance().remove(b),
            ItemKey::Val(v) => self.env.storage().instance().remove(v),
        }
    }
}

/// A single temporary value
#[derive(Clone)]
pub struct TemporaryItem<V> {
    env: Env,
    key: ItemKey,
    _phantom: core::marker::PhantomData<V>,
}

impl<V> TemporaryItem<V>
where
    V: IntoVal<Env, Val> + TryFromVal<Env, Val>,
{
    /// Create a new temporary item with hashed or short key:
    /// - If prefix is <= 32 bytes, use raw Bytes directly (no hash).
    /// - If longer, store sha256(prefix) as 32B BytesN.
    pub fn new_hashed(env: &Env, prefix: &str) -> Self {
        let prefix_bytes = soroban_sdk::Bytes::from_slice(env, prefix.as_bytes());
        let key = if prefix_bytes.len() <= 32 {
            ItemKey::Bytes(prefix_bytes)
        } else {
            let storage_key = hash_key(env, &prefix_bytes, None);
            ItemKey::Hashed(storage_key)
        };
        Self {
            env: env.clone(),
            key,
            _phantom: core::marker::PhantomData,
        }
    }

    /// Create a new temporary item that uses a fully-specified Val (DataKey-style)
    pub fn new_raw(env: &Env, key_val: Val) -> Self {
        Self {
            env: env.clone(),
            key: ItemKey::Val(key_val),
            _phantom: core::marker::PhantomData,
        }
    }

    /// Legacy constructor for backward compatibility (uses hashed mode)
    pub fn new(env: &Env, prefix: &str) -> Self {
        Self::new_hashed(env, prefix)
    }

    /// Get the value
    pub fn get(&self) -> Option<V> {
        match &self.key {
            ItemKey::Hashed(k) => self.env.storage().temporary().get(k),
            ItemKey::Bytes(b) => self.env.storage().temporary().get(b),
            ItemKey::Val(v) => self.env.storage().temporary().get(v),
        }
    }

    /// Set the value with default TTL
    /// Note: Temporary storage entries expire automatically after their TTL
    pub fn set(&self, value: &V) {
        match &self.key {
            ItemKey::Hashed(k) => self.env.storage().temporary().set(k, value),
            ItemKey::Bytes(b) => self.env.storage().temporary().set(b, value),
            ItemKey::Val(v) => self.env.storage().temporary().set(v, value),
        }
    }

    /// Check if the value exists
    pub fn has(&self) -> bool {
        match &self.key {
            ItemKey::Hashed(k) => self.env.storage().temporary().has(k),
            ItemKey::Bytes(b) => self.env.storage().temporary().has(b),
            ItemKey::Val(v) => self.env.storage().temporary().has(v),
        }
    }

    /// Remove the value
    pub fn remove(&self) {
        match &self.key {
            ItemKey::Hashed(k) => self.env.storage().temporary().remove(k),
            ItemKey::Bytes(b) => self.env.storage().temporary().remove(b),
            ItemKey::Val(v) => self.env.storage().temporary().remove(v),
        }
    }

    /// Extend the TTL for this item
    pub fn extend_ttl(&self, threshold: u32, extend_to: u32) {
        match &self.key {
            ItemKey::Hashed(k) => self
                .env
                .storage()
                .temporary()
                .extend_ttl(k, threshold, extend_to),
            ItemKey::Bytes(b) => self
                .env
                .storage()
                .temporary()
                .extend_ttl(b, threshold, extend_to),
            ItemKey::Val(v) => self
                .env
                .storage()
                .temporary()
                .extend_ttl(v, threshold, extend_to),
        }
    }
}
