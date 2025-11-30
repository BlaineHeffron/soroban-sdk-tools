use soroban_sdk::{Env, IntoVal, TryFromVal, Val};

use crate::{
    key::{make_map_key, StorageKey},
    storage::types::Storage,
};

#[derive(Clone)]
enum MapKeyMode {
    Hashed(soroban_sdk::Bytes), // hash(prefix [+ key])
    Raw,                        // store key.to_key(env) directly (DataKey-style)
}

/// A persistent key-value map with automatic key optimization
#[derive(Clone)]
pub struct StorageMap<S, K, V, W = K>
where
    S: Storage,
    K: Into<W> + Clone,
    W: StorageKey,
    V: IntoVal<Env, Val> + TryFromVal<Env, Val>,
{
    env: Env,
    mode: MapKeyMode,
    _phantom: core::marker::PhantomData<(S, K, V, W)>,
}

impl<S, K, V, W> StorageMap<S, K, V, W>
where
    S: Storage,
    K: Into<W> + Clone,
    W: StorageKey,
    V: IntoVal<Env, Val> + TryFromVal<Env, Val>,
{
    /// Create a new persistent map that hashes the final key
    #[must_use]
    pub fn new_hashed(env: &Env, prefix: &str) -> Self {
        Self {
            env: env.clone(),
            mode: MapKeyMode::Hashed(soroban_sdk::Bytes::from_slice(env, prefix.as_bytes())),
            _phantom: core::marker::PhantomData,
        }
    }

    /// Create a new persistent map that stores fully formed keys directly (DataKey-style)
    #[must_use]
    pub fn new_raw(env: &Env) -> Self {
        Self {
            env: env.clone(),
            mode: MapKeyMode::Raw,
            _phantom: core::marker::PhantomData,
        }
    }

    fn composite_key_val(&self, key: &K) -> Val {
        let w: W = key.clone().into();
        match &self.mode {
            MapKeyMode::Hashed(prefix) => make_map_key(&self.env, prefix, &w),
            MapKeyMode::Raw => w.to_key(&self.env),
        }
    }

    /// Get a value from the map
    pub fn get(&self, key: &K) -> Option<V> {
        let k = self.composite_key_val(key);
        S::get(&self.env, &k)
    }

    /// Set a value in the map
    pub fn set(&self, key: &K, value: &V) {
        let k = self.composite_key_val(key);
        S::set(&self.env, &k, value);
    }

    /// Check if a key exists
    pub fn has(&self, key: &K) -> bool {
        let k = self.composite_key_val(key);
        S::has(&self.env, &k)
    }

    /// Remove a key from the map
    pub fn remove(&self, key: &K) {
        let k = self.composite_key_val(key);
        S::remove(&self.env, &k);
    }

    /// Extend the TTL for a key
    pub fn extend_ttl(&self, key: &K, threshold: u32, extend_to: u32) {
        let k = self.composite_key_val(key);
        S::extend_ttl(&self.env, &k, threshold, extend_to);
    }

    /// Get the storage key for a given map key
    pub fn get_storage_key(&self, key: &K) -> Val {
        self.composite_key_val(key)
    }

    /// Executes the passed function and sets the returned value to storage.
    pub fn update(&self, key: &K, f: impl FnOnce(Option<V>) -> V) -> V {
        let k = self.composite_key_val(key);
        S::update(&self.env, &k, f)
    }
}
