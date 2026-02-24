use soroban_sdk::{Env, IntoVal, TryFromVal, Val};

use crate::{key::hash_key, storage::types::Storage};

/// Internal enum to represent the storage key for a `StorageItem`
#[derive(Clone)]
enum ItemKey {
    /// Hashed key (for prefixes > 32 bytes)
    Hashed(soroban_sdk::BytesN<32>),
    /// Short key <= 32 bytes, not hashed
    Bytes(soroban_sdk::Bytes),
    /// Fully specified key Val (DataKey-style)
    Val(Val),
}

impl IntoVal<Env, Val> for ItemKey {
    fn into_val(&self, env: &Env) -> Val {
        match self {
            ItemKey::Hashed(k) => k.into_val(env),
            ItemKey::Bytes(b) => b.into_val(env),
            ItemKey::Val(v) => *v,
        }
    }
}

impl ItemKey {
    pub fn from_prefix(env: &Env, prefix: &str) -> Self {
        let prefix_bytes = soroban_sdk::Bytes::from_slice(env, prefix.as_bytes());
        if prefix_bytes.len() <= 32 {
            ItemKey::Bytes(prefix_bytes)
        } else {
            let storage_key = hash_key(env, &prefix_bytes, None);
            ItemKey::Hashed(storage_key)
        }
    }
}

/// A singleton value which handles
#[derive(Clone)]
pub struct StorageItem<S, V> {
    env: Env,
    key: ItemKey,
    _phantom: core::marker::PhantomData<(S, V)>,
}

impl<S, V> StorageItem<S, V>
where
    S: Storage,
    V: IntoVal<Env, Val> + TryFromVal<Env, Val>,
{
    /// Create a new persistent item with hashed or short key:
    /// - If prefix is <= 32 bytes, use raw Bytes directly (no hash).
    /// - If longer, store sha256(prefix) as 32B `BytesN`.
    #[must_use]
    pub fn new_hashed(env: &Env, prefix: &str) -> Self {
        Self {
            env: env.clone(),
            key: ItemKey::from_prefix(env, prefix),
            _phantom: core::marker::PhantomData,
        }
    }

    /// Create a new persistent item that uses a fully-specified Val (DataKey-style)
    #[must_use]
    pub fn new_raw(env: &Env, key_val: Val) -> Self {
        Self {
            env: env.clone(),
            key: ItemKey::Val(key_val),
            _phantom: core::marker::PhantomData,
        }
    }

    #[must_use]
    /// Get the value from storage
    pub fn get(&self) -> Option<V> {
        S::get(&self.env, &self.key)
    }

    /// Set the value in storage
    pub fn set(&self, value: &V) {
        S::set(&self.env, &self.key, value);
    }

    #[must_use]
    /// Check if the value exists in storage
    pub fn has(&self) -> bool {
        S::has(&self.env, &self.key)
    }

    /// Remove the value from storage
    pub fn remove(&self) {
        S::remove(&self.env, &self.key);
    }

    /// Extend the TTL for the item
    pub fn extend_ttl(&self, threshold: u32, extend_to: u32) {
        S::extend_ttl(&self.env, &self.key, threshold, extend_to);
    }

    /// Get the storage key
    pub fn get_storage_key(&self) -> Val {
        self.key.into_val(&self.env)
    }

    /// Executes the passed function passing it the current value and sets the returned value to storage.
    pub fn update(&self, f: impl FnOnce(Option<V>) -> V) -> V {
        S::update(&self.env, &self.key, f)
    }
}
