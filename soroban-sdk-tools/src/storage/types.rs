use crate::key::{hash_key, make_map_key, StorageKey};
use soroban_sdk::{
    storage::{Instance, Persistent, Temporary},
    Env, IntoVal, TryFromVal, Val,
};

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

pub trait Storage {
    fn from_env(env: &Env) -> Self
    where
        Self: Sized;

    fn has<K>(&self, key: &K) -> bool
    where
        K: IntoVal<Env, Val>;

    fn get<K, V>(&self, key: &K) -> Option<V>
    where
        V::Error: core::fmt::Debug,
        K: IntoVal<Env, Val>,
        V: TryFromVal<Env, Val>;

    fn set<K, V>(&self, key: &K, val: &V)
    where
        K: IntoVal<Env, Val>,
        V: IntoVal<Env, Val>;

    /// Update a value stored against a key.
    ///
    /// Loads the value, calls the function with it, then sets the value to the
    /// returned value of the function.  If no value is stored with the key then
    /// the function is called with None.
    ///
    /// The returned value is the value stored after updating.
    fn update<K, V>(&self, key: &K, f: impl FnOnce(Option<V>) -> V) -> V
    where
        K: IntoVal<Env, Val>,
        V: IntoVal<Env, Val>,
        V: TryFromVal<Env, Val>;

    /// Update a value stored against a key.
    ///
    /// Loads the value, calls the function with it, then sets the value to the
    /// returned value of the function.  If no value is stored with the key then
    /// the function is called with None.  If the function returns an error it
    /// will be passed through.
    ///
    /// The returned value is the value stored after updating.
    fn try_update<K, V, E>(
        &self,
        key: &K,
        f: impl FnOnce(Option<V>) -> Result<V, E>,
    ) -> Result<V, E>
    where
        K: IntoVal<Env, Val>,
        V: IntoVal<Env, Val>,
        V: TryFromVal<Env, Val>;

    /// Extend the TTL of the data under the key.
    ///
    /// Extends the TTL only if the TTL for the provided data is below `threshold` ledgers.
    /// The TTL will then become `extend_to`.
    ///
    /// The TTL is the number of ledgers between the current ledger and the final ledger the data can still be accessed.
    fn extend_ttl<K>(&self, key: &K, threshold: u32, extend_to: u32)
    where
        K: IntoVal<Env, Val>;

    fn remove<K>(&self, key: &K)
    where
        K: IntoVal<Env, Val>;
}

impl Storage for Persistent {
    fn has<K>(&self, key: &K) -> bool
    where
        K: IntoVal<Env, Val>,
    {
        self.has(key)
    }

    fn get<K, V>(&self, key: &K) -> Option<V>
    where
        V::Error: core::fmt::Debug,
        K: IntoVal<Env, Val>,
        V: TryFromVal<Env, Val>,
    {
        self.get(key)
    }

    fn set<K, V>(&self, key: &K, val: &V)
    where
        K: IntoVal<Env, Val>,
        V: IntoVal<Env, Val>,
    {
        self.set(key, val);
    }

    fn update<K, V>(&self, key: &K, f: impl FnOnce(Option<V>) -> V) -> V
    where
        K: IntoVal<Env, Val>,
        V: IntoVal<Env, Val>,
        V: TryFromVal<Env, Val>,
    {
        self.update(key, f)
    }

    fn try_update<K, V, E>(
        &self,
        key: &K,
        f: impl FnOnce(Option<V>) -> Result<V, E>,
    ) -> Result<V, E>
    where
        K: IntoVal<Env, Val>,
        V: IntoVal<Env, Val>,
        V: TryFromVal<Env, Val>,
    {
        self.try_update(key, f)
    }

    fn extend_ttl<K>(&self, key: &K, threshold: u32, extend_to: u32)
    where
        K: IntoVal<Env, Val>,
    {
        self.extend_ttl(key, threshold, extend_to);
    }

    fn remove<K>(&self, key: &K)
    where
        K: IntoVal<Env, Val>,
    {
        self.remove(key);
    }

    fn from_env(env: &Env) -> Self
    where
        Self: Sized,
    {
        env.storage().persistent()
    }
}

impl Storage for Instance {
    fn has<K>(&self, key: &K) -> bool
    where
        K: IntoVal<Env, Val>,
    {
        self.has(key)
    }

    fn get<K, V>(&self, key: &K) -> Option<V>
    where
        V::Error: core::fmt::Debug,
        K: IntoVal<Env, Val>,
        V: TryFromVal<Env, Val>,
    {
        self.get(key)
    }

    fn set<K, V>(&self, key: &K, val: &V)
    where
        K: IntoVal<Env, Val>,
        V: IntoVal<Env, Val>,
    {
        self.set(key, val);
    }

    fn update<K, V>(&self, key: &K, f: impl FnOnce(Option<V>) -> V) -> V
    where
        K: IntoVal<Env, Val>,
        V: IntoVal<Env, Val>,
        V: TryFromVal<Env, Val>,
    {
        self.update(key, f)
    }

    fn try_update<K, V, E>(
        &self,
        key: &K,
        f: impl FnOnce(Option<V>) -> Result<V, E>,
    ) -> Result<V, E>
    where
        K: IntoVal<Env, Val>,
        V: IntoVal<Env, Val>,
        V: TryFromVal<Env, Val>,
    {
        self.try_update(key, f)
    }

    fn extend_ttl<K>(&self, _: &K, threshold: u32, extend_to: u32)
    where
        K: IntoVal<Env, Val>,
    {
        self.extend_ttl(threshold, extend_to);
    }

    fn remove<K>(&self, key: &K)
    where
        K: IntoVal<Env, Val>,
    {
        self.remove(key);
    }

    fn from_env(env: &Env) -> Self
    where
        Self: Sized,
    {
        env.storage().instance()
    }
}

impl Storage for Temporary {
    fn has<K>(&self, key: &K) -> bool
    where
        K: IntoVal<Env, Val>,
    {
        self.has(key)
    }

    fn get<K, V>(&self, key: &K) -> Option<V>
    where
        V::Error: core::fmt::Debug,
        K: IntoVal<Env, Val>,
        V: TryFromVal<Env, Val>,
    {
        self.get(key)
    }

    fn set<K, V>(&self, key: &K, val: &V)
    where
        K: IntoVal<Env, Val>,
        V: IntoVal<Env, Val>,
    {
        self.set(key, val);
    }

    fn update<K, V>(&self, key: &K, f: impl FnOnce(Option<V>) -> V) -> V
    where
        K: IntoVal<Env, Val>,
        V: IntoVal<Env, Val>,
        V: TryFromVal<Env, Val>,
    {
        self.update(key, f)
    }

    fn try_update<K, V, E>(
        &self,
        key: &K,
        f: impl FnOnce(Option<V>) -> Result<V, E>,
    ) -> Result<V, E>
    where
        K: IntoVal<Env, Val>,
        V: IntoVal<Env, Val>,
        V: TryFromVal<Env, Val>,
    {
        self.try_update(key, f)
    }

    fn extend_ttl<K>(&self, key: &K, threshold: u32, extend_to: u32)
    where
        K: IntoVal<Env, Val>,
    {
        self.extend_ttl(key, threshold, extend_to);
    }

    fn remove<K>(&self, key: &K)
    where
        K: IntoVal<Env, Val>,
    {
        self.remove(key);
    }

    fn from_env(env: &Env) -> Self
    where
        Self: Sized,
    {
        env.storage().temporary()
    }
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
        self.env.logs().add("map mode is raw ", &[
            match &self.mode {
                MapKeyMode::Hashed(_) => false.into_val(&self.env),
                MapKeyMode::Raw => true.into_val(&self.env),
            },
        ]);
        match &self.mode {
            MapKeyMode::Hashed(prefix) => make_map_key(&self.env, prefix, &w),
            MapKeyMode::Raw => w.to_key(&self.env),
        }
    }

    /// Get a value from the map
    pub fn get(&self, key: &K) -> Option<V> {
        let k = self.composite_key_val(key);
        S::from_env(&self.env).get(&k)
    }

    /// Set a value in the map
    pub fn set(&self, key: &K, value: &V) {
        let k = self.composite_key_val(key);
        self.env.logs().add("setting key", &[k.clone()]);
        S::from_env(&self.env).set(&k, value);
    }

    /// Check if a key exists
    pub fn has(&self, key: &K) -> bool {
        let k = self.composite_key_val(key);
        S::from_env(&self.env).has(&k)
    }

    /// Remove a key from the map
    pub fn remove(&self, key: &K) {
        let k = self.composite_key_val(key);
        S::from_env(&self.env).remove(&k);
    }

    /// Extend the TTL for a key
    pub fn extend_ttl(&self, key: &K, threshold: u32, extend_to: u32) {
        let k = self.composite_key_val(key);
        S::from_env(&self.env).extend_ttl(&k, threshold, extend_to);
    }
}

/// A single persistent value
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
    pub fn get(&self) -> Option<V> {
        S::from_env(&self.env).get(&self.key)
    }

    pub fn set(&self, value: &V) {
        S::from_env(&self.env).set(&self.key, value);
    }

    #[must_use]
    pub fn has(&self) -> bool {
        S::from_env(&self.env).has(&self.key)
    }

    pub fn remove(&self) {
        S::from_env(&self.env).remove(&self.key);
    }

    pub fn extend_ttl(&self, threshold: u32, extend_to: u32) {
        S::from_env(&self.env).extend_ttl(&self.key, threshold, extend_to);
    }
}
