use soroban_sdk::{
    storage::{Instance, Persistent, Temporary},
    Env, IntoVal, TryFromVal, Val,
};

#[allow(clippy::missing_errors_doc)]
/// Abstraction over different storage types
pub trait SorobanStorage {
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

impl SorobanStorage for Persistent {
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

impl SorobanStorage for Instance {
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

impl SorobanStorage for Temporary {
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

pub trait Storage {
    fn has<K>(env: &Env, key: &K) -> bool
    where
        K: IntoVal<Env, Val>;

    fn get<K, V>(env: &Env, key: &K) -> Option<V>
    where
        V::Error: core::fmt::Debug,
        K: IntoVal<Env, Val>,
        V: TryFromVal<Env, Val>;

    fn set<K, V>(env: &Env, key: &K, val: &V)
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
    fn update<K, V>(env: &Env, key: &K, f: impl FnOnce(Option<V>) -> V) -> V
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
        env: &Env,
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
    fn extend_ttl<K>(env: &Env, key: &K, threshold: u32, extend_to: u32)
    where
        K: IntoVal<Env, Val>;

    fn remove<K>(env: &Env, key: &K)
    where
        K: IntoVal<Env, Val>;
}

impl<T: SorobanStorage> Storage for T {
    fn has<K>(env: &Env, key: &K) -> bool
    where
        K: IntoVal<Env, Val>,
    {
        T::from_env(env).has(key)
    }

    fn get<K, V>(env: &Env, key: &K) -> Option<V>
    where
        V::Error: core::fmt::Debug,
        K: IntoVal<Env, Val>,
        V: TryFromVal<Env, Val>,
    {
        T::from_env(env).get(key)
    }

    fn set<K, V>(env: &Env, key: &K, val: &V)
    where
        K: IntoVal<Env, Val>,
        V: IntoVal<Env, Val>,
    {
        T::from_env(env).set(key, val);
    }

    fn update<K, V>(env: &Env, key: &K, f: impl FnOnce(Option<V>) -> V) -> V
    where
        K: IntoVal<Env, Val>,
        V: IntoVal<Env, Val>,
        V: TryFromVal<Env, Val>,
    {
        T::from_env(env).update(key, f)
    }

    fn try_update<K, V, E>(
        env: &Env,
        key: &K,
        f: impl FnOnce(Option<V>) -> Result<V, E>,
    ) -> Result<V, E>
    where
        K: IntoVal<Env, Val>,
        V: IntoVal<Env, Val>,
        V: TryFromVal<Env, Val>,
    {
        T::from_env(env).try_update(key, f)
    }

    fn extend_ttl<K>(env: &Env, key: &K, threshold: u32, extend_to: u32)
    where
        K: IntoVal<Env, Val>,
    {
        T::from_env(env).extend_ttl(key, threshold, extend_to);
    }

    fn remove<K>(env: &Env, key: &K)
    where
        K: IntoVal<Env, Val>,
    {
        T::from_env(env).remove(key);
    }
}
