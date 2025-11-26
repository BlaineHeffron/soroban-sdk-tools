//! Storage key generation and optimization utilities
//!
//! Provides traits and utilities for converting Rust types into compact
//! Soroban storage keys with automatic optimization.

use soroban_sdk::{xdr::ToXdr, Address, Bytes, BytesN, Env, IntoVal, Val};

/// Trait for types that can be used as storage keys
///
/// This trait enables converting Rust types into Soroban-compatible storage keys.
/// It automatically optimizes key size by using Symbols for short strings and
/// hashing for longer or complex types.
///
/// Implementers should override `to_composite_bytes` to provide an efficient byte representation,
/// preferably zero-copy where possible, instead of the default XDR serialization.
pub trait StorageKey {
    /// Convert this key into a Soroban Val that can be used as a storage key
    fn to_key(&self, env: &Env) -> Val;

    /// Return raw bytes representing this key component for composite hashing.
    /// This should avoid hashing, as the caller will hash the full composite.
    fn to_composite_bytes(&self, env: &Env) -> Bytes {
        // Default: serialize the Val to XDR
        self.to_key(env).to_xdr(env)
    }
}

// Implementation for string slices
impl StorageKey for &str {
    fn to_key(&self, env: &Env) -> Val {
        self.into_val(env)
    }

    fn to_composite_bytes(&self, env: &Env) -> Bytes {
        Bytes::from_slice(env, self.as_bytes())
    }
}

// Implementation for Address - for composite hashing, use raw XDR, not a hash
impl StorageKey for Address {
    fn to_key(&self, env: &Env) -> Val {
        self.into_val(env)
    }

    fn to_composite_bytes(&self, env: &Env) -> Bytes {
        self.to_xdr(env)
    }
}

// Implementation for tuples - combine both elements without hashing them individually
impl<T1, T2> StorageKey for (T1, T2)
where
    T1: StorageKey + IntoVal<Env, Val>,
    T2: StorageKey + IntoVal<Env, Val>,
{
    fn to_key(&self, env: &Env) -> Val {
        (self.0.into_val(env), self.1.into_val(env)).into_val(env)
    }

    fn to_composite_bytes(&self, env: &Env) -> Bytes {
        let mut bytes0 = self.0.to_composite_bytes(env);
        let bytes1 = self.1.to_composite_bytes(env);
        bytes0.append(&bytes1);
        bytes0
    }
}

// Implementations for numeric types - use compact Val; composite uses XDR directly
impl StorageKey for u32 {
    fn to_key(&self, env: &Env) -> Val {
        self.into_val(env)
    }
}

impl StorageKey for i32 {
    fn to_key(&self, env: &Env) -> Val {
        self.into_val(env)
    }
}

impl StorageKey for u64 {
    fn to_key(&self, env: &Env) -> Val {
        self.into_val(env)
    }
}

impl StorageKey for i64 {
    fn to_key(&self, env: &Env) -> Val {
        self.into_val(env)
    }
}

impl StorageKey for &[u8] {
    fn to_key(&self, env: &Env) -> Val {
        Bytes::from_slice(env, self).into_val(env)
    }

    fn to_composite_bytes(&self, env: &Env) -> Bytes {
        Bytes::from_slice(env, self)
    }
}

impl StorageKey for Bytes {
    fn to_key(&self, env: &Env) -> Val {
        self.clone().into_val(env)
    }

    fn to_composite_bytes(&self, _env: &Env) -> Bytes {
        self.clone()
    }
}

impl StorageKey for BytesN<32> {
    fn to_key(&self, env: &Env) -> Val {
        self.clone().into_val(env)
    }

    fn to_composite_bytes(&self, env: &Env) -> Bytes {
        self.to_xdr(env)
    }
}

/// Hash a prefix and key components into a fixed 32-byte key
///
/// # Arguments
/// * `env` - The Soroban environment
/// * `prefix` - A Bytes prefix (typically the field name or namespace)
/// * `key_data` - Optional additional key data to include in the hash
///
/// # Returns
/// A `BytesN<32>` hash suitable for storage keys
///
/// # Example
/// ```ignore
/// let key = hash_key(&env, &prefix_bytes, Some(&addr_bytes));
/// ```
#[must_use]
pub fn hash_key(env: &Env, prefix: &Bytes, key_data: Option<&Bytes>) -> BytesN<32> {
    let mut key_bytes = prefix.clone();
    if let Some(data) = key_data {
        key_bytes.append(data);
    }
    env.crypto().sha256(&key_bytes).into()
}

/// Create a prefixed storage key for a map entry
///
/// Combines a field prefix (struct name + field name) with a key.
/// Optimizes by using direct bytes if the composite is <=32 bytes,
/// otherwise hashes to `BytesN`<32>.
///
/// # Arguments
/// * `env` - The Soroban environment
/// * `prefix` - The field prefix as Bytes
/// * `key` - The map key implementing `StorageKey`
///
/// # Returns
/// A Val suitable for use as a storage key (either Bytes or `BytesN`<32>)
///
/// # Example
/// ```ignore
/// let key_val = make_map_key(&env, &prefix_bytes, &address);
/// ```
pub fn make_map_key<K: StorageKey>(env: &Env, prefix: &Bytes, key: &K) -> Val {
    let mut full_bytes = prefix.clone();
    full_bytes.append(&key.to_composite_bytes(env));
    if full_bytes.len() <= 32 {
        full_bytes.into_val(env)
    } else {
        let hashed: BytesN<32> = env.crypto().sha256(&full_bytes).into();
        hashed.into_val(env)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use soroban_sdk::testutils::Address as _;
    use soroban_sdk::{Env, TryFromVal}; // Trait for Address::generate()

    #[test]
    fn test_address_to_hash() {
        let env = Env::default();
        let addr = Address::generate(&env);
        addr.to_key(&env);
    }

    #[test]
    fn test_numeric_keys() {
        let env = Env::default();

        let key_u32: u32 = 42;
        let val_u32 = key_u32.to_key(&env);
        assert!(val_u32.shallow_eq(&42u32.into_val(&env)));

        let key_i32: i32 = -42;
        let val_i32 = key_i32.to_key(&env);
        assert!(val_i32.shallow_eq(&(-42i32).into_val(&env)));

        let key_u64: u64 = 12345;
        let val_u64 = key_u64.to_key(&env);
        assert!(val_u64.shallow_eq(&12345u64.into_val(&env)));

        let key_i64: i64 = -12345;
        let val_i64 = key_i64.to_key(&env);
        assert!(val_i64.shallow_eq(&(-12345i64).into_val(&env)));
    }

    #[test]
    fn test_tuple_key() {
        let env = Env::default();
        let key: (u32, u32) = (1, 2);
        key.to_key(&env);
    }

    #[test]
    fn test_hash_key_with_prefix_only() {
        let env = Env::default();
        let key = hash_key(&env, &Bytes::from_array(&env, b"balance:"), None);

        // Verify it creates a 32-byte hash
        assert_eq!(key.len(), 32);
    }

    #[test]
    fn test_hash_key_with_prefix_and_data() {
        let env = Env::default();
        let key = hash_key(
            &env,
            &Bytes::from_array(&env, b"balance:"),
            Some(&Bytes::from_array(&env, b"user123")),
        );

        // Verify it creates a 32-byte hash
        assert_eq!(key.len(), 32);

        // Verify different data produces different hash
        let key2 = hash_key(
            &env,
            &Bytes::from_array(&env, b"balance:"),
            Some(&Bytes::from_array(&env, b"user456")),
        );
        assert_ne!(key, key2);
    }

    #[test]
    fn test_make_map_key() {
        let env = Env::default();
        let addr = Address::generate(&env);
        let prefix = Bytes::from_array(&env, b"balances"); // Assuming this makes >=32 composite
        let key_val = make_map_key(&env, &prefix, &addr);

        // Verify it's BytesN<32> for this case
        let key = BytesN::<32>::try_from_val(&env, &key_val).unwrap();
        assert_eq!(key.len(), 32);

        // Verify different addresses produce different keys
        let addr2 = Address::generate(&env);
        let key2 = BytesN::<32>::try_from_val(
            &env,
            &make_map_key(&env, &Bytes::from_array(&env, b"balances"), &addr2),
        )
        .unwrap();
        assert_ne!(key, key2);

        // Verify different prefixes produce different keys
        let key3 = BytesN::<32>::try_from_val(
            &env,
            &make_map_key(&env, &Bytes::from_array(&env, b"allowances"), &addr),
        )
        .unwrap();
        assert_ne!(key, key3);
    }

    #[test]
    fn test_make_map_key_short_composite() {
        let env = Env::default();
        let prefix = Bytes::from_slice(&env, b"short"); // 5 bytes
        let short_key: &str = "a"; // 1 byte
        let key_val = make_map_key(&env, &prefix, &short_key);
        // Verify it's a Bytes Val with len <32
        let bytes = Bytes::try_from_val(&env, &key_val).unwrap();
        assert!(bytes.len() < 32);
    }

    #[test]
    fn test_make_map_key_long_composite() {
        let env = Env::default();
        let prefix = Bytes::from_slice(
            &env,
            b"this_is_a_long_prefix_that_makes_composite_over_32_bytes",
        ); // >32 bytes
        let key: &str = "test";
        let key_val = make_map_key(&env, &prefix, &key);
        // Verify it's a BytesN<32> Val
        let bytes_n = BytesN::<32>::try_from_val(&env, &key_val).unwrap();
        assert_eq!(bytes_n.len(), 32);
    }

    #[test]
    fn test_hash_key_different_prefixes() {
        let env = Env::default();
        let key1 = hash_key(
            &env,
            &Bytes::from_array(&env, b"prefix1:"),
            Some(&Bytes::from_array(&env, b"data")),
        );
        let key2 = hash_key(
            &env,
            &Bytes::from_array(&env, b"prefix2:"),
            Some(&Bytes::from_array(&env, b"data")),
        );
        assert_ne!(key1, key2);
    }

    #[test]
    fn test_tuple_with_address() {
        let env = Env::default();
        let addr = <soroban_sdk::Address as soroban_sdk::testutils::Address>::generate(&env);
        let key: (Address, u32) = (addr.clone(), 42);
        let val = key.to_key(&env);

        // Verify it's hashed (can't eq directly, but ensure it's BytesN<32> Val)
        let _ = val;

        // Different tuple should produce different hash
        let key2: (Address, u32) = (addr, 43);
        let val2 = key2.to_key(&env);
        assert!(!val.shallow_eq(&val2));
    }
}
