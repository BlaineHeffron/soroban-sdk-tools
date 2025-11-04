//! Shared utilities for macro parsing and code generation
//!
//! This module provides common functionality used across the contract,
//! storage, and error macros.

use syn::Type;

/// Get the base type name from a Type (e.g., PersistentMap<K, V> -> "PersistentMap")
pub fn get_type_name(ty: &Type) -> Option<String> {
    if let Type::Path(type_path) = ty {
        if let Some(last_segment) = type_path.path.segments.last() {
            return Some(last_segment.ident.to_string());
        }
    }
    None
}

/// Check if a type is a storage map type (PersistentMap, InstanceMap, TemporaryMap)
pub fn is_storage_map_type(ty: &Type) -> bool {
    if let Some(type_name) = get_type_name(ty) {
        matches!(
            type_name.as_str(),
            "PersistentMap" | "InstanceMap" | "TemporaryMap"
        )
    } else {
        false
    }
}

/// Check if a type is a storage item type (PersistentItem, InstanceItem, TemporaryItem)
pub fn is_storage_item_type(ty: &Type) -> bool {
    if let Some(type_name) = get_type_name(ty) {
        matches!(
            type_name.as_str(),
            "PersistentItem" | "InstanceItem" | "TemporaryItem"
        )
    } else {
        false
    }
}

/// Check if a type is any recognized storage type
pub fn is_storage_type(ty: &Type) -> bool {
    is_storage_map_type(ty) || is_storage_item_type(ty)
}
