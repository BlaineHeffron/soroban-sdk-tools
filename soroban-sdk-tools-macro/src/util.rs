//! Shared utilities for macro parsing and code generation
//!
//! This module provides common functionality used across the contract,
//! storage, and error macros.

use syn::{Error, Ident, Type};

pub trait TypeExt {
    /// Get the base type name from a Type (e.g., `PersistentMap`<K, V> -> "`PersistentMap`")
    fn get_type_name(&self) -> Option<String>;

    /// Extract key and value types from a map type
    fn extract_map_generics(&self) -> syn::Result<(Type, Type, Ident)>;

    /// Check if a type is a storage map type (`PersistentMap`, `InstanceMap`, `TemporaryMap`)
    fn is_storage_map_type(&self) -> bool {
        matches!(
            self.get_type_name().as_deref(),
            Some("PersistentMap" | "InstanceMap" | "TemporaryMap")
        )
    }

    /// Check if a type is a storage item type (`PersistentItem`, `InstanceItem`, `TemporaryItem`)
    fn is_storage_item_type(&self) -> bool {
        matches!(
            self.get_type_name().as_deref(),
            Some("PersistentItem" | "InstanceItem" | "TemporaryItem")
        )
    }

    /// Check if a type is any recognized storage type
    fn is_storage_type(&self) -> bool {
        self.is_storage_map_type() || self.is_storage_item_type()
    }

    /// Validate that a type is one of the supported storage types
    fn validate_storage_type(&self) -> syn::Result<()>;
}

/// Combine multiple `syn::Error` instances into a single error
pub fn combine_errors(errors: Vec<Error>) -> Error {
    errors
        .into_iter()
        .reduce(|mut a, b| {
            a.combine(b);
            a
        })
        .expect("At least one error expected")
}

impl TypeExt for Type {
    fn get_type_name(&self) -> Option<String> {
        if let Type::Path(type_path) = self {
            if let Some(last_segment) = type_path.path.segments.last() {
                return Some(last_segment.ident.to_string());
            }
        }
        None
    }

    /// Extract key and value types from a map type
    fn extract_map_generics(&self) -> syn::Result<(Type, Type, Ident)> {
        if let Type::Path(type_path) = self {
            if let Some(segment) = type_path.path.segments.last() {
                let ident = segment.ident.clone();
                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                    if args.args.len() == 2 {
                        if let (syn::GenericArgument::Type(k), syn::GenericArgument::Type(v)) =
                            (&args.args[0], &args.args[1])
                        {
                            return Ok((k.clone(), v.clone(), ident));
                        }
                    } else if args.args.len() == 3 {
                        // For when W is present
                        if let (
                            syn::GenericArgument::Type(k),
                            syn::GenericArgument::Type(v),
                            syn::GenericArgument::Type(_w),
                        ) = (&args.args[0], &args.args[1], &args.args[2])
                        {
                            return Ok((k.clone(), v.clone(), ident));
                        }
                    }
                }
            }
        }
        Err(Error::new_spanned(
            self,
            "Invalid map type: expected a type like PersistentMap<K, V>",
        ))
    }

    fn validate_storage_type(&self) -> syn::Result<()> {
        if !self.is_storage_type() {
            let type_name = self
                .get_type_name()
                .unwrap_or_else(|| "<unknown>".to_string());
            return Err(Error::new_spanned(
                self,
                format!(
                    "Field type '{type_name}' is not a valid storage type.\n\
                 \n\
                 Supported types:\n\
                 - Map types: PersistentMap<K, V>, InstanceMap<K, V>, TemporaryMap<K, V>\n\
                 - Item types: PersistentItem<V>, InstanceItem<V>, TemporaryItem<V>."
                ),
            ));
        }

        Ok(())
    }
}
