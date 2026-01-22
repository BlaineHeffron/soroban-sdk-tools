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
pub use soroban_sdk_tools_macro::{contractimport_with_errors, contractstorage, scerr};

// Public modules
pub mod error;
pub mod key;
pub mod storage;

#[cfg(any(test, feature = "testutils"))]
pub mod auth;

// Re-export commonly used types
pub use error::{ContractError, ContractErrorSpec, ErrorSpecEntry};
pub use key::StorageKey;
pub use storage::{
    InstanceItem, InstanceMap, PersistentItem, PersistentMap, TemporaryItem, TemporaryMap,
};

// ============================================================================
// Macro Factory Pattern for Unified Flattened Errors
// ============================================================================
//
// These macros support the unified flattened error spec feature.
// Getter macros are generated directly by `contractimport_with_errors!` and
// collected by `__build_unified_spec!` when `#[from_contract_client]` variants
// reference types from import modules.
//
// ## How It Works
//
// 1. `contractimport_with_errors!` generates getter macros (`__scerr_{TypeName}_variants`)
//    that provide variant information (name, code, doc) for each error type.
//
// 2. When scerr processes a root enum with `#[from_contract_client]` variants that
//    reference types from import modules (e.g., `math_imported::MathError`), it
//    emits a call to `__build_unified_spec!` with the getter macro paths.
//
// 3. `__build_unified_spec!` uses continuation-passing style to call each getter
//    macro, accumulating flattened variants, then generates a final enum with
//    `#[contracterror]` for XDR spec generation.
//
// ## Requirements
//
// The unified spec is ONLY generated when:
// - Inner contracts are imported via `contractimport_with_errors!` (not local Cargo deps)
// - `#[from_contract_client]` variants reference types from those imports
//
// When using local Cargo dependencies, only namespace markers are generated in the
// `{Name}Spec` enum, and TypeScript developers must manually merge error objects.

// Re-export paste for internal use in macro expansion
#[doc(hidden)]
pub use paste::paste;

/// Entry point for building unified flattened error specs.
///
/// This macro collects error variants from multiple getter macros and builds
/// a unified enum with all variants. It uses a continuation-passing style to
/// process mixins one at a time.
///
/// The generated enum is wrapped in a hidden module (`__scerr_unified_{name:lower}`)
/// and annotated with `#[contracterror]` to generate XDR spec entries automatically.
///
/// # Arguments
///
/// - `@name`: The name for the generated enum
/// - `@unit`: Unit variants from the root enum (format: `{ name: Ident, code: expr, doc: "..." }`)
/// - `@mixins`: List of getter macro paths to process
///
/// # Generated Output
///
/// The macro generates:
/// ```rust,ignore
/// #[doc(hidden)]
/// pub mod __scerr_unified_myerror {
///     #[contracterror]
///     #[derive(Copy, Clone, Debug, Eq, PartialEq)]
///     #[repr(u32)]
///     pub enum MyError {
///         Unauthorized = 1,
///         InvalidInput = 2,
///         MathError_DivisionByZero = 3922378893,
///         MathError_NegativeInput = 3925386672,
///     }
/// }
/// ```
///
/// # Example
///
/// ```ignore
/// soroban_sdk_tools::__build_unified_spec! {
///     @name MyError
///     @unit [
///         { name: Unauthorized, code: 1, doc: "unauthorized" },
///         { name: InvalidInput, code: 2, doc: "invalid input" }
///     ]
///     @mixins [ math_imported::__scerr_MathError_variants ]
/// }
/// ```
#[doc(hidden)]
#[macro_export]
macro_rules! __build_unified_spec {
    // Entry point: start with empty accumulator
    // Unit variants now include doc comments: { name: Ident, code: expr, doc: "..." }
    (
        @name $name:ident
        @unit [ $($unit:tt),* $(,)? ]
        @mixins [ $($mixins:path),* $(,)? ]
    ) => {
        $crate::__build_unified_spec! {
            @name $name
            @acc []
            @unit [ $($unit),* ]
            @pending [ $($mixins),* ]
        }
    };

    // Base case: no more pending getters, emit the final enum with #[contracterror]
    // This generates the XDR spec automatically via soroban-sdk.
    // The enum is wrapped in a hidden module to avoid name collision with the runtime enum.
    (
        @name $name:ident
        @acc [ $( { name: $acc_name:ident, code: $acc_code:expr, doc: $acc_doc:literal } ),* $(,)? ]
        @unit [ $( { name: $unit_name:ident, code: $unit_code:expr, doc: $unit_doc:literal } ),* $(,)? ]
        @pending []
    ) => {
        $crate::paste! {
            #[doc(hidden)]
            pub mod [<__scerr_unified_ $name:lower>] {
                /// Unified flattened error spec with all error variants.
                /// This enum contains all unit variants plus flattened variants from imported contracts.
                #[$crate::soroban_sdk::contracterror]
                #[derive(Copy, Clone, Debug, Eq, PartialEq)]
                #[repr(u32)]
                #[allow(non_camel_case_types)]
                pub enum $name {
                    $(
                        #[doc = $unit_doc]
                        $unit_name = $unit_code,
                    )*
                    $(
                        #[doc = $acc_doc]
                        $acc_name = $acc_code,
                    )*
                }
            }
        }
    };

    // Recursive case: call the first pending getter with our state
    (
        @name $name:ident
        @acc [ $($acc:tt),* ]
        @unit [ $($unit:tt),* ]
        @pending [ $first:path $(, $rest:path)* ]
    ) => {
        $first! {
            @name $name
            @acc [ $($acc),* ]
            @unit [ $($unit),* ]
            @pending [ $($rest),* ]
        }
    };
}

#[cfg(any(test, feature = "testutils"))]
pub use auth::ContractClientExt;
