# **Technical Architecture for soroban-sdk-tools**

The soroban-sdk-tools project compliments the rs-soroban-sdk by enhancing smart contract development on the Soroban platform with advanced storage management, composable error handling, and improved authorization testing. To address Rust compilation constraints, the procedural macros are housed in a separate crate, soroban-sdk-tools-macro, following the structure of the existing loam project. Below is the detailed architecture of the soroban-sdk-tools crate and its companion macro crate.

## **Crate Structure**

The project is organized into two primary crates: soroban-sdk-tools (core functionality) and soroban-sdk-tools-macro (procedural macros). The soroban-sdk-tools crate focuses on storage management, error handling, and authorization testing, while soroban-sdk-tools-macro contains the procedural macro logic for generating storage and error-related code. The directory structure is designed to maintain modularity and align with the loam project's conventions.

### **Directory Structure**

soroban-sdk-tools/  
├── soroban-sdk-tools  
│   └── src  
│       ├── lib.rs          \# Crate root, re-exports public types and macros  
│       ├── storage.rs      \# Storage types (PersistentMap, InstanceMap, etc.), StorageKey trait  
│       ├── error.rs        \# Error enum helper traits and conversions  
│       ├── key.rs          \# Key encoding, hashing, and namespacing logic  
│       └── auth.rs         \# Authorization testing utilities and contract client  
├── soroban-sdk-tools-macro  
│   └── src  
│       ├── lib.rs          \# Macro crate root, re-exports procedural macros  
│       ├── contract.rs     \# Macro logic for contract-related annotations  
│       ├── storage.rs      \# Macro logic for \#\[contractstorage\] annotations  
│       ├── error.rs        \# Macro logic for \#\[scerr\] annotations  
│       └── util.rs         \# Shared utilities for macro parsing and code generation

### **Module Descriptions**

#### **soroban-sdk-tools Crate**

The soroban-sdk-tools crate provides the core functionality for storage management, error handling, and authorization testing. It defines high-level abstractions over Soroban's native APIs, ensuring efficient storage key generation, unique error codes, and streamlined authorization flows.

* **lib.rs**: The crate root re-exports public types, traits, and macros for ease of use.   
* **storage.rs**: Defines storage-related types such as PersistentMap\<K,V\>, InstanceMap\<K,V\>, TemporaryMap\<K,V\>, and single-value variants like PersistentItem. These types wrap Soroban's storage interfaces (env.storage().persistent(), .instance(), etc.) and use the StorageKey trait to convert Rust keys (e.g., enums, strings) into compact on-chain keys. The module includes mechanisms for automatic key size minimization to optimize ledger efficiency.

* **error.rs**: Provides traits and helper types for error handling. It includes implementations for converting error enums across the Soroban host boundary (e.g., TryFromVal/IntoVal). This module ensures compatibility with the \#\[scerr\] macro-generated enums, facilitating composable error propagation.

* **key.rs**: Implements low-level key generation logic, including shortening, hashing, and namespacing. The StorageKey trait is defined here, enabling any type implementing it to serve as a storage key in Soroban. This module ensures unique and compact storage keys.

* **auth.rs:** Introduces utilities for improved authorization testing, including a new contract client that simplifies the process of testing authorization flows. Currently, in Soroban tests, developers must manually craft mock authorization entries using methods like env.mock\_auths(), which requires constructing AuthorizedInvocation structures that precisely mirror the contract invocation details—such as the contract ID, function name (as a Symbol), and arguments. This duplication is redundant and error-prone, as the mock entries essentially replicate the parameters of the subsequent contract call. The new contract client addresses this by automatically generating the required mock authorization entries based on the invocation parameters and the authorizing address(es), then executing the call. This eliminates manual duplication and reduces confusion in test setup. For example, instead of handcrafting mocks like:

| env.mock\_auths(&\[(    user.clone(),    AuthorizedInvocation {        function: AuthorizedFunction::Contract((            contract\_id.clone(),            symbol\_short\!("increment"),            (user.clone(), 5\_u32).into\_val(\&env),        )),        sub\_invocations: std::vec\!\[\],    })\]);client.increment(\&user, &5); |
| :---- |

  developers can use the enhanced client for a streamlined invocation:

| client.with\_auth(\&user).increment(\&user, &5); |
| :---- |

  #### 

  #### This approach integrates seamlessly with Soroban's Env and supports both single and multi-address authorizations, while allowing assertions on recorded authorizations via env.auths() for verification. The utilities build upon Soroban's existing testing framework, enhancing developer productivity without altering core SDK behavior.

#### 

#### **soroban-sdk-tools-macro Crate**

The soroban-sdk-tools-macro crate contains the procedural macro logic, separated due to Rust compilation requirements. This crate generates Rust code for storage and error handling based on user annotations.

* **lib.rs**: The macro crate root re-exports the procedural macros for use in soroban-sdk-tools. 

* **contract.rs**: Contains macro logic for contract-related annotations, ensuring seamless integration with Soroban’s contract system.

* **storage.rs**: Defines the \#\[contractstorage\] macro, which parses storage-related annotations and generates code for storage types (e.g., PersistentMap, InstanceMap). It supports shorter key names and composable storage patterns while ensuring unique key definitions.

* **error.rs**: Implements the \#\[scerr\] macro, which generates repr(u32) enums with unique variant codes for error handling. It also emits TryFromVal/IntoValimplementations and helper conversions for cross-contract error propagation. The macro supports a \#\[scerr(root)\] variant for designating a top-level error enum that composes errors from sub-modules or external contracts, incorporating attributes such as \#\[transparent\] for direct passthrough of nested errors and \#\[from\_contract\_client\] for errors originating from cross-contract calls.

* **util.rs**: Provides shared utilities for macro parsing and code generation, ensuring consistency across the contract, storage, and error modules.

### **Storage Management Abstractions**

Soroban provides three storage types via `env.storage()`: persistent, instance, and temporary storage. Each functions as an independent key-value map in the ledger, with distinct cost and time-to-live (TTL) semantics. The `soroban-sdk-tools` crate will expose these as typed maps with enhanced key management:

- **PersistentMap\<K,V\>**: Wraps `env.storage().persistent()`. Stores values indefinitely, subject to contract-defined TTL.  
- **InstanceMap\<K,V\>**: Wraps `env.storage().instance()`. Manages small, per-contract-instance data, sharing TTL with the contract instance.  
- **TemporaryMap\<K,V\>**: Wraps `env.storage().temporary()`. Handles ephemeral data that expires upon TTL exhaustion.

These maps mirror Soroban's native structures but incorporate generic type parameters and ergonomic methods (e.g., `get`, `set`, `update`). For instance, a usage example might appear as follows:

balances.set(\&user\_addr, &100);

Internally, this expands to `env.storage().persistent().set(&(prefix + key), &100)`, where the prefix ensures a unique namespace, derived from the struct and field names.

This also makes storage retrieval easier because the types are known. For example, in many contracts like this from Open Zeppelin wrap the call to storage so that the compiler can infer the type from the return type:

| pub fn total\_supply(e: \&Env) \-\> i128 {   e.storage().instance().get(\&StorageKey::TotalSupply).unwrap\_or(0)} |
| :---- |

With Soroban storage Total Supply would be a Persistent Item

| total\_suppy.get().unwrap\_or(0) |
| :---- |

To facilitate streamlined declarations, the crate introduces attribute macros for bulk storage definition. For example:

| \#\[derive(Default)\]\#\[contractstorage\]pub struct TokenStorage {    balances: PersistentMap\<Address, u32\>,    owner: PersistentItem\<Address\>,} |
| :---- |

The `#[contractstorage]` macro generates initialization code and accessors that interact with the ledger. Keys are derived from Rust values (e.g., strings or enum variants) and converted into compact `Symbol` or `BytesN` formats, prioritizing uniqueness and minimal byte consumption.

The primary advancement over Loam's existing storage framework lies in key size optimization. Loam's current approach constructs prefixes combining the struct name and individual field name before appending the map's key (e.g., an address). In `soroban-sdk-tools`, users gain two configurable options for key shortening:

1. **Automated Shortening via Macro**: If users are unconcerned about potential collisions from future contract extensions or upgrades, a top-level macro scans the file to aggregate all storage instances. It assigns abbreviated prefixes using the first letter of item names (assuming no cross-struct collisions), escalating to the first letter of the struct plus the first (or subsequent) letter of the item as necessary. For inherently lengthy keys (e.g., those involving address types), the macro hashes the prefix and name into a fixed 32-byte key.  
     
2. **User-Specified Short Forms**: For greater control over uniqueness—particularly in anticipation of upgrades—users can annotate individual fields within the struct using a field-level attribute macro, such as `#[short_key("bal")]`, to specify a custom short name for that field's key. This ensures predictable and collision-resistant key generation without relying solely on automated logic.

The crate builds upon Loam's SDK, re-exporting or wrapping its storage types (e.g., `PersistentMap` from `loam_soroban_sdk`) while integrating the new macros and utilities. Our `StorageKey` trait extends Loam's `LoamKey` to incorporate this optimization logic, promoting modular design and reduced on-chain resource usage.

### **Error Handling System**

Soroban defines contract errors as `#[repr(u32)]` enums annotated with `#[contracterror]`. The `soroban-sdk-tools` crate introduces a composable error handling framework, named `scerr`, to enhance developer productivity and ensure robust error propagation. This framework, implemented in the `soroban-sdk-tools-macro` crate, provides the following key features:

**Automatic Code Assignment**: The `#[scerr]` attribute macro, applied to an error enum, automatically assigns each variant a unique `u32` code. This eliminates manual numbering and prevents overlaps. For example:

| \#\[scerr\]pub enum Error {    InvalidInput,    Overflow,} |
| :---- |

 expands to:

| \#\[repr(u32)\]\#\[derive(Copy, Clone)\]pub enum Error {    InvalidInput \= 1,    Overflow \= 2,} |
| :---- |

*  The macro ensures compliance with Soroban’s requirements for error enums, including `#[repr(u32)]` and `#[derive(Copy, Clone)]`.

* **Namespacing and Collision Prevention**: To avoid conflicts between error codes from different modules or contracts, the `#[scerr(root)]` attribute is applied to the top-level error enum in the contract. This collects all `#[scerr]`\-annotated enums from sub-modules or dependencies and performs compile-time checks to ensure no duplicate codes exist. It generates unique codes by incorporating a namespace, derived from the contract or crate name, into the code calculation. For instance, if two modules define `InvalidInput`, the macro prefixes each with a module-specific identifier or hashes the enum name to guarantee uniqueness.

* **Conversion Traits**: The `#[scerr]` macro automatically implements `TryFromVal<Env>` and `IntoVal<Env>` for the enum, enabling seamless error propagation across the Soroban host boundary. For cross-contract calls, where Contract A invokes Contract B, the framework supports wrapping B’s error into A’s namespace using attributes like `#[transparent]` for direct error passthrough (e.g., `#[transparent] AError(#[from] a::Error)`) and `#[from_contract_client]` for client-side errors from external contracts (e.g., `#[from_contract_client] ContractCError(#[from] c::Error)`). A `FromContractError` trait is generated to facilitate conversions between error enums, ensuring type-safe error handling.  
* **Error Propagation and Logging**: The crate provides utility functions to simplify error handling, such as a wrapper around Soroban’s `panic_with_error!` macro for triggering panics with contract errors. It also supports returning `Result<T, Error>` from functions with minimal boilerplate. To enhance error context, the framework integrates with an upstream CLI change, leveraging the contract specification to map error codes to descriptive messages for improved debugging and logging.

By encapsulating Soroban’s error mechanisms in the `#[scerr]`macro, the framework enables developers to write concise, type-safe code while ensuring robust error propagation. The `#[scerr(root)]` macro, placed on the top-level error enum (e.g., in the main contract file), scans all `#[scerr]`\-annotated enums to enforce unique code assignments. This approach leverages Rust’s procedural macro system to analyze the contract’s structure at compile time, preventing runtime errors due to code collisions.

The `scerr` framework builds upon and refines patterns from the `loam` project, ensuring compatibility with existing Soroban error handling while introducing composability and ease of use. This results in a streamlined development experience, reduced risk of errors, and improved contract maintainability.

### **Authorization Testing Utilities**

The soroban-sdk-tools crate enhances authorization testing by providing utilities that automate the mocking of authorizations in test environments. Soroban's testing framework requires developers to handle authorization explicitly when contracts use `require_auth` or `require_auth_for_args`. In unit tests, this often involves calling `env.mock_auths()` with handcrafted `AuthorizedInvocation` entries that duplicate the contract call's structure, leading to redundancy and potential mismatches if the call parameters change.

The proposed utilities introduce a wrapper around the standard contract client generated by Soroban's SDK. This enhanced client includes methods like `with_auth` (or similar chaining APIs) that accept authorizing addresses and automatically construct the corresponding mock entries using the forthcoming invocation's details. Key features include:

- **Automatic Mock Generation**: The client introspects the function name and arguments at runtime (leveraging Rust's type system and Soroban's `Val` conversions) to build `AuthorizedInvocation` structures, ensuring they match the call without manual intervention.  
- **Support for Complex Flows**: Handles nested invocations (sub-invocations) by recursively building the authorization tree if the contract call involves cross-contract interactions.  
- **Integration with Existing Tests**: Compatible with `env.mock_all_auths()` for broad mocking, but provides fine-grained control for targeted testing scenarios, such as verifying specific failures or partial authorizations.  
- **Assertion Helpers**: Utility functions to compare expected versus recorded authorizations post-invocation, simplifying test assertions.

This system reduces boilerplate in tests, minimizes errors from mismatched mocks, and promotes clearer test code. It builds upon patterns from the official Soroban SDK and loam project, ensuring compatibility while introducing these developer-friendly abstractions. The implementation resides in auth.rs, with potential macro support in soroban-sdk-tools-macro if needed for compile-time enhancements.

### 

### **Key Utilities and Namespacing**

The `soroban-sdk-tools` crate includes a key-generation utility module (`key.rs`) to support efficient and collision-resistant storage key creation for both storage and error handling systems. This module provides the following components:

* **StorageKey Trait**: The `StorageKey` trait enables types such as `Symbol`, `(u64, u64)`, or custom types to serve as keys for storage maps. The trait is extended to support automatic conversion of various key types into Soroban’s required `Bytes` or `BytesN` formats, ensuring compatibility with the ledger. This allows macros to process diverse key types while maintaining compact on-chain representations.

* **Key Shortening Mechanisms**: The crate optimizes key sizes to reduce on-chain resource consumption, building on the approach defined in the storage management framework. Two options are provided:

  1. **Automated Shortening**: A top-level macro (e.g., `#[contractstorage]` at the crate root) scans all storage instances in the contract. It generates abbreviated prefixes using the first letter of item names if no naming conflicts exist across structs. If conflicts arise, it combines the first letter of the struct name with the first (or subsequent) letter of the item name. For long keys, such as those involving addresses, the macro hashes the prefix and name into a fixed 32-byte key.  
  2. **User-Specified Short Forms**: Users can annotate individual fields within storage structs using a field-level attribute macro, such as `#[short_key("custom_short")]`, to specify custom short-form names for precise control over key uniqueness, particularly for contracts anticipating future upgrades. This ensures predictable and collision-resistant key generation.  
* **Namespace Prefixing**: To prevent collisions, each storage struct or map is assigned an implicit prefix derived from the contract or module name. This prefix is transparently incorporated into the key during code generation, ensuring that keys like "COUNTER" in different modules do not conflict on-chain.

* **Constant Key Generation**: The crate leverages Soroban’s `symbol_short!` macro for embedding short string keys and provides utilities to hash longer identifiers into fixed-size keys at compile time, further minimizing ledger footprint.

These mechanisms yield measurable savings in storage and computational resources. For example, consider a contract with the following DataKey enum:

| \#\[contracttype\]  pub enum DataKey {    Preference(Address),  } |
| :---- |

Applying key reduction to "Preference" (original) → "P" (first letter) → Hashed (prefix \+ key as BytesN32) results in the following metrics for updating the user preference:

- **Key/Data Size**:   
    
  - max\_rw\_key\_byte:  
    - Preference: 120  
    - P: 112 (6.7% reduction)  
    - Hashed: 84 (30% reduction)  
  - max\_rw\_data\_byte (and ledger\_write\_byte for updates):  
    - Preference: 160  
    - P: 152 (5% reduction)  
    - Hashed: 124 (22% reduction)


- **CPU and Memory**:  
    
  - cpu\_insn:  
    - Preference: 489,854  
    - P: 486,542 (0.7% reduction)  
    - Hashed: 490,175 (0.07% increase)  
  - mem\_byte:  
    - Preference: 1,290,949  
    - P: 1,290,122 (0.06% reduction)  
    - Hashed: 1,286,857 (0.3% reduction)

From these metrics one can see that the hashing mechanism can yield substantial ledger savings (\~30% in key size) for a tiny fractional increase in computational resources (0.07% increase in CPU instructions for this example). If one prefers using the short form which still enables readable keys, a \~7% reduction in key size is obtained.

These utilities, housed in `key.rs`, are used by both the storage and error handling systems to ensure that on-chain keys are compact, human-readable, and collision-resistant.

### **Macro API and Usage**

The procedural macros, defined in the `soroban-sdk-tools-macro` crate (`src/contract.rs`, `src/storage.rs`, `src/error.rs`, and `src/util.rs`), provide an ergonomic interface for developers to define storage and error structures. The following macros are implemented:

* **\#\[contractstorage\]**: Applied to a struct to designate its fields as storage items (e.g., `PersistentMap`, `InstanceMap`, `PersistentItem`). The macro parses field types and optional attributes (e.g., `#[short_key("shortname")]`) to generate initialization code and accessor methods. It also implements `Default` to initialize maps with keys derived from field names, incorporating the key shortening and namespacing logic from the `key.rs` module. For example:

| \#\[contractstorage\]pub struct TokenStorage {    \#\[short\_key("b")\]    balances: PersistentMap\<Address, u32\>,    owner: PersistentItem\<Symbol, Address\>,} |
| :---- |

           This generates code to initialize the fields and interface with the Soroban ledger.

* **\#\[scerr\]**: Applied to an enum to define a contract error type. The macro ensures the enum is annotated with `#[repr(u32)]`, implements required traits (`Copy`, `Clone`, `TryFromVal`, `IntoVal`), and assigns unique u32 codes to each variant. It also generates a Display implementation for diagnostic purposes, facilitating error logging and debugging. When used with `(root)`, it designates the enum as the top-level error aggregator, supporting attributes like `#[transparent]` and `#[from_contract_client]` for composable error handling across modules and contracts.

* **Helper Macros**: The crate includes a function-like macro, `error!()`, which wraps Soroban’s `panic_with_error!` to simplify triggering panics with contract errors. This macro integrates with the contract specification to map error codes to descriptive messages, enhancing debugging.

These macros, implemented in the `soroban-sdk-tools-macro` crate, rely on the `storage`, `error`, and `key` modules in the `soroban-sdk-tools` crate for runtime logic. They enable developers to write concise, annotated structs and enums, with the macros generating all necessary Soroban-compatible code, including key optimization and error handling boilerplate. This approach ensures a streamlined development experience while maintaining compatibility with Soroban’s requirements.

## **Integration with Soroban SDK**

Under the hood, all APIs from soroban-sdk are used. For instance:

* **Env Usage**: Our types will keep a reference to Env,and calls like map.get(\&key) will call self.env.storage().persistent().get(\&key).  
     
* **No std**: The crate will compile to no\_std Wasm, just like Soroban contracts. It will depend on soroban-sdk (as Loam does) so it can run inside the Soroban host.  
     
* **Interop with Official SDK**: We will not duplicate core features of the official SDK. Instead, we build *on top*. For example, Soroban already has a Map type, but we provide a more Rust-idiomatic alternative (PersistentMap\<K,V\>). We rely on Soroban’s Env, Val, ScErrorType, and other primitives for actual storage and error encoding.

