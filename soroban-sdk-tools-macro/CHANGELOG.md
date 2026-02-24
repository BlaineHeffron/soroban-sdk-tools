# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.1](https://github.com/BlaineHeffron/soroban-sdk-tools/compare/soroban-sdk-tools-macro-v0.1.0...soroban-sdk-tools-macro-v0.1.1) - 2026-02-24

### Added

- add try_invoke to CallBuilder and AuthClient
- add real auth
- add real key authorization testing (Ed25519, Secp256k1, Secp256r1)
- set abort and unknown error codes, remove user set options
- dont allow explicit discriminants in root mode
- add SequentialError trait for 0-based inner error mapping
- sequential code assignment
- add options for handling abort, unknown codes
- scerr macro

### Fixed

- unify cfg gates to not(target_family=wasm) and resolve clippy warnings
- resolve clippy warnings and fmt in auth code
- remove allocations
- switch cfg for auth client to non-wasm
- cfg gates, testutils feature for tests, and invoke-without-auth test
- qualify traits inline to remove needing to import them
- prevent cross-contract error code mismapping in ?? operator path
- use sequential codes in contractimport spec/tree instead of native codes
- bit split 10/22 instead of 22/10
- passthrough when inner decoding fails
- fix/cleanup
- fix/remove redundant specs

### Other

- storage convenience methods ([#18](https://github.com/BlaineHeffron/soroban-sdk-tools/pull/18))
- use soroban_spec_rust::generate_type_ident instead of custom impl
- auth client
- clippy
- remove unnecessary / dead fallback code that wouldn't get reached
- fmt, clippy
- remove unused description() method from ContractError trait
- use match with range patterns in generated from_code/into_code
- remove mode and support nested code passthrough
- generate flattened errors in spec
- add contractimport_with_errors
- refactor scerr_impl
- fmt
- make root_max a const
- attr parsing refactor
- common error aggregation
