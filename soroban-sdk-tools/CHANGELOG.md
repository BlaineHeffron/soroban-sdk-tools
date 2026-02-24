# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.1](https://github.com/BlaineHeffron/soroban-sdk-tools/compare/soroban-sdk-tools-v0.1.0...soroban-sdk-tools-v0.1.1) - 2026-02-24

### Added

- add try_invoke to CallBuilder and AuthClient
- add real auth
- add real key authorization testing (Ed25519, Secp256k1, Secp256r1)
- set abort and unknown error codes, remove user set options
- add SequentialError trait for 0-based inner error mapping
- sequential code assignment
- add options for handling abort, unknown codes
- add errors example contracts to test wasm spec
- scerr macro

### Fixed

- unify cfg gates to not(target_family=wasm) and resolve clippy warnings
- resolve clippy warnings and fmt in auth code
- relax CallBuilder fn_name from &'static str to &'a str
- prevent u32 overflow in signature_expiration_ledger
- normalize secp256k1 signatures to low-S form
- setup_mock_auth clears prior auth state when authorizers is empty
- cfg gates, testutils feature for tests, and invoke-without-auth test
- prevent cross-contract error code mismapping in ?? operator path
- bit split 10/22 instead of 22/10

### Other

- Merge pull request #19 from BlaineHeffron/fix/auth-testutils-gate
- storage convenience methods ([#18](https://github.com/BlaineHeffron/soroban-sdk-tools/pull/18))
- Split auth.rs into auth submodules
- update examples and tests for borrow-based auth API
- use shorthand field init and widen auth cfg gate to non-wasm
- document sign/authorize mutual exclusion panic on CallBuilder
- move NONCE_COUNTER thread_local to module scope
- auth client
- fmt, clippy
- remove unused description() method from ContractError trait
- remove mode and support nested code passthrough
- generate flattened errors in spec
- add contractimport_with_errors
- attr parsing refactor
- common error aggregation
