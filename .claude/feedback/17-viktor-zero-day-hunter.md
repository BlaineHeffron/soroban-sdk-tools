---
agent: Viktor Kras
background: Security researcher specializing in zero-day discovery in compiler toolchains and macro systems, previously found CVEs in Rust proc-macro crates, runs a smart contract audit firm
date: 2026-03-21
---

# Review by Viktor Kras

## Overall Impression

I hunt bugs in the layers people trust implicitly: compilers, macro expanders, build systems. When a developer writes `#[contracttrait]`, they trust that the generated code faithfully implements their intent. My job is to find the cases where that trust is misplaced.

This macro is well-written. The code is cleaner than most proc-macro crates I audit. But I found several attack surfaces that range from "interesting edge case" to "potential auth bypass under specific conditions."

## Strengths

1. **The auth parsing is strict.** The `extract_auth_attr` function only accepts `Self::method` (two-segment path) or a single identifier. It does not accept arbitrary expressions, which eliminates an entire class of injection attacks. Many proc-macro crates I audit will happily accept `Self::method(); evil_code()` as an attribute argument. This one does not.

2. **The error handling in the macro is correct.** Errors are propagated via `syn::Result` and converted to compile errors via `into_compile_error()`. There are no panics in the happy path, and malformed input produces a compile-time error rather than silently generating incorrect code. This is the right approach.

3. **The `strip_auth_attrs` function prevents auth attributes from leaking into generated code.** If `#[auth]` were accidentally preserved in the generated output, it could interact with other macro passes in unexpected ways. Stripping it is correct.

4. **The sealed macro uses `$contract:ty` and `$provider:ty`, not `$contract:ident`.** This means the macro works correctly with generic types and path-qualified types, not just simple identifiers. This is a subtle but important detail that most macro authors get wrong.

## Concerns

1. **CRITICAL: The `env` identifier is hardcoded in `build_delegate_args`.** The function unconditionally pushes `env` as the first argument to the provider call:
   ```rust
   let env_ident: Ident = parse_quote!(env);
   args.push(quote! { #env_ident });
   ```
   But the actual parameter might be named something other than `env`. If a developer writes:
   ```rust
   #[contracttrait]
   pub trait MyTrait {
       fn my_method(environment: &Env) -> Address;
   }
   ```
   The generated code will call `Self::Provider::my_method(env)` where `env` is not in scope -- the parameter is named `environment`. This is a compile error, not a silent bug, but it is confusing and undocumented. The macro should extract the actual parameter name from the signature.

2. **The `is_env_type` function only checks the last segment.** It checks if the last path segment is `Env`, which means it would match `my_module::Env`, `evil::Env`, or even a user-defined `struct Env` that is not `soroban_sdk::Env`. If a crate defines its own `Env` type, the macro would misidentify it as the Soroban environment. This is an unlikely but exploitable edge case -- a malicious dependency could define a type named `Env` to confuse the macro's parameter detection.

3. **The `Param` auth source does not validate that the parameter is of type `Address`.** If a developer writes `#[auth(amount)]` where `amount: i128`, the generated code will call `amount.require_auth()`. This will fail at compile time because `i128` does not implement the `require_auth` method. But the error message will be confusing -- it will point to the generated code, not the `#[auth]` attribute. The macro should validate that param-referenced auth sources have type `Address` (or `&Address`) and emit a clear error otherwise.

4. **Namespace collision in sealed macro.** The `impl_ownable!` macro generates methods directly on `$contract`. If the contract already has a method with the same name (e.g., a manually written `pub fn owner(...)` in another `#[contractimpl]` block), this will cause a "duplicate definitions" compile error. The error message will not indicate that the collision is between a manually written method and a macro-generated one. This should be documented or detected.

5. **The `extern crate alloc as __alloc_{Trait}` pattern can fail.** If the trait name contains characters that are valid in Rust identifiers but produce an alias that conflicts with a keyword or existing import, the generated code will fail. More practically, if two traits have names that differ only in case (e.g., `Ownable` and `OWNABLE`), the `to_snake_case` function would produce the same alias, causing a collision.

6. **The `AuthClient` closure captures `&self.inner` by reference.** The closures `invoker` and `try_invoker` capture `inner` and `inner2` (both `&self.inner`). If the `AuthClient` is used across an `env` operation that modifies the client's state, the captured references could observe inconsistent state. This is unlikely in practice but worth considering for thread-safety if Soroban ever supports concurrent execution.

7. **No validation that `#[auth]` methods do not return `Address`.** If a method marked `#[auth(Self::owner)]` itself returns `Address`, the generated code first calls `owner()`, then calls `require_auth()`, then calls the provider method which also calls `owner()` (if it is the same method). This is a double-read from storage. Not a security bug, but a gas waste that could be optimized.

## Suggestions

1. **Fix the hardcoded `env` parameter name.** Extract the actual name of the first `Env`-typed parameter from the signature and use it in `build_delegate_args`. This is a correctness bug, not just a style issue.

2. **Add a validation pass after method extraction.** Before generating code, iterate over all methods and check:
   - Auth sources reference existing parameters or are `Self::method` where `method` is a method on the same trait
   - Auth-source parameters are of type `Address`
   - No two methods have the same name
   - The env parameter exists and is the first parameter

3. **Add integration tests that exercise edge cases.** Specifically:
   - Trait method with env parameter named something other than `env`
   - `#[auth]` referencing a parameter that is not `Address`
   - Two composed traits with methods of the same name
   - Trait with no methods
   - Trait with only auth-guarded methods (no read methods)

4. **Consider adding a `#[deny(unsafe_code)]` to the generated output.** This prevents a malicious provider from using `unsafe` to bypass Rust's safety guarantees.

5. **Fuzz the macro.** Use a proc-macro fuzzing framework (e.g., `cargo-fuzz` with `syn::parse`) to feed random token streams to `contracttrait_inner` and verify that it either produces valid code or a compile error -- never a panic or silently incorrect output.

## Unique Perspective

Zero-day hunters think in terms of "trust boundaries." Every time code crosses a trust boundary -- from user input to generated code, from generated code to the Soroban runtime, from one contract to another -- there is an opportunity for an attacker.

The most interesting trust boundary in this codebase is between the developer's `#[contracttrait]` annotation and the generated code. The developer writes `#[auth(Self::owner)]` and trusts that the macro will generate a correct `require_auth()` call. If the macro generates incorrect code (due to edge cases like the hardcoded `env` name), the developer has no way to know without reading the generated output.

This is the fundamental tension of proc macros: they increase productivity by hiding complexity, but they also hide bugs. The generated code needs to be as correct as hand-written code, because no one is reviewing it.

My recommendation: add a `cargo expand` test that snapshots the generated output for every example in the test suite, and review those snapshots as part of every PR. The generated code IS the security boundary. Treat it with the same rigor as hand-written security code.

## Would I Use This?

After fixing the hardcoded `env` name issue and adding the validation pass, yes. The macro is well-structured and the security model is sound for its stated scope. But I would run `cargo expand` on every contract that uses it and review the generated code before deploying to production. Trust, but verify.
