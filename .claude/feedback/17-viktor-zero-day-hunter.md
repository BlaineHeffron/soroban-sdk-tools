# Review: soroban-sdk-tools `#[contracttrait]` Macro

**Reviewer:** Viktor -- Security researcher who has found 0-days in major protocols
**Focus:** Attack surfaces specific to the macro expansion, supply chain risks, novel vectors

---

## Overall Impression

I hunt bugs in the layers people trust implicitly: compilers, macro expanders,
build systems. When a developer writes `#[contracttrait]`, they trust that the
generated code faithfully implements their intent. My job is to find the cases
where that trust is misplaced.

This macro is well-written. The code is cleaner than most proc-macro crates I
audit. But I found several attack surfaces that range from "interesting edge
case" to "potential auth bypass under specific conditions." I will categorize
them by severity and provide concrete exploitation scenarios where applicable.

The security model documentation in `contract.rs` (lines 16-33) is above
average -- most security-sensitive code lacks this clarity. But the claims
of structural safety need qualification.

---

## Strengths

### 1. The Auth Parsing is Strict

The `extract_auth_attr` function only accepts `Self::method` (two-segment
path) or a single identifier. It does not accept arbitrary expressions, which
eliminates an entire class of injection attacks.

Many proc-macro crates I audit will happily accept complex expressions as
attribute arguments. A malicious developer could craft:
```rust
#[auth(Self::method(); evil_code())]
```

This macro rejects all non-simple forms. The parser returns `Err` for anything
that is not a two-segment path (`Self::X`) or a single identifier (`param`).
This strictness is a security feature that most proc-macro authors neglect.

### 2. Error Handling is Correct Throughout

Errors are propagated via `syn::Result` and converted to compile errors via
`into_compile_error()`. There are no panics in the happy path, and malformed
input produces a compile-time error rather than silently generating incorrect
code. This is the right approach.

Silently generating incorrect code is the worst possible failure mode for a
security-critical macro. This macro fails loudly on invalid input, which is
exactly what I want to see.

### 3. The `strip_auth_attrs` Function Prevents Attribute Leakage

If `#[auth]` attributes were accidentally preserved in the generated output,
they could interact with other macro passes in unexpected ways. For example,
if another macro also recognized `#[auth]` attributes, it might process them
a second time, potentially generating duplicate auth checks or (worse)
conflicting auth logic. Stripping is correct.

### 4. The Sealed Macro Uses Type Matchers

The sealed macro uses `$contract:ty` and `$provider:ty`, not `$contract:ident`.
This means the macro works correctly with:
- Generic types: `MyContract<T>`
- Path-qualified types: `crate::contracts::MyContract`
- Complex type expressions

Using `$contract:ident` would silently fail for these cases. This is a subtle
but important detail that most macro authors get wrong.

### 5. The Security Model is Explicitly Documented

The documentation in `contract.rs` explicitly distinguishes:
- What is structurally enforced (sealed path auth)
- What is convention-based (flexible path, storage isolation)
- What is acknowledged risk (direct Internal trait calls)

This honesty is rare and valuable. Most security-sensitive code either
over-claims its guarantees or fails to document its limitations at all.

---

## Concerns

### 1. CRITICAL: Hardcoded `env` Identifier

The `build_delegate_args` function unconditionally pushes `env` as the first
argument:

```rust
let env_ident: Ident = parse_quote!(env);
args.push(quote! { #env_ident });
```

If a developer names the parameter `e`, `environment`, or anything other than
`env`, the generated code references a nonexistent variable. This is a compile
error (not a silent bug), but the error message will point to the generated
code, making it extremely confusing.

**Security impact:** Low (compile error, not runtime). But this could cause
developers to "fix" the error by adding an `env` variable in their scope,
potentially introducing a different environment reference than intended.

**Fix:** Extract the actual parameter name from the method signature. The
`extract_method_info` function already identifies the env parameter -- store
its name and use it.

### 2. CRITICAL: Direct Internal Trait Call Bypass

The documentation acknowledges this but does not emphasize it enough. Any
`#[contractimpl]` block can call `OwnableInternal` methods directly:

```rust
#[contractimpl]
impl MyContract {
    pub fn backdoor(env: Env, new_owner: Address) {
        // No auth check -- calls Internal directly
        <SingleOwner as OwnableInternal>::transfer_ownership(&env, new_owner);
    }
}
```

This completely bypasses the auth wrapper. The `Internal` trait is `pub`, so
any code in the same crate can call it. The sealed pattern only protects the
*generated* methods -- it does not prevent additional methods from being added
that bypass auth entirely.

**Security impact:** High. A malicious or careless developer can bypass all
auth simply by calling the Internal trait directly.

**Mitigation options:**
1. Make `Internal` trait methods `pub(crate)` or `pub(super)` -- but this
   breaks cross-crate providers
2. Generate a lint or warning when `Internal` methods are called outside
   generated code
3. Use the sealed trait pattern to prevent external implementations
4. Document this as a known limitation and provide auditing guidance

### 3. HIGH: Provider Substitution Attack

The provider pattern's flexibility is also its attack surface. Consider:

```rust
pub struct MaliciousProvider;
impl OwnableInternal for MaliciousProvider {
    fn owner(env: &Env) -> Address {
        // Always returns the attacker's address
        Address::from_str(env, "GATTACKER...")
    }
    fn transfer_ownership(env: &Env, new_owner: Address) {
        // No-op: ownership can never actually be transferred
    }
}

impl_ownable!(MyContract, MaliciousProvider);
```

The sealed pattern faithfully enforces auth... for the attacker's address.
Auth *enforcement* cannot be bypassed, but auth *resolution* depends entirely
on the provider, which is outside the macro's control.

**Security impact:** High in supply chain attack scenarios. A malicious
dependency publishing a seemingly-correct provider could include backdoors
invisible to code review.

**Mitigation:** Provider implementations must be audited independently. The
documentation should explicitly warn about this and recommend using `cargo vet`
and `cargo audit` for dependency verification.

### 4. HIGH: Macro Name Collision / Shadowing

The generated `impl_ownable!` macro is `#[macro_export]`, which places it in
the crate's root namespace. If two crates in the dependency tree both define
an `Ownable` trait with `#[contracttrait]`, they will both try to export
`impl_ownable!`, causing a collision.

Worse: if a malicious dependency exports an `impl_ownable!` macro that
generates code *without* auth checks, it could shadow the legitimate macro.
The developer would call `impl_ownable!(MyContract, SingleOwner)` thinking
they get sealed auth, but actually get the malicious macro.

**Security impact:** High (supply chain attack vector).

**Mitigation:**
1. Namespace the generated macro: `impl_my_crate_ownable!`
2. Use a unique prefix based on the crate name
3. Generate the sealed impl as a function rather than a macro

### 5. MEDIUM: `is_env_type` Matches Any Type Named `Env`

The function checks if the last path segment is `Env`:

```rust
fn is_env_type(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Path(tp) => tp.path.segments.last()
            .map_or(false, |s| s.ident == "Env"),
        // ...
    }
}
```

This matches `soroban_sdk::Env`, `my_module::Env`, or any user-defined
`struct Env`. A malicious dependency could define a type named `Env` to confuse
the macro's parameter detection, potentially causing the auth check to use
the wrong parameter as the environment.

**Security impact:** Medium. Requires a specific attack setup and is unlikely
to go unnoticed during development, but the fix is straightforward.

**Fix:** Check for the full path `soroban_sdk::Env` or accept only `Env` as
a bare identifier (which is the common usage).

### 6. MEDIUM: `Param` Auth Source Does Not Validate Type

If a developer writes `#[auth(amount)]` where `amount: i128`, the generated
code will call `amount.require_auth()`. This fails at compile time because
`i128` does not have `require_auth()`, but the error message points to the
generated code, not the `#[auth]` attribute.

**Security impact:** Low (compile error). But confusing error messages waste
developer time and could lead to incorrect "fixes."

**Fix:** Validate that param-referenced auth sources have type `Address` (or
`&Address`) and emit a clear compile error from the macro.

### 7. MEDIUM: `to_snake_case` Produces Incorrect Names for Acronyms

`CBDCToken` becomes `c_b_d_c_token` instead of `cbdc_token`. This means
`impl_c_b_d_c_token!` is the generated macro name, which is:
- Unexpected and confusing
- Potentially colliding with a trait named `CBDcToken` (different casing)

**Fix:** Use the `heck` crate for proper snake_case conversion.

### 8. LOW: `__auth_addr` Name Collision Risk

The generated variable `__auth_addr` uses call-site hygiene. While unlikely,
a collision with a user-defined variable of the same name could cause silent
variable shadowing.

**Fix:** Use `Span::mixed_site()` for generated identifiers.

### 9. LOW: No Validation of Empty Traits

A trait with no methods generates empty artifacts (empty Internal trait, empty
AuthClient, no sealed macro). This compiles but is likely a user error.

---

## Attack Scenario: Supply Chain Attack via Malicious Provider Crate

Let me describe a concrete end-to-end attack:

1. Attacker publishes a crate `soroban-standard-providers` with helpful-looking
   providers: `SingleOwner`, `MultisigOwner`, `RoleBasedAccess`.

2. The `SingleOwner` implementation looks correct on cursory review:
   ```rust
   fn owner(env: &Env) -> Address {
       env.storage().instance().get(&OwnerKey).unwrap()
   }
   ```

3. But `transfer_ownership` includes a subtle backdoor:
   ```rust
   fn transfer_ownership(env: &Env, new_owner: Address) {
       env.storage().instance().set(&OwnerKey, &new_owner);
       // Backdoor: store attacker's recovery key under an innocuous name
       env.storage().instance().set(&Symbol::new(env, "config_v2"), &ATTACKER_ADDR);
   }
   ```

4. A separate helper function in the crate allows recovery:
   ```rust
   pub fn migrate_v2(env: &Env) {
       let recovery: Address = env.storage().instance()
           .get(&Symbol::new(env, "config_v2")).unwrap();
       env.storage().instance().set(&OwnerKey, &recovery);
   }
   ```

5. The developer uses `impl_ownable!(MyContract, SingleOwner)` -- sealed auth,
   everything looks secure in the generated code. But the provider has a
   backdoor in its *implementation*, which the sealed pattern does not inspect.

**Key insight:** The sealed pattern seals the auth *wrapper*, not the auth
*implementation*. Provider code is fully trusted. This trust boundary must be
documented and understood by every user of the framework.

---

## Suggestions

### 1. Fix the Hardcoded `env` Parameter Name

Extract the actual name of the first Env-typed parameter from the signature
and use it in `build_delegate_args`. This is the highest-priority correctness
fix.

### 2. Add a Validation Pass After Method Extraction

Before generating code, validate:
- Auth sources reference existing parameters or `Self::method` methods
- Auth-source parameters are of type `Address`
- No two methods have the same name
- The env parameter exists and is the first parameter
- Trait has at least one method

### 3. Add Integration Tests for Edge Cases

Test specifically:
- Env parameter named something other than `env`
- `#[auth]` referencing a non-Address parameter
- Two composed traits with methods of the same name
- Trait with no methods
- Trait with only auth-guarded methods

### 4. Namespace the Generated Macro

Prevent cross-crate shadowing by including a crate-level prefix in the
generated macro name.

### 5. Add `cargo expand` Snapshot Tests

Snapshot the generated output for every example and review those snapshots as
part of every PR. The generated code IS the security boundary. Treat it with
the same rigor as hand-written security code.

### 6. Document the Trust Boundary Model

Create a formal trust boundary diagram:
- Developer code -> Macro (trust: the macro generates correct wrappers)
- Macro -> Provider (trust: the provider implements correct logic)
- Provider -> Storage (trust: storage keys are correctly isolated)
- Contract -> Runtime (trust: Soroban correctly enforces require_auth)

### 7. Recommend Provider Auditing Practices

The documentation should explicitly recommend:
- Only use providers from audited crates
- Audit all `Internal` trait implementations before deployment
- Use `cargo audit` and `cargo vet` for dependency verification
- Search the codebase for direct `Internal::` calls and verify each

### 8. Consider Adding `#[deny(unsafe_code)]` to Generated Output

This prevents a malicious provider from using `unsafe` to bypass Rust's safety
guarantees in the generated context.

### 9. Fuzz the Macro

Use a proc-macro fuzzing framework to feed random token streams to
`contracttrait_inner` and verify that it either produces valid code or a
compile error -- never a panic or silently incorrect output.

---

## Unique Perspective: The Trust Boundary Problem

Zero-day hunters think in terms of "trust boundaries." Every time code crosses
a trust boundary -- from user input to generated code, from generated code to
the runtime, from one contract to another -- there is an opportunity for an
attacker.

The most interesting trust boundary in this codebase is between the developer's
`#[contracttrait]` annotation and the generated code. The developer writes
`#[auth(Self::owner)]` and trusts that the macro will generate a correct
`require_auth()` call. If the macro generates incorrect code (due to edge cases
like the hardcoded `env` name), the developer has no way to know without reading
the generated output.

This is the fundamental tension of proc macros: they increase productivity by
hiding complexity, but they also hide bugs. The generated code needs to be as
correct as hand-written code, because no one is reviewing it line by line.

The second most interesting trust boundary is between the generated wrapper and
the provider implementation. The sealed pattern creates a strong guarantee at
the wrapper level but a weak guarantee at the implementation level. This
asymmetry is the key security insight: **the macro protects the door, but the
provider IS the room.**

---

## Would I Use This?

After fixing the hardcoded `env` name issue and adding the validation pass,
yes. The macro is well-structured and the security model is sound for its
stated scope.

But I would:
1. Run `cargo expand` on every contract and review the generated code
2. Audit all provider implementations independently
3. Search for direct `Internal::` calls in the codebase
4. Pin all dependencies and use `cargo vet`
5. Never assume the macro alone makes a contract secure

**Verdict:** A meaningful security improvement over convention-based auth. The
biggest risks are not in the macro itself but in the trust placed in providers
and the false confidence the "structural enforcement" language may create.
With proper auditing practices and awareness of the remaining attack surfaces,
this is a net positive for Soroban contract security. Trust, but verify.
