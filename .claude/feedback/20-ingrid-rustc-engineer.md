# Review: soroban-sdk-tools `#[contracttrait]` Macro

**Reviewer:** Ingrid -- Compiler engineer who works on the Rust compiler itself (rustc)
**Focus:** Proc macro hygiene, expansion ordering, interaction with rust-analyzer, future Rust features

---

## Overall Impression

As someone who works on the Rust compiler's macro infrastructure, I evaluate
proc macros through a specific lens: hygiene, diagnostic quality, interaction
with other macros and tooling, and adherence to macro best practices. This
codebase is above average for a proc-macro crate. The use of `syn` and `quote`
is idiomatic, error handling is correct, and the generated code structure is
reasonable.

But there are hygiene issues and diagnostic gaps that would affect the developer
experience in edge cases. Some of these are common mistakes in the proc-macro
ecosystem; others are specific to this crate's architecture. I will address
both the immediate issues and the longer-term implications for Rust language
evolution.

The codebase demonstrates mature engineering practices: `syn::Result`
propagation, clean `quote!` templates, correct cfg guards. The authors clearly
understand proc macros. The issues I identify are subtle -- the kind that only
surface in edge cases or when interacting with other tools.

---

## Strengths

### 1. Correct use of `syn::Result` Throughout

Every parsing operation that can fail returns `syn::Result`, and the top-level
`contracttrait_impl` converts errors to `into_compile_error()`. This means
malformed input always produces a meaningful compile error at the correct span
location, rather than an ICE or a panic. This is the single most important
thing a proc macro can get right, and this codebase gets it right.

Many proc-macro crates I encounter use `.unwrap()` or `.expect()` on parsing
results, which converts a user error into a compiler panic with no useful
diagnostic information. This crate does not have that problem.

### 2. The `quote!` Usage is Clean and Readable

The generated code blocks in `generate_outer_trait`, `generate_sealed_impl_macro`,
and `generate_auth_client` are well-structured. The use of interpolated
variables (`#method_name`, `#delegate_args`) makes the template readable. I
have seen proc macros where the `quote!` blocks are hundreds of lines of
unreadable token soup -- this is not one of them.

The templates are also well-factored: each generation function handles one
concern (inner trait, outer trait, auth client, sealed macro). This makes the
code auditable and testable.

### 3. The `#[macro_export]` on the Sealed Macro is Correct

Using `macro_rules!` with `#[macro_export]` ensures the sealed macro is
available at the crate root, which is necessary for cross-crate usage. Some
proc-macro crates generate macros that are only usable within the defining
crate due to visibility issues. This crate handles it correctly.

The use of `$contract:ty` and `$provider:ty` (not `$contract:ident`) is also
correct -- it means the macro works with generic types and path-qualified types,
not just simple identifiers. This is a subtle but important detail that most
macro authors get wrong.

### 4. The `#[cfg(not(target_family = "wasm"))]` Guard is Correct

AuthClient uses `alloc::boxed::Box` and closures that are inappropriate for
the WASM target. Gating it behind a cfg attribute ensures it does not affect
the production binary. This is the right pattern for test-only utilities in
the Soroban ecosystem.

### 5. The Auth Attribute Parsing is Strict

The `extract_auth_attr` function only accepts `Self::method` (two-segment
path) or a single identifier. It does not accept arbitrary expressions, which
eliminates an entire class of injection attacks. The parser rejects:
- `Self::method(); evil_code()` -- not a valid expression form
- `some::long::path::method` -- more than two segments rejected
- Complex expressions -- only simple paths accepted

This strictness is a security feature, even if the error messages could be
more descriptive about what forms ARE accepted.

---

## Concerns

### 1. CRITICAL: Hardcoded `env` Identifier in `build_delegate_args`

This is the most significant issue. The function unconditionally uses `env`
as the first argument:

```rust
let env_ident: Ident = parse_quote!(env);
args.push(quote! { #env_ident });
```

But the actual parameter might be named something other than `env`. If a
developer writes:

```rust
#[contracttrait]
pub trait MyTrait {
    fn my_method(environment: &Env) -> Address;
}
```

The generated code will call `Self::Provider::my_method(env)` where `env` is
not in scope -- the parameter is named `environment`. This produces a confusing
compile error pointing to the generated code.

**Fix:** Extract the actual parameter name from the method signature. The
`extract_method_info` function already identifies the env parameter (line 154:
`if first && is_env_type(&pat_type.ty)`). Store its name in `MethodInfo` and
use it in `build_delegate_args`:

```rust
struct MethodInfo {
    // ... existing fields ...
    env_name: Ident,  // the actual name of the env parameter
}
```

This is a correctness bug, not just a style issue. It should be fixed before
any release.

### 2. Hygiene Violation: `__auth_addr` Uses Call-Site Hygiene

The generated `let __auth_addr = ...` uses call-site hygiene (the default for
`quote!` identifiers). This means the variable is visible to user code in the
same scope. If a developer happens to have a variable named `__auth_addr` in
scope (unlikely but possible with nested macros), it will shadow or conflict.

In `rustc`'s macro system, we use `Span::mixed_site()` or definition-site
spans for generated identifiers that should not be visible to the user. With
`proc_macro2`, you can use `Span::mixed_site()`:

```rust
let auth_var = Ident::new("__auth_addr", Span::mixed_site());
```

This creates an identifier that is visible within the generated code but does
not interact with user-defined identifiers. It is the recommended practice for
proc macros that generate local variables.

Additionally, the name `__auth_addr` is poor even setting aside hygiene
concerns. Double underscores are reserved by convention for compiler/language
internals. A more descriptive name like `_contracttrait_auth_address` would be
clearer and avoid the reserved-name convention.

### 3. The `extern crate alloc as __alloc_{Trait}` Pattern is Fragile

In edition 2021+, `extern crate` is rarely needed because items from `alloc`
are available via `use alloc::...`. But in `no_std` environments (which Soroban
contracts are), `extern crate alloc` is necessary to make the `alloc` crate
available.

The issue is that generating multiple `extern crate alloc as X` declarations
(one per trait) in the same module will work, but creates several problems:

1. If two traits are defined in the same module, there are two `extern crate`
   declarations with different aliases. While Rust allows this, it is unusual.

2. The alias combines underscores with PascalCase (`__alloc_Ownable`), which
   is inconsistent with Rust naming conventions and confusing for tooling.

3. If two traits have names that differ only in case (unlikely but possible),
   the aliases could conflict.

**Fix:** Generate a single `extern crate alloc` (without alias) and use
`alloc::boxed::Box` directly. Since the AuthClient code is gated behind
`#[cfg(not(target_family = "wasm"))]`, there is no conflict with the `no_std`
WASM target. If multiple traits are in the same module, they share the same
`extern crate alloc` declaration, which is correct.

Alternatively, use `::alloc::boxed::Box` (absolute path) which does not
require any `extern crate` declaration in edition 2021.

### 4. No Span Propagation on Generated Code

The `quote!` macro uses `Span::call_site()` by default, which means error
messages point to the macro invocation site (the `#[contracttrait]` attribute),
not to the specific part of the trait definition that caused the error.

For example, if a provider does not implement a required method, the error
"method `owner` not found on type `SingleOwner`" will point to the
`#[contracttrait]` attribute rather than to the `fn owner(...)` method
definition in the trait. This is confusing because the user cannot see which
method is missing.

**Fix:** Use `quote_spanned!` with the span of the relevant trait method:

```rust
let method_span = method.sig.ident.span();
quote_spanned! { method_span =>
    #auth_check
    Self::Provider::#method_name(#delegate_args)
}
```

This makes error messages point to the specific method definition, which is
dramatically more helpful for debugging. It is the difference between "error
somewhere in this trait" and "error on this specific method."

### 5. The `to_snake_case` Function Does Not Handle Acronyms

The function (lines 389-402) handles simple PascalCase but not:
- Consecutive uppercase: `HTTPServer` becomes `h_t_t_p_server` (should be
  `http_server`)
- Acronyms: `CBDCToken` becomes `c_b_d_c_token` (should be `cbdc_token`)
- Trailing acronyms: `MyHTTP` becomes `my_h_t_t_p` (should be `my_http`)

Since this function determines macro names (`impl_h_t_t_p_server!`), incorrect
case conversion produces unusable macro names.

**Fix:** Use the `heck` crate's `ToSnakeCase` trait. It handles all edge cases
and is the standard solution in the Rust ecosystem. Adding a small dependency
is better than maintaining a buggy reimplementation.

```rust
use heck::ToSnakeCase;
let trait_snake = trait_name.to_string().to_snake_case();
```

### 6. Missing Input Validation for Edge Cases

The macro does not validate several inputs that could produce confusing or
incorrect output:

- **Empty trait:** What if the trait has no methods? The generated Internal
  trait, AuthClient, and sealed macro will be empty. This compiles but is
  likely a user error.

- **`#[auth]` on a method with no parameters:** If a method has only `env`
  and no other parameters, and `#[auth(param)]` references a nonexistent
  parameter, the error will come from the generated code, not from the macro.

- **Method with `&self`:** Soroban contract methods cannot have `&self`, but
  the macro does not check for this. If present, the generated code will
  have mysterious errors.

- **Non-function trait items:** If the trait contains associated types,
  constants, or other non-function items, they are silently ignored. This
  should at least produce a warning.

**Fix:** Add a validation pass after parsing and before code generation:

```rust
fn validate_trait(trait_def: &ItemTrait, methods: &[MethodInfo]) -> syn::Result<()> {
    if methods.is_empty() {
        return Err(Error::new_spanned(trait_def, "trait must have at least one method"));
    }
    for method in methods {
        if let Some(AuthSource::Param(ref param)) = method.auth {
            if !method.params.iter().any(|p| p.name == *param) {
                return Err(Error::new_spanned(
                    param,
                    format!("auth source '{}' is not a parameter of this method", param)
                ));
            }
        }
    }
    Ok(())
}
```

### 7. The `generate_auth_client_method` Clones Parameters Twice

The function generates both `arg_clones` and `try_arg_clones` -- two sets of
cloned parameters for the `invoker` and `try_invoker` closures respectively.
This means every parameter is cloned twice:

```rust
let clone_name = format_ident!("{}_clone", name);
let try_clone_name = format_ident!("{}_try_clone", name);
```

If a parameter type is expensive to clone (e.g., a large `Bytes` value), this
is wasteful. Consider:

1. Using `Rc` to share the parameters between closures
2. Generating a single closure that returns a `Result` and wrapping it in two
   entry points
3. Using `Arc` for thread-safety if that becomes relevant

This is a performance concern, not a correctness concern, but it matters for
contracts that pass large data structures.

### 8. No Interaction Testing with Other Soroban Macros

The `#[contracttrait]` macro delegates to `soroban_sdk::contracttrait`. But
proc macros are applied outside-in: the outermost attribute runs first. If
`#[contracttrait]` is applied after another attribute that modifies the trait,
the macro may receive unexpected input.

For example:
```rust
#[some_other_macro]  // runs first, might modify the trait
#[contracttrait]     // runs second, receives the modified trait
pub trait Ownable { ... }
```

This is a general concern with all proc macros, but it is particularly
important for security-critical code. The documentation should specify:
- `#[contracttrait]` should be the innermost (last applied) attribute
- Or, the macro should detect and report when the trait has already been
  modified by another macro

### 9. The Generated `type Provider` Has No Doc Comment

The `generate_outer_trait` function produces:
```rust
type Provider: #internal_trait_name;
```

without a doc comment. IDE tools that display documentation will show "no
documentation available" for the most important type in the trait.

**Fix:** Add a generated doc comment:
```rust
/// The provider type that implements the business logic for this trait.
/// Must implement `#internal_trait_name`.
type Provider: #internal_trait_name;
```

### 10. No `#[automatically_derived]` on Generated Impls

Generated impl blocks should have `#[automatically_derived]` to tell `rustc`
and tools like `clippy` that the code was generated. Without this attribute,
clippy may produce warnings on generated code that the developer cannot fix:

```rust
#[automatically_derived]
#[soroban_sdk::contractimpl]
impl $contract {
    #(#method_impls)*
}
```

This is a best practice for all proc macros that generate impl blocks.

---

## Suggestions

### 1. Fix the Hardcoded `env` Name (Highest Priority)

Extract the actual parameter name from the signature and use it in
`build_delegate_args`. This is the most impactful fix.

### 2. Use `Span::mixed_site()` for Generated Identifiers

Apply `Span::mixed_site()` to all generated identifiers (`__auth_addr`, clone
variables, etc.) to prevent hygiene conflicts with user code.

### 3. Add `quote_spanned!` for Improved Diagnostics

Use method-specific spans for delegation calls and auth checks. This makes
error messages point to the relevant trait method, not the macro attribute.

### 4. Replace `to_snake_case` with the `heck` Crate

Use `heck::ToSnakeCase` for proper acronym handling. Adding this small
dependency is worth it for correctness.

### 5. Add `cargo expand` Snapshot Tests

Generate the expanded output for each test case and commit it to the
repository. This serves both as documentation (developers can see what the
macro generates) and as a regression test (changes to generated code are
visible in diffs). Snapshot tests are the gold standard for proc-macro
quality assurance.

### 6. Add `#[automatically_derived]` to Generated Impl Blocks

This suppresses irrelevant clippy warnings and signals to tools that the code
is machine-generated.

### 7. Add a Validation Pass Before Code Generation

Validate all inputs before generating code:
- Methods have an env parameter
- Auth sources reference valid targets
- No `&self` parameters
- Trait has at least one method

### 8. Use Absolute Paths (`::alloc::boxed::Box`) Instead of `extern crate`

This eliminates the fragile `extern crate alloc as __alloc_X` pattern and
works correctly in all editions and environments.

### 9. Consider Future Rust Features

Several upcoming Rust features could benefit or affect this macro:

- **Trait aliases (RFC 1733):** Would allow `type OwnableWithAuth = Ownable<Provider = SingleOwner>` which could simplify the wiring.

- **Impl trait in type aliases:** Would enable expressing the provider as an
  existential type rather than an associated type.

- **Better proc-macro diagnostics (RFC 3456):** Would allow the macro to emit
  structured diagnostics with notes, help messages, and suggestions, improving
  the developer experience significantly.

- **Negative trait bounds:** Would allow expressing "this type does NOT
  implement X," which could be useful for preventing providers from being
  used in incorrect contexts.

The macro should be designed to be compatible with these features when they
stabilize.

---

## Unique Perspective: The Cost of Proc Macros

Proc macros are the most powerful and most dangerous feature in Rust. They
transform source code at compile time, and the developer cannot see the
transformation without running `cargo expand`. Every hygiene violation, every
hardcoded identifier, every missing span -- these become invisible bugs that
produce confusing error messages.

The Rust compiler team has spent years building a hygiene system that prevents
macro-generated identifiers from conflicting with user code. `macro_rules!`
macros benefit from this system automatically. Proc macros, by design, operate
outside this system -- they generate raw token streams without automatic
hygiene. This means proc-macro authors bear the responsibility of maintaining
hygiene manually.

This codebase does a better job than most. But the hardcoded `env` name and
the call-site-hygiene `__auth_addr` are exactly the kind of issues that would
be caught automatically by the compiler's built-in hygiene if this were a
`macro_rules!` macro instead of a proc macro. These are the costs of the
proc-macro escape hatch, and they should be addressed explicitly.

The broader lesson is that proc macros should generate the minimum amount of
code necessary. Every generated identifier is a potential hygiene conflict.
Every generated expression is a potential span problem. The cleaner approach is
to have the proc macro generate calls to well-tested library functions, rather
than generating the logic inline. This would localize the hygiene surface to
the function call boundary.

---

## Would I Use This?

Yes, with the caveat that I would always run `cargo expand` on any crate that
uses it to verify the generated output. The macro is well-structured and the
code is clean. The hygiene issues are fixable and do not represent fundamental
design problems.

After fixing:
1. The hardcoded `env` parameter name
2. Adding `Span::mixed_site()` for generated identifiers
3. Adding `quote_spanned!` for improved diagnostics
4. Replacing `to_snake_case` with `heck`
5. Adding validation pass for edge cases

...this would be a solid proc-macro crate that I would feel comfortable
recommending to other Rust developers.

**Verdict:** Above average proc-macro engineering with fixable hygiene issues.
The architecture is sound, the error handling is correct, and the code is
readable. The priority fixes (hardcoded env name, span propagation) would
make this one of the better proc-macro crates in the Soroban ecosystem.
The team clearly understands proc macros -- they just need to apply the
finishing touches that distinguish "works in common cases" from "works in
all cases."
