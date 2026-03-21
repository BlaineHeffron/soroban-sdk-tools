# Review: soroban-sdk-tools -- A Perl Hacker's Honest Assessment

**Reviewer:** Hank Morrisson
**Background:** 30-year Perl veteran; CPAN contributor since 1996; shipped production systems in 14 languages; maintains a dim view of frameworks that mistake complexity for capability
**Focus:** Over-engineering, TMTOWTDI philosophy, whether this adds real value vs. syntax sugar

---

## Executive Summary

Look, I have been writing code since before half these blockchain kids were born. I have seen every paradigm come and go -- OOP, AOP, SOA, microservices, serverless, and now "composable smart contracts." Every generation thinks they invented modularity. Every generation wraps things in macros and calls it progress.

So when I look at soroban-sdk-tools and its `#[contracttrait]` macro, I ask the only question that matters: **Does this actually help you ship correct code faster, or is it just a fancier way to be wrong?**

The answer, grudgingly, is: it helps. But not without caveats, and not without the kind of over-engineering smell that makes my nose twitch.

---

## 1. TMTOWTDI vs. TOOWTDI

Perl's philosophy: There's More Than One Way To Do It.
Python's philosophy: There should be One -- and preferably Only One -- obvious way to do it.
soroban-sdk-tools' philosophy: There are exactly Two Ways To Do It, and one of them is sealed.

The framework offers:
- **Option A:** `#[contractimpl(contracttrait)]` -- flexible, overridable, you are in control
- **Option B:** `impl_ownable!()` -- sealed, non-overridable, the macro is in control

This is actually a reasonable design. In Perl, I would write:

```perl
sub transfer_ownership {
    my ($self, $new_owner) = @_;
    $self->_enforce_auth();  # you could skip this. you'd be wrong, but you could.
    $self->{owner} = $new_owner;
}
```

And I would trust the developer to call `_enforce_auth()`. The leading underscore is the convention. The convention is the contract. If you violate it, that is your problem.

Rust's type system does not trust you, and in the context of smart contracts holding real money, that lack of trust is appropriate. I concede this point. The `impl_ownable!` sealed pattern is the right default for code that moves money.

But I notice the blog post and docs spend significant energy on the sealed pattern while the actual example (`examples/trait-test/src/lib.rs`) uses the flexible Option A:

```rust
#[contractimpl(contracttrait)]
impl Ownable for TestContract {
    type Provider = SingleOwner;
}
```

This means the example code uses the less-secure option. The documentation promotes the more-secure option. This is a consistency gap that will confuse adopters. Pick one default and make the other the explicitly-documented escape hatch.

---

## 2. The "Two-Trait Generation" -- Is This Actually Novel?

The blog post calls this "the core innovation":

```rust
// You write:
#[contracttrait]
pub trait Ownable {
    fn owner(env: &Env) -> Address;
    #[auth(Self::owner)]
    fn transfer_ownership(env: &Env, new_owner: Address);
}

// Macro generates:
// - OwnableInternal (business logic)
// - Ownable (auth wrapper with type Provider)
```

Let me be blunt: this is the Strategy pattern with a macro hiding the boilerplate. GoF, 1994. Thirty-two years ago. The "internal trait" is the strategy interface. The "provider" is the concrete strategy. The "outer trait" is the context.

In Perl:

```perl
package Ownable;
sub new { bless { strategy => $_[1] }, $_[0] }
sub transfer_ownership {
    my ($self, $new_owner) = @_;
    $self->_enforce_auth();
    $self->{strategy}->transfer_ownership($new_owner);
}
```

The Rust version is better because:
1. It is compile-time, not runtime (zero-cost)
2. The auth enforcement is structural, not convention-based
3. The macro eliminates the boilerplate that makes Strategy tedious

But let us not pretend this is a new idea. It is a well-known pattern made ergonomic through Rust's macro system. That is valuable. It is not revolutionary.

---

## 3. The Macro Expansion -- What You Are Actually Trusting

I read `soroban-sdk-tools-macro/src/contract.rs` carefully. All 727 lines. Here is what the macro does:

1. Parses your trait definition
2. Extracts `#[auth]` annotations
3. Generates an Internal trait (stripped of auth)
4. Generates an Outer trait (with auth injected as default impls)
5. Generates an AuthClient (test helper)
6. Generates a sealed impl macro

The code is clean. The `extract_auth_attr` function handles two cases (`Self::method` and `param_name`) with appropriate error messages. The `build_delegate_args` function constructs forwarding calls. The `generate_sealed_impl_macro` uses `$provider` as a macro metavariable correctly.

What concerns me:

### a) The `alloc` alias

```rust
let alloc_alias = format_ident!("__alloc_{}", trait_name);
// ...
extern crate alloc as #alloc_alias;
```

Each trait gets its own `extern crate alloc` with a unique alias. This is a workaround for Rust's module system. It works, but it means every trait you define adds a crate alias to your namespace. With 10 composed traits, you have 10 aliases. This is not a bug, but it is the kind of accidental complexity that makes code harder to reason about.

### b) Clone proliferation in AuthClient

```rust
let arg_clones: Vec<_> = method.params.iter().map(|p| {
    let name = &p.name;
    let clone_name = format_ident!("{}_clone", name);
    quote! { let #clone_name = #name.clone(); }
}).collect();

let try_arg_clones: Vec<_> = method.params.iter().map(|p| {
    let name = &p.name;
    let clone_name = format_ident!("{}_try_clone", name);
    quote! { let #clone_name = #name.clone(); }
}).collect();
```

Every parameter gets cloned twice -- once for the invoke path, once for the try_invoke path. For Address types on Soroban, cloning is cheap (it is essentially a copy of an index into the environment's object table). But for large `Bytes` or `Vec` parameters, this could be meaningful. And the naming convention (`{name}_clone`, `{name}_try_clone`) is the kind of thing that will produce confusing error messages when something goes wrong.

### c) No validation that `#[auth(Self::method)]` references an actual method

```rust
Some(AuthSource::ProviderMethod(resolver)) => {
    quote! {
        let __auth_addr = Self::Provider::#resolver(env);
        __auth_addr.require_auth();
    }
}
```

If I write `#[auth(Self::nonexistent_method)]`, the macro happily generates code that calls `Self::Provider::nonexistent_method(env)`. The error will come from the Rust compiler, not from the macro. The error message will reference generated code, not my source code. This is a DX issue that matters.

Perl has `AUTOLOAD`. Rust has inscrutable proc-macro error messages. Neither is ideal.

---

## 4. What This Is Really Worth: An Honest Line Count

The blog post claims "~35 lines in 1 file" vs. OZ's "~80 lines across 3 files." Let me check this claim.

The soroban-sdk-tools approach for Ownable:

```rust
// Trait definition: 6 lines
#[contracttrait]
pub trait Ownable {
    fn owner(env: &Env) -> Address;
    #[auth(Self::owner)]
    fn transfer_ownership(env: &Env, new_owner: Address);
}

// Provider: 11 lines
pub struct SingleOwner;
impl OwnableInternal for SingleOwner {
    fn owner(env: &Env) -> Address {
        env.storage().instance().get(&Symbol::new(env, "owner")).expect("not initialized")
    }
    fn transfer_ownership(env: &Env, new_owner: Address) {
        env.storage().instance().set(&Symbol::new(env, "owner"), &new_owner);
    }
}

// Wiring: 1 line
impl_ownable!(MyContract, SingleOwner);
```

That is 18 lines of user-written code. The claim of 35 lines includes the generated code that the user does not write. This is misleading. The user writes 18 lines. The macro generates the rest.

The OZ approach for Ownable requires the user to write approximately 5-10 lines (the `impl Ownable for MyContract {}` block plus any `#[only_owner]` annotations).

So the real comparison is:
- soroban-sdk-tools: 18 lines (trait + provider + wiring)
- OZ: ~5-10 lines (impl block + annotations)

Wait. OZ is actually fewer lines for the consumer? Yes, because OZ's library code (the 80 lines) is already written for you. You just use it.

The soroban-sdk-tools advantage is not in consumer line count -- it is in the structural guarantees and the ability to swap providers. That is a real advantage. But the blog post should not conflate "library implementation complexity" with "consumer code complexity."

---

## 5. The Provider Pattern -- When Abstraction Becomes Tax

I have a rule of thumb: **If you add an abstraction layer, you need to use it at least three times to justify the cost.**

The Provider pattern adds:
1. An Internal trait
2. A Provider struct
3. A `type Provider = X` declaration

This is justified when you have multiple providers (SingleOwner, MultisigOwner, TimelockOwner). The blog post mentions these alternatives. But the example code only shows `SingleOwner`. The OZ comparison only shows `SingleOwner`.

If 90% of contracts use `SingleOwner`, the Provider pattern is a tax on the common case for the benefit of the uncommon case. In Perl, we would call this "premature abstraction."

### The counterargument

The counterargument is that smart contracts are not regular software. A wrong auth check can lose millions. Having the abstraction layer even if you only use one provider gives you structural auth enforcement for free. The Provider is not just for swapping -- it is the mechanism that enables the two-trait split that enables sealed auth.

I accept this counterargument. But the documentation should make it explicit: "The Provider pattern exists primarily for structural auth enforcement. Implementation swapping is a secondary benefit."

---

## 6. Error Handling: `#[scerr]` Sounds Nice, Show Me the Edge Cases

The blog mentions `#[scerr]` for composable error handling:

```rust
#[scerr]
pub enum MyError {
    CustomError,
    #[transparent]
    Ownable(#[from] OwnableError),
    #[transparent]
    Pausable(#[from] PausableError),
}
```

Questions from the cranky veteran:

1. What happens when two composed traits produce errors with the same code after auto-chaining? The blog says "no collisions possible" but does not explain the mechanism. Const generics? Sequential allocation? Hash-based?

2. What is the maximum error code value? Soroban uses `u32` for error codes. With enough composed traits, do we risk overflow?

3. Can error codes be stable across contract upgrades? If I add a new error variant to `OwnableError`, do all downstream `PausableError` codes shift? This would break client code that matches on error codes.

4. Where is the `#[scerr]` implementation? It is mentioned in the docs but I do not see it in the macro code I reviewed. Is it in a different crate? Is it implemented yet?

These are not theoretical concerns. In Perl, we learned the hard way that auto-generated error codes are a nightmare for backward compatibility. `$!` is a global, and everyone hates it, but at least it is stable.

---

## 7. Testing: The AuthClient Is Actually Good

I will say something nice now, because I am not a complete curmudgeon.

The `AuthClient` with `.authorize(&owner).invoke()` is genuinely better than `mock_all_auths()`. Here is why:

`mock_all_auths()` is the equivalent of Perl's `no strict 'refs'` -- it turns off the safety checks so your tests pass. Your tests pass, but they do not test anything meaningful. It is security theater for test suites.

The `AuthClient` pattern:

```rust
auth_client.transfer_ownership(&new_owner)
    .authorize(&owner)
    .invoke();
```

This actually tests that:
1. The method exists
2. The auth address matches
3. The invocation succeeds with correct auth

And the try-invoke pattern:

```rust
let result = auth_client.transfer_ownership(&new_owner)
    .authorize(&wrong_person)
    .try_invoke();
assert!(result.is_err());
```

This tests negative cases -- something `mock_all_auths()` makes impossible by design.

The `CallBuilder` implementation in the macro code is a bit heavy (boxed closures, double-cloning), but the API is clean. In Perl terms, this is a well-designed test harness, and test harnesses are where abstraction pays its biggest dividends.

---

## 8. The Snake Case Conversion: A Nit That Matters

```rust
fn to_snake_case(s: &str) -> String {
    let mut result = String::new();
    for (i, c) in s.chars().enumerate() {
        if c.is_uppercase() {
            if i > 0 {
                result.push('_');
            }
            result.push(c.to_lowercase().next().unwrap());
        } else {
            result.push(c);
        }
    }
    result
}
```

This handles `Ownable` -> `ownable` and `FungibleToken` -> `fungible_token`. But it does not handle:

- `HTTPClient` -> `h_t_t_p_client` (wrong, should be `http_client`)
- `IOError` -> `i_o_error` (wrong, should be `io_error`)
- `ABCDef` -> `a_b_c_def` (wrong, should be `abc_def`)

Consecutive uppercase letters are treated as individual words. This is a known footgun in snake_case conversion. The `heck` crate handles this correctly. Using a hand-rolled implementation when a well-tested crate exists is the kind of not-invented-here syndrome that I have been complaining about since 1997.

In a proc macro, adding a dependency has compile-time cost. But `heck` is tiny. And getting trait names wrong in generated macro names is the kind of bug that will consume hours of debugging for a confused user.

---

## 9. The Blog Post: Marketing vs. Engineering

The blog post is well-written marketing. It positions soroban-sdk-tools as a natural evolution of OZ's patterns. It is respectful of the competition. It makes concrete claims with code examples. This is good developer relations.

But it makes some claims that rub me the wrong way:

### "The developer never writes auth code"

They write `#[auth(Self::owner)]`. That is auth code. It is declarative auth code, which is better than imperative auth code, but it is still auth code. The developer must:
1. Know which methods need auth
2. Know which address provides the auth
3. Write the annotation correctly

"The developer writes less auth code and cannot forget the enforcement" is accurate. "The developer never writes auth code" is not.

### "Single definition generates everything"

The user writes: trait definition + provider implementation + contract wiring. That is three things. OZ requires: storage module + trait module + contract usage. That is also three things. The things are different (and the soroban-sdk-tools versions are arguably better), but "single definition" is misleading.

### "Zero overhead"

The blog says "Zero overhead" for WASM size. This is plausible due to monomorphization + LTO. But it is stated without proof. Where are the benchmarks? Show me two WASM binaries -- one hand-written, one macro-generated -- and let me diff them. "Trust me, it is zero overhead" is not engineering, it is faith.

---

## 10. Final Verdict: Would Hank Use This?

If I were building a Soroban contract today -- and I say this as someone who still prefers to write everything by hand -- I would use soroban-sdk-tools for one reason: **the sealed auth pattern.**

The sealed `impl_ownable!` macro is the single most valuable feature. It takes a class of bugs (forgotten auth checks) that has cost the blockchain industry billions of dollars and makes them structurally impossible. That is worth the abstraction tax.

The Provider pattern is nice but I would use it only if I actually needed multiple implementations. For a simple single-owner contract, the Provider adds complexity without benefit.

The AuthClient is genuinely useful for testing and I would adopt it immediately.

The `#[scerr]` error composition sounds promising but I want to see the implementation before I commit to it.

The blog post oversells. The framework under-documents. The macro code is clean but has edge cases (snake_case, missing method validation). These are normal growing pains for a young project.

### Score: 7/10

Solid engineering underneath marketing that needs to calm down. Would use. Would complain about while using.

---

*Reviewed by Hank Morrisson, Perl monk, 30-year veteran, professional curmudgeon*
*Review date: 2026-03-21*
