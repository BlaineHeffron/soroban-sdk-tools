# Review: Xiu -- Chinese Calligrapher

**Reviewer:** Xiu, Chinese calligrapher who sees beauty in the structure of code
**Focus:** Aesthetic quality of the macro output, balance between complexity and elegance
**Date:** 2026-03-21
**Materials Reviewed:**
- `docs/oz-comparison.md`
- `docs/blog-post-composable-contracts.md`
- `examples/trait-test/src/lib.rs`
- `soroban-sdk-tools-macro/src/contract.rs`

---

## Preface: On the Aesthetics of Structure

In Chinese calligraphy, we speak of "gu fa" -- the bone method. The structure
of a character must be sound before any embellishment. A brushstroke can be
beautiful, but if the skeleton is wrong, the character fails.

Code, like calligraphy, has bone structure. The macro's generated output is the
skeleton upon which every contract is built. If the generated code is ugly,
cluttered, or unbalanced, every contract built with it inherits that ugliness.

I evaluate this codebase not merely for correctness, but for beauty. And by
beauty, I mean clarity of intent, economy of expression, balance of
responsibilities, and the harmony between the written and the generated.

---

## 1. The Trait Definition: A Single Brushstroke

Consider the core artifact -- the trait definition:

```rust
#[contracttrait]
pub trait Ownable {
    fn owner(env: &Env) -> Address;

    #[auth(Self::owner)]
    fn transfer_ownership(env: &Env, new_owner: Address);
}
```

This is six lines. It expresses:
- A readable state query (`owner`)
- A state mutation (`transfer_ownership`)
- An authorization constraint (`#[auth(Self::owner)]`)
- The relationship between the query and the constraint

In calligraphy, the highest compliment is "yi bi cheng" -- completed in a single
stroke. This trait definition achieves that quality. Everything the developer
needs to understand about ownership is contained in six lines. There is nothing
superfluous. There is nothing missing.

**Assessment: Beautiful. This is the finest element of the entire design.**

---

## 2. The Generated Two-Trait Structure: Balance and Tension

The macro expands those six lines into two traits:

```rust
pub trait OwnableInternal {
    fn owner(env: &Env) -> Address;
    fn transfer_ownership(env: &Env, new_owner: Address);
}

#[soroban_sdk::contracttrait]
pub trait Ownable {
    type Provider: OwnableInternal;

    fn owner(env: &Env) -> Address {
        Self::Provider::owner(env)
    }

    fn transfer_ownership(env: &Env, new_owner: Address) {
        let __auth_addr = Self::Provider::owner(env);
        __auth_addr.require_auth();
        Self::Provider::transfer_ownership(env, new_owner)
    }
}
```

### What Is Beautiful

The **symmetry** between the two traits. `OwnableInternal` is the undressed
form -- pure function signatures. `Ownable` is the dressed form -- the same
functions with auth clothing. The internal trait is yin (receptive, waiting
for implementation). The outer trait is yang (active, enforcing).

The `type Provider: OwnableInternal` line is a pivot point. It connects
the two halves while keeping them separate. In calligraphy, this is the
"zhuan zhe" -- the turning point where a stroke changes direction without
lifting the brush.

### What Is Less Beautiful

The variable name `__auth_addr` is ugly. The double underscore is a convention
for internal/compiler-generated identifiers, but it creates visual noise in
what should be a clean delegation pattern. In a beautifully structured piece,
every element should be intentional.

Suggestion: Use a more descriptive name like `authorized_address` or, if you
want to indicate it is generated, use a single-underscore prefix that reads
more naturally:

```rust
let _auth = Self::Provider::owner(env);
_auth.require_auth();
```

The underscore prefix is idiomatic Rust for "this is used but its name is not
the point." The double underscore is C-lineage noise.

### The `env` Hardcoding

In `build_delegate_args` (line 306), the env parameter name is hardcoded:

```rust
let env_ident: Ident = parse_quote!(env);
args.push(quote! { #env_ident });
```

This is a hidden assumption. If someone names their env parameter `e` (which
is common in Soroban code, and used extensively in OZ's examples), the
generated code would fail or produce confusing errors.

This is like a calligrapher who assumes all paper is the same size. The tool
should adapt to the material.

**Recommendation:** Extract the actual env parameter name from the method
signature rather than assuming `env`.

---

## 3. The Macro Source Code Itself: Reading the Brushwork

### 3.1 Structure and Flow

The file `contract.rs` is organized in sections separated by banner comments:

```rust
// -----------------------------------------------------------------------------
// Auth attribute parsing
// -----------------------------------------------------------------------------
```

This is a workmanlike approach to organization. The sections flow logically:
1. Auth attribute parsing (reading the annotation)
2. Method info extraction (understanding the input)
3. Internal trait generation (first output)
4. Outer trait generation (second output)
5. Sealed macro generation (third output)
6. AuthClient generation (fourth output)
7. Entry point (orchestration)

The flow is top-down, which is correct for a procedural macro. You read from
the beginning and understand the transformation step by step. This is "du shu"
-- reading in order -- and it works.

### 3.2 Function Naming

The function names are descriptive and consistent:
- `extract_auth_attr` -- extracts
- `strip_auth_attrs` -- strips
- `generate_internal_trait` -- generates
- `build_delegate_args` -- builds

Each verb matches its purpose. This is good naming. In calligraphy, we say
"ming shi qi yi" -- the name makes the meaning clear.

### 3.3 The `MethodInfo` Struct

```rust
struct MethodInfo {
    name: Ident,
    sig: Signature,
    attrs: Vec<Attribute>,
    auth: Option<AuthSource>,
    params: Vec<ParamInfo>,
    env_is_ref: bool,
}
```

This struct is the "zhong feng" (center stroke) of the entire macro. Everything
flows through it. It collects all information about a method in one place and
then is consumed by multiple generators.

The `#[allow(dead_code)]` on `env_is_ref` is a blemish:

```rust
    #[allow(dead_code)]
    env_is_ref: bool,
```

If a field is not used, it should not exist. If it will be used in the future,
it should be commented with intent. A dead field is an unfinished stroke --
the brush was lifted before the character was complete.

**Recommendation:** Either use `env_is_ref` (likely for handling `&Env` vs `Env`
parameter patterns) or remove it with a TODO comment explaining the intended
future use.

---

## 4. The Provider Implementation: Calligraphy of the Developer

The example in `trait-test/src/lib.rs` shows how a developer implements
a provider:

```rust
pub struct SingleOwner;

impl OwnableInternal for SingleOwner {
    fn owner(env: &Env) -> Address {
        env.storage()
            .instance()
            .get(&soroban_sdk::Symbol::new(env, "owner"))
            .expect("not initialized")
    }

    fn transfer_ownership(env: &Env, new_owner: Address) {
        env.storage()
            .instance()
            .set(&soroban_sdk::Symbol::new(env, "owner"), &new_owner);
    }
}
```

### The Repetition Problem

`soroban_sdk::Symbol::new(env, "owner")` appears three times in this file
(including the `init` function). This violates DRY in the developer's code,
though it is not the macro's fault.

However, the macro *could* help. If `#[contracttrait]` generated storage key
constants alongside the traits:

```rust
pub mod ownable_keys {
    pub const OWNER: &str = "owner";
}
```

This is debatable. The macro currently does not generate storage, and that is
a deliberate design choice (storage is the provider's responsibility). But
the repetition in the example code suggests that a companion `#[contractstorage]`
macro (mentioned in the OZ comparison) would be valuable.

### The Beauty of Constraint

What IS beautiful about the provider implementation is what it does NOT contain.
There is no auth logic. No `require_auth()`. No authorization checks of any kind.
The provider is pure data operations.

This is the calligraphic principle of "liu bai" -- leaving white space. The
absence of auth in the provider is as important as the presence of auth in the
outer trait. The white space gives the black strokes meaning.

---

## 5. The Sealed Macro: A Stamp of Authority

```rust
impl_ownable!(MyContract, SingleOwner);
```

In Chinese calligraphy, a finished work bears a seal (yin zhang). The seal
certifies the work's authenticity. It cannot be altered after impression.

The `impl_{trait_snake}!` macro is this seal. Once impressed, the auth
pattern is fixed. The methods are inherent, not overridable. The security
is stamped.

### Aesthetic of the Generated Macro

The macro source generation (lines 329-386) is clean:

```rust
#[macro_export]
macro_rules! #macro_name {
    ($contract:ty, $provider:ty) => {
        #[soroban_sdk::contractimpl]
        impl $contract {
            #(#method_impls)*
        }
    };
}
```

The generated `macro_rules!` is minimal. Two parameters: the contract and the
provider. No configuration, no options, no complexity. This is correct -- a
seal should be simple. You press it into the wax, and it is done.

### A Minor Flaw

The doc comment on the generated macro:

```rust
/// Sealed implementation macro for non-overridable auth enforcement.
///
/// Usage: `impl_#trait_snake!(MyContract, MyProvider);`
```

The `#trait_snake` in the doc comment is a placeholder, not the actual macro
name. If the trait is `Ownable`, the doc should say `impl_ownable!`. But since
this is a generated doc comment inside a `quote!` block, it cannot be dynamically
interpolated without additional work.

This is a small flaw, but documentation is part of the aesthetic. A beautiful
tool should describe itself correctly.

**Recommendation:** Generate the doc comment with the actual trait name:

```rust
let doc_str = format!("Sealed implementation macro for `{}`. Usage: `{}!(MyContract, MyProvider);`", trait_name, macro_name);
```

---

## 6. The AuthClient: Flowing Water

The AuthClient code (lines 408-573) is the most complex part of the macro.
It generates a test client with fluent API:

```rust
auth_client.transfer_ownership(&new_owner)
    .authorize(&owner)
    .invoke();
```

### Aesthetic Quality

The fluent API reads like a sentence: "Transfer ownership to new_owner,
authorized by owner, invoke." This is good. Method chains that read as
natural language are beautiful code.

### Internal Complexity

The implementation, however, is dense. The `generate_auth_client_method`
function (lines 466-573) creates:
- Parameter clones with `_clone` suffix
- Try-parameter clones with `_try_clone` suffix
- Boxed closures capturing cloned values
- Tuple conversions for argument passing

This is the "nei gong" (internal work) -- hidden complexity that enables
external simplicity. The external API is beautiful *because* the internal
implementation bears the burden.

However, the clone naming convention is noisy:

```rust
let clone_name = format_ident!("{}_clone", name);
// ...
let try_clone_name = format_ident!("{}_try_clone", name);
```

Having both `new_owner_clone` and `new_owner_try_clone` in the generated
code is visually heavy. This is six syllables where two would do.

**Recommendation:** Use shorter suffixes or restructure to avoid the double
clone pattern:

```rust
let c = format_ident!("_c_{}", name);   // compact clone
let tc = format_ident!("_tc_{}", name);  // compact try-clone
```

Or better, restructure to clone once and share via `Rc`:

```rust
let shared = Rc::new(name.clone());
```

---

## 7. The OZ Comparison Document: Persuasion Through Juxtaposition

The `oz-comparison.md` document is well-structured. The side-by-side format
works because it lets the reader see the difference immediately.

### The Line Count Argument

"~80 lines across 3 files" vs "~35 lines in 1 file + macro" -- this is
a compelling metric. Fewer lines generally means fewer places for bugs to hide.

But line count is not the only measure of beauty. A single-character
calligraphic work ("yi") can be more beautiful than a ten-character poem
if the single character is perfectly executed. The question is not just
"how many lines" but "how clear are those lines."

Both the OZ and soroban-sdk-tools approaches are clear. OZ is verbose but
explicit. soroban-sdk-tools is concise but relies on macro magic. These
are different aesthetic values:
- OZ follows "gong bi" (meticulous style) -- every detail visible
- soroban-sdk-tools follows "xie yi" (freehand style) -- essential strokes only

Neither is inherently superior. The choice depends on the viewer's values.
But for smart contracts, where security is paramount, I lean toward the
freehand style -- fewer strokes means fewer opportunities for error.

---

## 8. The Blog Post: Craftsmanship of Communication

The blog post is a well-structured piece of technical writing:

1. Problem statement (shared challenge)
2. Core innovation (two-trait generation)
3. Security analysis (override problem)
4. DI pattern (provider swapping)
5. Testing (AuthClient)
6. Error handling (scerr)
7. OZ recommendations (what to adopt)
8. CGP connection (theoretical grounding)
9. Performance (zero overhead)
10. Call to action (try it today)
11. Collaboration invitation (respectful tone)

### Strengths

The tone is respectful toward OZ. Phrases like "we deeply respect" and
"could benefit the entire ecosystem" are diplomacy done right. In calligraphy,
we say "qian xu" -- humility. The work speaks for itself; the artist need
not boast.

The code examples are well-chosen. Each demonstrates exactly the point being
made, no more. This is good editing.

### Weaknesses

The document switches between "we" and "our" and "I" somewhat inconsistently.
In a single-author work, use "I." In a multi-author work, use "we" consistently.
The final byline names two authors, so "we" is correct -- but make it consistent
throughout.

The "Try It Today" section shows `soroban-sdk-tools = "0.1.0"` -- a version
number that screams "alpha." This undercuts the authority of the preceding
technical arguments. Consider whether to include the version number at all,
or frame it differently: "available as an early release" rather than presenting
a 0.1.0 version alongside claims of "OpenZeppelin-level composability."

---

## 9. Balance Assessment: Complexity vs. Elegance

### The Complexity Budget

Every system has a complexity budget. Where does this system spend it?

| Component | Complexity | Justification |
|-----------|-----------|---------------|
| Trait definition | Very low | Excellent -- the API surface is minimal |
| Generated Internal trait | Low | Pure method signatures, no logic |
| Generated Outer trait | Medium | Auth wrapping adds one level of indirection |
| Provider implementation | Low-Medium | Developer writes pure business logic |
| Sealed macro | Low | Two parameters, no configuration |
| AuthClient | High | Closures, clones, boxed callbacks |
| Macro source code | High | 727 lines of proc-macro code |

The complexity is correctly distributed: high in the tooling (macro source),
low in the usage (trait definitions, provider implementations). The developer
interacts with the low-complexity surface and is shielded from the
high-complexity internals.

This is the hallmark of good tool design. A calligraphy brush is a complex
instrument (bamboo, animal hair, precise binding), but the interface is
simple: hold it, dip it, write.

### The Elegance Score

I evaluate elegance on four axes:

1. **Economy** (saying much with little): 9/10 -- Six-line trait definitions
   generating hundreds of lines of correct code.

2. **Clarity** (understanding without study): 7/10 -- The `#[auth]` annotation
   is immediately clear. The provider pattern requires some learning.

3. **Consistency** (same patterns throughout): 8/10 -- The auth/internal/outer
   pattern is consistent across traits. Minor inconsistencies in naming.

4. **Harmony** (parts fitting together): 8/10 -- Supertraits compose naturally.
   The AuthClient is slightly discordant with the rest of the system (much more
   complex than other generated artifacts).

**Overall Elegance: 8/10**

---

## 10. Specific Aesthetic Issues

### 10.1 The `extern crate alloc` Alias

```rust
let alloc_alias = format_ident!("__alloc_{}", trait_name);
// ...
extern crate alloc as #alloc_alias;
```

This generates `extern crate alloc as __alloc_Ownable;` to avoid conflicts
when multiple traits generate auth clients. Pragmatic but ugly. The double
underscore prefix with a PascalCase suffix violates Rust naming conventions.

**Suggestion:** Use snake_case: `__alloc_ownable`. Or better, find a way to
avoid the per-trait alloc import entirely by using a shared one from the
`soroban_sdk_tools` crate.

### 10.2 The `#[allow(dead_code)]` Annotation

Already discussed. Remove the dead field or use it.

### 10.3 The `_is_ref` Field in ParamInfo

```rust
struct ParamInfo {
    name: Ident,
    ty: syn::Type,
    _is_ref: bool,
}
```

Another underscore-prefixed unused field. Same issue as `env_is_ref`. Either
use it or remove it.

### 10.4 The Comment Style

The banner comments use dashes:

```
// -----------------------------------------------------------------------------
// Auth attribute parsing
// -----------------------------------------------------------------------------
```

This is 77 characters of dashes. Functional but heavy. Consider a lighter style:

```
// --- Auth attribute parsing ---
```

Or use Rust's doc module convention if these become separate modules:

```
//! # Auth attribute parsing
```

---

## 11. The Example Code: A Demonstration of the Aesthetic

The `trait-test/src/lib.rs` file is the first thing a potential user sees. It
must be beautiful.

### Current State

It is clean and well-organized:
1. Trait definition (lines 1-19)
2. Provider implementation (lines 21-37)
3. Supertrait definition (lines 39-49)
4. Supertrait provider (lines 51-70)
5. Contract wiring (lines 72-97)
6. Tests (lines 99-180)

The comments are helpful. The test names are descriptive. The flow from
simple to complex is correct.

### One Issue

The tests use `mock_all_auths()` for the basic tests and `AuthClient`
for the auth-specific tests. This is correct structurally, but it
undermines the blog post's argument that `mock_all_auths()` is inferior.

If the example code itself uses `mock_all_auths()`, the reader might think:
"Well, even they use it." Consider converting ALL tests to use `AuthClient`
to demonstrate that it is a complete replacement, not just an alternative.

**Recommendation:** At minimum, add a comment explaining why `mock_all_auths()`
is used in the basic tests: "Using mock_all_auths for brevity; see
test_ownable_auth_client for precise auth testing."

---

## 12. Final Reflection: The Character of This Code

In calligraphy, every character tells a story about the calligrapher. The
speed of the stroke, the pressure on the brush, the moment of hesitation
before a difficult turn -- all visible in the final work.

This codebase tells the story of developers who:
- Value security (the sealed macro, the auth-first design)
- Respect the ecosystem (the OZ comparison's diplomatic tone)
- Think structurally (the two-trait split, the provider pattern)
- Are not yet finished (dead code fields, missing event emission)

The unfinished parts are not flaws in the aesthetic sense. In Chinese
painting, "liu bai" (leaving white space) is deliberate. The `env_is_ref`
field says "there is more to come." The absence of event generation says
"this is the next stroke."

A masterwork is not one that is complete. It is one where the intention
is clear and the execution is sound. This codebase has clear intention and
mostly sound execution.

The six-line trait definition is the finest stroke. Everything else --
the macro, the providers, the sealed pattern, the AuthClient -- exists
to serve that single, elegant expression of a contract's authorization
structure.

That is beautiful.

---

**Overall Assessment:** High aesthetic quality with room for polish. The trait
definition surface is excellent. The generated code is functional but could be
more graceful in naming and internal structure. The documentation strikes the
right tone.

**Verdict:** A work of deliberate beauty, not yet fully polished. Like a
calligrapher's practice piece that shows mastery in the strokes but needs
one more pass for the spacing.
