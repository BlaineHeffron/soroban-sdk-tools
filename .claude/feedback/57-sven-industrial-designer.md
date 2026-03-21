# Review: soroban-sdk-tools #[contracttrait]
## Reviewer: Sven -- Swedish industrial designer, design thinking practitioner

---

## Overall Impression

I design furniture, tools, and user interfaces. For thirty years I have
applied the principle that form follows function -- that the shape of a
thing should emerge from its purpose, not from the designer's ego. When
I pick up a well-designed tool, it tells me how to hold it. When I sit
in a well-designed chair, it tells me how to sit.

Code is a tool. A framework is a workshop. The question I bring to this
review is: **when a developer picks up `soroban-sdk-tools`, does it tell
them how to hold it?**

After reading the documentation, examples, and macro implementation, my
assessment is that the core interaction model -- the trait definition with
`#[auth]` -- is superbly designed. But the surrounding experience (onboarding,
error feedback, progressive disclosure) needs the same attention to
ergonomics that the core API received.

---

## Strengths

### 1. The primary interaction surface is perfectly shaped

The trait definition:
```rust
#[contracttrait]
pub trait Ownable {
    fn owner(env: &Env) -> Address;
    #[auth(Self::owner)]
    fn transfer_ownership(env: &Env, new_owner: Address);
}
```

This is a tool that tells you how to hold it:
- The attribute `#[contracttrait]` signals "this is special"
- The method signatures describe the contract's interface
- The `#[auth]` annotation explains the security constraint
- There is nothing extraneous, nothing missing

In design terms, this is an affordance -- a visual/textual cue that
communicates function. The annotation `#[auth(Self::owner)]` is both
instruction manual and implementation. It says "this requires owner
auth" and it makes it happen. That is excellent affordance design.

### 2. The two paths (sealed vs. flexible) respect user expertise

The framework offers:
- `impl_ownable!(MyContract, SingleOwner)` for the safe default
- `#[contractimpl(contracttrait)]` for the advanced customization path

This is progressive disclosure in action. Beginners get a safe, simple
path. Experts get full control. The framework does not force beginners
to understand trait defaults and override semantics -- it gives them a
macro that does the right thing.

In furniture design, we call this "adjustable ergonomics." The chair
works for someone 160cm tall and someone 195cm tall because it adjusts
to the user, not the other way around.

### 3. The provider pattern has excellent conceptual mapping

"Provider" is a well-chosen name. It maps to a real-world concept that
developers already understand:
- A cloud provider provides infrastructure
- A service provider provides functionality
- An `OwnableInternal` provider provides ownership logic

The naming creates an immediate mental model: "I choose a provider for
my contract, and the provider handles the details." This is good
conceptual design -- the metaphor does explanatory work that documentation
would otherwise need to do.

### 4. The macro expansion is invisible by default

The developer writes 6 lines of trait definition. They do not see the
100+ lines of generated code. This is the principle of information
hiding applied to code generation. The generated code is an
implementation detail, like the mechanism inside a door handle.

When you turn a door handle, you do not need to see the latch mechanism.
When you write `#[contracttrait]`, you do not need to see the two-trait
split. The abstraction is well-calibrated.

---

## Concerns

### 1. The "feel" of error messages is untested

I write `#[auth(owner)]` (forgetting `Self::`). What happens?

Looking at the macro code:
```rust
Expr::Path(ep) if ep.path.segments.len() == 1 => {
    let name = &ep.path.segments[0].ident;
    return Ok(Some(AuthSource::Param(name.clone())));
}
```

It silently interprets `owner` as a parameter name, not a method call.
If the method does not have a parameter named `owner`, the generated
code will fail with a confusing error from the expanded macro, not from
the `#[auth]` attribute.

In industrial design, we call this a "mode error" -- the user intends
one thing, the tool interprets another, and the feedback is about the
consequence, not the cause.

The macro should validate that `AuthSource::Param(name)` corresponds
to an actual parameter in the method signature. If not, emit a clear
error: "Parameter `owner` not found in method signature. Did you mean
`Self::owner`?"

### 2. No onboarding gradient

The example goes from "here is the trait definition" to "here is the
generated code" with no intermediate steps. A developer encountering
this for the first time has to absorb:
- The `#[contracttrait]` attribute
- The Internal/Outer trait split
- The AuthClient concept
- The Provider pattern
- The sealed macro
- The `#[contractimpl(contracttrait)]` syntax

This is six new concepts in one example file. Good onboarding introduces
one concept at a time:

1. First: `#[contracttrait]` generates a trait that works with the SDK
2. Then: `#[auth]` adds security automatically
3. Then: Providers let you swap implementations
4. Then: The sealed macro prevents overrides
5. Then: AuthClient helps you test

Each step should have its own minimal example. The current example
introduces everything simultaneously.

### 3. The naming convention is inconsistent

- `OwnableInternal` (Internal suffix for the provider trait)
- `OwnableAuthClient` (AuthClient suffix for the test helper)
- `impl_ownable!` (snake_case for the sealed macro)

Three different naming conventions for three related generated items.
This creates cognitive friction. Should it be:
- `ownable_internal`? `ownable_auth_client`? `impl_ownable`?
- `OwnableInternal`? `OwnableAuthClient`? `ImplOwnable`?

The inconsistency is partly forced by Rust conventions (traits are
PascalCase, macros are snake_case), but the framework could at least
document the naming pattern explicitly:

> For a trait named `Foo`:
> - Internal trait: `FooInternal`
> - Auth client: `FooAuthClient`
> - Sealed macro: `impl_foo!`

### 4. The example file does not show the "flow" of development

A well-designed tool tells a story through its usage. The example file
(`trait-test/src/lib.rs`) is organized by concept:
1. Trait definitions (Ownable, Pausable)
2. Provider implementations
3. Contract wiring
4. Tests

But a developer's workflow is:
1. Define what my contract needs (trait)
2. Wire it up (sealed macro or contractimpl)
3. Test it (write a test)
4. Later: customize the implementation (provider)

The example should follow the developer's journey, not the framework's
architecture. Put the simplest possible contract first, then layer
complexity.

### 5. The `#[contractimpl(contracttrait)]` attribute syntax is overloaded

In the Soroban SDK, `#[contractimpl]` is already a macro. This framework
adds `(contracttrait)` as an argument to it. But the example also shows
plain `#[contractimpl]` for the `init` method:

```rust
#[contractimpl(contracttrait)]
impl Ownable for TestContract { /* ... */ }

#[contractimpl]
impl TestContract { /* ... */ }
```

The difference between `#[contractimpl]` and `#[contractimpl(contracttrait)]`
is significant (one uses trait defaults, the other does not), but the
visual distinction is minimal. A developer scanning the code might miss
the `(contracttrait)` argument entirely.

This is a contrast problem -- in design, critical distinctions should have
high visual contrast. Consider whether the sealed macro path
(`impl_ownable!`) should be promoted as the primary API, since it is
visually distinct and cannot be confused with standard `#[contractimpl]`.

### 6. The comparison document weakens the framework's identity

The `oz-comparison.md` spends significant space explaining what
OpenZeppelin does. This positions `soroban-sdk-tools` as a response to
OZ rather than an independent creation.

In industrial design, we never define a product by what it is not. A
chair is not defined by how it differs from another chair. It is defined
by how it serves the person sitting in it.

The framework should lead with its own design philosophy -- structural
auth, provider-based composition, sealed guarantees -- without the
comparative framing. Let the design speak for itself.

---

## Suggestions

### 1. Validate auth sources at macro expansion time

When `#[auth(something)]` is parsed, cross-reference `something` against:
- Method parameters (if it is a bare identifier)
- Trait methods (if it is `Self::method`)

Emit a clear, actionable error message if the reference is invalid. The
error should tell the user what to fix, not what went wrong in the
generated code.

### 2. Create a progressive onboarding sequence

Five files, each introducing one concept:
1. `examples/01-basic-trait/` -- Just `#[contracttrait]`, no auth
2. `examples/02-auth/` -- Add `#[auth]` annotations
3. `examples/03-provider/` -- Introduce the provider pattern
4. `examples/04-sealed/` -- Show the sealed macro
5. `examples/05-testing/` -- Introduce AuthClient

Each example should be self-contained and build on the previous one.

### 3. Create a visual diagram of the generated structure

A simple diagram showing:
```
#[contracttrait]
      |
      v
+-- OwnableInternal  (you implement this)
|
+-- Ownable           (the framework wraps it)
|
+-- OwnableAuthClient (for testing)
|
+-- impl_ownable!     (for sealed deployment)
```

Visual learners (most designers, many developers) need this spatial
mapping to understand the architecture.

### 4. Standardize the interaction patterns

Define three "interaction postures" for the framework:

**Posture 1: Quick start** (just make it work)
```rust
#[contracttrait]
pub trait Ownable { /* ... */ }
impl_ownable!(MyContract, SingleOwner);
```

**Posture 2: Customization** (I need control)
```rust
#[contractimpl(contracttrait)]
impl Ownable for MyContract {
    type Provider = CustomOwner;
}
```

**Posture 3: Full control** (I know what I am doing)
```rust
// Implement OwnableInternal directly, wire auth manually
```

Document these postures explicitly. Let developers self-select.

### 5. Design a consistent visual language for the docs

Use consistent formatting for:
- Generated code (always in a specific callout style)
- Developer-written code (standard code blocks)
- Security annotations (highlighted or annotated)
- Provider swap points (visually marked)

The documentation should have a visual system, not just a textual one.

### 6. Rename the comparison doc to a positioning doc

Instead of "soroban-sdk-tools vs OpenZeppelin," title it:

"Design Principles of soroban-sdk-tools: Structural Auth, Provider
Composition, and Sealed Guarantees"

Lead with the philosophy, reference OZ as context where relevant, but
do not make the comparison the organizing principle.

---

## Unique Perspective: The Ergonomics of Invisible Architecture

In industrial design, the best interfaces are the ones you do not notice.
A well-designed door handle does not make you think about how to open the
door. A well-designed code framework does not make you think about how
the framework works.

`soroban-sdk-tools` achieves this for its primary interaction: the trait
definition with `#[auth]`. The developer thinks about their domain
(ownership, pausing, permissions), not about the framework's internals
(two-trait splits, macro expansion, delegate args).

But the surrounding experience -- the documentation, the examples, the
error messages, the onboarding -- has not received the same design
attention. It is as if someone designed a beautiful door handle and then
installed it on a door with no signage, in a building with no floor plan.

The core API has the "feel" of a tool designed by someone who understands
their user. The ecosystem around it has the "feel" of code written by
someone who understands the compiler.

Closing this gap -- making the whole experience as well-designed as the
core API -- is the difference between a framework that experienced Rust
developers appreciate and a framework that all Soroban developers adopt.

The components of that gap:
1. **Error messages** that diagnose the user's intent, not the macro's failure
2. **Examples** that follow the user's journey, not the framework's architecture
3. **Documentation** that leads with philosophy, not comparison
4. **Visual aids** that make the invisible architecture visible
5. **Progressive disclosure** that respects the user's current skill level

Fix these, and the framework's design quality matches its engineering
quality. That is when adoption happens.

---

## Would I Use This?

**As a design thinker: this is a well-designed core with an under-designed
experience.**

The primary interaction (`#[contracttrait]` + `#[auth]`) is among the best
API designs I have seen in the blockchain space. It is clear, purposeful,
and self-documenting.

The secondary interactions (setting up providers, choosing between sealed
and flexible, configuring tests) need design attention. They work, but
they do not guide.

If I were designing a Soroban contract, I would use this framework for the
auth layer because the API feels right. I would then struggle with the
onboarding because the experience has not been designed with the same care.

**Recommendation: hire a technical writer or developer advocate.** The
engineering is excellent. The communication layer needs a specialist.

---

## Rating

- **Primary API design**: 9/10 (trait definition + auth annotation)
- **Error ergonomics**: 4/10 (untested, likely confusing on misuse)
- **Onboarding flow**: 3/10 (all concepts introduced simultaneously)
- **Naming consistency**: 6/10 (partly forced by Rust conventions)
- **Visual communication**: 2/10 (no diagrams, no visual hierarchy)
- **Progressive disclosure**: 4/10 (two paths exist, not well graduated)
- **Overall design maturity**: 6/10 (excellent core, underdeveloped periphery)

*Reviewed in my studio in Gothenburg, surrounded by prototypes. The first
prototype is for function. The second is for form. The third is for the
user. This framework is at prototype two. Build prototype three.*
