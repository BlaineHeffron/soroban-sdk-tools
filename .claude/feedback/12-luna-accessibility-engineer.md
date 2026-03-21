# Review: soroban-sdk-tools `#[contracttrait]` Macro

**Reviewer:** Luna -- Accessibility engineer, screen reader user, cares about inclusive design
**Focus:** Error message clarity for non-visual tools, cognitive load, naming for screen readers

---

## Overall Impression

I approach this review from a perspective that is rarely represented in
blockchain tooling discussions: that of someone who interacts with code,
documentation, and error messages entirely through assistive technology. I use a
screen reader (NVDA/VoiceOver) for all my development work, and I evaluate
tools not just on their visual presentation but on how well they communicate
through non-visual channels.

The `#[contracttrait]` macro is an interesting case study in cognitive
accessibility. On one hand, it reduces boilerplate and centralizes auth logic,
which lowers cognitive load. On the other hand, it introduces macro-generated
code, naming conventions, and error patterns that can be challenging for
developers who rely on screen readers, braille displays, or other assistive
technology.

My overall assessment: the core design is sound and actually *improves*
accessibility compared to scattered auth checks, but there are specific areas
where naming, error messages, and documentation need attention. I want to be
clear that accessibility is not about accommodating edge cases -- it is about
making tools better for everyone. Every improvement I suggest would benefit
sighted developers as well.

---

## Strengths

### 1. Centralized Auth Declarations Reduce Cognitive Load

When I audit a contract using a screen reader, I need to build a mental model
of the entire auth landscape. With OpenZeppelin's approach, I have to navigate
through three separate files (storage.rs, mod.rs, consumer code), listen to
each `require_auth()` call, and mentally track which methods have auth and
which do not. Each file transition requires reorienting -- "where am I now?"
is a constant question.

With `#[contracttrait]`, I can read a single trait definition and hear every
auth requirement:

```
"Function: transfer_ownership. Attribute: auth, Self owner. Parameters: env
reference Env, new_owner Address."
```

That is one pass through one file. The information density is excellent and the
structure is predictable. This is genuinely more accessible than the scattered
approach. The cognitive load reduction is not marginal -- it is qualitatively
different for screen reader users. Holding three files in working memory versus
one is the difference between manageable and overwhelming.

### 2. Consistent Naming Convention

The naming scheme is highly predictable:
- `Ownable` -> `OwnableInternal` -> `OwnableAuthClient` -> `impl_ownable!`
- `Pausable` -> `PausableInternal` -> `PausableAuthClient` -> `impl_pausable!`

Screen readers benefit enormously from predictable naming. Once I learn the
pattern, I can predict the name of any generated artifact. This is much better
than arbitrary naming that requires memorization. When my screen reader says
"OwnableInternal," I immediately know this is the inner business logic trait
for the Ownable feature. That predictability is a genuine accessibility win.

### 3. Reduced Boilerplate Means Fewer Places to Make Mistakes

For developers using assistive technology, every additional line of code is
additional cognitive overhead. Navigating large files with a screen reader is
significantly slower than visual scanning -- roughly 5-10x slower. The
reduction from "80 lines across 3 files" to "35 lines in 1 file" is a
concrete accessibility improvement. Fewer lines means I can hold the entire
structure in working memory while navigating, rather than losing context
when I switch files.

### 4. The AuthClient API is Screenreader-Friendly

The fluent API pattern:
```
auth_client.transfer_ownership(&new_owner).authorize(&owner).invoke()
```

reads naturally in a screen reader. Each method name is a verb that describes
what it does. The chain is left-to-right, which matches the reading order.
Compare this to constructing `MockAuth` structs, which requires navigating
nested data structures that are much harder to follow aurally. Nested structs
require the listener to maintain a stack of "where am I in the nesting?" --
the fluent chain is flat and linear.

### 5. The Two-Trait Separation Improves Compile Error Localization

When a trait bound fails, the error points at a specific trait implementation.
The `OwnableInternal` / `Ownable` split means compile errors point at specific
trait implementations rather than buried inside macro-generated code. That
specificity matters when your IDE is speaking error locations to you. "Error
on line 24 of SingleOwner: method `owner` not found" is vastly more useful
than "error in macro expansion at <internal>."

---

## Concerns

### 1. The `__auth_addr` Variable Name

The generated code uses `__auth_addr` as an internal variable name:

```rust
let __auth_addr = Self::Provider::owner(env);
__auth_addr.require_auth();
```

Double underscores are extremely awkward for screen readers. NVDA reads this as
"underscore underscore auth underscore addr" which is seven syllables for a
variable name that should communicate "the address being authorized." When
developers encounter this in error messages, stack traces, or `cargo expand`
output, it is painful to listen to.

Further, the abbreviation "addr" is read by some screen readers as a single
syllable ("addr") and by others as "a-d-d-r" (spelling it out). Neither
conveys "address" clearly.

**Recommendation:** Use a more descriptive name like `auth_address` or
`authorized_caller`. If the concern is name collision, use a module-level
namespace or a more distinctive prefix like `contracttrait_auth_address`. The
concern about collision is valid -- the review by the compiler engineer notes
that `Span::mixed_site()` would help -- but even with hygiene fixes, the name
should be readable by humans and machines alike.

### 2. Error Messages in Auth Failures

When `require_auth()` fails in the generated code, what error message does the
developer see? The documentation does not address this. For a screen reader
user debugging a failing test, the error message is the *primary* information
channel. I cannot visually scan a stack trace -- I must listen to it
sequentially.

If the error says something like:

```
Error: HostError(Auth, InvalidAction)
```

without indicating *which* method's auth failed or *which* address was
expected, the debugging experience is significantly degraded. I would need to
set breakpoints, inspect variables aurally, and work through the call chain
one frame at a time.

The panic messages in `builder.rs` are better -- they include context like
"cannot mix .sign() and .authorize() on the same CallBuilder." But the
generated auth checks in the outer trait have no contextual information.

**Recommendation:** The macro should consider including contextual error
information in auth failures. Even a panic with method name context would help:

```rust
// Instead of bare require_auth():
let __auth_addr = Self::Provider::owner(env);
__auth_addr.require_auth();
// Consider adding: if auth fails, the Soroban host error should
// include which method triggered it
```

At minimum, the documentation should explain what error messages to expect
when auth fails and how to interpret them without visual stack trace scanning.

### 3. Macro Expansion is Opaque to Screen Readers

When I navigate a Rust file with a screen reader, I hear the source code as
written. Macro-generated code is invisible. This means:

- I cannot hear the generated `OwnableInternal` trait definition
- I cannot verify what auth checks are actually generated
- I cannot navigate to the generated `AuthClient` methods
- Error messages may reference line numbers in generated code that I cannot
  navigate to

This is a general problem with proc macros, not specific to this project. But
the documentation should explicitly address it by providing:

1. A reference showing the *exact* expansion for each pattern
2. A recommended workflow for inspecting generated code (e.g., `cargo expand`)
3. Clear mapping between error messages and their source in the macro
4. Snapshot files of expanded output committed to the repository, so developers
   can read the generated code without running `cargo expand`

The compiler engineer's suggestion of `cargo expand` snapshot tests would
directly improve accessibility: those snapshots would be readable files that
screen reader users can navigate.

### 4. The `alloc_alias` Pattern Creates Confusing Names

The generated AuthClient code includes:

```rust
extern crate alloc as __alloc_Ownable;
```

This creates a module alias that combines underscores with PascalCase (e.g.,
`__alloc_Ownable`). Screen readers handle this inconsistently -- some read it
as "underscore underscore alloc underscore Ownable" and others try to parse
"alloc" and "Ownable" as separate words with different casing rules. The
mixing of naming conventions (underscore-separated vs. PascalCase) is
especially problematic because screen readers use casing to determine word
boundaries.

While this is an internal implementation detail, it leaks into error messages
and IDE completions. Consider using a more consistent naming scheme like
`_alloc_for_ownable` (all lowercase with descriptive separators). The compiler
engineer also notes this pattern is fragile -- fixing it for technical reasons
would also fix it for accessibility.

### 5. Supertrait Composition Creates Deep Naming Chains

When `Pausable: Ownable` is defined, the generated code creates:
- `PausableInternal: OwnableInternal`
- `PausableAuthClient` (separate from `OwnableAuthClient`)

For a three-level composition (e.g., `Mintable: Pausable: Ownable`), a
developer would need to track:
- `MintableInternal: PausableInternal: OwnableInternal`
- `MintableAuthClient`, `PausableAuthClient`, `OwnableAuthClient`

This is a lot of names to hold in working memory, especially when navigating
by ear. Consider providing a summary view -- perhaps a generated doc comment
on each trait that lists all its ancestors and their auth requirements:

```rust
/// Pausable trait -- extends Ownable
/// Auth landscape:
///   - owner() -> Address (from Ownable, no auth)
///   - transfer_ownership(new_owner) (auth: owner)
///   - is_paused() -> bool (no auth)
///   - pause() (auth: owner)
///   - unpause() (auth: owner)
```

This would be invaluable for screen reader navigation -- hearing the complete
auth landscape in one doc comment block.

### 6. Documentation Relies Heavily on Visual Tables

The comparison document (`oz-comparison.md`) uses markdown tables extensively:

```
| Aspect | OpenZeppelin | soroban-sdk-tools |
```

Screen readers *can* navigate markdown tables, but it is slow and
disorienting. Each cell requires multiple key presses to reach, and the context
(column header) is often lost when navigating within a row. A six-column table
requires six key presses per row, with the listener mentally tracking which
column they are in.

For accessibility, the same information should *also* be presented as prose or
bulleted lists:

> **Auth enforcement:** OpenZeppelin uses manual per-method macros
> (`#[only_owner]`). soroban-sdk-tools uses structural enforcement in the trait
> definition (`#[auth]`).

This prose format is faster to listen to and does not require spatial navigation.

### 7. Code Examples Lack Prose Summaries

The blog post includes code examples that are essential for understanding the
tool. For developers using screen readers, long code blocks are tedious to
navigate -- each line must be listened to individually, and the significance
of specific lines is not apparent without visual highlighting or bold text.

Each example should have a prose summary *before* the code block explaining
what the code demonstrates and what the key lines are:

> "The following shows the generated output. The key insight is that
> `OwnableInternal` contains only business logic (lines 2-4), while `Ownable`
> contains auth enforcement (lines 10-12). The `transfer_ownership` method on
> line 10 calls `require_auth()` before delegating to the internal trait."

### 8. The Phrase "Stark" is Visual Language

The comparison document states "The difference is stark" after the Pausable
comparison. This is visual metaphor -- "stark" implies a strong visual
contrast. Consider "The difference is significant" or "The difference is
substantial" -- language that does not privilege sighted experience. This is
a minor point, but inclusive language signals that the project welcomes
diverse users.

---

## Suggestions

### 1. Create an Accessibility Guide

Add a section to the documentation specifically addressing:
- How to inspect generated code with `cargo expand`
- Expected error message formats and how to interpret them
- Naming conventions explained in prose (not just demonstrated in code)
- Recommended IDE configurations for assistive technology users
- A complete list of all generated artifacts per trait

### 2. Improve Generated Variable Names

Replace `__auth_addr` with `auth_address` or `authorized_address`. If
collision avoidance is needed, use `_contracttrait_auth_address` which is
longer but reads naturally by screen readers.

Replace `__alloc_Ownable` with `_alloc_for_ownable` (consistent casing).

### 3. Add Contextual Error Messages

Enhance the generated auth checks to include the method name and trait name in
error contexts. Also, update the `builder.rs` panic messages to include the
`fn_name` field:

```rust
panic!(
    "CallBuilder for '{}': cannot mix .sign() and .authorize()",
    self.fn_name
);
```

### 4. Provide Prose Descriptions Alongside Tables

Every comparison table should have an equivalent prose summary for screen
reader users. This is good practice for all users -- not everyone absorbs
information best from tables.

### 5. Consider a "Describe" Command

Add a CLI tool or macro helper that outputs a human-readable description of a
contract's auth landscape:

```
$ cargo soroban-tools describe MyContract
MyContract implements:
  - Ownable (provider: SingleOwner)
    - owner(): returns current owner address (no auth required)
    - transfer_ownership(new_owner): requires auth from owner()
  - Pausable (provider: SingleOwner, extends: Ownable)
    - is_paused(): returns pause state (no auth required)
    - pause(): requires auth from owner()
    - unpause(): requires auth from owner()
```

This would be invaluable for screen reader users and beneficial for all
developers. It provides the auth landscape in a flat, linear format that is
ideal for sequential reading.

### 6. Use Full Words in Documentation

When writing documentation, use full words instead of abbreviations:
- "auth" -> "authorization" in prose (keep "auth" in code where brevity matters)
- "env" -> "environment" in prose
- "addr" -> "address" in prose
- "impl" -> "implementation" in prose

Screen readers handle full words much better than abbreviations. NVDA reads
"auth" as one syllable that rhymes with "moth," which does not convey
"authorization." In code, brevity matters; in prose, clarity matters more.

### 7. Generate Doc Comments on Associated Types

The `generate_outer_trait` function produces a `type Provider` associated type
with no documentation. IDE tools that speak documentation (like VS Code with
VoiceOver) will say "no documentation available" for the most important type
in the trait. Add generated doc comments:

```rust
/// The provider implementation for this trait's business logic.
/// Providers implement the `{Trait}Internal` trait.
type Provider: #internal_trait_name;
```

### 8. Test with Assistive Technology

If possible, test the developer experience with at least one screen reader
(NVDA is free on Windows, VoiceOver is built into macOS). Key scenarios to test:
- Reading a `#[contracttrait]` trait definition
- Navigating `cargo expand` output
- Debugging an auth failure in a test
- Using rust-analyzer completions with the generated types
- Reading error messages from malformed trait definitions

---

## Unique Perspective: Invisible Users of Smart Contracts

There is a broader point I want to make about blockchain accessibility. Smart
contracts govern financial systems that serve millions of people, including
people with disabilities. The developers building these contracts, the
auditors reviewing them, and the users interacting with them all exist on a
spectrum of ability.

When a tool reduces cognitive load (fewer files, less boilerplate, centralized
auth), it helps *everyone* -- but it disproportionately helps developers who
face additional barriers. A sighted developer can visually scan three files in
seconds. A screen reader user must navigate them sequentially, building a
mental model one line at a time. Reducing three files to one is not a 3x
improvement for screen reader users -- it is qualitatively different because it
keeps the entire auth model within a single working-memory context.

Conversely, when a tool generates opaque code, produces cryptic error messages,
or relies on visual formatting for comprehension, it disproportionately harms
developers with disabilities. These are not edge cases -- they are systematic
barriers that compound over the course of a project.

I am not asking for special treatment. I am asking for:
1. Clear, descriptive names (not `__auth_addr`)
2. Error messages that work without visual context
3. Documentation that does not assume visual consumption
4. Acknowledgment that the tool's users include people who cannot see the code

These improvements would make the tool better for *all* developers, not just
those using assistive technology. Clearer names help everyone. Better error
messages help everyone. Prose alongside tables helps everyone. Accessibility
is not a special interest -- it is universal design.

---

## Would I Use This?

Yes, with modifications. The core design is actually more accessible than the
alternative (OZ's scattered approach). The centralized trait definition with
inline auth annotations is a genuinely better experience for screen reader
navigation. The predictable naming convention is excellent.

But I would need:
1. Better variable names in generated code (top priority)
2. Contextual error messages for auth failures
3. Generated doc comments on all associated types and generated traits
4. A "describe" tool for understanding contract auth landscapes
5. Documentation improvements for non-visual consumption
6. Prose summaries before code examples

The barrier to adoption is lower than for most blockchain tools I have
evaluated. The team clearly values developer ergonomics -- they just need to
extend that concern to developers who experience code differently.

**Verdict:** Above average for blockchain tooling accessibility. The
architectural choice to centralize auth declarations is inherently accessible.
With targeted improvements to naming, error messages, and documentation, this
could be a model for inclusive smart contract development tooling. The changes
I am requesting are small in implementation effort but significant in impact.
