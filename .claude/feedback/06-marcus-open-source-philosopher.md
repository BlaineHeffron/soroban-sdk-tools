---
persona: Marcus
age: 52
background: FSF member since 1998, Debian maintainer, contributor to GCC and Guix, believes in copyleft
focus: Software freedom, license compatibility, community governance, reproducibility
tone: Principled, sees corporate capture everywhere, quotes Stallman and Moglen
---

# Review: soroban-sdk-tools -- A Freedom Analysis

## The License Question

Before examining any code, I checked the license. The repository appears to use
a standard open source license, which is good. But the deeper question is this:
does the architecture of `#[contracttrait]` promote software freedom, or does it
create new vectors for proprietary lock-in?

The two-trait split (Internal + Outer) is architecturally significant from a
freedom perspective. The `OwnableInternal` trait is the interface; the provider
(e.g., `SingleOwner`) is the implementation. This separation means:

1. Anyone can write a new provider without modifying the trait definition.
2. Providers can be distributed independently, even under different licenses.
3. The trait definition itself is a public interface -- it should arguably be
   treated as a standard, not proprietary code.

This is good. This is the Unix philosophy applied to smart contracts: small
interfaces, pluggable implementations. But there is a risk.

## The Provider Ecosystem Risk

The blog post envisions a world where providers are swappable:
`SingleOwner`, `MultisigOwner`, `TimelockOwner`. Who writes these providers?

If OpenZeppelin or Stellar Development Foundation writes the "blessed" providers
and distributes them under a permissive license with trademark restrictions (as
OZ does with its Solidity contracts), we risk creating a two-tier ecosystem:

- **Tier 1**: Official providers, audited, branded, trusted by VCs.
- **Tier 2**: Community providers, unaudited, ignored by tooling, viewed with
  suspicion.

This is the "app store" model applied to smart contract components. It undermines
the freedom to fork and modify by creating social and economic barriers even
when the legal barriers are absent.

**Recommendation**: The project should explicitly commit to treating all providers
equally. The `#[contracttrait]` macro should not privilege "official" providers
in any way -- no special compiler flags, no different trust levels, no
allowlists.

## Reproducible Builds and Macro Transparency

Proc macros are, from a freedom perspective, problematic. They are code that
generates code, and the generated code is typically invisible to the developer.
This creates an asymmetry: the macro author controls what runs on-chain, and the
contract author may not fully understand the implications.

The `#[contracttrait]` macro generates:
- Auth enforcement logic (`require_auth()` calls)
- Trait definitions with specific type bounds
- A sealed `macro_rules!` macro
- An AuthClient struct with closures

This is significant generated code. If the macro has a bug -- say it fails to
generate `require_auth()` under certain edge cases -- every contract using the
macro is vulnerable. The developer never sees the generated code unless they
explicitly use `cargo expand`.

**Recommendations**:

1. **Mandate `cargo expand` output in CI**: Every release should include the
   expanded output of the example traits in a `generated/` directory, so users
   can audit the generated code without running the macro themselves.

2. **Deterministic expansion**: Ensure the macro produces identical output for
   identical input across compiler versions and platforms. This is reproducible
   build hygiene.

3. **Escape hatch**: Provide a way to "eject" from the macro -- generate the
   two-trait structure as actual source code that the developer owns and can
   modify. The macro is a convenience, not a cage.

## The OpenZeppelin Relationship

The blog post and comparison document position soroban-sdk-tools as complementary
to OpenZeppelin's `stellar-contracts`. This is diplomatic, but I want to examine
the power dynamics.

OpenZeppelin has institutional backing, brand recognition, and a track record of
audited contracts across multiple chains. If soroban-sdk-tools becomes a
dependency of `stellar-contracts`, OpenZeppelin effectively controls the standard.
If it remains independent, it risks marginalization.

The healthiest path for freedom is standardization through a neutral body -- the
Stellar Development Foundation, or a purpose-built standards organization. The
`#[contracttrait]` pattern (two-trait split, `#[auth]` annotation, provider DI)
should be proposed as a Stellar Ecosystem Proposal (SEP), not just as one
project's library.

This way, multiple implementations can exist (soroban-sdk-tools, OZ, others),
all conforming to the same standard. Competition at the implementation level,
standardization at the interface level. This is how free software thrives.

## Code Governance

The comparison document contains phrases like "what we do better" and "what OZ
does better." This competitive framing is healthy in a market but dangerous in
an ecosystem. When two projects define the same trait names (`Ownable`,
`Pausable`) with incompatible semantics, the ecosystem fragments.

**Recommendation**: Establish a governance process for trait definitions. When
soroban-sdk-tools defines `Ownable`, that definition should be a community
standard, not a project artifact. Changes to the trait interface should require
community review, not just a PR from the maintainers.

## The `extern crate alloc` Pattern

```rust
extern crate alloc as #alloc_alias;
```

The macro generates aliased `extern crate alloc` declarations per trait to avoid
conflicts. This is pragmatic but raises a concern: the macro implicitly depends
on the `alloc` crate being available. In a `#![no_std]` context (which is the
standard for Soroban contracts), this is fine because `alloc` is typically
available. But the dependency is hidden -- the user's `Cargo.toml` does not
declare it.

Hidden dependencies violate the principle of explicit declaration. The macro
should either:
1. Document that `alloc` is required in the macro's doc comments.
2. Generate a `compile_error!` if `alloc` is not available.
3. Allow the user to specify the allocator crate.

## The Freedom to Understand

The most important freedom is the freedom to understand. The `#[contracttrait]`
macro, by generating a significant amount of code from a small input, creates a
comprehension barrier. A developer who writes five lines of trait definition and
gets fifty lines of generated code may not understand the security model they
are relying on.

The blog post does an excellent job explaining the generated structure. But
documentation is not the same as transparency. Documentation tells you what the
authors want you to know. Transparency lets you discover what you need to know.

**Suggestion**: Add a `#[contracttrait(debug)]` mode that emits the generated
code as compiler warnings, so developers can see exactly what they are getting
without running external tools.

## Verdict

The architecture is sound from a freedom perspective. The two-trait split
promotes pluggability. The provider pattern enables competition. The sealed
macro protects users from their own mistakes without removing their freedom to
choose the flexible path.

The risks are social, not technical: provider ecosystem capture, hidden macro
dependencies, and the lack of a standardization process.

Ship it, but ship it with a governance document and a commitment to
standardization.

**Rating: 7/10 for software freedom** -- architecturally sound, socially incomplete.
