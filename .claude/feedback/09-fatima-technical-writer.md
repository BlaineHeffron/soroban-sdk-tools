---
persona: Fatima
age: 38
background: Technical writer at AWS (5 years) and Stripe (3 years), now freelance for blockchain projects
focus: Information architecture, progressive disclosure, API reference quality, example-driven docs
tone: Constructive, structured, always asks "who is the reader and what do they need right now"
---

# Review: soroban-sdk-tools -- Documentation Architecture Assessment

## Executive Summary

The project has two documentation artifacts: an OZ comparison document and a blog
post. Both are well-written technical analyses aimed at experienced Soroban
developers. What is missing is everything else: getting started guides, API
references, concept explanations, migration guides, troubleshooting, and
architecture decision records.

I am evaluating the documentation that exists and recommending the documentation
that should exist.

## Existing Documentation Assessment

### Blog Post: "Structural Auth Enforcement and Provider-Based Composition"

**Audience**: Experienced Soroban developers who are already using OZ's
`stellar-contracts` or building from scratch.

**Strengths**:
- Clear problem statement in the opening section
- Side-by-side code comparisons are effective
- The CGP connection table grounds the abstract concepts
- "Try It Today" section provides a clear call to action

**Weaknesses**:

1. **Buries the lede**: The most important concept -- the two-trait split -- does
   not appear until section 2, after 500 words of context-setting. A technical
   reader who already knows the problem space will skim or abandon the post
   before reaching the core innovation.

   **Fix**: Lead with the code. Show the 5-line input and the generated output
   in the first 100 words, then explain why it matters.

2. **Inconsistent code examples**: Some examples use `env: &Env` (reference),
   others use `env: Env` (owned). The blog post's sealed macro example shows
   `pub fn transfer_ownership(env: Env, ...)` with owned Env, while the trait
   definition uses `&Env`. This inconsistency will confuse readers who try to
   copy-paste.

3. **Missing error handling**: No example shows what happens when auth fails,
   when a provider panics, or when a trait bound is unsatisfied. Error scenarios
   are the most important documentation for developers debugging their code.

4. **Overloaded comparison**: The post simultaneously introduces the framework,
   compares it to OZ, proposes improvements to OZ, and invites collaboration.
   These are four different content types for four different audiences. They
   should be four different documents.

### OZ Comparison Document

**Audience**: Framework maintainers, potential adopters evaluating alternatives.

**Strengths**:
- Well-structured with clear section headers
- The comparison table format is effective
- "What OZ Does Better" section demonstrates intellectual honesty

**Weaknesses**:

1. **Not a comparison, but an argument**: The document is titled "comparison"
   but reads as advocacy. Every section concludes with "our approach is better
   because..." A genuine comparison would present both approaches neutrally and
   let the reader decide.

   **Fix**: Rename to "Architectural Differences" and present each difference
   as a tradeoff, not a winner.

2. **Missing context for OZ decisions**: The document explains WHY
   soroban-sdk-tools made its design choices but never explains why OZ made
   different choices. OZ's lack of DI for Ownable may be deliberate -- simpler
   code, fewer concepts, smaller attack surface. Without understanding the
   "why" behind OZ's choices, the comparison is one-sided.

3. **Code snippets are not runnable**: The OZ code snippets include comments
   like `// === packages/access/src/ownable/storage.rs ===` but are not
   complete, compilable examples. A reader cannot verify the comparison.

## Missing Documentation

### Tier 1: Must-Have (blocks adoption)

1. **Getting Started Guide** (30-minute tutorial)
   - Install soroban-sdk-tools
   - Write your first `#[contracttrait]`
   - Implement a provider
   - Wire it to a contract
   - Test with AuthClient
   - Deploy to testnet

2. **API Reference** (auto-generated from doc comments)
   - The macro's doc comments in `contract.rs` are excellent (`//!` module docs).
     These should be rendered as a dedicated API reference page, not buried in
     source code.

3. **Concept Guide: The Two-Trait Pattern**
   - What is generated and why
   - Internal trait vs Outer trait
   - When to use sealed vs flexible
   - Diagrams showing the generated code structure

### Tier 2: Should-Have (accelerates adoption)

4. **Migration Guide: From Vanilla Soroban**
   - "You have a contract with manual `require_auth()` calls. Here is how to
     migrate to `#[contracttrait]`."

5. **Migration Guide: From OZ stellar-contracts**
   - "You are using OZ's Ownable. Here is how to migrate to soroban-sdk-tools'
     Ownable with the same behavior."

6. **Troubleshooting Guide**
   - Common error messages and their solutions
   - "I overrode a trait method and lost auth" -- explanation + fix
   - "My provider does not satisfy the supertrait bound" -- explanation + fix
   - "The AuthClient cannot find my method" -- explanation + fix

7. **Architecture Decision Records (ADRs)**
   - Why two traits instead of one?
   - Why `type Provider` instead of generic parameters?
   - Why sealed macros instead of just documentation?
   - Why `extern crate alloc` aliasing?

### Tier 3: Nice-to-Have (differentiates the project)

8. **Cookbook** (recipe-style solutions)
   - "Add ownership to any contract"
   - "Make a pausable token"
   - "Switch from single-owner to multisig"
   - "Test that unauthorized users are rejected"

9. **Security Model Document**
   - Formal description of what the framework guarantees and what it does not
   - Trust boundaries for providers
   - Known limitations and edge cases

10. **Interactive Playground**
    - Web-based tool where users write a `#[contracttrait]` and see the
      generated code in real time

## Doc Comment Quality in Source Code

The proc macro source (`contract.rs`) has high-quality module-level
documentation. However, individual functions lack doc comments:

| Function | Has doc comment? | Needs doc comment? |
|----------|------------------|--------------------|
| `extract_auth_attr` | Yes (brief) | Yes -- good |
| `strip_auth_attrs` | Yes (brief) | Yes -- good |
| `extract_method_info` | Yes (brief) | Yes -- good |
| `generate_internal_trait` | Yes (brief) | Needs expansion |
| `generate_outer_trait` | Yes (brief) | Needs expansion |
| `build_delegate_args` | Yes (brief) | Yes -- good |
| `generate_sealed_impl_macro` | Yes (detailed) | Yes -- excellent |
| `to_snake_case` | No | Yes -- should note limitations |
| `generate_auth_client` | Yes (brief) | Needs expansion |
| `generate_auth_client_method` | Yes (brief) | Needs expansion |
| `extract_return_types` | Yes (brief) | Yes -- good |
| `unpack_result_type` | Yes (brief) | Yes -- good |

The security model documentation at the top of the module is excellent. It
clearly distinguishes between "structurally enforced" and "convention-based"
guarantees. This should be promoted to a standalone document.

## The Example File Problem

The `examples/trait-test/src/lib.rs` file serves as both an example and a test
suite. This dual purpose creates conflicts:

- As an example, it should be minimal and progressive (show the simplest case
  first, add complexity gradually).
- As a test suite, it should be comprehensive (cover edge cases, error paths,
  composition scenarios).

Currently, it leans toward test suite. The `init()` function appears without
explanation. The test methods use `mock_all_auths()` for `init` but AuthClient
for trait methods, without explaining why.

**Recommendation**: Split into two files:
1. `examples/simple-ownable/src/lib.rs` -- minimal, 30 lines, one trait, one
   test. This is the "Getting Started" example.
2. `examples/composed-traits/src/lib.rs` -- comprehensive, shows supertrait
   composition, multiple providers, AuthClient for all traits.

## Verdict

The project has strong technical documentation for the "how it works" level but
is missing documentation at the "how do I use it" and "what should I do when
things go wrong" levels. The blog post and OZ comparison should be refactored
into a documentation suite with clear audience segmentation.

**Rating: 5/10 for documentation completeness** -- excellent depth on
architecture, missing breadth on adoption.
