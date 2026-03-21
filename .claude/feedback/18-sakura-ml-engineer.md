# Review: soroban-sdk-tools `#[contracttrait]` Macro

**Reviewer:** Sakura -- Machine learning engineer exploring AI agents that deploy contracts
**Focus:** Can AI agents understand/generate this code? Composability for automated systems

---

## Overall Impression

I build AI systems that interact with smart contracts -- agents that analyze,
generate, deploy, and manage on-chain code. My perspective is unusual: I
evaluate developer tools not by how well *humans* use them, but by how well
*machines* can understand and manipulate them.

The `#[contracttrait]` macro presents an interesting case for AI-driven
contract development. On one hand, its declarative, pattern-based approach is
highly amenable to machine generation. On the other hand, the proc macro
expansion is opaque to current AI systems, and the implicit code generation
creates challenges for automated analysis and verification.

My assessment: this tool is above average for AI interoperability, primarily
because of its declarative trait-based design, but there are specific areas
where machine-readability could be improved. The key insight is that the
patterns this tool uses -- declarative annotations, provider-based DI, sealed
defaults -- are exactly the patterns that AI agents handle well. The main
barrier is the invisibility of macro-generated artifacts.

---

## Strengths

### 1. Declarative Trait Definitions are AI-Friendly

The core pattern is highly declarative:

```rust
#[contracttrait]
pub trait Ownable {
    fn owner(env: &Env) -> Address;
    #[auth(Self::owner)]
    fn transfer_ownership(env: &Env, new_owner: Address);
}
```

This is excellent for AI systems because:
- The structure is regular and predictable (every trait looks the same)
- Auth requirements are explicit annotations, not buried in implementation
- The trait interface is a complete specification of the contract's capabilities
- The pattern has very few degrees of freedom (trait name, methods, auth sources)

An LLM can generate this pattern after seeing 2-3 examples. More importantly,
an LLM can *reason about* this pattern: "transfer_ownership requires auth from
the address returned by owner()" is directly readable from the annotation. This
makes the pattern suitable for both code generation and code analysis tasks.

Compare this to OZ's approach where an LLM generating code must remember to
call `enforce_owner_auth()` inside method bodies. An LLM generating
soroban-sdk-tools code simply does not generate auth code at all -- it writes
the annotation and the business logic. Fewer responsibilities in generation
means fewer failure modes.

### 2. Provider Pattern Enables Compositional Code Generation

AI agents are best at composing known patterns rather than generating novel
code. This is a fundamental property of current LLMs: they excel at
recombination and pattern matching, not at creative algorithm design.

The provider pattern is perfectly suited for this:

- "Create a contract with single-owner auth" maps to `type Provider = SingleOwner`
- "Switch to multisig" maps to `type Provider = MultisigOwner`
- "Add pausability" maps to adding `Pausable` supertrait and wiring the provider

Each of these is a single-token or single-line change. LLMs excel at targeted
substitutions. The fact that swapping the ownership model requires changing
exactly one identifier is ideal for agent-driven contract configuration.

An AI agent managing a fleet of contracts could adjust governance models
through simple provider swaps -- no code generation required, just
configuration. This is the dream scenario for autonomous contract management.

### 3. The Sealed Macro is Safe for AI-Generated Code

The sealed pattern (`impl_ownable!`) is particularly valuable for AI-generated
code because it eliminates the most common AI code generation failure: forgetting
to add security checks. An AI that generates a contract with `impl_ownable!`
gets correct auth enforcement regardless of whether it "understands" the
security implications. The framework compensates for the AI's potential
oversight.

This is a significant advantage over OZ's approach, where an AI must correctly
place `#[only_owner]` macros on every method that needs protection. If the AI
forgets one, the contract has a security hole. With the sealed pattern, either
all methods are protected or the macro does not compile.

### 4. AuthClient Enables Automated Testing

The AuthClient fluent API:
```rust
auth_client.transfer_ownership(&new_owner).authorize(&owner).invoke();
```

is machine-parseable and machine-generatable. An AI agent can construct test
cases by:
1. Identifying all methods with `#[auth]` annotations
2. Generating positive tests (correct authorizer)
3. Generating negative tests (wrong authorizer, `try_invoke()`)
4. Generating edge case tests (expired auth, multiple methods in sequence)

This systematic test generation is possible because the auth requirements are
declarative and machine-readable. With OZ's `mock_all_auths()`, there is no
structured information for an AI to generate meaningful auth tests.

### 5. The Blog Post is Training Data Gold

The blog post at `docs/blog-post-composable-contracts.md` is exceptionally
well-structured for LLM consumption:
- Clear before/after code comparisons (ideal for few-shot learning)
- Explicit "Key Differences" tables (machine-parseable summaries)
- Named patterns ("Provider Pattern", "Sealed Auth") (concepts LLMs can anchor)
- Concrete examples with full context (runnable code, not pseudocode)

If this post is published and indexed, LLMs will learn these patterns from
it. The quality of the examples in the post directly affects the quality of
AI-generated code that uses this library. This is an underappreciated aspect
of developer documentation: good docs are training data for the AI agents
that will generate code using your library.

---

## Concerns

### 1. Macro-Generated Types are Invisible to Static Analysis

When an AI agent generates code that uses `OwnableInternal`, `OwnableAuthClient`,
or `impl_ownable!`, these identifiers do not exist in any source file the agent
can read. They are generated by the `#[contracttrait]` macro during compilation.

This means:
- Code completion tools cannot suggest them until after a build
- `rust-analyzer` may not resolve them until after full expansion
- An LLM trained on the source code alone will not see the generated API surface
- An AI auditor cannot verify the generated code without running `cargo expand`

For AI code generation, invisible types are a significant barrier. The agent
must *know* that `OwnableInternal` exists without ever seeing its definition.
This requires either training data that includes the expanded output, or
explicit documentation that lists all generated artifacts.

**Suggestion**: Generate a companion `.generated.rs` file (or a doc comment
block in the macro output) that explicitly lists all generated items with
their signatures. Alternatively, provide a `soroban-sdk-tools-stubs` crate
that contains stub definitions for documentation and IDE purposes.

### 2. The `trait-test` Example Mixes Patterns

The example in `examples/trait-test/src/lib.rs` uses `#[contractimpl(contracttrait)]`
(the flexible path) on lines 77-85, while commenting about `impl_ownable!`
(the sealed path) on lines 87-88. An AI agent parsing this example will learn
the flexible pattern, not the recommended sealed pattern.

LLMs are heavily influenced by the first example they see. If the primary
example demonstrates the flexible path, AI-generated code will predominantly
use the flexible path -- which the documentation itself says is less secure.

**Suggestion**: Provide two separate examples:
- `examples/sealed-auth/` -- demonstrates `impl_ownable!` exclusively
- `examples/flexible-auth/` -- demonstrates `#[contractimpl(contracttrait)]`

Clear example separation prevents LLMs from conflating the two approaches.
The sealed example should come first in documentation and file ordering.

### 3. Error Recovery Paths are Underspecified for AI Agents

When an AI agent deploys a contract and a call fails, it needs to understand
the error programmatically. The current error surfaces are:

- Rust panics with string messages (not machine-parseable)
- Soroban `contracterror` codes (machine-parseable but not generated by default)
- `try_invoke()` returns nested Results (parseable but complex)

The `try_invoke` return type is:

```rust
Result<
    Result<T, <T as TryFromVal<Env, Val>>::Error>,
    Result<Error, InvokeError>
>
```

This nested Result is challenging for LLMs to destructure correctly. In my
testing, GPT-4 and Claude both struggle with nested Result types in generated
code -- they tend to unwrap at the wrong level or confuse the inner and outer
error types.

**Suggestion**: Provide a flattened error type or a helper method:

```rust
pub fn try_invoke_flat(self) -> Result<T, ContractCallError> {
    match self.try_invoke() {
        Ok(Ok(val)) => Ok(val),
        Ok(Err(e)) => Err(ContractCallError::Conversion(e)),
        Err(Ok(e)) => Err(ContractCallError::Contract(e)),
        Err(Err(e)) => Err(ContractCallError::Invoke(e)),
    }
}
```

### 4. No JSON/Schema Description of Trait Interfaces

AI agents that construct transactions need to know the method signatures,
parameter types, and auth requirements of a deployed contract. Currently,
this information is only available by parsing Rust source code or WASM XDR.

Neither format is optimal for AI agents:
- Rust source requires a Rust parser (syn or tree-sitter)
- WASM XDR requires XDR decoding and does not include auth annotations

**Suggestion**: Generate a machine-readable trait descriptor:

```json
{
  "trait": "Ownable",
  "methods": [
    {
      "name": "owner",
      "params": [],
      "returns": "Address",
      "auth": null
    },
    {
      "name": "transfer_ownership",
      "params": [{"name": "new_owner", "type": "Address"}],
      "returns": null,
      "auth": {"source": "Self::owner", "type": "provider_method"}
    }
  ]
}
```

This would allow AI agents to construct valid calls without parsing Rust,
and would enable automated verification of auth coverage.

### 5. The Blog Post Shows Non-Production Patterns

The blog post shows `assert!(!PausableStorage::get_paused(env)...)` as a pause
check pattern. An LLM will reproduce `assert!` for error handling instead of
`panic_with_error!` or typed errors. The post should show the production-grade
pattern everywhere, since LLMs will copy the first pattern they see.

Similarly, the `trait-test` example uses `.expect("not initialized")` for
storage reads. An AI agent trained on this example will use `.expect()` strings
throughout its generated code, which produces unhelpful error messages in
production. The example should use typed error codes as the canonical pattern.

### 6. No Formal Grammar for AI Parsing

The `#[auth]` annotation supports two forms: `Self::method` and `param_name`.
This is simple enough for an LLM to learn from examples, but a formal
grammar would enable more reliable AI parsing:

```
auth_annotation := "#[auth(" auth_source ")]"
auth_source := "Self::" IDENT | IDENT
```

A formal grammar could be used by AI agents for both generation (producing
valid annotations) and analysis (extracting auth requirements from existing
code).

### 7. Supertrait Composition is Implicit

When `Pausable: Ownable` is defined, the generated `PausableInternal` extends
`OwnableInternal`. This is implicit -- there is no explicit documentation of
the inheritance chain in the generated artifacts. An AI agent parsing a
provider implementation:

```rust
impl PausableInternal for SingleOwner { ... }
```

cannot determine from the source code alone that `SingleOwner` must also
implement `OwnableInternal`. It must trace back to the trait definition and
follow the supertrait chain.

**Suggestion**: Generate explicit doc comments on Internal traits listing
all required supertrait implementations.

---

## Suggestions

### 1. Generate Machine-Readable Trait Descriptors

Output a JSON or TOML file describing each trait's methods, parameters,
return types, and auth requirements. This enables AI agents to interact with
contracts without parsing Rust.

### 2. Create Separate Examples for Each Path

Provide distinct, clean examples for the sealed and flexible paths. Label the
sealed example as the primary/recommended path. Ensure the first example an
AI encounters demonstrates the safest pattern.

### 3. Flatten the try_invoke Return Type

Provide a `try_invoke_flat()` method or a `ContractCallError` enum that
simplifies error handling for both humans and AI agents.

### 4. Document All Generated Artifacts Explicitly

For each `#[contracttrait]` invocation, list every generated artifact:
- `{Trait}Internal` trait with method signatures
- `{Trait}` outer trait with `type Provider`
- `{Trait}AuthClient` struct with method signatures
- `impl_{trait_snake}!` macro with usage example

This explicit listing enables AI agents to reference generated types
without relying on `cargo expand`.

### 5. Use Production Patterns in All Examples

Replace `assert!` with `panic_with_error!`, replace `.expect()` with typed
error codes. LLMs learn from examples -- every example should demonstrate
the production-grade pattern.

### 6. Add an AI Agent Integration Guide

Create a document specifically for AI agent developers:
- How to generate valid `#[contracttrait]` definitions programmatically
- How to construct transactions for contracts using this framework
- How to parse auth requirements from trait definitions
- How to generate comprehensive test suites from trait definitions

### 7. Provide a Stub Crate for IDE/AI Support

Create a `soroban-sdk-tools-stubs` crate that provides type stubs for all
generated artifacts. This would enable:
- IDE completion before building
- LLM training on explicit type definitions
- AI agent code analysis without `cargo expand`

---

## Unique Perspective: The AI-First Development Future

The most important trend in software development is the increasing role of AI
agents in code generation, deployment, and management. Within 2-3 years, a
significant fraction of smart contracts will be generated, tested, and deployed
by AI agents with minimal human oversight.

This has profound implications for framework design:

1. **Declarative is better than imperative for AI.** The more a framework
   expresses "what" rather than "how," the more reliably an AI can use it.
   `#[auth(Self::owner)]` is more AI-friendly than `owner.require_auth()`
   because the annotation is a static declaration that can be analyzed without
   executing the code.

2. **Defaults matter more for AI than for humans.** A human developer reads
   documentation and makes informed choices. An AI agent learns from examples
   and follows the most common pattern. If the default is unsafe, most
   AI-generated code will be unsafe.

3. **Invisible code is unverifiable code.** AI auditors need to see all code
   to verify it. Macro-generated code that is invisible to source analysis is
   a blind spot for AI verification. The push for `cargo expand` snapshots and
   generated documentation serves both human and AI needs.

4. **Machine-readable metadata enables automation.** JSON trait descriptors,
   formal grammars, and explicit artifact listings enable AI agents to interact
   with the framework programmatically. This is not a luxury feature -- it is
   the interface through which most future users will interact with the
   framework.

soroban-sdk-tools is better positioned for this future than OZ because its
declarative approach and structural guarantees align with how AI agents work.
But it needs to invest in machine-readable interfaces and explicit artifact
documentation to fully realize this advantage.

---

## Would I Use This?

For building AI agents that deploy contracts: yes, once the machine-readable
interfaces exist.

The declarative trait pattern is ideal for AI code generation. The provider
pattern enables AI-driven governance management. The sealed pattern provides
safety guarantees that compensate for AI's tendency to forget security checks.
The AuthClient enables systematic test generation.

The main barriers are:
1. Invisible macro-generated types (need stubs or explicit documentation)
2. Mixed patterns in examples (need clear, separated examples)
3. Complex nested Result types (need flattened error handling)
4. No machine-readable trait descriptors (need JSON/TOML output)
5. Non-production patterns in examples (need production-grade examples everywhere)

For an AI agent that USES contracts built with this framework: it is already
usable today. The contract interfaces are standard Soroban, and the auth
requirements are structurally enforced regardless of who (or what) calls them.

**Verdict:** The most AI-friendly smart contract composition framework I have
evaluated for Soroban. The declarative design is fundamentally right for
machine consumption. With machine-readable interfaces and explicit artifact
documentation, this could be the foundation for autonomous smart contract
management systems. The investment in making generated artifacts visible
would benefit both AI agents and human developers equally.
