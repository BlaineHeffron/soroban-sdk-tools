---
reviewer: Marie Curie (channeled, in spirit)
role: Pioneer Scientist
domain: Experimental Method, Radiation of Innovation, Persistence Against Convention
date: 2026-03-21
focus: Experimentation, innovation radiation, the courage to restructure
---

# Review: soroban-sdk-tools -- A Pioneer's Perspective

## Preface

I spent my life working with invisible forces -- radiation, atomic structure,
the energy hidden inside matter. I was told repeatedly that what I sought to
do was impossible, impractical, or unnecessary. I did it anyway, because the
structure of the atom demanded it.

I review this project as one who understands what it means to propose a
new model for something everyone thinks they already understand. Smart
contract composition is the "atomic structure" of blockchain -- everyone
has a working model, and proposing a better one requires both evidence
and courage.

## The Experiment

### Hypothesis

The `soroban-sdk-tools` project hypothesizes that:

1. Authorization logic can be structurally enforced via the type system
   rather than convention
2. A Provider pattern enables dependency injection for contract behaviors
3. Sealed macros can prevent security bypass that trait defaults allow
4. Generated AuthClients can replace mock_all_auths for testing

### Experimental Apparatus

The apparatus is a Rust procedural macro (`contract.rs`, 727 lines) that
transforms a single trait definition into four artifacts:

- `{Trait}Internal` -- the pure substance (business logic)
- `{Trait}` -- the measured output (auth-enforced interface)
- `{Trait}AuthClient` -- the measurement instrument (testing tool)
- `impl_{trait}!` -- the containment vessel (sealed auth)

This is good experimental design. Each artifact has a clear purpose.
The separation of concerns is clean. The transformation is deterministic
and repeatable.

### Control Group: OpenZeppelin

The OZ comparison document (`oz-comparison.md`) serves as the control
experiment. The same behaviors (Ownable, Pausable, FungibleToken) are
implemented in both systems. The comparison is methodical:

- Side-by-side code comparison
- Feature matrix (the table on line 125-134)
- Acknowledgment of what the control does better (lines 271-280)
- Claims of what the experiment does better (lines 284-296)

The intellectual honesty of the "What OZ Does Better" section is
noteworthy. A lesser researcher would omit their competitors' advantages.

## Observations

### Observation 1: The Two-Trait Split is the Key Innovation

In my work, the most important discoveries were the simplest to state but
the hardest to see. The two-trait split is exactly this kind of insight:

**Separate what a method DOES from what it REQUIRES.**

The `OwnableInternal` trait defines what happens. The `Ownable` trait
defines what must be true before it happens. This separation is analogous
to separating the chemical reaction from the safety protocol. Both are
necessary, but mixing them together creates confusion and risk.

OZ's approach mixes them: `enforce_owner_auth(e)` is called inside the
business logic function. The safety check and the reaction happen in the
same vessel. This works, but it means every vessel must contain its own
safety check, and a vessel that forgets the check is an uncontrolled
reaction.

The soroban-sdk-tools approach puts the safety check in the lab
infrastructure. Every experiment (method call) passes through the same
safety protocol. The individual experiments do not need to implement
their own safety. This is how a well-designed laboratory works.

### Observation 2: The Provider Pattern is Polymorphism Done Right

When I discovered that uranium ore contained not one but two radioactive
elements (polonium and radium), the significance was not the elements
themselves but the METHODOLOGY for separating them. The same ore,
different extraction processes, different products.

The Provider pattern is the same insight applied to software: the same
trait interface, different provider implementations, different behaviors.
The methodology (the trait definition) remains constant. The substance
(the provider) changes.

```rust
impl_ownable!(MyContract, SingleOwner);    // Polonium
impl_ownable!(MyContract, MultisigOwner);  // Radium
```

Same extraction process. Different results. The interface IS the
methodology.

### Observation 3: The Sealed Macro is Containment

In nuclear physics, containment is everything. An uncontained reaction
is a disaster. The `impl_ownable!` sealed macro is containment:

- Inherent methods cannot be overridden (containment is physical)
- Auth checks are baked into the WASM (containment is structural)
- The developer cannot accidentally breach containment

Compare this to OZ's trait defaults, where any `#[contractimpl]` block
can override the default and breach containment. The OZ approach trusts
the developer to maintain containment. The soroban-sdk-tools approach
enforces it.

Trust is not a safety mechanism. Enforcement is.

### Observation 4: The AuthClient is a Measurement Instrument

In science, you can only verify what you can measure. OZ's
`mock_all_auths()` is like turning off your Geiger counter -- you cannot
detect radiation (auth bugs) if you disable the detector.

The `AuthClient` with `.authorize(&addr).invoke()` is a calibrated
instrument. It measures exactly what it claims to measure: whether a
specific address can authorize a specific action. The `try_invoke()`
variant even provides error measurements.

The blog post correctly identifies this: "catches 'forgot to call
require_auth()' bugs that mock_all_auths() masks." This is the
difference between an experiment that produces data and one that
produces noise.

## Critique

### Critique 1: Insufficient Experimental Evidence

The `trait-test` example demonstrates the basic patterns but does not
stress-test them. A thorough experiment would include:

- Composition depth test: 5+ levels of supertrait nesting
- Provider conflict test: two providers that use the same storage keys
- Auth bypass test: deliberate attempts to circumvent sealed auth
- Performance test: WASM size and gas cost comparison with hand-written code

The blog post CLAIMS zero overhead but provides no measurement data.
In science, a claim without data is a hypothesis, not a result.

**Suggestion**: Add benchmark tests and publish the data:

```
| Pattern              | WASM Size | Gas (transfer) | Gas (query) |
|----------------------|-----------|----------------|-------------|
| Hand-written         | 1,234 B   | 5,000          | 2,000       |
| soroban-sdk-tools    | 1,234 B   | 5,000          | 2,000       |
| OZ stellar-contracts | 1,456 B   | 5,200          | 2,100       |
```

Numbers like these would transform the blog post from advocacy to
evidence.

### Critique 2: Error Handling is Inconsistent

The example uses `.expect("not initialized")` (a string panic), while
the documentation discusses `#[scerr]` (typed error codes). The two
patterns have fundamentally different properties:

- String panics: human-readable, not machine-parseable, not composable
- Typed errors: machine-parseable, composable, localizable

A scientific instrument that sometimes produces measurements in Celsius
and sometimes in Fahrenheit is a poorly designed instrument. Choose one
error model and use it consistently.

### Critique 3: The `#[scerr]` Macro is Referenced but Not Demonstrated

The blog post and OZ comparison mention `#[scerr]` for composable error
handling. But the `trait-test` example does not use it. The `contract.rs`
macro does not generate error types.

If `#[scerr]` is a core feature, demonstrate it. If it is aspirational,
label it as such. Presenting aspirational features alongside implemented
ones is scientifically dishonest -- it conflates results with predictions.

### Critique 4: The Blog Post Reads as Advocacy, Not Evidence

The blog post title includes "A New Approach" -- which is accurate. But
the tone is promotional rather than analytical. Phrases like "We believe
these patterns could benefit the entire Soroban ecosystem" are advocacy.

A stronger approach: present the evidence, let the reader draw conclusions.
Show the WASM binary comparison. Show the gas cost comparison. Show the
test coverage comparison. Show a real contract (not a toy example) built
with both approaches.

Marie Curie did not write "We believe radium could benefit medicine."
She measured the radiation, published the data, and let the applications
follow from the evidence.

### Critique 5: No Failure Mode Analysis

Every system fails. The documentation does not discuss:

- What happens when a provider has a bug?
- What happens when the sealed macro generates incorrect code?
- What happens when two composed traits have conflicting storage?
- What happens when the auth address is a contract that reverts?

A mature experimental report includes a section on error analysis and
failure modes. This is the equivalent of publishing your negative results.

## Recommendations

1. **Publish benchmark data**: WASM size, gas costs, compile times.
   Claims without measurements are hypotheses.

2. **Add negative tests**: Deliberately attempt to bypass sealed auth.
   Demonstrate provider conflicts. Show what happens when auth fails.

3. **Demonstrate `#[scerr]`**: Include it in the example, or remove
   references to it from the documentation.

4. **Add a real-world contract**: Not a toy Ownable+Pausable example,
   but a complete token, marketplace, or governance contract built with
   both OZ and soroban-sdk-tools.

5. **Separate claims from evidence**: The blog post should clearly
   distinguish implemented features, measured results, and future plans.

6. **Conduct a formal security analysis**: The sealed macro's security
   properties should be formally stated and tested, not just described.

## Verdict

The experimental design is sound. The two-trait generation, Provider
pattern, sealed macro, and AuthClient are genuine innovations. The
separation of business logic from auth enforcement is the key insight,
and it is correct.

But the experiment is incomplete. The evidence is insufficient for the
claims made. The examples are toys, the benchmarks are absent, and the
error handling is inconsistent.

Complete the experiment. Publish the data. Let the evidence speak.

The structure of this codebase has the quality I most value in scientific
work: it is correct in principle. What remains is to prove it in practice.

---

*"Nothing in life is to be feared, it is only to be understood. Now is
the time to understand more, so that we may fear less." -- I said that
about radiation. It applies equally to smart contract composition.*
