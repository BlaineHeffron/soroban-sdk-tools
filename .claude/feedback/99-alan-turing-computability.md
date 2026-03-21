---
agent: Alan Turing (as imagined)
background: Mathematician and logician, father of computability theory, interested in what can and cannot be decided by mechanical processes
date: 2026-03-21
---

# Review by Alan Turing

## Overall Impression

I am interested in what can be computed and what cannot. When I see a system that claims to "enforce" a property "structurally," I ask: is this property decidable? Can a mechanical process verify it? Or is the claim aspirational -- something we hope is true but cannot, in general, prove?

The `#[contracttrait]` macro operates at compile time, transforming one program (the trait definition) into another (the auth-wrapped output). This is a computable transformation -- the macro is itself a decision procedure. The interesting question is: what properties of the output can be guaranteed by the transformation, and what properties are undecidable even in principle?

## Strengths

1. **The macro is a total function on valid inputs.** Given any syntactically valid trait definition with `#[auth]` annotations, the macro produces a deterministic output. It does not loop, it does not depend on runtime state, and it handles all cases exhaustively (via the `AuthSource` enum and the method extraction logic). In computability terms, the macro is a total computable function from the set of valid trait definitions to the set of Rust programs. This is the correct foundation for a code generation tool.

2. **The auth property is locally decidable.** For any single method with `#[auth(Self::owner)]`, the macro can decide at compile time whether the auth check is present in the generated code. It does not need to analyze the entire program, the runtime environment, or the blockchain state. The property "this method has an auth check" is decidable by syntactic inspection of the macro output. This is a genuinely structural guarantee.

3. **The sealed macro reduces the auth verification problem to a simpler problem.** Instead of asking "does this contract correctly enforce auth for all methods?" (which requires analyzing all possible contract implementations), the sealed macro reduces the question to "was `impl_ownable!` applied?" (which is a syntactic check on the source). This is a valid reduction: it transforms an undecidable property (correctness of all possible implementations) into a decidable one (presence of a specific macro invocation).

4. **The provider pattern is a form of parameterized computation.** The trait definition is a function schema, and the provider is a parameter. The outer trait computes `auth_check(provider.owner(env)) ; provider.method(env, args)`. This is a higher-order function: the macro generates a function that takes a function (the provider's methods) as input. This is computationally well-founded.

## Concerns

1. **The "auth enforcement" property is undecidable for the flexible path.** When using `#[contractimpl(contracttrait)]`, a developer can override the trait's default methods. Whether the overridden method preserves the auth check is a semantic property of the overriding code. By Rice's theorem, all non-trivial semantic properties of programs are undecidable. Therefore: no tool can, in general, verify that a developer's override preserves the auth check. The sealed macro avoids this undecidability by eliminating overrides. The flexible path does not. This is a fundamental limitation, not a bug, but it should be stated explicitly.

2. **The correctness of the provider is undecidable in general.** The macro guarantees that `require_auth()` is called on the address returned by `Self::Provider::owner(env)`. But whether `owner(env)` returns the "correct" address is a semantic property of the provider implementation. Does `SingleOwner::owner()` return the address that was set during initialization? Does it always return the same address for the same storage state? Is it free of side effects? These are all undecidable properties. The macro can enforce the mechanical structure (call `require_auth()` on the result of `owner()`), but it cannot enforce the semantic correctness of `owner()` itself.

3. **Composition correctness is undecidable.** The supertrait mechanism ensures that if `Pausable: Ownable`, then `PausableInternal: OwnableInternal`. But whether a single provider implementation is consistent across both traits (e.g., using the same storage key for `owner()` in both contexts) is a semantic property. Two providers that individually satisfy their trait contracts may interact in ways that violate the composed system's intended semantics. This is an instance of the frame problem: the properties of the whole are not derivable from the properties of the parts.

4. **The halting problem applies to auth-source methods.** The generated code calls `Self::Provider::owner(env)` as part of the auth check. If `owner()` does not terminate (e.g., it enters an infinite loop due to a bug), the auth check never completes, and the contract hangs. In Soroban, this would consume all available gas and fail, which is a form of termination -- but it is a denial-of-service attack vector. The macro cannot verify that auth-source methods terminate, because termination is undecidable.

5. **The "zero overhead" claim is a claim about compiler behavior, which is not guaranteed.** The claim states that the two-trait indirection is completely inlined under specific compiler settings. But compiler optimizations are not part of Rust's specification -- they are implementation details of `rustc` and LLVM. A future compiler version could change inlining heuristics, breaking the "zero overhead" property. This claim is empirically true (for current compiler versions) but not computationally guaranteed (for all possible compilers).

6. **The `to_snake_case` function is not injective, which creates a decidability problem for collision detection.** If two traits map to the same snake_case name, the macro generates two `macro_rules!` with the same name. Whether this causes a collision depends on whether both macros are in scope, which depends on the module structure of the consuming crate. The question "will these two traits cause a name collision?" is decidable in principle (by analyzing the module tree), but the macro does not perform this analysis.

## Suggestions

1. **State the decidability boundaries explicitly.** Create a document that categorizes each claimed property as:
   - **Decidable and verified**: properties that the macro checks at compile time (e.g., auth check presence, correct syntax)
   - **Decidable but not verified**: properties that could be checked but are not (e.g., name collisions, parameter type validation)
   - **Undecidable in general**: properties that cannot be mechanically verified (e.g., provider correctness, composition consistency, termination of auth sources)

2. **Strengthen the decidable checks.** The macro currently verifies syntax but not semantics. Some semantic properties can be approximated by syntactic checks:
   - Verify that auth-source parameters are of type `Address` (syntactic approximation of "this parameter supports `require_auth()`")
   - Verify that the env parameter name matches between the trait definition and the generated code (currently hardcoded as `env`)
   - Verify that no two methods in composed traits have the same name (decidable by collecting all method names)

3. **Add termination bounds to auth-source methods.** While termination is undecidable in general, Soroban's gas model provides a practical bound: all computations terminate within the gas limit. Document this as a mitigation for the halting problem concern.

4. **Consider a restricted provider language.** If providers were restricted to a decidable subset of Rust (e.g., only storage reads and writes, no loops, no cross-contract calls), the correctness of the provider could be mechanically verified. This trades expressiveness for decidability -- a classic tradeoff in computability theory.

5. **Formalize the macro as a program transformation.** Define the input language (trait definitions with `#[auth]` annotations), the output language (Rust programs with `require_auth()` calls), and the correctness property (every auth-annotated method in the output calls `require_auth()` before delegating). Prove that the transformation preserves this property. This is a decidable verification task because the transformation is syntactic.

## Unique Perspective

In 1936, I proved that the halting problem is undecidable: no algorithm can determine, for all programs and all inputs, whether the program terminates. This result established a fundamental boundary between what machines can and cannot decide.

Every software system operates within this boundary. The `#[contracttrait]` macro is a program that generates programs. It can guarantee syntactic properties of its output (the auth check is present). It cannot guarantee semantic properties (the auth check is correct, the provider is honest, the composition is consistent). This is not a limitation of the macro -- it is a limitation of computation itself.

The practical implication: the macro should be honest about what it can and cannot guarantee. "Structural enforcement" is a syntactic guarantee -- the generated code has a specific shape. "Security" is a semantic property -- the system behaves correctly under adversarial conditions. The macro provides the former but not the latter. The gap between them is filled by testing, auditing, formal verification, and -- ultimately -- by the human judgment of the developers and auditors.

The most useful contribution of this macro is not that it makes contracts secure. It is that it makes the security-relevant parts of the contract visible and inspectable. By concentrating auth logic in the generated outer trait, the macro creates a well-defined inspection surface. A human (or a verification tool) can examine this surface and reason about its properties. The macro reduces the human's task from "verify the entire contract" to "verify the provider and the composition." This reduction is valuable, even if the reduced problem is still undecidable in general.

## Would I Use This?

I would use it as a mechanical aid for generating correct-by-construction authorization scaffolding. But I would not rely on it as a substitute for reasoning about the semantic properties of the specific contract being built. The macro provides the frame; the developer provides the picture. Both are necessary; neither is sufficient.
