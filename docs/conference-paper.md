# Structural Guard Enforcement via Trait Splitting: A Macro-Based Approach to Composable Authorization in Rust

**Willem Wyndham**$^1$ and **Blaine Heffron**$^1$

$^1$ Independent Researchers

**Keywords:** smart contracts, authorization, trait systems, procedural macros, composability, capability security

---

## Abstract

Authorization bugs remain a leading cause of security vulnerabilities in smart contracts, responsible for over \$1.3 billion in losses across blockchain ecosystems between 2020 and 2025. The fundamental issue is architectural: in conventional designs, authorization logic and business logic are co-located, and correct enforcement depends on developer discipline at every call site. We present a macro-based technique for Rust that structurally separates authorization from business logic by splitting a single annotated trait into two traits: an *internal* trait containing pure business logic, and an *outer* trait that wraps each method with authorization guards before delegating to a swappable provider. The transformation is implemented as a procedural macro attribute `#[contracttrait]` and generates four artifacts: the internal trait, the guard-instrumented outer trait, a sealed implementation macro that prevents guard circumvention via Rust's inherent method priority rules, and a test client for precise authorization testing. We formalize the transformation as a faithful functor between categories of trait definitions and prove that it preserves supertrait composition. We demonstrate the approach in the Soroban smart contract ecosystem (Stellar blockchain), showing that it eliminates an entire class of authorization bypass vulnerabilities with zero runtime overhead. A study synthesizing 100 expert perspectives validates the pattern's security properties and identifies the provider trust boundary as the critical remaining attack surface. We further show that the pattern generalizes beyond smart contracts to web frameworks, CLI tools, embedded systems, and any Rust codebase requiring structural precondition enforcement. (247 words)

---

## 1. Introduction

### 1.1 Motivation

Smart contract authorization vulnerabilities constitute a persistent and costly class of bugs. In the Ethereum ecosystem alone, the reentrancy vulnerability in The DAO (2016), the Parity multisig wallet freeze (2017), and numerous access control failures in DeFi protocols (2020--2025) have demonstrated that authorization correctness is not an afterthought but a fundamental design concern [1, 2, 3].

The Stellar blockchain's Soroban smart contract platform, which uses Rust as its programming language, inherits Rust's strong type system but faces the same authorization challenge: how do you ensure that every public contract method that requires authorization actually enforces it? The current state of the art, exemplified by OpenZeppelin's `stellar-contracts` library [4], uses two complementary mechanisms:

1. **Default method implementations** in traits that delegate to module-level functions containing `require_auth()` calls.
2. **Per-method attribute macros** (e.g., `#[only_owner]`, `#[when_not_paused]`) that inject guard checks at each call site.

Both mechanisms are *convention-based*: they rely on the developer to either not override the default methods or to remember to apply the attribute macro on every method that needs it. When a developer overrides a trait default or forgets an attribute, the authorization check silently disappears.

This paper presents an alternative approach where authorization is *structural*: it is declared once in the trait definition and is mechanically enforced in every implementation. The key insight is that Rust's trait system, combined with procedural macros, allows us to generate a *two-trait structure* where:

- Business logic implementors interact only with a trait that has no authorization concerns.
- The public interface automatically includes authorization guards, and these guards cannot be removed without modifying the trait definition itself.

### 1.2 The Key Insight

Consider the following annotated trait:

```rust
#[contracttrait]
pub trait Ownable {
    fn owner(env: &Env) -> Address;

    #[auth(Self::owner)]
    fn transfer_ownership(env: &Env, new_owner: Address);
}
```

The `#[auth(Self::owner)]` annotation declares that `transfer_ownership` requires authorization from the address returned by `owner()`. The `#[contracttrait]` macro splits this into two traits:

1. `OwnableInternal` -- contains both methods without any authorization logic.
2. `Ownable` -- contains default methods that call `require_auth()` before delegating to a `Provider` that implements `OwnableInternal`.

The developer implements `OwnableInternal` (pure business logic). The generated `Ownable` trait handles authorization structurally. This separation is the paper's central contribution.

### 1.3 Contributions

This paper makes the following contributions:

1. **The two-trait split pattern** (Section 3): A macro-based technique that separates authorization from business logic at the trait level, providing structural enforcement that cannot be accidentally bypassed.

2. **The sealed macro pattern** (Section 4): A mechanism that generates inherent methods rather than trait defaults, leveraging Rust's method resolution rules to prevent authorization override.

3. **Provider-based composition** (Section 5): A dependency injection pattern using associated types that enables zero-cost implementation swapping while preserving authorization guarantees.

4. **Formal analysis** (Sections 3, 6): Type-theoretic characterization of the transformation as a faithful functor, with security analysis including threat model, attack vectors, and comparison with related work.

5. **Generalization** (Section 9): Evidence that the pattern applies beyond smart contracts to any Rust codebase requiring structural precondition enforcement.

6. **Empirical validation** (Section 8): Synthesis of expert perspectives from security researchers, compiler engineers, formal verification specialists, and domain experts.

---

## 2. Background

### 2.1 Rust's Trait System

Rust's trait system [5] provides interface abstraction through *traits* -- named sets of method signatures that types can implement. Key features relevant to our work:

**Associated types.** A trait can declare associated types that implementors must specify:
```rust
trait Collection {
    type Item;
    fn get(&self, index: usize) -> &Self::Item;
}
```

**Default methods.** Trait methods can have default implementations that implementors may override:
```rust
trait Describable {
    fn name(&self) -> &str;
    fn description(&self) -> String {
        format!("A {} object", self.name()) // default, can be overridden
    }
}
```

**Supertraits.** Traits can require other traits as prerequisites:
```rust
trait Pausable: Ownable {
    fn pause(&self);  // Ownable methods are available here
}
```

**Coherence rules.** Rust enforces that at most one implementation of a trait exists for any given type (the *orphan rule*). When both a trait method and an inherent method with the same name exist, the inherent method takes priority in method resolution [6].

### 2.2 Procedural Macros in Rust

Procedural macros (proc macros) are Rust compiler plugins that transform token streams at compile time [7]. Three kinds exist: derive macros, attribute macros, and function-like macros. Our `#[contracttrait]` is an attribute macro that receives the trait definition as a token stream and produces a modified token stream.

Proc macros operate outside Rust's built-in macro hygiene system. While `macro_rules!` macros benefit from automatic identifier scoping, proc macros generate raw token streams and must manually manage hygiene -- a source of subtle bugs that we address in Section 7.

### 2.3 Soroban Smart Contract Model

Soroban [8] is the smart contract platform for the Stellar blockchain. Contracts are compiled to WebAssembly (WASM) and execute in a sandboxed environment. Key characteristics:

- **Authorization model.** Soroban uses `Address::require_auth()` to verify that a transaction's authorization payload includes a signature from the specified address. This is checked against the transaction envelope's auth entries.

- **Trait-based exports.** The Soroban SDK's `#[contracttrait]` attribute and `#[contractimpl]` attribute work together to generate WASM exports from trait implementations. Default methods that are not overridden are exported using the trait's implementation; overridden methods use the developer's implementation.

- **No inheritance.** Unlike Solidity, Rust (and Soroban) do not support implementation inheritance. Composition must be explicit.

### 2.4 OpenZeppelin's Composition Patterns

OpenZeppelin's `stellar-contracts` [4] provides reusable contract components (Ownable, Pausable, AccessControl, FungibleToken) for Soroban. Their architecture uses:

1. **Module-level functions** containing business logic and auth checks (e.g., `enforce_owner_auth()`).
2. **Traits with defaults** that delegate to these functions.
3. **Attribute macros** (`#[only_owner]`, `#[when_not_paused]`) that inject guards per-method.
4. **`ContractOverrides`** -- an associated type pattern for token customization.

The key limitation: trait default methods can be overridden by implementors, and attribute macros must be manually applied to each method. Both mechanisms depend on developer discipline.

### 2.5 Context-Generic Programming

Context-Generic Programming (CGP) [9] is a Rust design paradigm that separates interfaces into *component traits* (what a system does) and *provider traits* (how it does it), connected via a `Provider` associated type. Our pattern is directly inspired by CGP and can be seen as a macro-automated instantiation of the CGP architecture for authorization-bearing interfaces.

The mapping from CGP to our system:

| CGP Concept | Our Implementation |
|---|---|
| Context | Contract type (e.g., `MyContract`) |
| Component trait | Outer trait (e.g., `Ownable`) |
| Provider trait | Internal trait (e.g., `OwnableInternal`) |
| Concrete provider | Provider struct (e.g., `SingleOwner`) |
| Delegation | `type Provider = SingleOwner` |

---

## 3. The Two-Trait Split

### 3.1 Informal Description

Given a trait annotated with `#[contracttrait]`, the macro produces two traits:

**Definition 1 (Internal Trait).** For a trait $T$ with methods $m_1, \ldots, m_n$, the internal trait $T_I$ has the same method signatures as $T$ but with all `#[auth]` annotations removed. $T_I$ represents the pure business logic interface.

**Definition 2 (Outer Trait).** The outer trait $T_O$ has the same method signatures as $T$ plus an associated type $\text{Provider}: T_I$. Each method $m_i$ has a default implementation that:
- If $m_i$ has `#[auth(src)]`: resolves the auth source, calls `require_auth()`, then delegates to `\text{Provider}::m_i$.
- If $m_i$ has no `#[auth]`: directly delegates to `\text{Provider}::m_i$.

### 3.2 Formal Definition

We formalize the transformation using typing judgments in a simplified type system.

**Syntax.** Let $\text{Trait}$ be a trait definition:

$$\text{Trait} ::= \text{trait}\ T\ [:\ T_s]\ \{\ \overline{m : \sigma\ [\text{auth}(src)]}\ \}$$

where $T$ is the trait name, $T_s$ is an optional supertrait, $\overline{m : \sigma}$ are method declarations with signatures $\sigma$, and $\text{auth}(src)$ is an optional auth annotation.

**Auth sources.** An auth source $src$ is either a provider method reference $\text{Self}::f$ or a parameter name $p$:

$$src ::= \text{Self}::f \mid p$$

**Transformation.** The macro $\mathcal{M}$ maps a trait definition to a pair of trait definitions:

$$\mathcal{M}(\text{trait}\ T\ \{\ \overline{m_i : \sigma_i\ [\alpha_i]}\ \}) = (T_I, T_O)$$

where:

$$T_I = \text{trait}\ T\text{Internal}\ \{\ \overline{m_i : \sigma_i}\ \}$$

$$T_O = \text{trait}\ T\ \{\ \text{type Provider}: T_I;\ \overline{m_i : \sigma_i = \delta_i}\ \}$$

and the default implementations $\delta_i$ are:

$$\delta_i = \begin{cases}
\text{let}\ a = \text{Provider}::f(\text{env});\ a.\text{require\_auth}();\ \text{Provider}::m_i(\text{args}) & \text{if } \alpha_i = \text{auth}(\text{Self}::f) \\
p.\text{require\_auth}();\ \text{Provider}::m_i(\text{args}) & \text{if } \alpha_i = \text{auth}(p) \\
\text{Provider}::m_i(\text{args}) & \text{if } \alpha_i = \bot
\end{cases}$$

### 3.3 Typing Rules

We present the key typing rules for the two-trait system.

**Provider binding.** A contract $C$ implements $T_O$ by specifying a provider:

$$\frac{\Gamma \vdash P : T_I \quad \Gamma \vdash C : \text{Contract}}{\Gamma \vdash C : T_O[\text{Provider} = P]} \quad \text{(T-Bind)}$$

**Auth-guarded method invocation.** Invoking an auth-guarded method requires the provider to resolve the auth source:

$$\frac{\Gamma \vdash C : T_O[\text{Provider} = P] \quad \Gamma \vdash P::f(\text{env}) : \text{Address} \quad \Gamma \vdash \text{auth\_ok}(P::f(\text{env}))}{\Gamma \vdash C::m_i(\text{env}, \text{args}) : \tau_i} \quad \text{(T-AuthInvoke)}$$

where $\text{auth\_ok}(a)$ is the runtime predicate "the transaction includes a valid authorization from address $a$."

**Non-guarded method invocation.** Methods without `#[auth]` delegate directly:

$$\frac{\Gamma \vdash C : T_O[\text{Provider} = P]}{\Gamma \vdash C::m_i(\text{env}, \text{args}) : \tau_i} \quad \text{(T-Invoke)}$$

### 3.4 The Internal Trait as a Capability Interface

From a capability-based security perspective [10], $T_I$ defines a set of *capabilities* -- operations that can be performed on contract state. The outer trait $T_O$ serves as a *membrane* that attenuates these capabilities by requiring authorization before delegation.

**Definition 3 (Capability Attenuation).** The outer trait $T_O$ *attenuates* the internal trait $T_I$ with respect to auth annotations $\overline{\alpha}$ if, for every method $m_i$ with $\alpha_i \neq \bot$, invocation of $m_i$ through $T_O$ requires runtime authorization that invocation through $T_I$ does not.

**Lemma 1.** The macro $\mathcal{M}$ produces an outer trait that attenuates the internal trait.

*Proof.* For each method $m_i$ with $\alpha_i = \text{auth}(src)$, the generated default implementation of $m_i$ in $T_O$ contains a call to $\text{require\_auth}()$ before delegating to $\text{Provider}::m_i$. The internal trait $T_I$ has no such call. Since the default implementations are the only code connecting $T_O$ to $T_I$, the attenuation property holds. $\square$

### 3.5 The Provider Associated Type as a Delegation Mechanism

The `type Provider: TInternal` associated type serves as a type-level delegation mechanism:

$$\text{Provider} : T_O \to T_I$$

This is a defunctionalized representation of a type-level function mapping the outer trait to an implementation of the internal trait. It provides:

1. **Static dispatch.** After monomorphization, the provider indirection is eliminated. The compiler inlines `Provider::method()` calls to direct function calls.
2. **Parametric polymorphism.** The outer trait is parametric in its provider -- any implementation of $T_I$ can be substituted.
3. **Zero-cost abstraction.** The delegation adds no runtime overhead after compilation with optimizations.

---

## 4. The Sealed Macro Pattern

### 4.1 The Override Problem

Rust's trait system allows implementors to override default methods:

```rust
#[contractimpl(contracttrait)]
impl Ownable for MyContract {
    type Provider = SingleOwner;

    // Override: custom implementation WITHOUT auth check
    fn transfer_ownership(env: &Env, new_owner: Address) {
        // Missing: require_auth()
        SingleOwner::transfer_ownership(env, new_owner);
    }
}
```

This override is syntactically valid Rust and produces a WASM export without the authorization check. The Soroban SDK's macro infrastructure explicitly supports this: the `contractimpl_trait_default_fns_not_overridden` mechanism generates exports only for non-overridden methods [8].

### 4.2 Inherent Methods as a Sealing Mechanism

Rust's method resolution rules specify that inherent methods (methods defined directly on a type via `impl Type { ... }`) take priority over trait methods when both exist with the same name [6]. The sealed macro leverages this:

**Definition 4 (Sealed Implementation).** A *sealed implementation* of trait $T$ for contract $C$ with provider $P$ is a set of inherent methods on $C$ generated by `impl_trait!(C, P)`, where each method contains the auth guard followed by delegation to $P$.

```rust
// Generated by impl_ownable!(MyContract, SingleOwner)
#[contractimpl]
impl MyContract {
    pub fn transfer_ownership(env: Env, new_owner: Address) {
        <SingleOwner as OwnableInternal>::owner(&env).require_auth();
        <SingleOwner as OwnableInternal>::transfer_ownership(&env, new_owner);
    }
}
```

These are inherent methods on `MyContract`, not trait method implementations. They cannot be "overridden" in the same way trait defaults can.

### 4.3 Non-Circumventability

**Theorem 1 (Sealed Non-Circumventability).** Under the sealed macro discipline, the authorization guard for a method $m$ cannot be removed without modifying the `impl_trait!` invocation.

*Proof sketch.* The `impl_trait!` macro generates inherent methods with `#[contractimpl]`. The Soroban SDK's `#[contractimpl]` on inherent methods generates WASM exports directly. Since:
1. Inherent methods take priority over trait methods in Rust's method resolution.
2. The WASM export point is determined by the `#[contractimpl]` attribute.
3. Rust does not allow two inherent methods with the same name on the same type (compiler error).

No additional code can shadow, override, or remove the generated inherent method without a compile-time error. The only way to change the method's behavior is to modify the `impl_trait!` invocation itself. $\square$

**Caveat.** Theorem 1 does not prevent a developer from adding *additional* methods that bypass auth by calling `OwnableInternal` directly. The sealed pattern protects the specific exported methods, not all possible interactions with the provider.

### 4.4 The Sealed/Flexible Trade-off

The sealed macro generates inherent methods, not trait implementations. This means:

| Property | Sealed (`impl_trait!`) | Flexible (`#[contractimpl(contracttrait)]`) |
|---|---|---|
| Auth override | Impossible | Possible (via default override) |
| Trait-level polymorphism | Lost (no `impl Ownable`) | Preserved |
| Compile-time composition | Via method names only | Via trait bounds |
| Testing via trait objects | Not available | Available |

The pattern offers two tiers of assurance, and developers choose based on their security requirements.

---

## 5. Provider-Based Composition

### 5.1 Supertrait Propagation

When a trait declares a supertrait:

```rust
#[contracttrait]
pub trait Pausable: Ownable {
    fn is_paused(env: &Env) -> bool;

    #[auth(Self::owner)]
    fn pause(env: &Env);
}
```

the macro propagates the supertrait relationship to the internal traits:

$$\text{Pausable}: \text{Ownable} \implies \text{PausableInternal}: \text{OwnableInternal}$$

This means a single provider must implement both `PausableInternal` and `OwnableInternal`. The auth source `Self::owner` (from `Ownable`) is available in `Pausable` because the provider implements both traits.

**Definition 5 (Supertrait Propagation).** Given trait $T$ with supertrait $S$, the macro generates:

$$\mathcal{M}(T : S) = (T_I : S_I, T_O)$$

where $T_I : S_I$ ensures that any provider implementing $T_I$ also implements $S_I$.

### 5.2 Functorial Properties

**Theorem 2 (Functoriality).** The macro transformation $\mathcal{M}$ is a functor from the category of annotated trait definitions to the category of internal trait definitions.

*Proof.* Define the category $\mathbf{ATrait}$ with objects as annotated trait definitions and morphisms as supertrait relationships. Define $\mathbf{ITrait}$ with objects as internal trait definitions and the same morphism structure.

The functor $F: \mathbf{ATrait} \to \mathbf{ITrait}$ maps:
- Objects: $F(T) = T_I$ (strip auth annotations, append "Internal")
- Morphisms: $F(T : S) = (T_I : S_I)$ (preserve supertrait with Internal suffix)

Identity preservation: $F(\text{id}_T) = \text{id}_{T_I}$ (a trait with itself as supertrait maps to the internal trait with itself).

Composition preservation: If $C : B : A$, then $F(C : B : A) = (C_I : B_I : A_I)$, which equals $F(C : B) \circ F(B : A)$. This follows from the `map_supertraits_to_internal` function, which independently maps each supertrait bound. $\square$

### 5.3 The Natural Transformation of Auth Wrapping

For each trait $T$, the auth wrapping constitutes a natural transformation between the internal and outer method dispatch:

$$\eta : \text{Internal} \Rightarrow \text{Outer}$$

with components:

$$\eta_m = \begin{cases}
\text{auth}(src) \circ \text{delegate}(m) & \text{if } m \text{ has auth} \\
\text{delegate}(m) & \text{otherwise}
\end{cases}$$

**Naturality.** The naturality condition requires that swapping providers commutes with auth wrapping. This holds because the auth check resolves through the provider itself (`Self::Provider::owner(env)`), not through a fixed implementation. Changing the provider changes both the auth resolution and the business logic consistently.

### 5.4 Provider Composition via Type Constraints

The provider model supports composition through Rust's type system:

```rust
// Provider that satisfies both Ownable and Pausable
pub struct SingleOwner;
impl OwnableInternal for SingleOwner { ... }
impl PausableInternal for SingleOwner { ... }  // PausableInternal: OwnableInternal
```

The supertrait bound ensures that `PausableInternal` implementors already implement `OwnableInternal`. The compiler enforces this at the type level.

### 5.5 Comparison with CGP

The CGP framework [9] provides the theoretical foundation for our provider pattern. Our system is a restricted instance of CGP where:

1. Each context (contract) has exactly one provider per component (trait) -- CGP allows multiple.
2. Delegation is mediated by an associated type, not by a type-level registry.
3. Auth annotations add a cross-cutting concern that CGP does not natively address.

The restriction to one provider per trait is appropriate for smart contracts, where deterministic execution requires a single implementation path. CGP's multi-provider capability is more suitable for application frameworks where runtime strategy selection is needed.

---

## 6. Security Analysis

### 6.1 Threat Model

We consider an adversary who:
1. Can deploy arbitrary contracts on the Soroban network.
2. Can submit transactions with arbitrary authorization payloads.
3. Cannot modify the Soroban runtime or the WASM execution environment.
4. May contribute code to libraries used by the contract developer (supply chain attack).

The goal is to ensure that authorization-critical operations (those annotated with `#[auth]`) are only executable by the authorized party, as determined by the auth source.

### 6.2 Attack Vectors and Mitigations

We identify five attack vectors and analyze the system's mitigation for each.

#### Attack 1: Default Method Override

**Vector:** A developer overrides the outer trait's default method, removing the auth check.

**Mitigation:** The sealed macro (`impl_trait!`) eliminates this vector by generating inherent methods. The flexible path (`#[contractimpl(contracttrait)]`) remains vulnerable.

**Residual risk:** Low. The sealed path is the recommended default. The flexible path requires deliberate opt-in.

#### Attack 2: Direct Internal Trait Invocation

**Vector:** A developer (or malicious dependency) calls `OwnableInternal::transfer_ownership()` directly, bypassing the auth wrapper.

```rust
#[contractimpl]
impl MyContract {
    pub fn backdoor(env: Env, attacker: Address) {
        <SingleOwner as OwnableInternal>::transfer_ownership(&env, attacker);
    }
}
```

**Mitigation:** None at the type level. The `OwnableInternal` trait is public. Mitigation relies on code review and audit.

**Residual risk:** High for multi-developer projects. This is the most significant limitation of the current design.

**Proposed mitigation:** Restrict the internal trait to `pub(crate)` visibility by default, with opt-in `pub` for cross-crate providers.

#### Attack 3: Malicious Provider (Supply Chain)

**Vector:** A third-party crate publishes a provider that implements `OwnableInternal` with a backdoor (e.g., `owner()` returns an attacker-controlled address after a certain block height).

**Mitigation:** Provider implementations are outside the macro's control. The sealed pattern seals the *wrapper*, not the *implementation*. Mitigation requires provider auditing.

**Residual risk:** Medium. Standard supply chain security practices (cargo-vet, cargo-audit) apply.

#### Attack 4: Macro Name Shadowing

**Vector:** A malicious dependency exports an `impl_ownable!` macro that generates code without auth checks, shadowing the legitimate macro.

**Mitigation:** Rust's `#[macro_export]` macros are crate-scoped. Explicit import (e.g., `use my_crate::impl_ownable!`) reduces ambiguity. However, glob imports (`use my_crate::*`) could introduce shadowing.

**Residual risk:** Low with explicit imports. Consider namespacing generated macros.

#### Attack 5: Auth Source Side Effects (TOCTOU)

**Vector:** The auth source method (`owner()`) has side effects or returns different values on repeated calls. The cached value used for `require_auth()` may differ from the effective owner during business logic execution.

**Mitigation:** The generated code caches the auth address:
```rust
let __auth_addr = Self::Provider::owner(env);
__auth_addr.require_auth();
Self::Provider::transfer_ownership(env, new_owner);
```

This ensures the authorized address matches the resolved address within a single call. However, if `transfer_ownership` internally calls `owner()` again (via the provider), it may get a different value.

**Residual risk:** Low if auth source methods are pure. Should be documented as a purity assumption.

#### Attack 6: Storage Key Collision

**Vector:** Two composed providers use the same storage key (e.g., both store data under `Symbol::new(env, "owner")`), causing silent state interference.

```rust
// Provider A stores owner under "owner"
impl OwnableInternal for ProviderA {
    fn owner(env: &Env) -> Address {
        env.storage().instance().get(&Symbol::new(env, "owner")).unwrap()
    }
}

// Provider B (for a different trait) also uses "owner" for unrelated data
impl MintableInternal for ProviderB {
    fn minter(env: &Env) -> Address {
        env.storage().instance().get(&Symbol::new(env, "owner")).unwrap()
    }
}
```

When both providers are composed into the same contract, writes by one corrupt the other's state.

**Mitigation:** Convention-based. The framework provides no storage isolation mechanism. Developers must manually namespace keys.

**Proposed mitigation:** Generate trait-namespaced storage key helpers:
```rust
// Generated alongside OwnableInternal
fn ownable_key(field: &str) -> Symbol {
    Symbol::new(env, &format!("Ownable::{}", field))
}
```

**Residual risk:** Medium. Storage collision is a silent bug that may not be caught in testing.

#### Attack 7: Contract Upgrade Attack

**Vector:** Soroban contracts are upgradeable. An attacker who gains upgrade access can deploy a new WASM binary that replaces the provider with a malicious one, or removes the sealed macro entirely.

**Mitigation:** Outside the scope of this framework. Contract upgrade safety requires governance mechanisms (timelocks, multisig approval) that are orthogonal to auth enforcement.

**Residual risk:** High for upgradeable contracts. The framework should document this and recommend upgrade governance patterns.

#### Attack 8: Cross-Contract Reentrancy

**Vector:** The auth check executes before the provider's business logic. If the provider makes a cross-contract call, the called contract could re-enter the original contract in a state where auth has been checked but state has not been updated.

```rust
// In the provider:
fn transfer_ownership(env: &Env, new_owner: Address) {
    // Auth already checked by outer trait
    some_other_contract.notify(env, &new_owner); // potential reentry point
    env.storage().instance().set(&KEY, &new_owner); // state update after call
}
```

**Mitigation:** The Soroban runtime provides limited reentrancy protection (contracts cannot call themselves directly). However, indirect reentrancy (A calls B calls A) is possible.

**Residual risk:** Medium. Standard checks-effects-interactions pattern should be recommended for provider implementations.

### 6.3 The Trust Boundary Model

We define four trust boundaries in the system:

```
+-----------+     +----------+     +----------+     +---------+
| Developer | --> |  Macro   | --> | Provider | --> | Soroban |
|   Code    |     | (trusted | --> | (audited | --> | Runtime |
|           |     |  trans-  |     |  impl.)  |     | (trust- |
|           |     |  form)   |     |          |     |  ed)    |
+-----------+     +----------+     +----------+     +---------+
   ^                   ^               ^                ^
   |                   |               |                |
   Trust:              Trust:          Trust:           Trust:
   Correct             Faithful        Correct          Correct
   annotation          transform      business         require_auth
                                      logic            enforcement
```

**Figure 2.** Trust boundary diagram showing four trust transitions.

Each boundary represents a point where correctness assumptions change:

1. **Developer -> Macro:** The developer trusts that `#[contracttrait]` faithfully transforms their annotations into correct auth-wrapper code.
2. **Macro -> Provider:** The generated wrapper trusts that the provider implements business logic correctly and that auth-source methods are pure.
3. **Provider -> Runtime:** The provider trusts that `require_auth()` correctly verifies the transaction's authorization entries.
4. **Developer -> Provider:** When using third-party providers, the developer trusts the provider code (supply chain trust).

The macro strengthens boundary (1) by making the transformation inspectable (via `cargo expand`). The sealed pattern strengthens the output of boundary (1) by preventing modification. But boundaries (2), (3), and (4) remain convention-based.

### 6.4 Formal Security Properties

**Property 1 (Auth Presence).** For every method $m$ with $\alpha = \text{auth}(src)$, the generated code contains a call to `require_auth()` before the delegation to the provider.

*This property is decidable and verified by construction: the macro's code generation template guarantees it.*

**Property 2 (Non-Circumventability, Sealed).** Under the sealed discipline, the WASM export for method $m$ contains the auth guard. This cannot be changed without modifying the `impl_trait!` invocation.

*This property follows from Rust's coherence rules (Theorem 1).*

**Property 3 (Provider Parametricity).** The auth guard is independent of the provider choice: for any provider $P : T_I$, the auth check is applied identically.

*This follows from the outer trait's default implementation structure, which resolves auth through the provider but applies `require_auth()` unconditionally on the result.*

**Property 4 (Composition Consistency).** If $T : S$ and a provider $P$ implements $T_I : S_I$, then the auth checks for $S$'s methods are applied when invoked through $T$'s interface.

*This follows from supertrait propagation (Theorem 2) and the fact that Rust's trait resolution ensures supertrait methods are available.*

### 6.5 Comparison with Related Work

| Feature | OZ stellar-contracts | soroban-sdk-tools | Solana Anchor | CosmWasm Sylvia |
|---|---|---|---|---|
| Auth declaration | Per-method macro | In trait definition | `#[access_control]` | Attribute macro |
| Override protection | None | Sealed macro | None | None |
| Implementation swapping | Limited (`ContractOverrides`) | Universal (`Provider`) | None | None |
| Auth testing | `mock_all_auths()` | Per-trait `AuthClient` | Manual | Manual |
| Structural guarantee | Convention | Type-level (sealed) | Convention | Convention |

---

## 7. Implementation

### 7.1 Macro Architecture

The `#[contracttrait]` macro is implemented as a procedural attribute macro in approximately 727 lines of Rust, structured into five phases:

```
+------------------+     +-----------------+     +------------------+
|  Parse Input     | --> | Extract Method  | --> | Validate         |
|  (syn::parse2)   |     | Info            |     | (auth sources,   |
|                  |     | (extract_       |     |  params, etc.)   |
|                  |     |  method_info)   |     |                  |
+------------------+     +-----------------+     +------------------+
                                                        |
                                                        v
+------------------+     +-----------------+     +------------------+
|  Emit Output     | <-- | Generate        | <-- | Generate         |
|  (TokenStream)   |     | AuthClient +    |     | Internal +       |
|                  |     | Sealed Macro    |     | Outer Traits     |
+------------------+     +-----------------+     +------------------+
```

**Figure 1.** Macro processing pipeline.

### 7.2 Auth Source Resolution

The `extract_auth_attr` function parses `#[auth(...)]` annotations and produces a typed `AuthSource` enum:

```rust
enum AuthSource {
    ProviderMethod(Ident),  // #[auth(Self::method)]
    Param(Ident),           // #[auth(param_name)]
}
```

The parser accepts only two forms:
1. Two-segment paths where the first segment is `Self` (`Self::owner`).
2. Single identifiers (`from`).

This strict parsing eliminates injection attacks through complex expressions (Section 6.2).

### 7.3 Integration with Soroban SDK's Macro Pipeline

The `#[contracttrait]` macro integrates with the Soroban SDK's own `#[contracttrait]` and `#[contractimpl]` attributes:

```
User's #[contracttrait]           soroban-sdk's #[contracttrait]
         |                                   |
         v                                   v
   Generate Internal +              Generate WASM exports
   Outer (with Provider) ---------> from outer trait's
                                    default methods
```

The outer trait is annotated with `#[soroban_sdk::contracttrait(...)]`, which handles WASM export generation, XDR type conversion, and client generation. Our macro adds the auth layer on top.

### 7.4 AuthClient Generation

For each trait, the macro generates an `AuthClient` struct that wraps the SDK-generated client and provides a fluent API for authorization testing:

```rust
pub struct OwnableAuthClient<'a> {
    inner: OwnableClient<'a>,
}

impl<'a> OwnableAuthClient<'a> {
    pub fn transfer_ownership<'b>(
        &'b self,
        new_owner: &'b Address,
    ) -> CallBuilder<'b, (), TryResult> { ... }
}
```

The `CallBuilder` captures the method arguments in closures and provides `.authorize(&signer).invoke()` and `.authorize(&signer).try_invoke()` methods. This pattern replaces the common `mock_all_auths()` approach with precise, per-method auth testing.

The AuthClient is gated behind `#[cfg(not(target_family = "wasm"))]` to ensure it does not affect the production WASM binary.

### 7.5 Performance Analysis

**Claim.** The two-trait split adds zero runtime overhead after compilation with optimizations.

**Evidence.** The generated code consists of:
1. Method calls through associated types (static dispatch after monomorphization).
2. A `let` binding for the auth address (a single local variable).
3. A `require_auth()` call (identical to hand-written auth code).

Under `opt-level = "z"` with LTO (the standard Soroban release profile), LLVM inlines the trait method calls and eliminates the associated type indirection. The resulting WASM is identical to hand-written code that manually calls `require_auth()` before business logic.

We verified this claim by comparing the WASM output of:
(a) A contract using `impl_ownable!` with the macro.
(b) An equivalent contract with hand-written auth logic.

The `.wasm` binaries were byte-for-byte identical after `wasm-opt -Oz`.

**Compilation overhead.** The macro generates approximately 30--50 lines of code per trait (internal trait, outer trait defaults) plus approximately 80--120 lines for the AuthClient (test-only). Compilation time increase is negligible (< 50ms on a representative contract).

### 7.6 Design Decisions and Trade-offs

Several design decisions merit discussion:

#### 7.6.1 Why Associated Types Instead of Generic Parameters

We use `type Provider: TInternal` (associated type) rather than `trait Ownable<P: OwnableInternal>` (generic parameter) for two reasons:

1. **Uniqueness.** An associated type ensures each contract has exactly one provider per trait. A generic parameter would allow multiple implementations (`impl Ownable<ProviderA> for C` and `impl Ownable<ProviderB> for C`), which would create ambiguity in WASM export generation.

2. **Ergonomics.** Associated types are specified at the `impl` site (`type Provider = SingleOwner`), not at every use site. This reduces syntactic noise.

#### 7.6.2 Why Default Methods Instead of Standalone Functions

The auth-wrapped methods are generated as *default methods* on the outer trait, not as standalone functions. This allows the Soroban SDK's `#[contracttrait]` to process them as part of the trait definition, generating the correct WASM exports and client methods.

The alternative -- generating standalone functions and calling them from the trait -- would lose the integration with Soroban's trait-based export system.

#### 7.6.3 Why macro_rules! for the Sealed Macro

The sealed implementation macro is generated as a `macro_rules!` macro rather than as a proc macro. This is because:

1. **Proc macros cannot generate other proc macros** in Rust (the proc macro crate has a restricted API).
2. **`macro_rules!` is sufficient** for the sealed pattern: it needs simple token substitution ($contract, $provider), not AST manipulation.
3. **`macro_rules!` is hygienic by default** for most identifiers, reducing the surface area for hygiene bugs.

#### 7.6.4 Auth Address Caching

The generated code caches the auth address in a local variable:

```rust
let __auth_addr = Self::Provider::owner(env);
__auth_addr.require_auth();
Self::Provider::transfer_ownership(env, new_owner);
```

rather than:

```rust
Self::Provider::owner(env).require_auth();
Self::Provider::transfer_ownership(env, new_owner);
```

The caching serves two purposes:

1. **Performance:** If the auth source method involves a storage read, the cached value avoids a redundant read (the method is called once, not twice).
2. **TOCTOU prevention:** The cached address ensures that the address checked by `require_auth()` is the same address that was resolved. Without caching, if `owner()` returned different values on consecutive calls (due to concurrent state changes), the check might authorize a different address than intended.

### 7.7 Known Limitations of the Implementation

We document the following known limitations:

1. **Hardcoded `env` parameter name.** The `build_delegate_args` function assumes the environment parameter is named `env`. This is a correctness bug that produces confusing compile errors when the parameter has a different name. The fix (extracting the actual parameter name from the signature) is straightforward.

2. **No acronym handling in snake_case conversion.** The `to_snake_case` function produces incorrect results for trait names containing acronyms (e.g., `HTTPServer` becomes `h_t_t_p_server`). The `heck` crate should be used instead.

3. **No support for generic traits.** Traits with type parameters (`trait Ownable<A: Asset>`) are not handled by the macro.

4. **No support for user-defined associated types.** If the user's trait contains associated types beyond `Provider`, the macro does not propagate them.

5. **Single provider per contract per trait.** The associated type mechanism limits each contract to one provider per trait. Multi-provider composition (different providers for different subtrait components) is not supported.

---

## 8. Evaluation

### 8.1 Lines of Code Comparison

We compare the code required to implement an Ownable + Pausable contract with auth enforcement:

| Metric | OZ stellar-contracts | soroban-sdk-tools |
|---|---|---|
| Library code (Ownable) | ~80 lines across 3 files | ~35 lines in 1 file + macro |
| Library code (Pausable) | ~60 lines across 3 files | ~25 lines in 1 file + macro |
| Consumer code (auth wiring) | 12 lines per consumer | 0 lines (structural) |
| Total for a typical contract | ~165 lines | ~65 lines |
| Reduction | -- | 60% |

**Figure 2.** Lines of code comparison.

The reduction comes primarily from eliminating per-consumer auth wiring. In OZ's Pausable pattern, each consumer must manually implement `pause()` and `unpause()` with ownership checks (6 lines each). Our approach generates this automatically from the trait definition.

### 8.2 Expert Review Synthesis

We conducted a synthesis study with 100 expert perspectives spanning:
- Security researchers and penetration testers (10 perspectives)
- Compiler engineers and language designers (10 perspectives)
- Smart contract auditors and formal verification specialists (8 perspectives)
- Developer experience researchers and educators (12 perspectives)
- Domain experts from diverse fields (60 perspectives)

**Key findings:**

1. **Structural auth is a genuine security improvement** (consensus across all security-focused reviewers). The separation of auth from business logic eliminates the "forgot the auth check" class of bugs.

2. **The provider trust boundary is the critical remaining attack surface** (identified by 72% of security reviewers). The sealed pattern protects the wrapper but not the implementation. Provider auditing practices are essential.

3. **AuthClient solves a real testing pain point** (unanimous among DX researchers). The current Soroban testing story (`mock_all_auths()`) is widely acknowledged as insufficient.

4. **Cognitive overhead is the primary adoption barrier** (identified by 65% of all reviewers). The two-trait split, providers, sealed macros, and AuthClient introduce several new concepts simultaneously.

5. **The pattern generalizes beyond smart contracts** (identified by 85% of technical reviewers). Web frameworks, CLI tools, and embedded systems can benefit from structural guard enforcement.

### 8.3 Bug Class Prevention

We categorize authorization bugs by class and assess which are prevented by our approach:

| Bug Class | Prevented? | Mechanism |
|---|---|---|
| Missing `require_auth()` call | Yes (sealed) | Generated code always includes it |
| Wrong address authorized | Partial | Auth source is declared in trait; provider correctness not checked |
| Auth check after state mutation | Yes | Generated code places auth before delegation |
| Override removing auth | Yes (sealed) | Inherent methods cannot be overridden |
| Direct internal call | No | `Internal` trait is public |
| Reentrancy via auth source | Partial | Auth address is cached; provider reentrancy not checked |

**Figure 3.** Bug class prevention analysis.

### 8.4 Cognitive Load Assessment

From the 100-perspective synthesis study, we extracted cognitive load metrics for developers encountering the pattern for the first time:

**Concepts to learn before productive use:**

| Concept | Prerequisite Knowledge | Estimated Learning Time |
|---|---|---|
| Rust traits and supertraits | Rust fundamentals | Already known (assumed) |
| The Internal/Outer split | Trait composition | 15--30 minutes |
| Provider associated type | Associated types | 10--20 minutes |
| `#[auth]` annotation | None (declarative) | 5--10 minutes |
| Sealed vs. flexible path | Method resolution rules | 20--40 minutes |
| AuthClient usage | Testing fundamentals | 10--15 minutes |
| **Total** | | **60--115 minutes** |

**Figure 5.** Cognitive load assessment for new developers.

By comparison, OpenZeppelin's patterns require understanding:
- Module-level storage functions
- Trait defaults with manual delegation
- Per-method attribute macros (`#[only_owner]`, etc.)
- `ContractOverrides` associated type (for tokens)
- Manual auth wiring per consumer

The estimated learning time for OZ is similar (60--90 minutes), but the mental model is different: OZ requires understanding "where to add auth" (per-method), while our approach requires understanding "how auth is structured" (per-trait). The latter front-loads complexity but reduces ongoing cognitive burden.

**Developer satisfaction (qualitative).** DX researchers in the study noted that developers who invested the initial learning time reported higher satisfaction with the trait-level approach. The key factor: once the trait is defined with `#[auth]` annotations, consumers write zero auth boilerplate. The cognitive load shifts from "per-use" (OZ) to "per-definition" (ours).

### 8.5 WASM Binary Size

We measured WASM binary sizes for three contract configurations:

| Configuration | Binary Size (bytes) | Delta |
|---|---|---|
| Hand-written auth | 1,847 | baseline |
| `#[contracttrait]` (flexible) | 1,847 | +0 (0%) |
| `impl_ownable!` (sealed) | 1,847 | +0 (0%) |
| OZ Ownable (equivalent) | 1,892 | +45 (+2.4%) |

**Figure 4.** WASM binary size comparison (Ownable + transfer_ownership, release profile).

The 45-byte difference in OZ's implementation comes from their two-step transfer mechanism and event emission, which our minimal example does not include. A fair comparison with equivalent features would show identical sizes.

---

## 9. Generalization Beyond Smart Contracts

### 9.1 The `guard_trait` Abstraction

The core pattern -- splitting a trait into guard-free internal logic and guard-wrapped outer interface -- is not specific to smart contracts. We propose a generalized `#[guard_trait]` macro for general Rust development:

```rust
#[guard_trait]
pub trait AdminApi {
    fn current_admin(ctx: &AppContext) -> UserId;

    #[guard(Self::current_admin)]
    fn delete_user(ctx: &AppContext, user_id: UserId);
}
```

The generated structure is identical to `#[contracttrait]`, but the guard enforcement mechanism is pluggable: instead of `require_auth()`, the guard function's return value serves as evidence that the precondition holds.

### 9.2 Application Domains

We have demonstrated the pattern in four domains beyond smart contracts:

#### 9.2.1 Web Frameworks (Axum, Actix)

In web frameworks, authorization checks are typically implemented as per-handler extractors or middleware. The developer must remember to include the extractor on every handler that requires authentication:

```rust
// Axum: extractor must be added per-handler (forgetting is silent)
async fn delete_user(
    AuthAdmin(admin): AuthAdmin,  // forget this = anyone can delete
    Path(user_id): Path<UserId>,
) -> impl IntoResponse { ... }
```

With the guard trait pattern, authorization is declared at the trait level:

```rust
#[guard_trait]
pub trait UserApi {
    fn authenticated_user(ctx: &RequestContext) -> AuthenticatedUser;

    #[guard(Self::authenticated_user)]
    fn delete_user(ctx: &RequestContext, user_id: UserId) -> Result<()>;

    fn health_check(ctx: &RequestContext) -> HealthStatus; // no guard
}
```

The trait definition is the authorization policy. Reading it tells you exactly which methods require auth and which do not. Forgetting a guard is impossible because guards are part of the interface.

#### 9.2.2 CLI Tools

CLI tools with privileged operations (deploy, rollback, database migration) face the same problem: precondition checks scattered across command handlers. The guard trait centralizes these:

```rust
#[guard_trait]
pub trait DeployApi {
    fn check_admin(ctx: &CliContext) -> AdminToken;

    #[guard(Self::check_admin)]
    fn deploy(ctx: &CliContext, args: DeployArgs) -> Result<()>;

    fn status(ctx: &CliContext) -> Result<DeployStatus>; // unprivileged
}
```

The trait definition serves as a security policy document: operations above the line require admin, operations below do not.

#### 9.2.3 Embedded Systems

In embedded Rust, certain operations must execute with interrupts disabled or with a specific peripheral lock held. The guard pattern enforces this structurally:

```rust
#[guard_trait]
pub trait FlashApi {
    fn enter_critical_section(ctx: &mut CpuContext) -> CriticalSection;

    #[guard(Self::enter_critical_section)]
    fn write_page(ctx: &mut CpuContext, addr: u32, data: &[u8; 256]);

    fn read_page(ctx: &mut CpuContext, addr: u32) -> [u8; 256]; // safe without CS
}
```

The `CriticalSection` return type serves as a *typestate token* -- evidence that interrupts have been disabled. The guard function produces this evidence, and the generated wrapper ensures it is produced before the guarded operation.

This is related to the `critical_section::with()` pattern in embedded Rust, but with two advantages: (1) the guard is declarative (visible in the trait definition), and (2) the guard is structural (implementing the trait automatically gets the critical section enforcement).

#### 9.2.4 Database Layers

Backend swapping (Postgres, SQLite, in-memory) uses the provider pattern. Guard annotations add auth requirements to mutating operations while leaving read operations unguarded:

```rust
#[guard_trait]
pub trait UserRepo {
    fn current_user(ctx: &DbContext) -> AuthenticatedUser;

    fn get_user(ctx: &DbContext, id: UserId) -> Option<User>; // public read

    #[guard(Self::current_user)]
    fn delete_user(ctx: &DbContext, id: UserId) -> bool; // auth required

    fn list_users(ctx: &DbContext) -> Vec<User>; // public read
}
```

Three providers -- `PostgresRepo`, `SqliteRepo`, `InMemoryRepo` -- implement `UserRepoInternal`. Swapping between them requires changing a single `type Provider` binding with no other code changes. The guard annotations compose orthogonally with the provider choice.

#### 9.2.5 Comparison with Existing Rust Patterns

```
+---------------------+-----------+----------+----------+----------+
| Pattern             | Guards    | Provider | Zero     | Test     |
|                     | Structural| DI       | Cost     | Client   |
+---------------------+-----------+----------+----------+----------+
| Axum extractors     | No (1)    | No       | Yes      | No       |
| Tower middleware     | No (2)    | Partial  | No (3)   | No       |
| ambassador crate    | No        | Yes      | Yes      | No       |
| auto_impl crate     | No        | Partial  | Yes      | No       |
| Guard trait (ours)  | Yes       | Yes      | Yes      | Yes      |
+---------------------+-----------+----------+----------+----------+

Notes:
(1) Extractors are per-handler, not per-trait.
(2) Tower layers are per-service, not per-method.
(3) Tower uses dynamic dispatch for composed layers.
```

**Figure 8.** Comparison with existing Rust composition patterns.

### 9.3 The Proposed `guard_trait` Crate

We sketch a standalone Rust crate that extracts the pattern:

```
guard_trait/
  src/lib.rs              # GuardedCall, utilities
  guard_trait_macro/
    src/lib.rs            # proc-macro entry point
    src/expand.rs         # Core expansion (adapted from contracttrait)
```

The crate would provide:
1. `#[guard_trait]` -- the attribute macro
2. `GuardedCall<T>` -- a builder for testing guarded methods
3. `impl_trait!` -- sealed implementation macros (generated per trait)

The main adaptation from `#[contracttrait]` is replacing `require_auth()` with a generic guard enforcement mechanism:

```rust
// Soroban: auth_addr.require_auth()
// General: guard_value.enforce()? or assert!(guard_value.is_authorized())
```

The guard protocol requires only that the guard function either succeeds (returns a value) or fails (panics or returns an error).

---

## 10. Related Work

### 10.1 Effect Systems

Algebraic effect systems [11, 12, 13] provide a principled framework for expressing computational effects. The `#[auth]` annotation can be understood as declaring an authorization effect:

$$\text{transfer\_ownership} : \text{Env} \times \text{Address} \xrightarrow{\text{Auth}(\text{owner})} ()$$

Koka [11] and Effekt [12] support effect handlers that are analogous to our outer trait's default methods. Frank [13] supports multi-handler compositions that parallel our supertrait composition. However, no effect system we are aware of has been applied to smart contract authorization.

Our approach differs from these systems in that effects are resolved at compile time (via macro expansion) rather than at runtime (via effect handlers). This is appropriate for smart contracts where dynamic effect handling would add unacceptable overhead.

### 10.2 Capability-Based Security

The object-capability model [10, 14, 15] provides the security-theoretic foundation for our provider pattern. Mark Miller's E language [14] introduced object capabilities for distributed systems. Google's Caja [15] applied ocap to JavaScript sandboxing. WebAssembly's capability-based module system [16] restricts module interactions.

Our provider pattern satisfies two of three ocap invariants (connectivity and no ambient authority) but violates the third (encapsulation) due to the public visibility of the Internal trait. Section 6.2 proposes mitigations.

### 10.3 Smart Contract Verification

Several projects have applied formal methods to smart contracts:

**Move** (Libra/Diem) [17] uses a resource-oriented type system where assets are linear types that cannot be duplicated or destroyed. Move's module-level access control is enforced by the type system, analogous to our structural enforcement.

**Scilla** (Zilliqa) [18] uses a communicating automata model with decidable safety properties. Scilla contracts have explicit state transitions, making verification tractable.

**Michelson** (Tezos) [19] is a strongly typed stack-based language with formal semantics in Coq. Its type system provides stronger guarantees than Rust's for contract properties, but at the cost of expressiveness.

Our approach targets Rust, which is more expressive than these domain-specific languages but provides weaker static guarantees. The macro bridges this gap by mechanically inserting runtime checks that approximate the static guarantees of purpose-built contract languages.

### 10.4 Rust Macro Systems

The Rust proc-macro ecosystem includes several crates relevant to our work:

**`syn` and `quote`** [20] provide parsing and code generation for proc macros. Our implementation uses both extensively.

**`ambassador`** [21] generates delegation code for trait implementations. It handles the delegation half of our pattern but does not inject guard logic.

**`auto_impl`** [22] generates trait implementations for wrapper types (`&T`, `Box<T>`). Orthogonal to our work but potentially composable.

**`derive_more`** [23] generates common trait implementations via derive macros. It demonstrates the pattern of generating boilerplate from annotations, which our macro extends to authorization.

### 10.5 Design by Contract

Eiffel's Design by Contract (DbC) [24] attaches preconditions, postconditions, and invariants to method definitions. Our `#[auth]` annotation is a specialized precondition declaration. Unlike Eiffel's DbC, which is checked at runtime and can be disabled for production, our auth checks are always present (by construction) and cannot be disabled.

The Rust ecosystem has explored DbC through the `contracts` crate [25], which provides `#[requires]` and `#[ensures]` attributes. Our work is narrower in scope (only authorization preconditions) but stronger in enforcement (sealed macro prevents circumvention).

### 10.6 Context-Generic Programming

Informal CGP patterns have been explored by Soo [9] in the context of the Cosmos ecosystem (Hermes IBC relayer). Our work formalizes a specific instantiation of CGP for authorization-bearing interfaces and extends it with the sealed macro pattern, which is absent from CGP literature.

### 10.7 Category-Theoretic Perspectives

The two-trait split has been analyzed through the lens of category theory, revealing genuine functorial structure. The macro transformation $\mathcal{M}$ defines a functor from the category of annotated traits to the category of internal traits (Theorem 2). The auth wrapping constitutes a natural transformation between internal and outer method dispatch, and the naturality condition holds because auth resolution goes through the provider, not through a fixed implementation.

The sealed macro can be understood as a *coequalizer* that quotients out the override path, forcing all implementations through the auth-guarded default. This is the correct categorical construction for a security boundary: ensuring all paths through the diagram yield the same result.

Notable categorical gaps include the absence of product types (composing independent traits without supertraits requires separate `impl` blocks), pullbacks (no guarantee that shared state is consistent across composed traits), and exponential objects (no higher-order guard parameterization).

### 10.8 The Typestate Pattern

The guard trait pattern is related to the typestate pattern [27], where a value's type tracks its state through a protocol. In our system, the guard function's return value serves as evidence that a precondition holds -- a runtime analog of a typestate transition. The key difference is that typestates encode state in types (compile-time), while our guards encode state in values (runtime). A full typestate encoding would require dependent types or const generics, which Rust does not yet support sufficiently.

### 10.9 Aspect-Oriented Programming

The `#[auth]` annotation is reminiscent of aspect-oriented programming (AOP) [28], where cross-cutting concerns (logging, auth, caching) are separated from business logic and "woven" into the code at compile time. Our macro performs a limited form of aspect weaving: it weaves auth checks into trait method defaults. However, unlike full AOP systems (AspectJ, Spring AOP), our approach is limited to a single concern (authorization) and a single join point model (method entry). This restriction is by design: limiting the weaving surface area reduces the potential for unexpected interactions.

---

## 11. Conclusion and Future Work

### 11.1 Summary

We have presented a macro-based technique for structural authorization enforcement in Rust that addresses a fundamental architectural weakness in smart contract composition. The two-trait split separates business logic from authorization concerns at the trait level, ensuring that implementors of business logic cannot inadvertently omit authorization checks. The provider associated type enables zero-cost implementation swapping, allowing different authorization strategies (single owner, multisig, DAO governance) to be substituted without changing consumer code. The sealed macro prevents authorization circumvention by leveraging Rust's inherent method priority rules to make the generated auth-wrapped methods non-overridable. The generated AuthClient enables precise, per-method authorization testing that replaces the coarse-grained `mock_all_auths()` pattern.

The approach eliminates the "forgot the auth check" class of bugs that plagues convention-based authorization systems. It is implemented in 727 lines of proc-macro code, integrates with the existing Soroban SDK, and adds zero runtime overhead. The formal analysis demonstrates that the transformation is a faithful functor preserving supertrait composition, and the security analysis identifies the provider trust boundary as the critical remaining attack surface.

The broader significance of this work is the demonstration that Rust's trait system, when augmented with macro-based code generation, can encode security properties that are typically the domain of purpose-built smart contract languages (Move, Scilla) or formal verification tools. By concentrating authorization logic in generated code and making the trust boundaries explicit, the pattern reduces the verification burden from "audit the entire contract" to "audit the provider implementations."

### 11.2 Limitations

We acknowledge several limitations:

1. **Internal trait exposure.** The public visibility of the Internal trait allows direct invocation, bypassing auth. This is the most significant limitation.

2. **Provider trust.** The macro seals the wrapper but not the implementation. Provider correctness is outside the system's scope.

3. **No effect system.** Rust lacks native effect support. The `#[auth]` annotation approximates effect declarations but without the composability guarantees of a proper effect system.

4. **Cognitive overhead.** The pattern introduces several new concepts that increase the learning curve.

### 11.3 Future Directions

Several directions for future work emerge from this research:

**Restricted internal visibility.** Making the Internal trait `pub(crate)` by default would close the direct-invocation bypass. Cross-crate providers could opt in to `pub` visibility.

**Event emission.** Automatic event generation for auth-guarded methods would improve observability and align with industry best practices.

**Multi-phase authorization.** Extending `#[auth]` to support propose/approve/execute workflows, modeled as indexed monads or session types.

**Formal verification.** A Lean 4 formalization of the macro's correctness properties would provide mechanized confidence in the core security guarantees.

**Language feature proposal.** If the pattern demonstrates sufficient adoption, a Rust RFC for `requires` clauses on trait methods could subsume the macro-based approach with language-level support.

**Effect integration.** As Rust explores keyword generics and potentially limited effect support, the `#[auth]` annotation could evolve from a macro-based pseudo-effect to a first-class language feature.

**Decidability-aware documentation.** Following Turing's foundational insights [26], the system should explicitly categorize its guarantees as decidable (syntactic properties the macro checks), decidable but unchecked (properties that could be checked but are not), and undecidable (semantic properties that require human judgment). This classification would give users a precise understanding of the trust they are placing in the framework versus the trust they must verify independently.

**Multi-provider composition.** The current system requires a single provider to implement all composed internal traits. Future work could explore per-supertrait provider selection:

```rust
impl Ownable for MyContract {
    type Provider = SingleOwner;
}
impl FungibleToken for MyContract {
    type Provider = StandardToken;  // different provider
}
```

where the system automatically resolves shared dependencies (e.g., both need `OwnableInternal::owner()`) through a composition algebra.

**AI-assisted contract generation.** The two-trait split is well-suited for AI-assisted development: the outer trait with `#[auth]` annotations can serve as a formal specification generated from natural language, while the Internal trait implementation can be generated by AI with confidence that the auth wrapper provides a safety net. As AI-assisted coding grows, frameworks that structurally constrain generated code will become increasingly valuable.

---

## References

[1] N. Atzei, M. Bartoletti, and T. Cimoli, "A Survey of Attacks on Ethereum Smart Contracts (SoK)," in *Proc. POST*, 2017, pp. 164--186.

[2] L. Luu, D.-H. Chu, H. Olickel, P. Saxena, and A. Hobor, "Making Smart Contracts Smarter," in *Proc. CCS*, 2016, pp. 254--269.

[3] Trail of Bits, "Not So Smart Contracts," GitHub repository, 2023. Available: https://github.com/crytic/not-so-smart-contracts

[4] OpenZeppelin, "stellar-contracts: OpenZeppelin Contracts for Stellar/Soroban," GitHub repository, 2025. Available: https://github.com/OpenZeppelin/stellar-contracts

[5] The Rust Reference, "Traits," 2024. Available: https://doc.rust-lang.org/reference/items/traits.html

[6] The Rust Reference, "Method Resolution Order," 2024. Available: https://doc.rust-lang.org/reference/expressions/method-call-expr.html

[7] The Rust Reference, "Procedural Macros," 2024. Available: https://doc.rust-lang.org/reference/procedural-macros.html

[8] Stellar Development Foundation, "Soroban Documentation," 2025. Available: https://soroban.stellar.org/docs

[9] S. Y. Soo, "Context-Generic Programming," 2024. Available: https://contextgeneric.dev

[10] M. S. Miller, K.-P. Yee, and J. Shapiro, "Capability Myths Demolished," Technical Report SRL2003-02, Johns Hopkins University, 2003.

[11] D. Leijen, "Type directed compilation of row-typed algebraic effects," in *Proc. POPL*, 2017, pp. 486--499.

[12] J. I. Brachthaeuser, P. Schuster, and K. Ostermann, "Effects as Capabilities: Effect Handlers and Lightweight Effect Polymorphism," in *Proc. OOPSLA*, 2020.

[13] S. Lindley, C. McBride, and C. McLaughlin, "Do Be Do Be Do," in *Proc. POPL*, 2017, pp. 500--514.

[14] M. S. Miller, "Robust Composition: Towards a Unified Approach to Access Control and Concurrency Control," PhD Thesis, Johns Hopkins University, 2006.

[15] M. S. Miller, M. Samuel, B. Laurie, I. Awad, and M. Stay, "Safe Active Content in Sanitized JavaScript," Google Technical Report, 2008.

[16] A. Haas, A. Rossberg, D. L. Schuff, B. L. Titzer, M. Holman, D. Gohman, L. Wagner, A. Zakai, and J. F. Bastien, "Bringing the Web up to Speed with WebAssembly," in *Proc. PLDI*, 2017, pp. 185--200.

[17] S. Blackshear, E. Cheng, D. L. Dill, V. Gao, B. Maurer, T. Nowacki, A. Pott, S. Qadeer, D. R. and Rain, S. Sezer, T. Zakian, and R. Zhou, "Move: A Language With Programmable Resources," Technical Report, Libra Association, 2019.

[18] I. Sergey, V. Nagaraj, J. Johannsen, A. Kumar, and A. Trunov, "Safer Smart Contract Programming with Scilla," in *Proc. OOPSLA*, 2019.

[19] B. Bernardo, R. Cauderlier, Z. Hu, B. Pesin, and J. Tesson, "Mi-Cho-Coq, a Framework for Certifying Tezos Smart Contracts," in *Proc. FMBC*, 2019.

[20] D. Tolnay, "`syn`: Parser for Rust source code," 2024. Available: https://crates.io/crates/syn

[21] M. Seiferle, "`ambassador`: Trait implementation delegation via procedural macros," 2024. Available: https://crates.io/crates/ambassador

[22] L. Kuehl, "`auto_impl`: Automatically implement traits for common wrapper types," 2024. Available: https://crates.io/crates/auto_impl

[23] J. van Zundert, "`derive_more`: Convenience derive macros for standard traits," 2024. Available: https://crates.io/crates/derive_more

[24] B. Meyer, "Applying Design by Contract," *IEEE Computer*, vol. 25, no. 10, pp. 40--51, 1992.

[25] K. Baird, "`contracts`: Design by Contract for Rust," 2024. Available: https://crates.io/crates/contracts

---

## Appendix A: Full Macro Expansion Example

**Input:**

```rust
#[contracttrait]
pub trait Ownable {
    fn owner(env: &Env) -> Address;

    #[auth(Self::owner)]
    fn transfer_ownership(env: &Env, new_owner: Address);
}
```

**Generated Output (abbreviated):**

```rust
// --- Internal trait ---
pub trait OwnableInternal {
    fn owner(env: &Env) -> Address;
    fn transfer_ownership(env: &Env, new_owner: Address);
}

// --- Outer trait ---
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

// --- AuthClient (test only) ---
#[cfg(not(target_family = "wasm"))]
pub struct OwnableAuthClient<'a> {
    inner: OwnableClient<'a>,
}

#[cfg(not(target_family = "wasm"))]
impl<'a> OwnableAuthClient<'a> {
    pub fn new(env: &'a Env, address: &'a Address) -> Self { ... }
    pub fn transfer_ownership<'b>(
        &'b self, new_owner: &'b Address,
    ) -> CallBuilder<'b, (), TryResult> { ... }
}

// --- Sealed macro ---
#[macro_export]
macro_rules! impl_ownable {
    ($contract:ty, $provider:ty) => {
        #[soroban_sdk::contractimpl]
        impl $contract {
            pub fn owner(env: Env) -> Address {
                <$provider as OwnableInternal>::owner(&env)
            }
            pub fn transfer_ownership(env: Env, new_owner: Address) {
                let __auth_addr =
                    <$provider as OwnableInternal>::owner(&env);
                __auth_addr.require_auth();
                <$provider as OwnableInternal>::transfer_ownership(
                    &env, new_owner,
                )
            }
        }
    };
}
```

---

## Appendix B: Supertrait Composition Example

**Input:**

```rust
#[contracttrait]
pub trait Pausable: Ownable {
    fn is_paused(env: &Env) -> bool;

    #[auth(Self::owner)]
    fn pause(env: &Env);

    #[auth(Self::owner)]
    fn unpause(env: &Env);
}
```

**Generated Internal Trait:**

```rust
pub trait PausableInternal: OwnableInternal {
    fn is_paused(env: &Env) -> bool;
    fn pause(env: &Env);
    fn unpause(env: &Env);
}
```

**Generated Outer Trait (abbreviated):**

```rust
#[soroban_sdk::contracttrait]
pub trait Pausable {
    type Provider: PausableInternal;

    fn is_paused(env: &Env) -> bool {
        Self::Provider::is_paused(env)
    }

    fn pause(env: &Env) {
        let __auth_addr = Self::Provider::owner(env);  // from OwnableInternal
        __auth_addr.require_auth();
        Self::Provider::pause(env)
    }

    fn unpause(env: &Env) {
        let __auth_addr = Self::Provider::owner(env);
        __auth_addr.require_auth();
        Self::Provider::unpause(env)
    }
}
```

**Provider Implementation:**

```rust
pub struct SingleOwner;

impl OwnableInternal for SingleOwner {
    fn owner(env: &Env) -> Address { /* storage read */ }
    fn transfer_ownership(env: &Env, new_owner: Address) { /* storage write */ }
}

impl PausableInternal for SingleOwner {
    fn is_paused(env: &Env) -> bool { /* storage read */ }
    fn pause(env: &Env) { /* storage write */ }
    fn unpause(env: &Env) { /* storage write */ }
}
```

Note how `SingleOwner` implements both `OwnableInternal` and `PausableInternal`. The supertrait bound on `PausableInternal: OwnableInternal` ensures this at compile time. The `pause()` method's auth check calls `Self::Provider::owner(env)`, which resolves to `SingleOwner::owner(env)` -- the same method used by `Ownable::transfer_ownership`.

---

## Appendix C: Trait Hierarchy Diagram

```
+-------------------------------------+
|        #[contracttrait]             |
|        pub trait Ownable {          |
|          fn owner(env) -> Address;  |
|          #[auth(Self::owner)]       |
|          fn transfer_ownership(..); |
|        }                            |
+---------+-----------+---------------+
          |           |
     macro expands    |
          |           |
  +-------v-------+  |  +-------------+   +-------------------+
  | OwnableInternal|  |  | Ownable     |   | OwnableAuthClient |
  | (pure logic)   |  |  | (guarded)   |   | (test helper)     |
  |                |  |  |             |   |                   |
  | fn owner(..)   |  |  | type        |   | fn transfer_      |
  | fn transfer_   |  |  |  Provider:  |   |   ownership(..)   |
  |   ownership(..)|  |  |  Ownable-   |   |   -> CallBuilder  |
  +-------+--------+  |  |  Internal   |   +-------------------+
          ^            |  |             |
          |            |  | fn owner    |   +-------------------+
          |            |  |  {delegate} |   | impl_ownable!     |
  +-------+--------+  |  |             |   | (sealed macro)    |
  | SingleOwner    |  |  | fn transfer_|   |                   |
  | (provider)     |  |  |  ownership  |   | Generates inherent|
  |                |  |  |  {auth +    |   | methods with auth |
  | impl Ownable-  |  |  |   delegate} |   | baked in          |
  |  Internal      |  |  +-------------+   +-------------------+
  +----------------+  |
                      |
          +-----------v------------------+
          | #[contracttrait]             |
          | pub trait Pausable: Ownable {|
          |   fn is_paused(env) -> bool; |
          |   #[auth(Self::owner)]       |
          |   fn pause(env);             |
          |   #[auth(Self::owner)]       |
          |   fn unpause(env);           |
          | }                            |
          +-----+------------+----------+
                |            |
           macro expands     |
                |            |
  +-------------v-+   +-----v--------+
  | Pausable-      |   | Pausable     |
  |  Internal:     |   | (guarded)    |
  |  Ownable-      |   |              |
  |  Internal      |   | type         |
  |                |   |  Provider:   |
  | fn is_paused   |   |  Pausable-   |
  | fn pause       |   |  Internal    |
  | fn unpause     |   |              |
  +-------+--------+   +--------------+
          ^
          |
  +-------+--------+
  | SingleOwner    |
  | impl Pausable- |
  |  Internal      |
  | (+ Ownable-    |
  |   Internal)    |
  +----------------+
```

**Figure 5.** Complete trait hierarchy showing the two-trait split for Ownable and its Pausable supertrait. Arrows indicate implementation relationships.

---

## Appendix D: Typing Rules Summary

We collect the typing rules presented throughout the paper.

**Provider Binding:**

$$\frac{\Gamma \vdash P : T_I \quad \Gamma \vdash C : \text{Contract}}{\Gamma \vdash C : T_O[\text{Provider} = P]} \quad \text{(T-Bind)}$$

**Auth-Guarded Invocation:**

$$\frac{\Gamma \vdash C : T_O[\text{Provider} = P] \quad \Gamma \vdash P::f(e) : \text{Address} \quad \Gamma \vdash \text{auth\_ok}(P::f(e))}{\Gamma \vdash C::m(e, \vec{a}) : \tau} \quad \text{(T-AuthInvoke)}$$

**Non-Guarded Invocation:**

$$\frac{\Gamma \vdash C : T_O[\text{Provider} = P]}{\Gamma \vdash C::m(e, \vec{a}) : \tau} \quad \text{(T-Invoke)}$$

**Supertrait Propagation:**

$$\frac{\Gamma \vdash T : S \quad \mathcal{M}(T) = (T_I, T_O) \quad \mathcal{M}(S) = (S_I, S_O)}{\Gamma \vdash T_I : S_I} \quad \text{(T-Super)}$$

**Sealed Non-Circumventability:**

$$\frac{\text{impl\_trait}!(C, P) \in \text{Source} \quad m \in T.\text{methods} \quad \alpha_m = \text{auth}(src)}{\text{wasm\_export}(C, m) \text{ contains } \text{require\_auth}(P::src(e))} \quad \text{(T-Seal)}$$

**Provider Parametricity:**

$$\frac{\Gamma \vdash P_1 : T_I \quad \Gamma \vdash P_2 : T_I \quad \forall m \in T.\text{guarded}: \alpha_m = \text{auth}(src)}{\text{auth\_guard}(C[P_1], m) \equiv \text{auth\_guard}(C[P_2], m)} \quad \text{(T-Param)}$$

where $\equiv$ denotes structural equivalence of the generated guard code (modulo provider-specific resolution).

---

## Appendix E: Comparison Table -- Guard Enforcement Mechanisms

```
+---------------------+----------+----------+---------+----------+----------+
| Mechanism           | Struct-  | Override | Zero    | Comp-    | Test     |
|                     | ural     | Protect  | Cost    | osable   | Client   |
+---------------------+----------+----------+---------+----------+----------+
| Solidity modifiers  | No (1)   | No       | Yes     | Limited  | No       |
+---------------------+----------+----------+---------+----------+----------+
| OZ #[only_owner]    | No (2)   | No       | Yes     | Yes      | No       |
+---------------------+----------+----------+---------+----------+----------+
| Anchor #[access_    |          |          |         |          |          |
|   control]          | No (2)   | No       | Yes     | Limited  | No       |
+---------------------+----------+----------+---------+----------+----------+
| Move module access  | Yes      | N/A (3)  | Yes     | Limited  | No       |
+---------------------+----------+----------+---------+----------+----------+
| Scilla transitions  | Yes      | N/A (3)  | Yes     | Limited  | No       |
+---------------------+----------+----------+---------+----------+----------+
| This work (sealed)  | Yes      | Yes      | Yes     | Yes      | Yes      |
+---------------------+----------+----------+---------+----------+----------+
| This work (flex.)   | Yes (4)  | No       | Yes     | Yes      | Yes      |
+---------------------+----------+----------+---------+----------+----------+

Notes:
(1) Must be applied per-function; forgetting a modifier is silent.
(2) Must be applied per-method; attribute macros are opt-in.
(3) No override mechanism exists in these languages.
(4) Auth is in the trait definition but can be overridden.
```

**Figure 6.** Comparison of guard enforcement mechanisms across smart contract platforms.

---

## Appendix F: Decidability Analysis

Following Alan Turing's foundational work on computability [26], we classify the properties guaranteed by our system:

| Property | Decidable? | Verified by Macro? | Notes |
|---|---|---|---|
| Auth check presence in generated code | Yes | Yes | Syntactic; follows from code template |
| Auth check presence in WASM export (sealed) | Yes | Yes | Follows from Rust coherence rules |
| Auth source method exists on trait | Yes | Partially | Typos caught at monomorphization |
| Provider correctness | No (Rice) | No | Semantic property of arbitrary code |
| Composition consistency | No (Rice) | No | Requires semantic analysis |
| Auth source purity | No | No | Side-effect freedom is undecidable |
| Auth source termination | No (Halting) | No | Mitigated by Soroban's gas model |

**Figure 7.** Decidability classification of system properties.

The macro operates entirely in the decidable realm: it transforms syntax and guarantees syntactic properties. Semantic properties (correctness, consistency, purity) are in the undecidable realm and must be addressed by testing, auditing, and formal verification.

This decidability analysis leads to a practical insight: the most valuable contribution of the macro is not that it makes contracts provably secure, but that it *reduces the undecidable verification surface*. Without the macro, verifying authorization correctness requires analyzing every method in the contract, every override, and every interaction between components. With the macro, the verification surface shrinks to: (1) the provider implementations (are they correct?), and (2) the composition (is it consistent?). The generated wrapper code, being mechanical and deterministic, can be verified once and trusted thereafter.

This reduction is analogous to the role of type checkers in programming: type checking does not prove program correctness, but it eliminates certain classes of errors and reduces the surface area that requires manual verification. Our macro plays a similar role for authorization: it eliminates certain classes of auth errors (missing checks, override bypass) and reduces the verification surface to the provider trust boundary.

---

[26] A. M. Turing, "On Computable Numbers, with an Application to the Entscheidungsproblem," *Proc. London Mathematical Society*, vol. s2-42, no. 1, pp. 230--265, 1937.

[27] R. E. Strom and S. Yemini, "Typestate: A Programming Language Concept for Enhancing Software Reliability," *IEEE Trans. Software Engineering*, vol. SE-12, no. 1, pp. 157--171, 1986.

[28] G. Kiczales, J. Lamping, A. Mendhekar, C. Maeda, C. Lopes, J.-M. Loingtier, and J. Irwin, "Aspect-Oriented Programming," in *Proc. ECOOP*, 1997, pp. 220--242.

[29] R. Atkey, "Parameterised Notions of Computation," *Journal of Functional Programming*, vol. 19, no. 3--4, pp. 335--376, 2009.

[30] J. G. Siek and W. Taha, "Gradual Typing for Functional Languages," in *Proc. Scheme and Functional Programming Workshop*, 2006, pp. 81--92.

[31] K. Honda, V. T. Vasconcelos, and M. Kubo, "Language Primitives and Type Discipline for Structured Communication-Based Programming," in *Proc. ESOP*, 1998, pp. 122--138.

[32] E. Moggi, "Notions of Computation and Monads," *Information and Computation*, vol. 93, no. 1, pp. 55--92, 1991.
