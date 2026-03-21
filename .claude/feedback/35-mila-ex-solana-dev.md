# Review: soroban-sdk-tools -- Cross-Chain Architecture Comparison

**Reviewer:** Mila -- Former Solana core developer, freelance blockchain architect
**Date:** 2026-03-21
**Materials Reviewed:** oz-comparison.md, blog-post-composable-contracts.md, trait-test example, contract.rs macro

---

## Overall Impression

I spent three years on Solana's runtime team and another two building with
Anchor. I now freelance across chains -- Solana, Ethereum L2s, CosmWasm,
and increasingly Soroban. When I evaluate a composability framework, I
compare it against every other system I have worked with, because the
lessons from one chain apply to all of them.

`soroban-sdk-tools`' `#[contracttrait]` macro is the most interesting
composability innovation I have seen since Anchor's `#[derive(Accounts)]`.
The two-trait pattern with structural auth enforcement is genuinely novel.
But it also has blind spots that are visible when you compare it to what
other ecosystems have learned the hard way.

---

## Comparison with Anchor (Solana)

### What Anchor Got Right That soroban-sdk-tools Should Study

1. **Account constraints as types** -- Anchor's `#[account(constraint = ...)]`
   system embeds validation logic directly into the account struct. When
   you write `#[account(has_one = authority)]`, the framework generates
   the validation check. This is similar to `#[auth(Self::owner)]` but
   more general -- it validates relationships between accounts, not just
   authorization.

   soroban-sdk-tools should consider generalizing `#[auth]` to `#[check]`
   or `#[guard]` with arbitrary validation expressions:

   ```rust
   #[guard(Self::is_not_paused)]
   #[auth(Self::owner)]
   fn admin_action(env: &Env);
   ```

   This would subsume OZ's `#[when_not_paused]` macro and make the
   structural enforcement pattern universal, not just for auth.

2. **IDL generation** -- Anchor generates an Interface Description Language
   (IDL) file from the program definition. This IDL drives client SDK
   generation in TypeScript, Python, and other languages. The blog post
   mentions XDR spec metadata for TypeScript clients, but this should be
   a first-class concern with its own dedicated tooling.

3. **Error handling with error_code!** -- Anchor's error code system
   (`#[error_code]`) automatically assigns numeric codes and generates
   error descriptions. The `#[scerr]` macro mentioned in the blog post
   seems similar, but without seeing its implementation, I cannot compare
   directly. The auto-chaining via const generics sounds promising.

4. **Program testing with bankrun** -- Anchor's test framework evolved from
   `solana-test-validator` (slow, full node) to `bankrun` (fast, in-process).
   The `AuthClient` is a step in this direction, but soroban-sdk-tools
   should ensure the testing story goes beyond auth. Integration tests
   between composed contracts are the real pain point.

### What Anchor Got Wrong That soroban-sdk-tools Avoids

1. **No sealed pattern** -- Anchor programs can always override instruction
   handlers. There is no sealed equivalent. This has led to real security
   incidents where custom handlers bypassed framework-level checks.
   soroban-sdk-tools' `impl_ownable!` is a genuine security improvement.

2. **Account ownership model** -- Anchor's account ownership model
   (programs own accounts) creates composability friction. Soroban's
   instance storage model is simpler and more composable. The provider
   pattern benefits from this simpler storage model.

3. **CPIs are painful** -- Cross-program invocations in Anchor require
   manual CPI context construction. Soroban's contract invocation model
   is simpler, and the `#[contracttrait]` macro could leverage this for
   cross-contract composition.

---

## Comparison with CosmWasm

### CosmWasm's Message-Based Composition

CosmWasm composes contracts through message passing:

```rust
// CosmWasm: explicit message types
#[cw_serde]
pub enum ExecuteMsg {
    TransferOwnership { new_owner: String },
    Pause {},
}
```

Every contract interaction is a message. Composition happens by sending
messages between contracts. Auth is validated per-message in the handler.

soroban-sdk-tools' approach is fundamentally different: composition
happens at the TYPE level (trait implementation + provider), not the
MESSAGE level. This has several implications:

1. **Type safety** -- soroban-sdk-tools' approach is type-checked at
   compile time. CosmWasm's message approach is stringly typed (messages
   are JSON). Advantage: soroban-sdk-tools.

2. **Composability granularity** -- CosmWasm can compose at the message
   level (one contract sends a message to another). soroban-sdk-tools
   composes at the trait level (one contract implements multiple traits).
   These are different granularities, and both are useful. soroban-sdk-tools
   should document how to achieve message-level composition (cross-contract
   calls) in addition to trait-level composition.

3. **Upgradability** -- CosmWasm contracts can be migrated (upgraded)
   through a migrate message. The sealed pattern in soroban-sdk-tools
   makes this more complex -- if auth is sealed, how do you upgrade the
   contract? The documentation should address this.

### CosmWasm's Sylvia Framework

The Sylvia framework for CosmWasm is the closest parallel to
soroban-sdk-tools:

```rust
// Sylvia: trait-based composition for CosmWasm
#[interface]
pub trait Ownable {
    #[msg(query)]
    fn owner(&self, ctx: QueryCtx) -> StdResult<OwnerResp>;

    #[msg(exec)]
    fn transfer_ownership(&self, ctx: ExecCtx, new_owner: String) -> StdResult<Response>;
}
```

Sylvia generates message types, handlers, and query clients from trait
definitions. It is the CosmWasm equivalent of what soroban-sdk-tools does
for Soroban.

Key difference: Sylvia does NOT have structural auth enforcement. Auth is
still manual in the handler. soroban-sdk-tools' `#[auth]` attribute is
an advancement over Sylvia.

Key similarity: Both use traits as the composition primitive. Both generate
boilerplate from trait definitions. Both face the challenge of making
generated code auditable.

---

## Comparison with Solidity/OpenZeppelin (EVM)

The blog post and comparison document cover this well, but I want to add
the cross-chain perspective.

### Solidity's Inheritance Model

Solidity uses multiple inheritance with linearized resolution (C3
linearization). This allows:

```solidity
contract MyToken is ERC20, Ownable, Pausable {
    function transfer(address to, uint256 amount) public override whenNotPaused {
        super.transfer(to, amount);
    }
}
```

The `override` keyword and `super` calls create a composition model that
is flexible but dangerous (the diamond problem, accidental overrides).

soroban-sdk-tools' two-trait pattern avoids the diamond problem entirely
because there is no inheritance -- there is trait implementation with
delegation to a provider. This is a clean design.

### What EVM OZ Does That Soroban OZ Does Not

1. **Proxy/upgrade patterns** -- EVM OZ has extensive proxy patterns
   (TransparentProxy, UUPS, Beacon). Soroban's upgrade model is different
   (WASM replacement), but the composability framework should address it.

2. **Governor/voting** -- EVM OZ has a full governance framework. The
   provider pattern would be excellent for governance (swap `SimpleVote`
   for `QuadraticVote` for `ConvictionVote`).

3. **ERC standard compliance** -- EVM OZ implements all ERCs. Soroban's
   equivalent standards (SEPs) should be implemented as composable traits
   with provider-based DI.

---

## Deep Dive: The Macro Implementation

### contract.rs Analysis

The macro implementation is clean and well-structured. Specific observations:

1. **`AuthSource` enum** (line 49-54) -- Two variants: `ProviderMethod`
   and `Param`. This is minimal and correct. I would suggest adding a
   third variant for future extensibility:

   ```rust
   /// Multi-party auth (all parties must authorize)
   MultiAuth(Vec<AuthSource>),
   ```

   Multi-sig requirements at the trait level would be powerful.

2. **`extract_method_info`** (line 142-191) -- This function assumes the
   first parameter is always `env: &Env`. This is a Soroban convention
   but not enforced by the SDK. If someone writes a method without an env
   parameter, the function will misinterpret the first parameter. Consider
   adding explicit detection and a clear error message.

3. **`generate_outer_trait`** (line 235-303) -- The generated outer trait
   uses `type Provider: {Trait}Internal` as an associated type. This is
   the right pattern, but I notice it does not support generic providers
   (providers with type parameters). If I want:

   ```rust
   pub struct TimelockOwner<const DELAY: u32>;
   ```

   The current macro may not support this. Const generics in providers
   could enable powerful configuration patterns.

4. **`generate_sealed_impl_macro`** (line 329-386) -- The sealed macro
   generates `#[soroban_sdk::contractimpl] impl $contract { ... }`. This
   means the sealed methods are inherent methods on the contract type.
   Question: what happens if two sealed macros generate methods with the
   same name? E.g., both `impl_ownable!` and `impl_pausable!` generating
   a method called `owner()`. Name collisions should be handled or
   documented.

5. **`to_snake_case`** (line 389-402) -- This handles simple PascalCase
   to snake_case conversion but does not handle acronyms correctly.
   `HTTPServer` would become `h_t_t_p_server` instead of `http_server`.
   Consider using a crate like `heck` for robust case conversion.

6. **`generate_auth_client`** (line 409-463) -- The `#[cfg(not(target_family
   = "wasm"))]` guard is correct -- AuthClient should not be in the WASM
   binary. But the `extern crate alloc as #alloc_alias` pattern is
   fragile. Multiple AuthClients in the same module will generate multiple
   `extern crate` statements with different aliases. This works but is
   unusual. Consider a single shared alloc import.

7. **`extract_return_types`** (line 577-609) -- This function handles
   `Result<T, E>` unpacking. But it does not handle nested Results
   (`Result<Result<T, E1>, E2>`) or type aliases (`type MyResult<T> =
   Result<T, MyError>`). For now this is fine, but as usage grows,
   edge cases will appear.

---

## Cross-Chain Composability Lessons

### Lesson 1: Upgrade Paths Are Critical

Every chain has learned that contracts need to be upgradeable. The sealed
pattern makes this more complex. The documentation should explicitly
address:

- How to upgrade a contract that uses `impl_ownable!`
- How to migrate from one provider to another in a deployed contract
- How to add new traits to an existing contract via upgrade

### Lesson 2: Cross-Contract Calls Break Assumptions

When contract A calls contract B, the auth context changes. If contract A
has `#[auth(Self::owner)]` on a method, and that method calls contract B,
contract B does not inherit A's auth context. This is well-understood in
Soroban (auth contexts are explicit), but the documentation should address
cross-contract composition patterns.

### Lesson 3: Standards Must Be Strict

ERC-20 succeeded because it was a strict standard -- every implementation
must have the same function signatures. The provider pattern allows
variation in IMPLEMENTATION but not in INTERFACE. This is correct and
important. The blog post should emphasize this: **the trait is the
standard, the provider is the implementation.**

### Lesson 4: Testing Must Cover Edge Cases

Every chain has had security incidents from edge cases in composed
contracts. The `AuthClient` is excellent for testing the happy path
(authorized caller succeeds) and the basic failure path (unauthorized
caller fails). But it should also support:

- Re-entrancy testing (Soroban prevents this at the runtime level, but
  the test framework should acknowledge it)
- State transition testing (after pause, before pause, during transfer)
- Gas/resource limit testing
- Concurrent auth testing (two authorized calls in the same transaction)

---

## Architecture Recommendations

### 1. Generalize #[auth] to #[guard]

The `#[auth]` attribute is a specific case of a general guard pattern.
Other useful guards:

```rust
#[guard(Self::is_not_paused)]   // state check
#[guard(Self::is_initialized)]  // lifecycle check
#[auth(Self::owner)]            // authorization check
#[guard(Self::has_role, "admin")]  // role check
```

All of these generate a check before delegation to the provider. The
framework should support arbitrary guard expressions.

### 2. Support Event Generation

The blog post acknowledges that OZ handles events better. The macro should
generate events automatically:

```rust
#[contracttrait]
pub trait Ownable {
    fn owner(env: &Env) -> Address;

    #[auth(Self::owner)]
    #[emit(OwnershipTransferred { previous_owner, new_owner })]
    fn transfer_ownership(env: &Env, new_owner: Address);
}
```

The generated outer trait would emit the event after successful delegation
to the provider.

### 3. Add Provider Composition

Currently, a single provider implements a single Internal trait. For complex
contracts, providers should compose:

```rust
pub struct ComposedProvider;
impl OwnableInternal for ComposedProvider { /* delegate to StorageOwner */ }
impl PausableInternal for ComposedProvider { /* delegate to StoragePausable */ }
impl FungibleTokenInternal for ComposedProvider { /* custom logic */ }
```

This works today but requires boilerplate. A `#[delegate]` macro could
reduce it:

```rust
#[delegate(OwnableInternal to StorageOwner)]
#[delegate(PausableInternal to StoragePausable)]
pub struct ComposedProvider;
```

### 4. Cross-Contract Trait Verification

When contract A calls contract B, how does A verify that B implements a
specific trait? In Solidity, ERC-165 (interface detection) solves this.
Soroban should have an equivalent, and the `#[contracttrait]` macro could
generate the verification logic:

```rust
// Generated: does the contract at `addr` implement Ownable?
Ownable::supports_trait(&env, &addr) -> bool
```

---

## Blog Post Feedback

The blog post is technically strong but misses the cross-chain perspective.
Adding a section on "Lessons from Other Chains" would strengthen it
significantly and appeal to the multi-chain developer audience.

The CGP (Context-Generic Programming) section is interesting but may
confuse readers who are not familiar with CGP. Consider moving it to an
appendix or separate deep-dive post.

The performance section ("Zero Overhead") should include benchmark numbers,
not just claims. Show the WASM binary sizes with and without the macro.
Show the gas costs compared to hand-written code. Numbers are more
convincing than assertions.

---

## Verdict

As a cross-chain architect, I see `soroban-sdk-tools` as a significant
innovation in the composability space. The two-trait pattern with structural
auth is genuinely novel -- I have not seen this pattern in Anchor, Sylvia,
or any EVM framework.

The macro implementation is clean and well-structured. The main risks are:

1. Limited generality (auth-only, not general guards)
2. No event generation
3. No cross-contract trait verification
4. Potential name collisions in sealed macros
5. Missing upgrade path documentation

These are all solvable problems. The foundation is solid. If the team
executes on the standard library and addresses the gaps identified above,
this could become the best composability framework in the blockchain
space, not just in Soroban.

**Rating:** 8/10 -- Genuinely novel architecture with clear execution
path. Would recommend to cross-chain development teams.

---

*"The best composability frameworks are the ones where the developer
cannot accidentally break the composition. soroban-sdk-tools achieves
this for auth. The next step is achieving it for everything."*
