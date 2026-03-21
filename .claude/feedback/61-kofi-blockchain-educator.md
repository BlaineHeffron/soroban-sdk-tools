# Review by Kofi -- Blockchain Educator, Universities Across Africa

## Reviewer Profile

I teach blockchain development at three universities across Ghana, Kenya, and Nigeria. My students range from second-year CS undergrads to professional developers seeking upskilling. I have spent five years watching students struggle with Solidity inheritance hierarchies. I was excited when Soroban chose Rust, because Rust's type system can encode invariants that Solidity cannot express. But new abstractions bring new teaching challenges. This review examines soroban-sdk-tools through the lens of curriculum design, learning progression, and the concrete confusion points I anticipate in the classroom.

---

## 1. Curriculum Placement: Where Does This Fit?

### Current Soroban Teaching Sequence (Typical 14-week course)

| Week | Topic |
|------|-------|
| 1-2 | Rust fundamentals, ownership, borrowing |
| 3-4 | Traits, generics, associated types |
| 5-6 | Soroban basics: #[contract], #[contractimpl], Env, storage |
| 7-8 | Token standards, cross-contract calls |
| 9-10 | Auth model, require_auth, MockAuth |
| 11-12 | Composition patterns (OZ-style) |
| 13-14 | Capstone project |

### Proposed Insertion Point

The two-trait pattern (`#[contracttrait]`) should be introduced at **Week 9**, replacing or augmenting the current auth module. Rationale: students already understand traits and generics by this point, and introducing structural auth before they learn the manual pattern means they never develop the bad habit of forgetting `require_auth()`.

However, this requires that **associated types** are thoroughly covered in Week 4. In my experience, associated types are where 40% of students hit a wall. The `type Provider` pattern will compound that confusion if the foundation is not solid.

### Recommendation

Add a "bridge lab" between Weeks 4 and 5 that uses associated types in a non-blockchain context (e.g., a shape renderer where `type Backend = SVG` or `type Backend = Canvas`). This builds the mental model before the blockchain-specific complexity arrives.

---

## 2. The Two-Trait Pattern as a Teaching Device

### What Works Beautifully

The separation of `OwnableInternal` (pure logic) and `Ownable` (auth-wrapped) maps perfectly to a pedagogical principle I use constantly: **separate concerns before combining them**.

In my current curriculum, I teach auth as an afterthought -- "now add `require_auth()` to this function." Students treat it like sprinkling salt on food. They do not understand it as a structural concern. The `#[auth(Self::owner)]` annotation changes this narrative fundamentally. Auth becomes a **design decision** made at the trait level, not a per-function afterthought.

I would teach this with a two-phase exercise:

**Phase 1: Write the Internal trait manually**
```rust
// Students implement OwnableInternal by hand
pub trait OwnableInternal {
    fn owner(env: &Env) -> Address;
    fn transfer_ownership(env: &Env, new_owner: Address);
}
```
Ask: "Where is the auth check? Who calls require_auth()?" Students realize: nobody does. This is intentionally logic-only.

**Phase 2: See the outer trait generated**
Show the generated `Ownable` trait with `require_auth()` injected. Ask: "Could you forget the auth check now?" The answer is no. The architecture prevents it.

This is a powerful "aha moment" that I rarely get to create in Solidity courses.

### What Will Confuse Students

1. **The word "Provider" is overloaded.** In web development (which many of my students know), "provider" means a React context provider, a cloud provider, or an auth provider (like OAuth). Here it means "the struct that implements the internal trait." I would prefer "Implementor" or "Backend" as a name, but I understand the CGP lineage.

2. **Two traits with similar names.** Students will write `impl Ownable for MyProvider` when they mean `impl OwnableInternal for MyProvider`. The compiler error will be opaque ("expected associated type `Provider`"). A custom diagnostic message would save hours of office-hours time.

3. **The `impl_ownable!` macro hides too much.** Students need to see what it generates at least once. I would add an `expand` subcommand or a doc page showing the exact expansion for each standard trait.

---

## 3. Lab Exercise Design

### Lab 1: "Build Your Own Ownership" (2 hours)

**Learning objective:** Understand the two-trait pattern by implementing it manually.

**Steps:**
1. Define `OwnableInternal` trait with `owner()` and `transfer_ownership()`.
2. Implement it for a `SimpleOwner` struct using instance storage.
3. Write a wrapper function that calls `require_auth()` before delegating.
4. Compare to the `#[contracttrait]` macro output.
5. Discussion: "What could go wrong if step 3 were optional?"

**Assessment:** Students must explain, in writing, why the two-trait pattern prevents auth bypass.

### Lab 2: "Provider Swap" (1.5 hours)

**Learning objective:** Understand dependency injection via `type Provider`.

**Steps:**
1. Start with `impl_ownable!(MyContract, SingleOwner)`.
2. Implement a `TimelockOwner` provider that stores a delay period.
3. Swap the provider: one line change.
4. Write tests verifying the timelock behavior.
5. Discussion: "How would you do this swap in OpenZeppelin's model?"

**Assessment:** Students submit both providers and a test that fails with `SingleOwner` but passes with `TimelockOwner`.

### Lab 3: "Supertrait Composition" (2 hours)

**Learning objective:** Compose `Pausable: Ownable` and understand cross-trait auth.

**Steps:**
1. Define `Pausable` with `#[auth(Self::owner)]` on `pause()` and `unpause()`.
2. Implement `PausableInternal` for `SingleOwner`.
3. Wire both traits to one contract.
4. Write a test that pauses the contract and verifies transfers are blocked.
5. Write a test that verifies only the owner can pause.

**Assessment:** Students must draw a diagram showing the trait hierarchy and auth flow.

### Lab 4: "AuthClient Testing" (1.5 hours)

**Learning objective:** Replace `mock_all_auths()` with precise auth testing.

**Steps:**
1. Write a test using `mock_all_auths()`. Intentionally remove `require_auth()` from a method. Note that the test still passes.
2. Rewrite using `OwnableAuthClient`. Note that the test now correctly fails.
3. Test auth failure: wrong address authorizing.
4. Discussion: "Why is `mock_all_auths()` dangerous for production code?"

**Assessment:** Students must write a test that catches an auth bug that `mock_all_auths()` would miss.

---

## 4. Student Confusion Points (Predicted)

### Confusion Point 1: "Where is my code running?"

Students struggle with the macro-generated boundary. They write `SingleOwner::transfer_ownership(env, new_owner)` in a `#[contractimpl]` block and do not understand why this bypasses auth. The mental model of "Internal = no auth, Outer = auth" needs to be reinforced with a visual diagram showing the call path:

```
Client call --> WASM export --> Outer trait (auth check) --> Internal trait (logic)
                                    ^^ this is the boundary
```

The blog post mentions this risk in passing ("A developer can call `{Trait}Internal` methods directly from any `#[contractimpl]` block, bypassing the auth wrapper"). This needs to be a red-box warning in the documentation, not a parenthetical.

### Confusion Point 2: "Why two traits instead of one?"

Students coming from Solidity are used to `modifier onlyOwner`. The Rust approach is more powerful but more abstract. I need a concrete analogy.

**My analogy:** "Think of a bank. The `Internal` trait is the vault -- it has the money and the ledger. The `Outer` trait is the teller window with the ID check. You can access the vault through the teller (safe) or you can walk into the vault directly (unsafe). The `impl_ownable!` macro welds the vault door shut so the only way in is through the teller."

This analogy works well because it maps to real-world experience my students have.

### Confusion Point 3: "What is `#[contractimpl(contracttrait)]`?"

The Soroban SDK already has `#[contracttrait]` as an attribute. This project redefines it in the `soroban_sdk_tools` namespace. Students will be confused about which one they are using. The macro source shows that the generated outer trait carries `#[soroban_sdk::contracttrait(...)]`, so the tool's `#[contracttrait]` is a superset. This should be stated explicitly and early in any teaching material.

### Confusion Point 4: "Sealed vs. Flexible -- when do I choose?"

The example code shows both `impl_ownable!` (sealed) and `#[contractimpl(contracttrait)] impl Ownable for ...` (flexible). Students need a decision flowchart:

- Are you building a standard contract with no custom auth? --> **Sealed** (`impl_ownable!`)
- Do you need timelocks, multi-sig threshold changes, or custom auth flows? --> **Flexible** (`#[contractimpl(contracttrait)]`)
- Are you unsure? --> **Start sealed.** You can always move to flexible later.

---

## 5. Assessment of Documentation Quality

### The Blog Post

**Strengths:**
- Excellent side-by-side comparisons with OZ.
- The security analysis of the override problem is compelling and well-explained.
- The CGP connection gives advanced students a path forward.

**Weaknesses for teaching:**
- Assumes familiarity with OZ's `stellar-contracts`. Most of my students have not used OZ on Stellar (some have used it on Ethereum).
- The "Try It Today" section jumps to `Cargo.toml` without showing project setup. Students need `cargo init`, `Cargo.toml` dependencies, and a full compilable example.
- No mention of error messages. When things go wrong (wrong Provider type, missing trait impl), what does the compiler say? Teaching materials must show error messages.

### The OZ Comparison Doc

**Strengths:**
- The comparison table is excellent for a slide deck.
- The "What OZ Does Better" section shows intellectual honesty, which builds trust.

**Weaknesses for teaching:**
- The Pausable comparison shows OZ requiring a `caller: Address` parameter while `soroban-sdk-tools` uses `Self::owner`. But these are different designs: OZ is checking if the caller is the owner, while `soroban-sdk-tools` is requiring the owner to authorize. Students need to understand this is not just syntactic sugar -- it is a different authorization model (caller-asserted vs. framework-checked).

### The Example Code

**Strengths:**
- Complete and compilable (I assume -- I have not tested it).
- Shows both ownership and pausability composition.
- Includes `AuthClient` tests.

**Weaknesses for teaching:**
- Uses `mock_all_auths()` in the first two tests, then switches to `AuthClient` in the last two. This sends a mixed message. If `AuthClient` is better, lead with it.
- No negative tests (expected failures). Students learn as much from seeing what fails as from seeing what succeeds.
- The `init` function is outside the trait system. This is realistic but confusing -- students will ask "why isn't initialization part of Ownable?"

---

## 6. Macro Code Review (contract.rs)

### Readability for Students

The macro code is well-structured with clear section comments. The `MethodInfo` and `ParamInfo` structs are good abstractions. However:

1. The `to_snake_case` function (line 389) is naive -- it does not handle consecutive uppercase letters (e.g., "HTTPServer" becomes "h_t_t_p_server" instead of "http_server"). For trait names like `RBAC` or `NFT`, this will produce surprising macro names.

2. The `extract_auth_attr` function only handles `Self::method` and bare parameter names. Students will try `#[auth(self.owner)]` (lowercase self, dot notation) and get a confusing error. A custom error message for this case would help.

3. The `extern crate alloc as __alloc_{TraitName}` pattern (line 415) is clever for avoiding conflicts, but students will see this in macro expansion output and be baffled. A comment in the generated code (if possible) would help.

### Correctness Concerns

1. The `env` parameter is assumed to be the first parameter and is hardcoded as `env` in `build_delegate_args` (line 309). If a developer names it `e` (as OZ does), the delegation will break. The code extracts the actual parameter pattern elsewhere -- this hardcoding should be replaced with the extracted name.

2. The `#[cfg(not(target_family = "wasm"))]` guard on `AuthClient` is correct -- it should not be in WASM builds. But the `extern crate alloc` alias is also behind this guard, which means the alloc crate is not available in WASM. This is fine for `AuthClient` but could be surprising if someone tries to reference the alias elsewhere.

---

## 7. Grading Rubric Implications

For my courses, I would update the grading rubric as follows:

| Criterion | Weight | Description |
|-----------|--------|-------------|
| Auth correctness | 25% | Uses `#[auth]` or sealed macros; no manual `require_auth()` in business logic |
| Provider design | 20% | Clean separation of concerns; provider is stateless and focused |
| Test quality | 25% | Uses `AuthClient` instead of `mock_all_auths()`; includes negative tests |
| Composition | 15% | Correct supertrait usage; no trait bound violations |
| Code clarity | 15% | Naming, documentation, module organization |

This shifts the weight from "does it compile and run" to "is the auth architecture correct." That is the right emphasis for production blockchain code.

---

## 8. Recommendations for the Project

### For Documentation

1. Add a "Getting Started for Educators" page with full project setup, lab exercises, and expected output.
2. Show compiler error messages for common mistakes.
3. Add a visual diagram of the two-trait call flow.
4. Lead examples with `AuthClient` tests, not `mock_all_auths()`.

### For the Macro

1. Fix the hardcoded `env` parameter name in `build_delegate_args`.
2. Add custom error messages for common `#[auth]` misuse (e.g., `self.owner`, `Owner` without `Self::`).
3. Consider a `#[contracttrait(explain)]` mode that emits the generated code as a doc comment or to a file for teaching purposes.

### For the Example

1. Add negative tests (wrong address authorizing, uninitialized contract).
2. Show initialization as part of a trait (e.g., `Initializable` trait).
3. Add a comment explaining why `init` is separate from `Ownable`.

### For the Ecosystem

1. Produce a 10-minute video walkthrough of the two-trait pattern.
2. Create a "cheat sheet" PDF comparing OZ patterns to `soroban-sdk-tools` patterns.
3. Partner with Stellar Development Foundation's educational programs to include this in official curricula.

---

## 9. Overall Assessment

This is the most teachable smart contract composition framework I have encountered in the Rust ecosystem. The two-trait pattern maps cleanly to pedagogical principles of separation of concerns. The `AuthClient` is a genuine improvement over `mock_all_auths()` for teaching correct auth testing.

The main risk is that the abstraction layer (macros, associated types, providers) adds cognitive load that may overwhelm students who are still building Rust fluency. The mitigation is careful curriculum sequencing and bridge exercises, as outlined above.

I would adopt this framework in my courses starting next semester, with the caveat that I need the documentation improvements listed above before I can do so responsibly.

**Rating: 8/10 for educational utility. Would be 9/10 with better error messages and a teaching guide.**

---

*Reviewed by Kofi, March 2026. Based on five years of blockchain education across three African universities.*
