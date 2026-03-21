# Review by Felix -- Sign Language Interpreter

*"Communication is not about the medium. It is about whether the message is received."*

---

## Overall Impression

I am a certified sign language interpreter (ASL, BSL, and International Sign) who has
spent the last three years exploring how DeFi can be made accessible to the Deaf and
hard-of-hearing community. My perspective on code is shaped by a career spent translating
between communication modalities -- spoken language to visual language, abstract concepts
to embodied expression, technical jargon to plain meaning.

When I review code, I ask: can a developer who thinks visually, who processes information
spatially rather than linearly, understand what is happening here? Can the error messages
be interpreted without hearing? Can the abstractions be explained without relying on
auditory metaphors?

This review focuses on accessibility -- not just for end users, but for developers
themselves.

---

## Visual Comprehension: How the Code Reads Spatially

### The Two-Trait Pattern: A Visual Metaphor

The `#[contracttrait]` macro transforms one trait into two: Internal and External. In
sign language, we have a similar concept called **classifiers** -- handshapes that
represent categories of objects or actions. A classifier for a person is different from
a classifier for a vehicle, but both can interact in the same signed narrative.

The Internal trait is a classifier for "business logic." The Outer trait is a classifier
for "public interface." When a developer sees both, they need to understand the
relationship between them.

**Visual assessment:** The current code layout in the example file
(`examples/trait-test/src/lib.rs`) places the trait definition, provider implementation,
and contract wiring in the same file. This is good for spatial comprehension -- everything
is visible in one scroll.

However, the GENERATED code is invisible. A developer using `#[contracttrait]` sees:

```rust
#[contracttrait]
pub trait Ownable {
    fn owner(env: &Env) -> Address;
    #[auth(Self::owner)]
    fn transfer_ownership(env: &Env, new_owner: Address);
}
```

And must mentally reconstruct the generated `OwnableInternal`, `Ownable` (outer),
`OwnableAuthClient`, and `impl_ownable!`. This is a significant cognitive load for any
developer, but especially for visual thinkers who benefit from seeing the complete
picture.

**Recommendation:** Generate a `cargo doc` page or a development-mode expansion that
shows the full generated code. Tools like `cargo expand` exist for this purpose, but
the friction should be minimal. Consider a `#[contracttrait(debug)]` mode that prints
the generated code as a compile-time warning, so developers can see exactly what they
are working with.

### The Blog Post: Textual Overload

The blog post is 383 lines of mostly text. For a visual learner, this is like listening
to a 20-minute monologue in a language you partially understand. The code blocks provide
relief, but the surrounding text is dense with abstract concepts.

In sign language interpretation, we use **visual vernacular** -- cinematic techniques
adapted for signing. We show, we demonstrate, we spatialize. The blog post could benefit
from:

1. **Diagrams:** A visual showing the transformation from one trait to two traits. Boxes
   and arrows. The two-trait structure is spatial -- it should be shown spatially.

2. **Side-by-side comparisons:** The OZ vs. sdk-tools comparison is done textually (show
   one, then the other). A true side-by-side (two columns) would be more visually
   effective. The comparison document does this with the table at the end, which is good,
   but the code blocks before the table are sequential, not parallel.

3. **Progressive disclosure:** Start with the simplest example (Ownable with one method),
   show the full expansion, then build up to composition. Currently, the blog post starts
   with the full Ownable (2 methods) and immediately introduces the generated code. A
   single-method example first would reduce cognitive load.

### The Macro Code: Visual Structure Assessment

The `contract.rs` file is 727 lines organized into clear sections with banner comments:

```
// -----------------------------------------------------------------------------
// Auth attribute parsing
// -----------------------------------------------------------------------------
```

This is excellent for visual navigation. The horizontal rules create visual landmarks
that a developer can use to orient themselves in the file. This is analogous to how
sign language uses space -- establishing reference points in the signing space that
subsequent signs can refer to.

The function names are descriptive and consistent:
- `extract_auth_attr` -- extracts
- `strip_auth_attrs` -- strips
- `generate_internal_trait` -- generates
- `generate_outer_trait` -- generates
- `generate_auth_client` -- generates
- `generate_sealed_impl_macro` -- generates

The verb-first naming convention creates a visual pattern: scan the function list and
immediately understand the action. Good.

---

## Error Message Accessibility

### Current Error Messages

The macro generates two error messages:

1. `"expected \`Self::method_name\` or a parameter name"` (line 69, 81)
2. `"expected a simple identifier pattern"` (line 165)

**Accessibility assessment:**

These messages are technical and assume familiarity with Rust syntax terminology. For a
developer who is not a native English speaker, or who processes text differently, these
messages need to be unpacked.

"Expected `Self::method_name` or a parameter name" -- what does "expected" mean here?
Expected by whom? For what purpose? In sign language interpretation, we always make the
subject and context explicit.

**Improved messages:**

```
"The #[auth(...)] attribute requires either:
  - Self::method_name  (to resolve the auth address from a provider method)
  - param_name         (to use a function parameter as the auth address)
Example: #[auth(Self::owner)] or #[auth(caller)]"
```

This provides:
1. Context (what was being parsed)
2. Options (what is valid)
3. Examples (concrete instances)

The improved message is longer but clearer. For screen readers (used by some Deaf
developers who prefer text), the structured format is easier to parse.

### Runtime Error Messages

The example's provider uses:

```rust
.expect("not initialized")
```

This is the most common error a user will encounter (calling a method before `init`), and
the error message is two words. No context, no guidance, no recovery suggestion.

**Improved message:**

```rust
.expect("Contract owner not initialized. Call init(owner_address) first.")
```

This tells the developer:
1. WHAT is not initialized (the contract owner)
2. HOW to fix it (call init)
3. WHAT to pass (an owner address)

### Error Codes vs. Error Messages

The blog post mentions `#[scerr]` for composable error handling with auto-chained error
codes. Error CODES are more accessible than error MESSAGES for some use cases:

1. Error codes are language-independent (important for non-English-speaking developers)
2. Error codes can be mapped to visual indicators (colors, icons, patterns)
3. Error codes are machine-parseable for accessible tooling

**Recommendation:** Generate both error codes AND descriptive messages. The code should
be the primary identifier, and the message should be the human-readable explanation. This
dual-format approach supports both programmatic and human consumption.

---

## Visual Programming: What the Macro Could Enable

### The Concept

Sign language is a visual-spatial language. Deaf developers often think about code
spatially -- relationships between components are understood as spatial relationships, not
linear text sequences. The `#[contracttrait]` macro, which transforms one definition
into multiple related artifacts, is a fundamentally spatial operation.

Imagine a visual development environment where:

1. The trait definition is a box in the center
2. The Internal trait is a box to the left (internal)
3. The Outer trait is a box to the right (external)
4. The AuthClient is a box below (testing)
5. Arrows show the relationships (delegation, auth enforcement, composition)

This would make the macro's behavior immediately understandable to visual thinkers. No
text needed -- the spatial layout IS the explanation.

### Practical Steps

1. **Generate a Mermaid diagram** alongside the code. The macro knows the trait structure;
   it could output a comment with a Mermaid diagram:

```rust
// Generated structure (paste into Mermaid viewer):
//
// graph LR
//   Ownable[Ownable\nouter trait] -->|delegates to| OwnableInternal[OwnableInternal\ninner trait]
//   OwnableInternal -->|implemented by| SingleOwner
//   Ownable -->|tested via| OwnableAuthClient
//   Ownable -->|sealed by| impl_ownable!
```

2. **Color-code generated code** in IDE plugins. If an IDE can distinguish macro-generated
   code from hand-written code, visual thinkers can quickly identify what is auto-generated
   vs. what they need to maintain.

3. **Generate ASCII art** in doc comments showing the trait relationship:

```
// +-----------------+     +-----------+
// | OwnableInternal | <-- | Ownable   |
// | (business logic) |     | (auth +   |
// | - owner()        |     |  dispatch)|
// | - transfer_      |     +-----------+
// |   ownership()    |           |
// +-----------------+     +-----+------+
//        ^                | impl_      |
//        |                | ownable!   |
//   SingleOwner           | (sealed)   |
//                         +------------+
```

---

## Non-Text Communication in Smart Contracts

### Events as Visual Signals

The blog post notes that OZ emits events for state changes while soroban-sdk-tools does
not (yet). Events are the smart contract's communication channel -- they are how the
contract "speaks" to the outside world.

For accessible DeFi, events should be designed as structured, machine-readable signals
that can be rendered in any modality:

1. **Text:** "Ownership transferred from Alice to Bob"
2. **Visual:** A dashboard icon changes from one avatar to another
3. **Haptic:** A vibration pattern on a mobile device
4. **Sign language avatar:** An animated character signs the event

All of these require the same structured data:

```rust
Event::OwnershipTransferred {
    from: old_owner,
    to: new_owner,
    timestamp: ledger_timestamp,
}
```

The `#[contracttrait]` macro knows which methods are auth-protected and what state they
change. It could generate standardized events for every state-changing method, providing
the structured data needed for multi-modal rendering.

**Recommendation:** Generate events automatically for every `#[auth]`-protected method.
The event should include:

- The method name (as a Symbol)
- The authorized address
- All method parameters
- The ledger sequence number

This provides enough data for any front-end to render the event in any modality.

### Error Feedback Loops

When a transaction fails due to auth failure, the user sees... nothing. Or a cryptic
error code. For Deaf users who rely on visual feedback, the absence of clear error
communication is a significant barrier.

The `AuthClient` provides excellent DEVELOPER feedback (tests pass or fail). But END USER
feedback is not addressed. Consider:

1. **Structured error responses** that front-ends can render visually
2. **Error categories** (auth failure, state error, input error) that map to distinct
   visual treatments
3. **Recovery suggestions** embedded in error data (not just error codes)

---

## Accessibility of the Documentation

### The Comparison Document

The comparison document uses a table format that is screen-reader friendly:

```markdown
| Aspect | OpenZeppelin | soroban-sdk-tools |
|--------|-------------|-------------------|
```

Tables are accessible when they have proper headers and consistent structure. This table
has both. Good.

However, the code blocks in the comparison are not annotated with language identifiers
in a way that assistive technology can distinguish "this is Rust code for OZ" from "this
is Rust code for sdk-tools." Adding comments at the top of each code block would help:

```rust
// OZ approach: ~80 lines across 3 files
```

vs.

```rust
// sdk-tools approach: ~35 lines in 1 file
```

These comments exist in some blocks but not all.

### The Blog Post

The blog post uses heading levels consistently (##, ###), which is excellent for screen
reader navigation. Headings are landmarks -- they let a user jump between sections
without reading everything linearly.

The inline code formatting (backticks) is used consistently for identifiers. This is
important for screen readers, which pronounce backticked text differently from regular
text.

**Issue:** The blog post has no `alt` text for any visual concept. There are no images
(which is fine), but there are also no text-based descriptions of spatial relationships.
The sentence "the macro generates a two-trait structure" assumes the reader can mentally
visualize a "structure." For some readers, a concrete description would help: "the macro
generates two separate traits from your single trait definition -- one for business logic
and one for auth-enforced public methods."

---

## Recommendations for Accessible DeFi

### 1. Generate Structured Events

Every `#[auth]` method should emit a structured event that front-ends can render in any
modality. This is the single most impactful accessibility improvement.

### 2. Improve Error Messages

Error messages should be:
- Descriptive (what went wrong)
- Actionable (how to fix it)
- Structured (parseable by machines for multi-modal rendering)
- Language-independent (error codes + structured data, not just English text)

### 3. Provide Visual Documentation

The macro's behavior should be documented with diagrams, not just text. Mermaid diagrams
in doc comments, ASCII art in code comments, or a generated visualization tool.

### 4. Support Visual Development Tools

The macro could generate metadata (as JSON or structured comments) that visual
development tools can consume to render the trait composition graphically. This would
make the framework accessible to developers who think visually.

### 5. Test Accessibility of Generated Interfaces

When the macro generates a trait that becomes a WASM export, the method names become
the public API. These names should be:
- Descriptive (not abbreviated)
- Consistent (verb_noun pattern)
- Screen-reader friendly (no ambiguous abbreviations)

The current naming (`owner`, `transfer_ownership`, `is_paused`, `pause`, `unpause`) is
excellent. Short, clear, consistent. Keep this standard.

### 6. Consider a "Plain Language" Mode

For non-technical users interacting with smart contracts through front-ends, generate
human-readable descriptions of each method:

```rust
/// Plain language: "See who currently controls this contract"
fn owner(env: &Env) -> Address;

/// Plain language: "Change who controls this contract (requires current controller's permission)"
#[auth(Self::owner)]
fn transfer_ownership(env: &Env, new_owner: Address);
```

These descriptions could be embedded in contract metadata and used by accessible
front-ends.

---

## The Bigger Picture

The Deaf community has historically been excluded from financial systems. Banks design
for hearing customers. ATMs have audio prompts but poor visual alternatives. Customer
service is phone-based. DeFi has the potential to be different -- it is fundamentally
digital, fundamentally visual (code on a screen), and fundamentally open.

But "potential" is not "reality." Smart contracts today are written by and for developers
who think in text. The error messages are English-centric. The documentation assumes
linear reading. The testing tools assume text output.

This codebase is better than most. The code structure is clean, the naming is consistent,
the generated artifacts are well-organized. But there is room to go further -- to make
the framework not just usable but welcoming to developers and users who experience the
world differently.

Accessibility is not a feature. It is a commitment to communication -- to ensuring that
the message is received, regardless of the medium.

---

## Accessibility Scorecard

| Aspect | Rating | Notes |
|--------|--------|-------|
| Code structure | A | Clean sections, consistent naming |
| Error messages | C | Too terse, English-only, no recovery guidance |
| Documentation | B | Good headers; needs diagrams and visual aids |
| Event emission | F | No events generated; critical for accessible UX |
| Visual tooling | D | No diagram generation or visual metadata |
| Screen reader compat | B+ | Consistent formatting; code blocks well-annotated |
| Multi-modal support | D | Text-only communication model |

**Overall accessibility rating: C+**

The foundation is solid. The code is well-structured for visual scanning. But the
communication layer (errors, events, documentation) needs significant work to be
truly accessible.

---

## Files Reviewed

| File | Accessibility Assessment |
|------|-------------------------|
| `docs/oz-comparison.md` | Good table structure; needs annotated code blocks |
| `docs/blog-post-composable-contracts.md` | Good heading structure; needs diagrams |
| `examples/trait-test/src/lib.rs` | Clean layout; error messages too terse |
| `soroban-sdk-tools-macro/src/contract.rs` | Excellent section structure; error messages need improvement |

---

*Felix Hartmann, CI/CT (NIC Certified). ASL/BSL/IS Interpreter.
"The best interface is the one that communicates clearly in every language,
including the ones without sound."*
