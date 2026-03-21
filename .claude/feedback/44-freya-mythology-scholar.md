# Review by Freya -- Norse Mythology Scholar & NFT Creator

*"Every rune tells a story. Every smart contract should too."*

---

## Overall Impression

I study the narrative structures of Norse mythology -- the Eddas, the sagas, the
skaldic poetry -- and I build NFT collections that encode these stories on-chain. When
I look at code, I see narrative. The `#[contracttrait]` macro tells a story about
authority, delegation, and the relationship between the one who commands and the one
who acts.

This is, at its core, the story of Odin and his ravens. Huginn (Thought) and Muninn
(Memory) fly out into the world, gather information, and return to Odin. Odin does not
fly himself -- he delegates. But he retains authority. The ravens are his providers.

Let me read this codebase as mythology.

---

## The Mythological Structure

### The Two Traits as Dual Nature

The macro generates two traits from one definition:

- `OwnableInternal` -- the inner nature, the soul, the business logic
- `Ownable` -- the outer nature, the face shown to the world, the auth-enforced interface

In Norse mythology, this duality is everywhere. Odin has many names -- Allfather,
Grimnir, Gangleri -- each a different face of the same being. The internal trait is
Odin's true self. The outer trait is whichever face he shows to the current audience.

The provider pattern deepens this: `SingleOwner` is one mask, `MultisigOwner` is another.
The underlying structure (the trait) remains the same, but the manifestation changes.

This is also the concept of **kenning** in skaldic poetry -- a metaphorical compound
used in place of a simple noun. "Whale-road" for sea. "Battle-sweat" for blood. The
provider pattern is a kenning for implementation: instead of saying "the owner," you say
"the entity returned by the provider's owner method when consulted against the current
state of the contract's storage." It is more precise, more expressive, and more beautiful
in its complexity.

---

## Storytelling Through Code: Narrative Analysis

### The Blog Post as Saga

The blog post (`blog-post-composable-contracts.md`) follows the classic saga structure:

1. **The World-Setting** (lines 1-14): Establish the world. Every smart contract
   ecosystem faces the same challenge. This is the "In the time of the gods" opening.

2. **The Challenge** (lines 15-22): The hero (soroban-sdk-tools) is introduced. Its
   four powers are listed. This is the arming of the hero.

3. **The Journey** (lines 26-67): The core innovation is revealed -- the two-trait
   structure. This is the hero's first trial.

4. **The Adversary** (lines 72-122): The Override Problem is the dragon. The hero slays
   it with the Sealed Auth pattern. This is the climactic battle.

5. **The Companions** (lines 125-189): Provider-based DI and supertrait composition.
   These are the hero's allies. The companions are tested (Pausable extends Ownable).

6. **The Treasure** (lines 194-241): AuthClient and error handling. These are the
   rewards of the quest.

7. **The Diplomacy** (lines 245-377): The hero extends an olive branch to OpenZeppelin.
   This is the peace negotiation after the battle. "We are not proposing to replace..."
   is the saga's reconciliation.

8. **The Homecoming** (lines 348-383): "Try It Today." The hero returns home with the
   treasure.

This is a well-told saga. The narrative arc is complete. The hero faces challenges,
overcomes them, and offers to share the rewards. The only structural issue is that the
"What OZ Does Better" section (the hero's acknowledgment of the adversary's strengths)
comes AFTER the battle, not before. In the Eddas, the hero always acknowledges the
dragon's power before fighting it, not after. Moving this section earlier would strengthen
the narrative.

### The Comparison Document as Flyting

The OZ comparison document (`oz-comparison.md`) is structured as a **flyting** -- the
Norse tradition of ritualized insult-exchange between warriors. In a flyting, two warriors
trade increasingly clever boasts and insults, each trying to demonstrate superiority
without losing dignity.

The comparison follows this pattern:

1. Show the opponent's approach (respectfully)
2. Show your own approach (more concisely)
3. Summarize with a table (the judges' scorecard)

"The difference is stark" (line 190) is the flyting's crescendo -- the moment the poet
drops the metaphor and states the victory directly.

The "What OZ Does Better" section at the end is the post-flyting handshake. Well done.
A flyting without reconciliation is just an insult, and the Norse valued honor in both
victory and defeat.

---

## Narrative Composability: The Supertrait as Saga Cycle

### The Concept

Norse mythology is structured as interconnected cycles. The Volsunga Saga connects to
the Nibelungenlied. Odin's story connects to Thor's. Characters recur across stories,
maintaining consistent identities but playing different roles.

The supertrait pattern mirrors this:

```rust
#[contracttrait]
pub trait Pausable: Ownable {
    #[auth(Self::owner)]
    fn pause(env: &Env);
}
```

Pausable depends on Ownable. It uses Ownable's `owner()` for auth. This is the same
structural relationship as "Sigurd's story depends on Odin's story" -- you cannot
understand Sigurd without knowing who Odin is and what he wants.

In the generated code:

```rust
pub trait PausableInternal: OwnableInternal { ... }
```

The Internal trait's supertrait constraint is the genealogy of the saga. Just as Sigurd
descends from Volsung who descends from Odin, PausableInternal descends from
OwnableInternal. The lineage is explicit, type-checked, and cannot be falsified.

### Narrative Composability for NFTs

This is directly applicable to my work. Consider a mythology-themed NFT collection:

```rust
#[contracttrait]
pub trait MythologyNFT {
    fn origin_story(env: &Env, token_id: u64) -> Symbol;
    fn lineage(env: &Env, token_id: u64) -> Vec<u64>;

    #[auth(Self::owner)]
    fn transfer(env: &Env, token_id: u64, to: Address);
}

#[contracttrait]
pub trait SagaComposition: MythologyNFT {
    fn combine_artifacts(env: &Env, token_a: u64, token_b: u64) -> u64;
    fn saga_chapter(env: &Env, token_id: u64) -> u32;
}
```

The supertrait system ensures that any SagaComposition NFT is also a MythologyNFT. The
lineage is maintained. You cannot combine artifacts without first having an origin story.
The narrative structure is enforced at the type level.

This is powerful. In my current NFT contracts, I enforce narrative consistency through
convention -- "make sure you set the origin story before allowing trades." With
`#[contracttrait]`, the narrative structure is in the type system. The compiler becomes
the saga-keeper.

---

## Cultural Representation in Code

### Naming and Symbolism

The codebase uses ownership terminology that reflects a specific cultural framework:
"owner," "transfer_ownership," "provider." These are mercantile terms -- they come from
property law and business relationships.

In Norse culture, authority was not about "ownership" but about **stewardship**. A king
did not "own" his people -- he was their guardian, their ring-giver, their oath-holder.
The relationship was reciprocal, bound by mutual obligation.

Consider alternative naming that reflects different cultural models:

| Current (Property) | Norse (Stewardship) | Meaning |
|-------|--------|---------|
| `owner` | `guardian` or `steward` | Authority as responsibility |
| `transfer_ownership` | `pass_the_ring` | Authority transfer as ceremony |
| `SingleOwner` | `SoleGuardian` | Single point of stewardship |
| `MultisigOwner` | `ThingCouncil` | Collective governance (like the Althing) |
| `Pausable` | `Fimbulwinter` | The great winter that freezes all activity |

I am not suggesting these names for a general-purpose library. But the naming choices
reflect cultural assumptions about authority that are worth examining. In Norse society,
a leader who "owned" people was a tyrant; a leader who "guarded" them was a king. The
distinction matters.

### The Missing Story: What Happens to the Old Owner?

In the current implementation:

```rust
fn transfer_ownership(env: &Env, new_owner: Address) {
    OwnableStorage::set_owner(env, &new_owner);
}
```

The old owner simply disappears. There is no event, no acknowledgment, no record. In
Norse saga, a transfer of power is always a moment of high drama -- a king dies, a new
king rises, the skalds compose poetry about the transition. The old king is remembered.

In the code, the old king is overwritten in storage and forgotten. This is narratively
unsatisfying and practically problematic (no audit trail). The OZ comparison acknowledges
this: "OZ emits events for every state change." Events are the saga's memory.

**Recommendation:** The macro should generate event emission for every auth-protected
method. The event is the skaldic record -- the permanent testimony of what happened and
who authorized it.

---

## The Runes: Reading the Macro Code

### `contract.rs` as Runic Inscription

The macro implementation (`contract.rs`) is 727 lines of procedural macro code. Reading
it is like reading a runic inscription -- each symbol has precise meaning, but the overall
message requires expertise to decode.

The structure is clean:

1. **Auth parsing** (lines 46-98) -- reading the runes
2. **Method extraction** (lines 100-191) -- interpreting the runes
3. **Internal trait** (lines 193-228) -- carving the inner rune
4. **Outer trait** (lines 230-303) -- carving the outer rune
5. **Sealed macro** (lines 320-402) -- binding the runes with magic
6. **AuthClient** (lines 404-631) -- creating the testing artifact
7. **Supertrait handling** (lines 633-662) -- connecting the saga cycles
8. **Entry point** (lines 664-727) -- invoking the magic

The code is well-organized and readable. The function names are descriptive. The
comments are clear. This is a well-carved inscription.

### The Beauty of `extract_auth_attr`

```rust
fn extract_auth_attr(attrs: &[Attribute]) -> syn::Result<Option<AuthSource>> {
    for attr in attrs {
        if attr.path().is_ident("auth") {
            let expr: Expr = attr.parse_args()?;
            match &expr {
                Expr::Path(ep) if ep.path.segments.len() == 2 => { ... }
                Expr::Path(ep) if ep.path.segments.len() == 1 => { ... }
                _ => { return Err(...) }
            }
        }
    }
    Ok(None)
}
```

This function reads an `#[auth]` attribute and determines whether it refers to
`Self::method` (the provider resolves the address) or `param_name` (the address is
passed directly). Two sources of authority, cleanly distinguished.

In mythology, authority also has two sources: **divine right** (authority from above,
like `Self::owner`) and **popular mandate** (authority from the people, like `from` in
`#[auth(from)]` on a transfer method). The code captures this distinction elegantly.

---

## Recommendations for Narrative Enhancement

### 1. Add Origin Stories to Providers

Every provider should document its narrative -- why does it exist? What story does it
tell?

```rust
/// SingleOwner: The Sole Guardian
///
/// Like a Norse jarl ruling a single farmstead, this provider implements
/// single-address ownership. Authority is absolute but lonely.
pub struct SingleOwner;
```

### 2. Name the Generated Artifacts Consistently

The macro generates `{Trait}Internal`, `{Trait}AuthClient`, and `impl_{trait_snake}!`.
Three different naming conventions (suffix, suffix, prefix+snake). Consider consistent
naming:

- `{Trait}Internal` -- the inner nature
- `{Trait}Client` -- the outer interface (already exists from Soroban SDK)
- `{Trait}AuthClient` -- the testing guardian
- `{trait_snake}_sealed!` -- the binding rune (instead of `impl_{trait_snake}!`)

### 3. Document the Saga Structure

The blog post tells the story well, but the code itself lacks narrative. Add a
high-level architecture comment at the top of `contract.rs` that tells the story:

```
// This macro tells a story in four parts:
// 1. The Inner Nature -- business logic, pure and unguarded
// 2. The Outer Face -- auth-enforced, the face shown to the world
// 3. The Binding Rune -- sealed auth that cannot be broken
// 4. The Testing Eye -- AuthClient that sees all auth flows
```

### 4. Consider Narrative Composability

The supertrait system enforces structural composition. But it does not enforce narrative
composition. A `Pausable` contract that extends `Ownable` knows that it needs an owner.
But it does not know the STORY of why it was paused.

Consider a `#[reason]` attribute:

```rust
#[contracttrait]
pub trait Pausable: Ownable {
    #[auth(Self::owner)]
    #[reason]
    fn pause(env: &Env, reason: Symbol);
}
```

The `#[reason]` attribute would include the reason in generated events. Every pause has
a story. Every story deserves to be told.

---

## Conclusion

This codebase tells a compelling story about authority, delegation, and composability. The
two-trait structure is mythologically resonant -- inner nature and outer face, business
logic and auth enforcement. The provider pattern enables the kind of flexible authority
models that every culture has explored: sole rule, collective governance, delegated
authority.

The weaknesses are narrative: no events (no saga memory), no explicit acknowledgment of
transitions (the old owner vanishes silently), and naming that reflects only one cultural
model of authority (property ownership rather than stewardship).

The technical architecture is sound. The mythological architecture needs enrichment.

**Rating: 4 Mjolnirs out of 5.**

The fifth Mjolnir is earned when the code tells its own story -- when every authority
transfer emits an event, every pause has a reason, and every provider has an origin.

---

## Files Reviewed

| File | Narrative Quality |
|------|-------------------|
| `docs/oz-comparison.md` | Strong flyting structure; good reconciliation |
| `docs/blog-post-composable-contracts.md` | Complete saga arc; climax well-timed |
| `examples/trait-test/src/lib.rs` | Functional but narratively silent |
| `soroban-sdk-tools-macro/src/contract.rs` | Well-carved runes; needs saga commentary |

---

*Freya Lindgren, PhD. Department of Old Norse Studies, Uppsala University.
"The saga that is not told is the saga that is forgotten. Write your events."*
