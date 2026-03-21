# Review by Akira -- Manga Artist and Technical Illustrator

## Reviewer Profile

I draw technical manga -- stories that teach complex concepts through visual narrative. My series "Blockchain Samurai" has been serialized in Weekly Young Engineer for two years. I specialize in making invisible things visible: data structures become cities, algorithms become battle techniques, and type systems become martial arts disciplines. When I read soroban-sdk-tools, I immediately started sketching. The two-trait pattern is deeply visual. It wants to be drawn. This review examines the project through the lens of visual metaphor, narrative structure, and how to make these concepts accessible through sequential art.

---

## 1. The Two-Trait Pattern: A Visual Metaphor

### The Castle and the Gate

The most natural visual metaphor for the two-trait pattern is a **castle with a gate**:

```
OUTER TRAIT (Castle Wall + Gate)
+------------------------------------------+
|                                          |
|  [Guard at Gate]                         |
|  "require_auth()"                        |
|  "Show me your seal!"                    |
|                                          |
|  +------------------------------------+ |
|  |  INNER TRAIT (Castle Interior)      | |
|  |                                     | |
|  |  [Steward at desk]                  | |
|  |  "transfer_ownership()"             | |
|  |  "I'll update the records."         | |
|  |                                     | |
|  +------------------------------------+ |
|                                          |
+------------------------------------------+
```

In manga form, I would draw this as a medieval Japanese castle (shiro). The outer wall has a guarded gate (the `Ownable` trait with `require_auth()`). Inside the wall, the steward (the `OwnableInternal` trait) handles the actual business. Visitors must pass through the gate to reach the steward.

The sealed macro (`impl_ownable!`) would be drawn as the gate being built directly into the castle wall -- inseparable, immovable. The flexible path would be drawn as a wooden gate that can be replaced with a different gate (but also could be removed by a careless builder).

### Panel Sequence for the Auth Flow

**Panel 1:** A traveler (transaction caller) approaches the castle gate. They hold a scroll (the transaction data) and a seal (their cryptographic signature).

**Panel 2:** The guard at the gate (outer trait) examines the seal. Speech bubble: "This seal matches the Lord's records. You may enter." (This is the `require_auth()` call.)

**Panel 3:** Inside the castle, the steward (internal trait) reads the scroll. Speech bubble: "Transfer the deed to the new owner? Very well, I shall update the registry." (This is the `transfer_ownership` internal logic.)

**Panel 4:** The steward writes in a large book (instance storage). The entry shows: "Owner: [new address]."

**Panel 5:** The traveler exits the castle. The guard stamps the scroll. "Transaction complete."

This five-panel sequence teaches the entire auth flow without any code. It can be understood by someone who has never programmed.

---

## 2. The Provider Pattern: Character Design

### Providers as Different Stewards

The provider pattern is best visualized as **different stewards** who can be assigned to the same castle:

**SingleOwner Steward:** A single figure sitting at a desk with one seal and one ledger. Simple, efficient, no complexity. Drawn as a calm, methodical character -- perhaps an older monk.

**MultisigOwner Steward:** A group of three figures who must all stamp a document before it takes effect. They sit at a round table with three seals. Drawn as a council -- perhaps three different personalities (cautious, aggressive, balanced) to show the dynamics of multi-party decision-making.

**TimelockOwner Steward:** A figure with an hourglass. They receive the request, stamp it, then place it in a "pending" drawer with the hourglass next to it. Speech bubble: "I have noted your request. It will take effect when the sand runs out."

The visual power is in the swap: in one chapter, the castle has the monk steward. In the next chapter, the same castle has the council. The walls (outer trait) are identical. The gate (auth check) is identical. Only the person inside changes. This makes the dependency injection concept viscerally understandable.

### Character Sheet for SingleOwner

```
Name: SingleOwner (Tanin no Nushi -- "Sole Master's Steward")
Appearance: Elderly monk in simple robes, bald head, calm expression
Equipment: One seal, one ledger, one ink brush
Personality: Methodical, reliable, asks no questions beyond "is this the master's seal?"
Weakness: If the master is absent, the steward can do nothing
Visual motif: Clean desk, single candle, precise calligraphy
```

### Character Sheet for MultisigOwner

```
Name: MultisigOwner (Gou-i no Shugo -- "Council of Guardians")
Appearance: Three figures in matching but differently colored robes
Equipment: Three seals, shared ledger, speaking stone (determines who speaks)
Personality: Deliberative, sometimes slow, but decisions are considered
Weakness: If one member is absent, quorum may not be reached
Visual motif: Round table, three candles, documents with multiple stamps
```

---

## 3. The Supertrait Hierarchy: Architectural Drawing

### Pausable : Ownable as Tower Upon Foundation

The supertrait relationship `Pausable: Ownable` should be drawn as a castle tower built on the castle foundation:

```
        +------------+
        | PAUSABLE   |  <-- Tower (higher-level trait)
        | pause()    |
        | unpause()  |
        +-----+------+
              |
    +---------+----------+
    | OWNABLE            |  <-- Foundation (base trait)
    | owner()            |
    | transfer_ownership |
    +--------------------+
```

The tower cannot exist without the foundation. The `pause()` function uses the owner's auth from the foundation (`#[auth(Self::owner)]`). In the drawing, this would be shown as a rope or chain running from the tower down to the foundation's gate, indicating that the tower's guard checks credentials through the foundation's guard.

### Multi-Trait Composition as a Castle Complex

A contract composing multiple traits would be drawn as a castle complex with multiple buildings, all sharing the same outer wall:

```
+--------------------------------------------------+
|                OUTER WALL (Auth)                  |
|                                                   |
|  +----------+  +----------+  +-----------+        |
|  | OWNABLE  |  | PAUSABLE |  | MINTABLE  |        |
|  | (Keep)   |  | (Tower)  |  | (Forge)   |        |
|  +----------+  +----------+  +-----------+        |
|                                                   |
|  [Steward/Provider serves all three buildings]    |
|                                                   |
+--------------------------------------------------+
```

The single steward (provider) walks between buildings, serving each one. This visually communicates that a single provider implements multiple traits -- a concept that confuses many developers but is immediately clear when drawn.

---

## 4. The Sealed Macro: Visual Storytelling

### The Forging Scene

The `impl_ownable!` macro should be depicted as a **forging scene**: the castle's gate is being constructed in a forge. Once complete, it is installed permanently. The key visual:

**Panel 1:** The macro forge (a workshop with the `impl_ownable!` name above the door). Inside, a blacksmith (the Rust compiler) hammers hot metal.

**Panel 2:** The finished gate -- a massive iron portcullis with the auth logic welded into the bars. Close-up of the bars shows tiny inscriptions: `require_auth()`, `Self::Provider::owner(env)`.

**Panel 3:** The gate is installed in the castle wall. Workers try to remove it. It cannot budge. Speech bubble: "This gate is part of the wall now. It cannot be replaced."

**Panel 4 (contrast):** A neighboring castle has a wooden gate (the flexible `#[contractimpl(contracttrait)]` path). A shady figure whispers to the carpenter: "Perhaps the gate does not need to check seals..."

This contrast between the iron gate (sealed) and the wooden gate (flexible) tells the security story without any code.

---

## 5. The AuthClient: The Testing Dojo

### Visual Concept

The `AuthClient` should be depicted as a **training dojo** where mock battles test the castle's defenses:

**Panel 1:** Inside a dojo, a figure (the test) wears the owner's seal and approaches a practice gate (a replica of the castle gate). The guard checks the seal. "Access granted."

**Panel 2:** Another figure wears a fake seal (wrong address). The guard blocks them. "Seal does not match. Access denied."

**Panel 3:** The dojo master (the developer) watches both attempts. Speech bubble: "Good. The gate works correctly against both authorized and unauthorized visitors."

**Panel 4 (contrast):** In another dojo, the practice gate has a sign: "`mock_all_auths()`" -- "Everyone welcome!" Figures walk through unchecked. The dojo master frowns. Speech bubble: "This tells me nothing about whether the gate actually works."

This sequence teaches why `AuthClient` is superior to `mock_all_auths()` through visual comedy.

---

## 6. The OZ Comparison: Battle Manga Style

### Chapter Structure

A comparison chapter would depict two castles side by side, each defending against the same threat:

**Page 1-2:** The threat is introduced -- an enemy army (malicious transactions) approaches.

**Page 3-4 (OZ Castle):**
- The castle has guards at each door (per-method `#[only_owner]` macros).
- One door was left unguarded (developer forgot `#[only_owner]`).
- An enemy sneaks through the unguarded door.
- The steward inside shouts: "How did you get past the guards?!"

**Page 5-6 (soroban-sdk-tools Castle):**
- The castle has a single gate with structural auth.
- All doors inside the wall are open (the internal trait has no auth).
- But the only way into the castle is through the gate.
- The enemy army batters the gate but cannot pass.
- The steward inside calmly processes transactions. "The gate holds. As it always does."

**Page 7:** Side-by-side comparison panel. Left: OZ castle with one compromised door. Right: soroban-sdk-tools castle, fully intact. Caption: "When auth is structural, there are no forgotten guards."

---

## 7. Visual Metaphors for Key Concepts

### Concept Map

| Concept | Visual Metaphor | Drawing Style |
|---------|----------------|---------------|
| `#[contracttrait]` | Blueprint/architectural plan | Technical drawing, clean lines |
| `OwnableInternal` | Castle interior (rooms, storage) | Detailed, warm tones |
| `Ownable` (outer) | Castle wall and gate | Strong lines, imposing |
| `type Provider` | Steward character | Character design, personality |
| `impl_ownable!` | Iron forge + permanent installation | Dynamic action, sparks |
| `#[auth(Self::owner)]` | Guard checking seal at gate | Close-up, dramatic |
| `AuthClient` | Training dojo with practice gate | Lighter tone, educational |
| `mock_all_auths()` | Unlocked, unguarded door | Comedy, warning colors |
| Supertrait | Tower built on foundation | Architectural cross-section |
| Provider swap | New steward entering castle | Character entrance scene |

### Color Coding

I would use a consistent color system across all panels:

- **Red/Crimson:** Auth-related elements (gates, seals, guards)
- **Blue/Indigo:** Business logic (storage, ledgers, steward's desk)
- **Gold:** Ownership (the owner's seal, the owner's throne)
- **Green:** Testing (dojo, practice equipment)
- **Gray/Steel:** Sealed elements (forged gate, permanent structures)
- **Brown/Wood:** Flexible elements (wooden gate, replaceable doors)

This color system allows readers to instantly identify the category of each visual element, even in complex panels.

---

## 8. Panel Layout Suggestions for Documentation

### Two-Page Spread: "The Two-Trait Pattern"

**Left Page (Internal Trait):**
- Large illustration of the castle interior
- The steward at their desk
- Storage shelves with labeled scrolls ("owner", "paused")
- Caption: "OwnableInternal: Pure logic. No guards. No gates. Just the work."

**Right Page (Outer Trait):**
- The same castle from outside
- The imposing gate with the guard
- A visitor presenting their seal
- Caption: "Ownable: The same work, but only after you prove who you are."

**Bottom Strip (both pages):**
- Four small panels showing the flow: Visitor arrives -> Guard checks seal -> Visitor enters -> Steward does work
- Code snippets below each panel matching the action

### Single Page: "Provider Swap"

**Top Half:** Two versions of the same castle interior, split-screen:
- Left: SingleOwner steward (monk) at desk
- Right: MultisigOwner stewards (council) at round table
- The castle walls, gate, and exterior are IDENTICAL in both versions

**Bottom Half:** Code showing the one-line change:
```rust
// Before:
impl_ownable!(MyContract, SingleOwner);
// After:
impl_ownable!(MyContract, MultisigOwner);
```
- Arrow connecting the code change to the visual change above

---

## 9. Narrative Arc for a Teaching Manga

### Chapter Outline: "Castle of Code" (12 chapters, ~20 pages each)

**Chapter 1: The Unguarded Castle**
- A young developer deploys a contract without auth. Chaos ensues.
- Lesson: Why auth matters.

**Chapter 2: The Guard at Every Door (OZ Approach)**
- The developer hires guards for each door. One door is missed.
- Lesson: Per-method auth is error-prone.

**Chapter 3: The Single Gate (Two-Trait Pattern)**
- An architect redesigns the castle with one gate. All doors inside are open.
- Lesson: Structural auth is safer than per-method auth.

**Chapter 4: The Steward's Role (Provider Pattern)**
- The steward handles all business inside the castle.
- Lesson: Separation of concerns.

**Chapter 5: The Iron Gate (Sealed Macro)**
- The gate is forged in iron, permanently installed.
- Lesson: `impl_ownable!` prevents override.

**Chapter 6: Changing Stewards (Provider Swap)**
- The old steward retires. A new council takes over.
- Lesson: Dependency injection.

**Chapter 7: The Tower (Supertrait Composition)**
- A watchtower is built on the castle, sharing its foundation.
- Lesson: `Pausable: Ownable`.

**Chapter 8: The Training Dojo (AuthClient)**
- The castle's defenses are tested in a dojo.
- Lesson: Testing auth without `mock_all_auths()`.

**Chapter 9: The Alliance (OZ Comparison)**
- Two castles compare their defenses and learn from each other.
- Lesson: Strengths of both approaches.

**Chapter 10: The Siege (Attack Scenarios)**
- Enemies test the castle's defenses.
- Lesson: Adversarial thinking.

**Chapter 11: The Province (Multi-Contract Composition)**
- Multiple castles form a province with shared governance.
- Lesson: Cross-contract composition.

**Chapter 12: The Legacy (Deployment and Maintenance)**
- The castle stands for generations.
- Lesson: Immutability, upgrades, and the long view.

---

## 10. Critique of Current Visual Communication

### The Blog Post

The blog post is text-heavy with code blocks. It has no diagrams, no visual aids, no color. For a visual thinker, it is like a manga with no illustrations -- technically complete but experientially impoverished.

**Specific issues:**
1. The two-trait generation is explained with two code blocks. A single diagram showing the transformation (input trait -> macro -> two output traits) would be clearer.
2. The security comparison with OZ is textual. A visual showing the "unguarded door" vs. "single gate" would be instantly persuasive.
3. The CGP table is dry. A character relationship diagram (like a manga's character page) would make the connections intuitive.

### The OZ Comparison

The comparison table is good -- tables are inherently visual. But the code comparisons suffer from the same text-heaviness as the blog post. Side-by-side diagrams showing the architectural differences would strengthen the argument significantly.

### The Example Code

The example code is well-commented, but the comments describe *what* the code does, not *why* it looks the way it does. Visual annotations (even ASCII art) showing the flow between traits, providers, and contracts would help.

---

## 11. Recommendations

### For Documentation

1. **Add diagrams.** At minimum: the two-trait generation flow, the auth call path, the provider swap mechanism, and the supertrait hierarchy.
2. **Use color-coded code blocks.** Highlight auth-related code in red, business logic in blue, provider declarations in gold.
3. **Create a visual glossary.** One-page illustration mapping each concept to its visual metaphor.

### For the Project

1. **Commission a technical manga.** Even a short 4-page manga explaining the two-trait pattern would dramatically increase accessibility. I would be willing to draw this.
2. **Create animated diagrams.** GIFs showing the auth flow, the provider swap, and the sealed macro installation.
3. **Add ASCII art to the macro documentation.** The `contract.rs` file has excellent text comments. ASCII art showing the code generation flow would make it even better.

### For Conference Presentations

1. Use the castle metaphor as the central visual theme.
2. Animate the five-panel auth flow sequence.
3. Show the "unguarded door" vs. "single gate" comparison as a side-by-side animation.

---

## 12. Overall Assessment

The soroban-sdk-tools project has excellent visual potential. The two-trait pattern, the provider swap, and the sealed macro are all deeply visual concepts that are currently expressed only in text and code. Adding visual communication would make the project accessible to a much broader audience -- not just experienced Rust developers, but also:

- Students learning blockchain development
- Managers evaluating composability frameworks
- Designers working on dApp user interfaces
- Auditors reviewing contract architecture

The concepts are clear, the architecture is clean, and the metaphors are natural. What is missing is the visual expression. The castle metaphor is waiting to be drawn.

**Rating: 7/10 for current visual communication. Would be 9.5/10 with proper illustrations, diagrams, and a teaching manga.**

---

*Reviewed by Akira, March 2026. Creator of "Blockchain Samurai," Weekly Young Engineer.*
