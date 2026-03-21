# Review by Leandro -- Capoeira Master

## Reviewer Profile

I am a Mestre of capoeira, the Afro-Brazilian martial art that disguises combat as dance. For 30 years, I have taught that the most effective defense looks like play, that every attack contains a retreat, and that the ginga -- the fundamental swaying movement -- is the foundation from which all technique emerges. I began exploring blockchain because one of my students, a software developer, showed me how smart contract patterns mirror capoeira's philosophy: fluidity within structure, defense through design, and the constant interplay between opposing forces.

This review examines soroban-sdk-tools through the lens of capoeira: flow, rhythm, the attack/defense balance, and the art of moving between levels of abstraction.

---

## 1. The Ginga: The Fundamental Movement

### Every Capoeirista Begins with the Ginga

In capoeira, the ginga is the rocking, triangular step pattern from which all movement flows. You never stand still. You are always in motion, always shifting your weight, always ready to attack or defend. A capoeirista who stops moving is vulnerable.

In soroban-sdk-tools, the fundamental movement is the **trait-provider-contract flow**:

```
Define trait (#[contracttrait])
    --> Implement provider (OwnableInternal)
        --> Wire to contract (impl_ownable! or #[contractimpl])
```

This three-step flow is the ginga of the framework. Every contract begins here. Every composition returns here. Like the ginga, it is simple in form but contains the entire system within it.

### The Rhythm of the Flow

In capoeira, the ginga has a specific rhythm -- typically in 2/4 time, swaying back and forth. The trait-provider-contract flow has its own rhythm:

1. **Declaration** (trait) -- the moment of intention, where you state what will happen
2. **Implementation** (provider) -- the moment of action, where the work is done
3. **Wiring** (contract) -- the moment of connection, where intention meets action

This three-beat rhythm is clean. It is the berimbau's fundamental toque: dom-dom-dom, dom-dom-dom. Each beat has a purpose. No wasted motion.

Compare to OpenZeppelin's rhythm:

1. Storage module -- dom
2. Trait definition -- dom
3. Module functions -- dom
4. Trait defaults -- dom
5. Consumer implementation -- dom
6. Per-method auth macros -- dom

Six beats instead of three. The rhythm is busier, harder to maintain, easier to stumble in. A capoeirista would say: "Too many movements. Simplify. Find the essential ginga."

---

## 2. Attack and Defense: The Auth Balance

### In Capoeira, Every Attack Is Also a Defense

The meia lua de compasso (half-moon of the compass) is a spinning kick that simultaneously moves your head away from the opponent's attack. You are attacking and defending in the same movement.

The `#[auth(Self::owner)]` annotation achieves this same duality:

```rust
#[auth(Self::owner)]
fn transfer_ownership(env: &Env, new_owner: Address);
```

In a single annotation, the method both:
- **Defends** the contract (by requiring the owner's auth before any action)
- **Enables** the action (by allowing the authenticated owner to transfer)

Attack and defense are fused into one movement. You cannot have one without the other. The owner's auth is not something applied *after* the action is defined -- it is *part of* the action's definition.

In OZ's approach, attack and defense are separated:

```rust
// The action (attack)
fn transfer_ownership(e: &Env, new_owner: Address, live_until_ledger: u32) { ... }

// The defense (applied separately)
#[only_owner]
pub fn admin_action(e: &Env) { ... }
```

In capoeira terms, this is like throwing a kick and then separately deciding whether to protect your face. By the time you think about defense, you may already be hit.

### The Malicia of the Sealed Macro

In capoeira, malicia is the art of deception -- doing something unexpected, disguising your true intention. A good capoeirista makes their most powerful technique look casual.

The `impl_ownable!` macro has malicia. It looks simple -- a one-liner:

```rust
impl_ownable!(MyContract, SingleOwner);
```

But it generates inherent methods that cannot be overridden. The simplicity hides a structural guarantee. This is the equivalent of a casual step that actually positions you for an unblockable sweep. The opponent (the attacker who wants to bypass auth) does not see the trap until it is too late.

### The Esquiva of the Internal Trait

An esquiva is a dodge -- a way of moving out of the line of attack without losing your position in the ginga. The `OwnableInternal` trait is an esquiva: it moves the business logic *out of the line of attack* (the auth boundary) while keeping it connected to the overall flow.

```rust
pub trait OwnableInternal {
    fn owner(env: &Env) -> Address;
    fn transfer_ownership(env: &Env, new_owner: Address);
}
```

There is no auth here. The logic has dodged below the auth line. But it has not left the game -- it is still connected through the Provider pattern, still part of the overall composition. The esquiva is not a retreat; it is a repositioning.

---

## 3. The Roda: The Circle of Composition

### Capoeira Is Played in a Roda

The roda (circle) is the physical space where capoeira happens. Players enter and exit the circle, but the circle continues. The berimbau sets the rhythm. The clapping and singing provide the energy. Inside the circle, two players engage in the game.

The soroban-sdk-tools composition model is a roda:

- **The berimbau (rhythm setter):** The `#[contracttrait]` macro, which sets the rules and generates the structure
- **The players:** The providers (`SingleOwner`, `MultisigOwner`), which enter and exit the circle
- **The circle itself:** The trait system, which persists regardless of which players are inside
- **The game:** The specific contract, where the traits and providers interact

### Entering and Exiting the Roda

In capoeira, entering the roda is called "buying the game" (comprar o jogo). You step in with a specific movement (the au, or cartwheel), replacing one of the two current players.

Provider swapping is buying the game:

```rust
// SingleOwner is in the roda
impl_ownable!(MyContract, SingleOwner);

// MultisigOwner buys the game
impl_ownable!(MyContract, MultisigOwner);
```

The roda (the trait system) continues. The rules do not change. The rhythm does not change. Only the player changes. This is beautiful -- the same fluidity that makes capoeira endlessly renewable.

### The Supertrait as a Compound Movement

In capoeira, compound movements link simple techniques into flowing sequences. A ginga flows into an armada (spinning kick), which flows into a negativa (ground defense), which flows back into the ginga. Each movement sets up the next.

The supertrait relationship creates compound movements:

```rust
#[contracttrait]
pub trait Pausable: Ownable {
    // ...
    #[auth(Self::owner)]
    fn pause(env: &Env);
}
```

`Ownable` sets up `Pausable`. The ginga (ownership) flows into the armada (pause). The `#[auth(Self::owner)]` is the connection point -- the moment where the first movement's momentum carries into the second.

In a well-played capoeira game, these transitions are invisible. The observer sees a continuous flow, not a sequence of discrete moves. In a well-composed contract, the transition from ownership to pausability should be equally seamless. The current implementation achieves this.

---

## 4. Jogo de Dentro (Inside Game) and Jogo de Fora (Outside Game)

### Two Levels of Play

Capoeira has two main modes:

- **Jogo de fora** (outside game): Large, sweeping movements. Standing kicks. Dramatic and visible. This is the public-facing game.
- **Jogo de dentro** (inside game): Close, tight movements. Ground work. Subtle and dangerous. This is where the real action happens.

The two-trait pattern maps perfectly:

- **Outer trait (jogo de fora):** The public interface. Auth checks. WASM exports. What the world sees.
- **Internal trait (jogo de dentro):** The real logic. Storage operations. State transitions. Where the actual work happens.

A master capoeirista moves between the two seamlessly. They play jogo de fora to control distance and rhythm, then drop into jogo de dentro for a decisive technique. Similarly, the soroban-sdk-tools pattern presents the outer trait to the world but does the real work in the internal trait.

### The Risk of Staying in Jogo de Dentro

In capoeira, a player who stays in jogo de dentro too long is vulnerable to a high kick. The ground game protects against sweeps but not against standing attacks.

In code, a developer who works directly with the internal trait (bypassing the outer trait) is vulnerable to auth bypass:

```rust
// Dangerous: calling internal trait directly (staying in jogo de dentro)
SingleOwner::transfer_ownership(env, new_owner);  // no auth check!
```

The macro documentation acknowledges this risk. In capoeira terms: "Stay in jogo de dentro for the work, but come back to jogo de fora for the defense."

---

## 5. The Berimbau: Rhythm and Tempo

### The Berimbau Sets the Rhythm

In capoeira, the berimbau -- a single-string bow instrument -- sets the tempo and style of play. A slow, melodic toque calls for a slow, methodical game. A fast, aggressive toque calls for rapid, acrobatic play. The players must match the rhythm.

The `#[contracttrait]` macro is the berimbau. It sets the rhythm of the entire composition:

- **One trait definition** determines the interface
- **One provider implementation** determines the logic
- **One wiring statement** determines the binding

This three-beat rhythm is the toque of the framework. All players (developers, auditors, testers) must follow this rhythm. Deviating from it (e.g., bypassing the provider, hardcoding auth) breaks the game.

### The Tempo of Development

A good capoeira game has tempo changes -- slow passages where the players test each other, fast passages where they exchange techniques, and pauses where they reset.

Smart contract development should have similar tempo changes:

1. **Slow tempo (design):** Define traits carefully. Think about auth requirements. Choose providers.
2. **Fast tempo (implementation):** Write provider logic. Wire contracts. Iterate quickly.
3. **Pause (testing):** Use `AuthClient` to verify auth. Run the full test suite. Reset and review.

The soroban-sdk-tools framework supports all three tempos. The trait definition forces slow, deliberate design. The provider implementation allows fast iteration. The `AuthClient` enables thorough testing pauses.

---

## 6. Chamada: The Call to Engagement

### The Chamada as Code Review

In capoeira, a chamada is when one player stops and extends their arm, calling the other player to approach. It is a test of trust: will the approaching player engage honestly, or will they attack during the vulnerable approach?

Code review in the soroban-sdk-tools context is a chamada. The developer extends their code for review: "Here is my provider. Here is my wiring. Are they correct?"

The sealed macro (`impl_ownable!`) makes the chamada safer. The reviewer does not need to verify the auth logic -- it is generated by the macro. They only need to verify:

1. Is the correct provider used?
2. Does the provider's internal logic correctly implement the business requirements?
3. Are the tests adequate?

This reduces the review surface dramatically. In capoeira terms, the chamada becomes safer because the macro protects against treacherous approaches.

### The Blog Post as a Chamada to OpenZeppelin

The blog post explicitly calls out to OpenZeppelin:

> "We believe these patterns could benefit the entire Soroban ecosystem, including OpenZeppelin's own `stellar-contracts`."

This is a chamada -- a call to engagement. The authors are extending their arm, inviting OZ to approach. The question is: will OZ engage? Will they see this as an honest invitation or as a competitive challenge?

In capoeira, a well-played chamada leads to mutual respect and a better game. The blog post strikes the right tone -- respectful, collaborative, acknowledging OZ's strengths. This is a chamada that should lead to a good game.

---

## 7. The Volta ao Mundo: Reset and Reflection

### When to Walk the Circle

In capoeira, when the game becomes too intense or one player is hurt, either player can initiate a volta ao mundo -- walking around the circle together, resetting the game. It is not surrender. It is recalibration.

In development, the equivalent is stepping back from implementation to reconsider the design. The soroban-sdk-tools codebase has several places where a volta ao mundo would be beneficial:

1. **The `to_snake_case` function.** It does not handle acronyms correctly. Step back, consider the edge cases, rewrite.

2. **The hardcoded `env` parameter.** The implementation assumes a specific name. Step back, consider the general case, fix.

3. **The missing event emission.** Standard providers do not emit events. Step back, consider accountability requirements, add events.

4. **The missing two-step transfer.** The `SingleOwner` provider supports one-step transfers only. Step back, consider the safety implications, add the two-step pattern.

None of these are fundamental design flaws. They are moments where the game got ahead of the reflection. A volta ao mundo -- a pause to consider -- would improve each one.

---

## 8. The Mandinga: The Magic of Composition

### Mandinga Is the Soul of Capoeira

Mandinga is the hardest concept to explain in capoeira. It is the magic, the trickery, the playful deception that transforms physical technique into art. A player with mandinga makes simple movements look impossible and impossible movements look simple.

The soroban-sdk-tools composition model has mandinga. The `type Provider = SingleOwner` line looks simple -- but it wires an entire auth system, generates WASM exports, and creates test infrastructure. A one-line change (`type Provider = MultisigOwner`) transforms the governance model without touching any other code.

This is mandinga: making the complex look simple. Making the impossible (seamless governance swaps) look obvious.

### Where the Mandinga Is Missing

The example code lacks mandinga in its tests. The first two tests use `mock_all_auths()`:

```rust
env.mock_all_auths();
client.transfer_ownership(&new_owner);
```

This is flat, mechanical, without style. There is no game here -- just "push the button, check the result."

The last two tests use `AuthClient`:

```rust
auth_client.transfer_ownership(&new_owner)
    .authorize(&owner)
    .invoke();
```

This has more mandinga -- it tells a story. "The owner authorizes the transfer. The transfer happens." There is narrative, there is flow. But the example presents the flat version first, undermining the mandinga.

**Recommendation:** Lead with the `AuthClient` tests. Show the game with style first. Then, if necessary, show the `mock_all_auths()` shortcut as an alternative for rapid prototyping.

---

## 9. The Flow of the Macro Code

### Reading contract.rs as a Capoeira Sequence

The macro code in `contract.rs` has its own flow:

1. **Auth attribute parsing** (lines 46-98) -- The ginga. Establishing the rhythm, finding the balance.
2. **Method info extraction** (lines 104-191) -- The jogo de fora. Surveying the space, understanding the position.
3. **Internal trait generation** (lines 197-228) -- The esquiva. Moving the logic below the auth line.
4. **Outer trait generation** (lines 234-303) -- The armada. The main technique, the public-facing movement.
5. **Sealed macro generation** (lines 324-402) -- The rasteira (sweep). The hidden technique that prevents the opponent from standing.
6. **AuthClient generation** (lines 408-631) -- The chamada. Creating the test interface.
7. **Main entry point** (lines 668-726) -- The volta ao mundo. Bringing everything together.

The flow is logical and well-sequenced. Each section sets up the next. The transitions are clean. In capoeira terms, this is a well-played sequence -- each movement flows naturally from the previous one.

### Where the Flow Breaks

The `generate_auth_client_method` function (line 466) is the longest and most complex section. It creates two sets of clones, two closures, and a complex return type. This is the equivalent of a flashy but awkward acrobatic move that interrupts the flow.

In capoeira, we would say: "Simplify. Find a way to do the same thing with fewer movements." The double-clone pattern could be simplified with shared references (as noted in other reviews).

---

## 10. The Roda of the Ecosystem

### Multiple Players, One Circle

The Soroban ecosystem is a roda with multiple players:

- **soroban-sdk-tools:** The innovative player, bringing new techniques
- **OpenZeppelin stellar-contracts:** The established player, with proven techniques
- **Soroban SDK:** The berimbau, setting the fundamental rhythm
- **Stellar Foundation:** The mestre, overseeing the game
- **Developers:** The audience and future players, learning by watching

A good roda benefits from diverse styles. soroban-sdk-tools brings a different style than OZ -- more structural, more composable, more macro-driven. The game is richer for having both styles present.

### The Game Between soroban-sdk-tools and OZ

The blog post frames this as a collaboration, not a competition. In capoeira, the best games are collaborative -- both players push each other to play better. A game where one player dominates is boring. A game where both players innovate and respond is beautiful.

The specific techniques soroban-sdk-tools offers to the game:
1. The two-trait pattern (a new movement)
2. Structural auth (a stronger defense)
3. Provider swapping (a fluidity technique)
4. AuthClient (a training method)

The techniques OZ offers:
1. Two-step transfers (a safety technique)
2. Event emission (an awareness technique)
3. RBAC (a complex movement vocabulary)
4. TTL management (a endurance technique)

The ideal outcome is that both players adopt each other's best techniques, creating a richer, more powerful game for the entire ecosystem.

---

## 11. Recommendations

### For the Framework (In Capoeira Terms)

1. **Clean up the ginga.** Fix the `env` hardcoding and `to_snake_case` issues. The fundamental movement must be flawless.

2. **Add more defensive techniques.** Two-step transfer, event emission, RBAC. A player who only attacks is predictable.

3. **Simplify the AuthClient flow.** The double-clone pattern interrupts the rhythm. Find a smoother movement.

4. **Lead with mandinga.** Put the `AuthClient` tests first in examples. Show the style before the shortcut.

### For the Documentation

1. **Find the rhythm.** The blog post is well-written but could use visual rhythm -- diagrams, flow charts, or even ASCII art that shows the movement between layers.

2. **Tell a story.** The OZ comparison is analytical. Make it narrative. "In the old way, the developer places guards at every door..."

3. **Show the game.** Add an end-to-end example that shows the full flow: trait definition, provider implementation, contract wiring, and testing. This is the complete ginga-to-volta-ao-mundo sequence.

### For the Community

1. **Enter the roda with humility.** The blog post does this well. Continue the collaborative tone.

2. **Learn from other players.** OZ's two-step transfer and event emission are genuinely good techniques. Adopt them.

3. **Teach the next generation.** Create tutorials, workshops, and examples that make the two-trait pattern accessible to beginners.

---

## 12. Overall Assessment

The soroban-sdk-tools framework has excellent flow. The three-beat rhythm (trait-provider-contract) is clean and sustainable. The two-trait pattern achieves the capoeira ideal of fusing attack and defense into a single movement. The Provider pattern provides the fluidity that allows the game to evolve without breaking the rhythm.

The main improvements needed are defensive: two-step transfers, event emission, and RBAC. These are not architectural changes -- they are additional movements that enrich the existing game.

In the roda of the Soroban ecosystem, soroban-sdk-tools plays a distinctive and valuable game. It introduces new movements that push the entire circle to play better. The chamada to OpenZeppelin is well-timed and well-played.

Axe. The game continues.

**Rating: 8/10. Clean ginga, strong mandinga. Needs more defensive vocabulary.**

---

*Reviewed by Leandro, March 2026. Mestre de Capoeira Angola, 30 years in the roda.*
