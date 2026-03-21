---
persona: Lao Tzu (contemporary interpretation)
age: Timeless
background: Taoist philosophy applied to software design, practitioner of wu wei (effortless action)
focus: Wu wei, the Tao of simplicity, the paradox of control, naturalness in systems design
tone: Paradoxical, uses analogies from nature, sees software as an expression of cosmic principles
---

# Review: soroban-sdk-tools -- The Tao of Composable Contracts

## The Tao That Can Be Coded Is Not the Eternal Tao

*The Tao Te Ching, Chapter 1:*
> The way that can be spoken of is not the constant way.

A framework that claims to solve composition is not the eternal composition.
The moment you codify patterns into macros, you freeze the water that should
flow. And yet -- ice serves a purpose. Let us examine when the freezing is
wisdom and when it is folly.

## Wu Wei and Structural Auth

*Chapter 43:*
> The softest thing in the world overcomes the hardest.

The `#[auth(Self::owner)]` annotation is wu wei -- effortless action. The
developer does nothing (writes no auth code), and auth is enforced everywhere.
The softest annotation overcomes the hardest security requirement.

Compare to the manual approach:

```rust
fn transfer_ownership(e: &Env, new_owner: Address) {
    let owner = get_owner(e).expect("no owner");
    owner.require_auth();           // the developer must act
    set_owner(e, &new_owner);       // the developer must act again
}
```

Each line is effort. Each line is a place where effort can fail. The Taoist
developer asks: can the system arrange itself so that the right thing happens
without effort?

The macro says: yes. Write the annotation. The Tao of the compiler does the
rest.

This is good. But be careful: wu wei is not laziness. Wu wei is alignment with
the natural flow. If the macro's flow is unnatural -- if it generates code that
surprises the developer -- then it is not wu wei but wei (forced action).

## The Paradox of the Sealed Path

*Chapter 22:*
> Yield and overcome. Bend and be straight.

The sealed macro (`impl_ownable!`) says: "You cannot override auth." This is
rigidity. The Tao does not favor rigidity.

And yet: the seal protects. The wall around the garden is rigid, but it allows
the garden to flourish. The riverbank is rigid, but it gives the river its path.

The paradox: by constraining the developer's freedom (sealed), the framework
enables greater freedom for the user (guaranteed safety). The developer yields
their freedom, and the system overcomes the auth bypass problem.

This is the Taoist principle of complementary opposites (yin and yang). The
sealed path (constraint, yin) and the flexible path (freedom, yang) are not
opponents. They are complements. A framework that offers only sealed is rigid.
A framework that offers only flexible is formless. Together, they form a whole.

The framework correctly offers both. The Tao is in the balance.

## The Provider as Water

*Chapter 78:*
> Nothing in the world is softer and weaker than water.
> Yet nothing is better at attacking the hard and strong.

The provider pattern is like water. Water takes the shape of its container. A
provider takes the shape of its trait. `SingleOwner` fills the `OwnableInternal`
vessel. `MultisigOwner` fills the same vessel differently. The vessel does not
change; the water does.

This is natural. A stream does not argue with a stone; it flows around it. A
provider does not argue with the trait interface; it conforms to it.

But water without a vessel spills. A provider without a well-defined trait
interface is formless -- it can do anything, which means it is constrained by
nothing. The trait is the vessel that gives the provider meaning.

The framework understands this: the `OwnableInternal` trait is the vessel;
the provider is the water. Good.

## The Naming of Names

*Chapter 1:*
> The name that can be named is not the constant name.

The framework generates many names: `OwnableInternal`, `Ownable`,
`OwnableAuthClient`, `impl_ownable!`. Each name is a signpost. But the Tao
reminds us: the signpost is not the destination.

A developer who understands the Tao of the framework does not think in terms
of "OwnableInternal" or "impl_ownable!". They think: "business logic goes
here. Auth goes there. Testing goes here." The names are scaffolding, not
structure.

The current names are adequate but noisy. The Tao favors fewer names.

Consider: does the developer need to know the name `OwnableInternal`? If
the provider's trait implementation were anonymous -- inferred from context
rather than explicitly named -- the developer would think less and do more.

```rust
// Current: developer must know the name
impl OwnableInternal for SingleOwner { ... }

// Taoist alternative: the name emerges from context
#[provider(Ownable)]
impl SingleOwner { ... }
```

The `#[provider(Ownable)]` attribute infers `OwnableInternal` from `Ownable`.
The developer writes the natural form; the compiler derives the technical form.
This is wu wei applied to naming.

## The Emptiness of Generated Code

*Chapter 11:*
> Thirty spokes share the wheel's hub;
> It is the center hole that makes it useful.

The generated outer trait is like the hub of a wheel. It is mostly empty --
default methods that delegate to the provider. The auth check and the
delegation are the "hole" -- the emptiness that makes the wheel turn.

```rust
fn transfer_ownership(env: &Env, new_owner: Address) {
    let __auth_addr = Self::Provider::owner(env);    // the spoke
    __auth_addr.require_auth();                       // the hole
    Self::Provider::transfer_ownership(env, new_owner) // the spoke
}
```

The method is three lines. Two are delegation (spokes). One is auth (the
hole). The useful emptiness -- the authorization that happens between the
delegation -- is what makes the framework valuable.

If the framework added more to the generated code (events, logging, metrics),
the useful emptiness would be filled. The wheel would stop turning. The Tao
counsels: add less, not more.

The blog post mentions adding event emission to the generated code. I counsel
caution. Events are important, but they should flow from the provider (where
the business logic knows WHAT happened), not from the outer trait (where only
auth is known). Adding events to the outer trait fills the emptiness.

## The Uncarved Block (Pu)

*Chapter 28:*
> Return to the state of the uncarved block.

The uncarved block (pu) is pure potentiality. Before the macro runs, the trait
definition is an uncarved block -- it could become many things. The macro carves
it into four specific artifacts.

Each carving removes possibilities. The uncarved block can become a bowl or a
flute. Once carved into a bowl, it cannot become a flute.

The framework's design carves conservatively. The four artifacts (Internal,
Outer, AuthClient, sealed macro) are minimal carvings. They remove few
possibilities while enabling many uses. This is good craftsmanship.

But the framework should resist the temptation to carve more. Every new
attribute (`#[event]`, `#[guard]`, `#[require]`) is another cut. Each cut
is justified by a use case. But the sum of cuts can destroy the block.

The Taoist developer asks: instead of adding `#[event]` to the macro, can
the framework provide a way for the developer to add events naturally? If
the provider pattern already allows events (through provider implementation),
perhaps the structural approach is unnecessary.

Not every problem needs a structural solution. Some problems need water,
not ice.

## The Ten Thousand Things

*Chapter 42:*
> The Tao gave birth to One. One gave birth to Two.
> Two gave birth to Three. Three gave birth to ten thousand things.

The macro gave birth to a trait definition (One). The trait definition gave
birth to Internal and Outer (Two). These gave birth to Provider, AuthClient,
and sealed macro (Three). From these, ten thousand contracts arise.

This is the natural order: simplicity gives birth to complexity through
structured generation. The developer writes One (the trait definition), and
the system generates the ten thousand things (contracts with auth, tests,
providers).

The framework follows the Tao of generativity. One input, many outputs. This
is correct.

But beware: if the One (trait definition) becomes complex -- if it requires
many annotations, many attributes, many constraints -- then the developer's
burden grows, and the system no longer follows wu wei.

The current trait definition is simple: method signatures with optional
`#[auth]`. This simplicity should be preserved fiercely. Every proposed new
annotation should be weighed against the cost of complexity at the input
level.

## Recommendations

1. **Preserve the emptiness**: Do not add structural event emission to the
   generated outer trait. Let events flow naturally from providers.

2. **Reduce the names**: Consider `#[provider(Ownable)]` syntax to hide the
   `OwnableInternal` name from the developer.

3. **Resist new annotations**: Each new attribute (`#[guard]`, `#[event]`,
   `#[require]`) is justified individually but collectively they fill the
   emptiness. Consider whether provider-level solutions suffice.

4. **Trust the developer**: The sealed path protects. The flexible path
   trusts. Both are necessary. Do not eliminate the flexible path in pursuit
   of safety. The river needs both banks AND the flowing water.

5. **Let the framework be invisible**: The best framework, like the best
   government, is the one the users do not notice. When auth "just works"
   and testing "just works," the framework has achieved wu wei.

## Verdict

*Chapter 17:*
> The best leaders are those the people hardly know exist.

The best framework is one the developer hardly knows exists. The
`#[contracttrait]` macro approaches this ideal: write a trait, add an
annotation, and auth enforcement appears. The developer acts without effort.

But the framework is not yet invisible. The vocabulary (Internal, Provider,
sealed macro, AuthClient) makes it visible. The learning curve makes it
felt. The gap between "I want ownership" and "I have ownership" still
requires conscious effort.

When that gap closes -- when a developer can write `#[ownable]` on a
contract and have everything handled -- the framework will have achieved
the Tao.

Until then, it walks the path.

**Rating**: The Tao does not rate. The river does not judge the stones. 7/10.
