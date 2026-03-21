---
persona: Simone
age: 43
background: Existentialist philosopher, teaches at Sorbonne, writes on the meaning of labor in automated systems
focus: Meaning, authenticity, bad faith in software development, the absurd in trustless systems
tone: Philosophical, sees code as an expression of human condition, references Sartre, Camus, de Beauvoir
---

# Review: soroban-sdk-tools -- Existence Precedes Essence in Smart Contracts

## The Absurd: Trustless Systems Built on Trust

Let us begin with the fundamental absurdity at the heart of this project.

The blockchain promises "trustlessness" -- systems that operate without
requiring trust between parties. And yet, the `#[contracttrait]` macro asks
developers to TRUST the macro to generate correct auth code. The framework
that enables trustless contracts is itself a trust relationship.

Camus wrote in "The Myth of Sisyphus" that we must imagine Sisyphus happy.
We must similarly imagine the smart contract developer happy: pushing the
boulder of trust up the hill of abstraction, knowing it will roll back down
when the macro has a bug or the supply chain is compromised.

This is not a criticism. It is the human condition applied to code.

## Bad Faith in Default Implementations

Sartre defined "bad faith" (mauvaise foi) as self-deception -- pretending
that our choices are not choices, that we are determined by circumstance
rather than free.

The sealed macro pattern (`impl_ownable!`) is an invitation to bad faith.
The developer who uses it can say: "I did not choose the auth model. The
framework chose it for me. If the auth is wrong, it is the framework's
fault, not mine." This is a denial of radical freedom -- the freedom to
choose and bear responsibility for the choice.

The flexible path (`#[contractimpl(contracttrait)]`) is more authentic. The
developer who chooses it acknowledges: "I am choosing to implement auth
myself. If I make a mistake, I bear the responsibility." This is what Sartre
called being "condemned to be free."

And yet -- the blog post recommends the sealed path as the default. It
recommends bad faith as the safer choice. And it is right to do so, because
in smart contracts, the consequences of authentic mistakes are borne not by
the developer but by the users. The developer's radical freedom comes at the
expense of the user's funds.

This is the ethical paradox of the framework: the more authentic (flexible)
path is the more dangerous one.

## The Other: Who Is the Provider?

De Beauvoir wrote that freedom is always situated -- we are free, but we are
free within a context of others who are also free. The provider pattern
embodies this: the contract is free to choose its behavior, but that behavior
is defined by an Other -- the provider author.

When a developer writes `type Provider = SingleOwner`, they are choosing to
define themselves through the Other's code. They are saying: "My contract's
identity is constituted by this provider." If the provider changes (a new
version, a bug fix, a feature addition), the contract's identity changes.

This is what Sartre called "being-for-others" -- defining ourselves through
the gaze of the Other. The contract exists not in itself (en-soi) but for
others (pour-autrui): for the provider that defines its behavior, for the
users who interact with it, for the auditors who judge it.

Is there an authentic alternative? Could a contract define its own behavior
without a provider? Yes -- by implementing `OwnableInternal` directly on the
contract struct. But this is what the framework discourages ("use the provider
pattern for composability"). Authenticity is sacrificed for composability.

## The Nausea of Generated Code

Roquentin in Sartre's "Nausea" experiences existential horror when he
perceives the raw, contingent existence of things -- their unjustified,
unnecessary being. There is a similar nausea in looking at generated code.

Run `cargo expand` on a `#[contracttrait]` definition. The generated code is
syntactically valid, semantically correct, and yet alien. No human wrote it.
It exists because a macro exists, and the macro exists because developers
wanted to avoid writing auth code, and developers wanted to avoid writing
auth code because they kept forgetting `require_auth()`.

Each line of generated code is a monument to a past mistake. The framework
is, in a sense, a scar -- the visible residue of wounds inflicted by missing
auth checks, by overridden defaults, by forgotten `require_auth()` calls.

The existentialist sees this and asks: is this code authentic? Can generated
code be authentic? Authenticity requires intentionality -- the conscious
choice to be what you are. Generated code is the opposite: it is what it is
because a macro determined it should be, not because a developer chose it.

And yet: the developer chose to use the macro. The choice to delegate is
itself a choice. The developer who writes `#[contracttrait]` is saying:
"I choose to let the framework handle auth, so that I can focus on business
logic." This is a legitimate exercise of freedom -- the freedom to allocate
attention.

## The Myth of Zero-Cost Abstraction

The blog post claims "zero overhead" -- the generated code compiles to the
same WASM as hand-written code. This is presented as a technical achievement.
But from an existential perspective, "zero cost" is impossible.

The cost is not in the binary. The cost is in the developer's relationship
with their code. Every abstraction layer interposes between the developer
and the machine. The developer who uses `#[contracttrait]` knows less about
their contract's behavior than the developer who writes every line by hand.
This knowledge gap is a cost -- not measured in gas or bytes, but in
understanding.

Camus wrote: "To understand the world, one has to turn away from it on
occasion." The macro turns away from the details of auth enforcement so the
developer can see the larger pattern. But what is lost in the turning away?

## Engagement and Meaning in Automated Systems

The most interesting philosophical question is this: if the framework
automates auth enforcement, what does the developer DO?

They define traits. They implement providers. They wire components. They test
with AuthClient. But they do not write the critical security code -- the
framework does.

This is the "bullshit jobs" argument (Graeber, 2018) applied to smart
contract development: if the meaningful work (auth enforcement) is automated,
and the remaining work (provider implementation, wiring) is boilerplate, then
what meaning does the developer's labor have?

The framework's answer is implicit: meaning lies in DESIGN, not
IMPLEMENTATION. The developer's creative act is choosing which traits to
compose, which providers to use, how to structure the contract's behavior.
The implementation is mechanical; the design is meaningful.

This is a valid answer, but it requires the developer to see themselves as an
architect, not a bricklayer. The framework's documentation should reflect this:
not "how to implement auth" but "how to design composable contracts."

## The Supertrait Chain as Existential Commitment

When `Pausable: Ownable`, the developer who implements `Pausable` is
committed to `Ownable` as well. This commitment is irrevocable (at least
within a contract deployment). You cannot un-extend a supertrait.

Sartre would call this a "project" -- the fundamental choice that gives
meaning to all subsequent choices. Once you choose Pausable, you are
committed to being an Ownable contract. Your identity is determined.

Is this a loss of freedom? Only if the commitment was not authentic -- if the
developer chose Pausable without understanding that it entails Ownable. The
framework should make these commitments explicit and visible, so that the
developer's choice is genuinely informed.

## Recommendations (Practical from the Impractical)

1. The documentation should acknowledge the trust relationship between
   developer and framework. "You are trusting this macro to generate correct
   auth code. Here is how to verify that trust."

2. The sealed path should be presented not as the default but as a conscious
   choice. "You are choosing to delegate auth enforcement to the framework.
   This means..."

3. `cargo expand` output should be promoted as a first-class artifact, not a
   debugging tool. Seeing the generated code is an act of understanding, and
   understanding is the antidote to bad faith.

4. The supertrait chain should be visible in the contract's public interface.
   Users interacting with the contract should be able to see: "This contract
   is Pausable, which means it is also Ownable, which means..."

## Verdict

The framework automates the meaningful and leaves the mechanical. This is
backwards from an existentialist perspective but correct from an engineering
perspective. The tension between these two evaluations is productive: it
forces us to ask what meaning we assign to developer labor, and whether
automation enhances or diminishes that meaning.

The answer, as always in existentialism, is: it depends on the developer.
One who uses the framework authentically -- understanding what it does,
choosing it consciously, verifying the generated code -- retains their
freedom. One who uses it inauthentically -- treating it as a black box,
avoiding understanding, blaming the framework when things go wrong -- falls
into bad faith.

The framework cannot make this choice for them. It can only make authentic
use possible.

**Rating: N/A** -- Existence precedes evaluation.
