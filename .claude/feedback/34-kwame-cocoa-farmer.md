# Review: soroban-sdk-tools -- Fair Trade Cocoa Verification

**Reviewer:** Kwame -- Ghanaian cocoa farmer exploring blockchain for fair trade
**Date:** 2026-03-21
**Materials Reviewed:** oz-comparison.md, blog-post-composable-contracts.md, trait-test example, contract.rs macro

---

## Overall Impression

I am not a programmer. I grow cocoa in the Ashanti region of Ghana. I sell
my beans to cooperatives who sell to exporters who sell to manufacturers who
sell to consumers. At every step, someone takes a cut and the story of my
cocoa gets lost. I am exploring blockchain because I was told it could prove
that my cocoa is fair trade -- that I was paid a fair price, that no child
labor was involved, that the beans are traceable from my farm to the
chocolate bar.

I am reviewing this documentation not as a developer but as a potential
end-user of a system built with these tools. My question is simple: **can
I understand the guarantees this system provides, and do those guarantees
matter for my cocoa?**

---

## Can I Understand What This Does?

### The Blog Post

The blog post is written for programmers. I understand perhaps 30% of it.
But certain concepts translate clearly:

1. **"The developer never writes auth code"** -- I understand this. In my
   supply chain, the person who checks whether a buyer is authorized should
   not be the same person who decides to sell. The system should enforce
   authorization automatically. If the computer always checks who has
   permission, that is better than trusting a person to check.

2. **"Swap implementations without changing consumer code"** -- I
   understand this partially. If I currently use one certification body
   and want to switch to another, the system should allow that without
   rebuilding everything. Like changing which weighing scale I use without
   changing how I record the weight.

3. **"Sealed auth patterns that prevent override-based security holes"** --
   This I understand deeply. In my experience, the most dangerous moment
   in the supply chain is when someone can override the rules. A middleman
   who can override the price verification, an exporter who can override
   the origin certification. If the system prevents overrides, that is
   powerful.

### The OZ Comparison

The comparison table (page 1, "Key Differences") is actually accessible to
me. The row "Override protection: None vs impl_ownable! macro (sealed)" tells
me something important: one system allows rule-breaking, the other does not.

For fair trade verification, "sealed" is the only acceptable option. If the
certification body that verifies my fair trade status can have its
verification logic overridden by the buyer, the certification is meaningless.

### The Code Examples

The code itself is opaque to me. But I can read the trait definition:

```rust
pub trait Ownable {
    fn owner(env: &Env) -> Address;

    #[auth(Self::owner)]
    fn transfer_ownership(env: &Env, new_owner: Address);
}
```

Even without understanding Rust, I can read the shape of this. There is an
"owner." The owner can "transfer ownership." The `#[auth(Self::owner)]` line
means only the owner can do the transfer. This is clear. This is a pattern
I can understand even if I cannot write the code.

---

## Does This Matter for My Cocoa?

### The Supply Chain Problem

My cocoa goes through these stages:

1. **Farm** -- I harvest and ferment the beans (me, Kwame)
2. **Cooperative** -- We aggregate beans from multiple farms
3. **Exporter** -- Buys from cooperatives, ships to Europe/US
4. **Manufacturer** -- Roasts and processes into chocolate
5. **Retailer** -- Sells to consumers

At each stage, someone claims the cocoa is "fair trade." But there is no
system that PROVES it. Certificates are paper. Paper can be forged. The
consumer has no way to verify.

### How This Tooling Could Help

Using the `#[contracttrait]` pattern, a supply chain verification system
could look like this (I am told by my technical advisor):

```rust
#[contracttrait]
pub trait SupplyChain: Ownable {
    fn record_harvest(env: &Env, farm: Address, batch_id: u64, weight_kg: u32);

    #[auth(Self::owner)]
    fn register_farm(env: &Env, farm: Address, location: GeoCoord);

    #[auth(farm)]
    fn submit_harvest(env: &Env, farm: Address, batch_id: u64, weight_kg: u32);

    #[auth(cooperative)]
    fn confirm_receipt(env: &Env, cooperative: Address, batch_id: u64);
}
```

The `#[auth(farm)]` means only I can submit my own harvest record. No one
can submit a harvest on my behalf. This is important because ghost harvests
(recording harvests that did not happen) are a real problem in cocoa
supply chains.

The `#[auth(cooperative)]` means only the cooperative can confirm they
received my beans. This creates a two-party verification: I say I sent it,
they say they received it. If both records exist, the transfer is verified.

### The Provider Pattern for Certification

Different fair trade certifications have different rules:

- **Fairtrade International** -- Minimum price guarantee + social premium
- **Rainforest Alliance** -- Environmental sustainability standards
- **UTZ** (now merged with RA) -- Good agricultural practices
- **Direct Trade** -- Buyer-farmer relationship, no middleman

If each certification is a "provider" (in the tooling's language), then a
single supply chain contract could support multiple certification standards.
A farm could be certified under multiple schemes simultaneously, and the
consumer could verify against whichever standard they care about.

This is genuinely useful. Today, switching certification bodies requires
completely new paperwork and processes. If it were a provider swap, it
could be done without disrupting the supply chain.

---

## What a Non-Programmer Needs to See

### Guarantees I Can Verify

The documentation talks about "compile-time guarantees" and "structural
enforcement." These are programmer concepts. For me, I need to know:

1. **Can someone change the price I was paid after the transaction?**
   (Immutability guarantee)

2. **Can someone submit a harvest record in my name without my permission?**
   (Auth guarantee -- the `#[auth(farm)]` pattern answers "no")

3. **Can the certification body change the rules after my cocoa was
   certified?** (Sealed auth guarantee -- `impl_supply_chain!` means no)

4. **Can I see the complete history of my cocoa batch from farm to
   consumer?** (Transparency guarantee -- not addressed in the docs)

5. **Can a buyer verify my cocoa's fair trade status without trusting a
   third party?** (Verification guarantee -- partially addressed)

The documentation should include a "Guarantees for Non-Technical Users"
section that answers these questions in plain language, without code.

### Visual Representation

The trait definition is surprisingly readable even to me. But a visual
representation would be even better:

```
[Kwame's Farm] --auth(farm)--> [Submit Harvest]
                                      |
                                      v
[Cooperative] --auth(cooperative)--> [Confirm Receipt]
                                      |
                                      v
[Exporter] --auth(exporter)--> [Record Export]
                                      |
                                      v
[Consumer] <-- [Verify Full Chain]
```

Each arrow shows who must authorize the action. This is the `#[auth]`
pattern made visual. The blog post should include diagrams like this.

---

## Specific Concerns for Supply Chain Use

### 1. Identity Without Smartphones

Many cocoa farmers do not have smartphones. Our identity in blockchain
systems is usually mediated through the cooperative. The `#[auth(farm)]`
pattern assumes the farm has its own cryptographic identity (private key).
In practice, the cooperative often holds keys on behalf of farmers.

This creates a trust problem: if the cooperative holds my key, they can
submit harvests on my behalf. The structural auth guarantee (`#[auth(farm)]`)
is only as strong as the key management.

The documentation should address this: **who holds the keys?** The provider
pattern could support different key management models:

- `SelfCustodyProvider` -- farmer holds their own key (ideal but rare)
- `CooperativeCustodyProvider` -- cooperative holds keys (common, trusted)
- `SharedCustodyProvider` -- multi-sig between farmer and cooperative

### 2. Offline Operation

My farm does not have reliable internet. Transactions need to happen
offline and sync later. The documentation does not address offline
operation at all. For agricultural supply chains, this is not optional --
it is essential.

### 3. Cost Per Transaction

Every transaction on the blockchain costs money. In my supply chain, the
value of a single bag of cocoa beans (65 kg) might be $150-200. If each
transaction costs $0.10, that is manageable. If it costs $5.00, it is
not viable. The documentation does not address transaction costs.

### 4. Dispute Resolution

What happens when I say I sent 500 kg of beans but the cooperative says
they only received 450 kg? The smart contract records both claims. But
the dispute resolution mechanism is not addressed. The `Ownable` pattern
determines who has authority, but it does not model conflict resolution.

A `Disputable` trait could address this:

```rust
#[contracttrait]
pub trait Disputable {
    #[auth(party)]
    fn raise_dispute(env: &Env, party: Address, batch_id: u64, claim: DisputeClaim);

    #[auth(Self::owner)]  // resolved by contract owner/arbitrator
    fn resolve_dispute(env: &Env, dispute_id: u64, resolution: Resolution);
}
```

### 5. Language and Literacy

The blog post is in English. The code comments are in English. The error
messages are in English. Cocoa farmers in Ghana speak Twi, Fante, Ewe,
Dagbani. A supply chain system built with these tools would need
localization at the interface layer. The tooling itself does not need to
be localized, but the documentation should acknowledge that the end-user
interface is a separate concern.

---

## What I Like Most

### The Sealed Pattern

The concept of a "sealed" contract -- one where the rules cannot be changed
after deployment -- is the single most important feature for supply chain
verification. In my experience, the problem is never the rules themselves.
The problem is that powerful people change the rules when it suits them.

If I can tell a consumer: "This contract is sealed. No one -- not me, not
the cooperative, not the exporter -- can change the verification rules,"
that is a guarantee no paper certificate can provide.

The `impl_supply_chain!` macro, as I understand it, provides exactly this
guarantee. This is worth building on.

### The Auth Pattern

The `#[auth(farm)]` pattern is intuitive even to me. It says: "Only the
farm can do this action." It is a permission written directly into the
structure of the system, not a policy that someone enforces (or forgets
to enforce). This is the difference between a lock and a promise.

In fair trade, we have too many promises and not enough locks.

---

## What Concerns Me

### 1. Complexity Gap

The gap between what I need to understand (who can do what) and what the
documentation explains (two-trait generation, provider-based DI, sealed
auth macros) is vast. I need a bridge. That bridge is not technical
documentation -- it is a human translator who can show me the system and
explain what it guarantees in my language.

The project should invest in partnerships with agricultural cooperatives
who can serve as this bridge. Technology without translation is
exclusionary.

### 2. Governance

Who deploys the supply chain contract? Who chooses the provider? Who
decides to seal the contract? These governance questions are not technical
questions -- they are power questions. If the exporter deploys the contract
and chooses the provider, the system serves the exporter's interests, not
mine.

The documentation should address governance models for supply chain
contracts. Who has authority, and how is that authority established?

### 3. Sustainability

Blockchain projects come and go. If I build my supply chain verification
on this tooling and the project is abandoned in two years, what happens?
The sealed contracts continue to work (they are on-chain), but no one can
help me understand or maintain them.

The project needs a sustainability plan that goes beyond the current
two-person team. For supply chain use, there must be institutional
backing or a community large enough to sustain the project independently.

---

## Verdict

I cannot evaluate the code. But I can evaluate the guarantees.

The guarantees this system provides -- sealed auth, structural enforcement,
provider-based flexibility -- are exactly what fair trade verification needs.
The ability to say "this contract's rules cannot be overridden" is more
valuable than any certificate.

But the system is not accessible to me or to the cooperatives I work with.
It is built for programmers, documented for programmers, and tested by
programmers. For it to serve farmers, it needs:

1. Plain-language guarantee documentation
2. Visual diagrams of auth flows
3. Partnerships with cooperatives for deployment and translation
4. Offline operation support
5. Low transaction costs
6. Dispute resolution patterns
7. Governance frameworks

The foundation is right. The direction is right. But the bridge between the
technology and the people it could serve is not yet built.

**Rating:** 6/10 -- The guarantees are genuine and powerful. The
accessibility is insufficient. Build the bridge.

---

*"I do not need to understand how the lock works. I need to know that it
cannot be opened by anyone except me. That is what 'sealed auth' means,
and that is what my cocoa needs."*
