# Review: soroban-sdk-tools -- Democratic Governance & Worker Cooperatives

**Reviewer:** Esperanza -- Labor union organizer exploring DAOs for worker cooperatives
**Date:** 2026-03-21
**Materials Reviewed:** oz-comparison.md, blog-post-composable-contracts.md, trait-test example, contract.rs macro

---

## Overall Impression

I organize garment workers in Central America. We are building a worker
cooperative that will own its production facility collectively. I have been
told that DAOs (decentralized autonomous organizations) can encode
cooperative governance rules in smart contracts -- rules that cannot be
changed by management, that enforce democratic voting, and that distribute
profits fairly.

I am evaluating `soroban-sdk-tools` through one lens: **can this tooling
help us build governance systems where workers have real power, not just
the illusion of power?**

The answer is nuanced. The provider pattern and structural auth are
excellent foundations for governance. But the current tooling is
designed for ownership models, not governance models. Ownership and
governance are different things, and the difference matters enormously
for worker cooperatives.

---

## Ownership vs. Governance

### The Ownable Trap

The `Ownable` trait -- the primary example in all the documentation --
models a single-owner pattern:

```rust
#[contracttrait]
pub trait Ownable {
    fn owner(env: &Env) -> Address;

    #[auth(Self::owner)]
    fn transfer_ownership(env: &Env, new_owner: Address);
}
```

This is fundamentally plutocratic. One owner. One decision-maker. The owner
can transfer ownership to anyone without consent from anyone else. This is
the governance model of a traditional corporation, encoded in code.

For a worker cooperative, ownership is collective. Decisions require votes.
No single person can transfer the organization's assets. The `Ownable`
trait does not model this.

### What Worker Cooperatives Need

A cooperative governance trait would look fundamentally different:

```rust
#[contracttrait]
pub trait Cooperative {
    fn member_count(env: &Env) -> u32;
    fn is_member(env: &Env, addr: Address) -> bool;
    fn quorum(env: &Env) -> u32;  // minimum votes needed

    #[auth(proposer)]
    fn propose(env: &Env, proposer: Address, action: ProposalAction) -> u64;

    #[auth(voter)]
    fn vote(env: &Env, voter: Address, proposal_id: u64, in_favor: bool);

    fn execute_proposal(env: &Env, proposal_id: u64);
    // No #[auth] -- anyone can execute a passed proposal
}
```

Notice the differences:

1. **No single owner** -- There is no `owner()` function. Authority is
   distributed across members.

2. **Proposal-based actions** -- Changes happen through proposals, not
   direct execution. This enforces deliberation.

3. **One person, one vote** -- The `#[auth(voter)]` pattern ensures each
   member votes as themselves. No one can vote on behalf of another.

4. **Quorum requirements** -- The provider enforces minimum participation
   before a proposal can execute.

5. **Separation of voting from execution** -- Anyone can trigger execution
   of a passed proposal. This prevents a single person from blocking
   implementation.

---

## The Provider Pattern for Democratic Governance

### What Excites Me

The provider pattern is genuinely powerful for governance because different
cooperatives have different governance models:

```rust
// Simple majority -- one member, one vote
pub struct SimpleMajority;

// Supermajority -- 2/3 required for major decisions
pub struct Supermajority;

// Consensus -- all members must agree
pub struct Consensus;

// Liquid democracy -- members can delegate votes
pub struct LiquidDemocracy;

// Quadratic voting -- cost of additional votes increases quadratically
pub struct QuadraticVoting;
```

Each of these implements `CooperativeInternal` with different voting
mechanics. The cooperative chooses its governance model by selecting a
provider. If the cooperative wants to change its governance model (e.g.,
from simple majority to supermajority as it grows), it proposes the change
through the existing governance system.

This is deeply aligned with cooperative principles: **the members choose
how they are governed.**

### What Concerns Me

1. **The sealed pattern is anti-democratic** -- The `impl_cooperative!`
   sealed macro makes the governance rules non-overridable. For security
   against external attackers, this is good. But for a cooperative, the
   members should be able to change ANY rule through democratic process,
   including the governance rules themselves. A cooperative whose
   constitution cannot be amended is not a democracy -- it is a
   constitutional monarchy.

   The solution: the sealed macro should support a "democratic override"
   -- a proposal-based mechanism for upgrading the contract, including
   the governance rules. The override itself must go through the
   governance process.

2. **Provider swapping requires code deployment** -- Changing from
   `SimpleMajority` to `Supermajority` requires deploying a new contract
   (with the new provider). This is a technical operation that most
   cooperative members cannot perform. The governance framework needs
   a runtime provider selection mechanism, not just a compile-time one.

3. **No delegation model** -- Many cooperatives use delegation: if a
   worker cannot attend a vote, they delegate their vote to a trusted
   colleague. The current `#[auth(voter)]` pattern requires the voter
   to sign personally. A delegation extension is needed:

   ```rust
   #[auth(voter_or_delegate(voter))]
   fn vote(env: &Env, voter: Address, proposal_id: u64, in_favor: bool);
   ```

---

## Anti-Plutocracy Analysis

### Token-Weighted Voting Is Not Democracy

The blog post mentions `FungibleToken` as a composable trait. In many DAOs,
voting power is proportional to token holdings. This is plutocracy -- rule
by the wealthy. For worker cooperatives, voting power must be equal
regardless of token holdings.

The provider pattern supports this because the `CooperativeInternal`
provider can enforce one-member-one-vote regardless of token balances. But
the documentation should explicitly address the difference between:

- **Token-weighted governance** (plutocratic) -- voting power = token balance
- **Member-based governance** (democratic) -- voting power = 1 per member
- **Reputation-based governance** (meritocratic) -- voting power = contribution

And recommend member-based governance for cooperatives.

### Sybil Resistance

One-person-one-vote requires Sybil resistance: preventing one person from
creating multiple identities to get multiple votes. The `#[auth(voter)]`
pattern ensures one ADDRESS per vote, but one person can control multiple
addresses.

For cooperatives, Sybil resistance typically comes from off-chain identity
verification (you must be a verified worker to be a member). The on-chain
system trusts the membership registration process. The `#[auth(Self::owner)]`
on `register_member` ensures only authorized entities can add new members.

But this creates a power concentration problem: whoever controls member
registration controls the cooperative. The registration function itself
should be governance-controlled:

```rust
#[contracttrait]
pub trait Membership: Cooperative {
    // Adding a member requires a governance proposal, not owner auth
    fn add_member(env: &Env, member: Address);
    // This method has NO #[auth] -- it is called by execute_proposal()
    // which itself checks that the proposal passed through governance

    #[auth(member)]
    fn resign(env: &Env, member: Address);
    // A member can resign voluntarily
}
```

### Protection Against Hostile Takeover

Worker cooperatives are vulnerable to hostile takeover: a well-funded
entity buys out enough members to control governance. The sealed pattern
can protect against this:

```rust
impl CooperativeInternal for AntiTakeoverProvider {
    fn vote(env: &Env, voter: Address, proposal_id: u64, in_favor: bool) {
        // Cap: no single entity can control more than 10% of total votes
        // through delegates or direct voting
        let voter_influence = get_influence(env, &voter);
        assert!(voter_influence < MAX_INFLUENCE, "influence cap exceeded");
        record_vote(env, &voter, proposal_id, in_favor);
    }
}
```

The sealed macro ensures this protection cannot be bypassed. This is where
`impl_cooperative!` becomes a tool of worker protection, not just code
security.

---

## Practical Governance Scenarios

### Scenario 1: Wage Decision

The cooperative votes on wages. Each worker is paid equally (cooperative
principle). A proposal to change the wage must pass by simple majority.

```
Proposal: Increase hourly wage from $12 to $14
Proposer: Maria (worker #47)
Quorum: 51% of 200 members = 102 votes needed
Result: 128 yes, 45 no, 27 abstain
Status: PASSED (128 > 102, and 128 > 45)
```

The `#[auth(voter)]` pattern ensures each worker votes exactly once. The
provider enforces quorum. The sealed macro ensures no one can bypass the
vote and set wages directly.

### Scenario 2: Expelling a Member

A worker is accused of misconduct. Expulsion requires a 2/3 supermajority
(to protect against arbitrary expulsion).

```
Proposal: Expel worker #23 for misconduct
Proposer: Ethics Committee (multi-sig of 3 elected members)
Quorum: 67% of 200 members = 134 votes needed
Vote threshold: 67% in favor (supermajority)
```

This requires a different provider than the wage decision. The provider
pattern allows different voting rules for different proposal types. The
`ProposalAction` enum would distinguish between `WageChange` (simple
majority) and `MemberExpulsion` (supermajority).

### Scenario 3: Constitutional Amendment

The cooperative wants to change its governance rules (e.g., change quorum
from 51% to 60%). This is a meta-governance action -- governance about
governance.

This is where the sealed pattern becomes a problem. If the governance
rules are sealed, they cannot be changed. The solution is a two-layer
architecture:

1. **Core contract** (sealed) -- Handles basic mechanics (vote recording,
   quorum checking, proposal execution)
2. **Constitution contract** (upgradeable) -- Stores governance parameters
   (quorum thresholds, voting periods, influence caps)

The core contract reads parameters from the constitution contract. The
constitution contract can be upgraded through the governance process. This
way, the mechanics are sealed (secure), but the parameters are
democratically adjustable.

---

## Worker Ownership of Code

### Who Writes the Contract?

A fundamental concern: the cooperative will hire a developer to write the
governance contract. That developer is not a member of the cooperative. They
have power over the cooperative's rules simply by being the one who writes
the code.

The tooling should support a pattern where:

1. The developer writes the framework (traits, providers, tests)
2. The cooperative REVIEWS the framework (through auditors they trust)
3. The cooperative DEPLOYS the contract (through a ceremony with member
   witnesses)
4. The developer has NO ongoing access (no admin keys, no upgrade rights)

The sealed pattern supports step 4 -- once deployed, the developer cannot
change the rules. But steps 2 and 3 require documentation and tooling that
does not currently exist.

### Code Audibility for Non-Programmers

The blog post claims the generated code is "identical to hand-written
code." This is good for auditors but irrelevant for workers. Workers
need to understand WHAT the contract does, not HOW it does it.

A "cooperative constitution" view -- a plain-language rendering of the
trait definitions and their auth rules -- would be invaluable:

```
GOVERNANCE RULES (generated from contract):

1. Any member can propose an action.
   - The proposer must authenticate themselves.

2. Any member can vote on a proposal.
   - Each member gets exactly one vote.
   - The voter must authenticate themselves.

3. A proposal passes when:
   - At least 51% of members have voted (quorum)
   - More votes are in favor than against (simple majority)

4. Anyone can execute a passed proposal.
   - No special permission is needed.

5. These rules CANNOT be changed without deploying a new contract.
   - Deploying a new contract itself requires a governance vote.
```

The `#[contracttrait]` macro could generate this plain-language
constitution alongside the code.

---

## Critique of the Blog Post

### What Is Missing

1. **No mention of governance** -- The blog post discusses ownership,
   pausability, and tokens. It does not mention governance, voting, or
   collective decision-making. This is a significant omission for a
   composability framework. Governance is THE use case for composable
   smart contracts in the cooperative economy.

2. **The word "owner" appears 47 times** -- The entire framing is
   ownership-centric. For cooperatives, there is no "owner." There are
   "members" with equal rights. The documentation should include examples
   of collective governance, not just single-owner patterns.

3. **No discussion of power dynamics** -- The blog post treats auth as a
   technical concern ("who can call this function"). But auth is
   fundamentally a POWER concern ("who has authority and why"). The
   documentation should discuss the political implications of different
   auth models.

### What Works

1. **The provider pattern as political choice** -- The ability to choose
   between `SimpleMajority`, `Supermajority`, and `Consensus` providers
   is a political choice encoded in code. This is exactly what cooperatives
   need: the ability to choose their own governance model and have it
   enforced structurally.

2. **The sealed pattern as constitutional protection** -- The ability to
   prevent rule changes (while allowing parameter changes through
   governance) is a powerful constitutional mechanism. This is the digital
   equivalent of a constitutional clause that requires supermajority to
   amend.

3. **AuthClient for governance testing** -- Testing that only members can
   vote, that non-members are rejected, that quorum is enforced -- this is
   exactly the testing a cooperative needs before trusting its governance
   to a smart contract.

---

## Recommendations for Worker Cooperative Use

### 1. Build a Governance Trait Library

The project needs composable governance traits:

- `Membership` -- member registration, resignation, expulsion
- `Proposal` -- proposal creation, voting, execution
- `Treasury` -- collective fund management, spending proposals
- `Delegation` -- vote delegation and revocation
- `Constitution` -- governance parameter management

Each trait should have multiple providers representing different governance
philosophies.

### 2. Add One-Person-One-Vote as a First-Class Pattern

The `#[auth(voter)]` pattern ensures the voter signs. But it does not
enforce that the voter is a member or that they have not already voted.
These checks happen in the provider. Consider structural enforcement:

```rust
#[auth(voter)]
#[require(Self::is_member(voter))]
#[require(!Self::has_voted(voter, proposal_id))]
fn vote(env: &Env, voter: Address, proposal_id: u64, in_favor: bool);
```

### 3. Document the Power Implications

Every `#[auth]` annotation is a power decision. Document this explicitly:

- `#[auth(Self::owner)]` -- Power is concentrated in one entity
- `#[auth(voter)]` -- Power is distributed among voters
- `#[auth(Self::has_role("admin"))]` -- Power is delegated to a role
- No `#[auth]` -- Action is permissionless (anyone can do it)

### 4. Support Multi-Sig as a Provider

Multi-sig (multiple signatures required) is the simplest form of
collective authorization. The provider pattern already supports this:

```rust
pub struct MultisigOwner;
impl OwnableInternal for MultisigOwner {
    fn owner(env: &Env) -> Address {
        // Returns a multi-sig address
        MultisigStorage::get_multisig(env)
    }
}
```

But multi-sig has limitations: it requires a fixed set of signers and a
fixed threshold. For cooperatives where membership changes, a governance
proposal system is more appropriate.

### 5. Address Worker Safety Concerns

Cooperatives in some regions face hostile environments -- anti-union
governments, predatory corporations. The governance contract must:

- Protect member identities (see privacy review)
- Prevent external entities from observing voting patterns
- Resist censorship (proposals cannot be blocked by external parties)

These are political requirements, not just technical ones. The
documentation should acknowledge them.

---

## Verdict

The `soroban-sdk-tools` framework has the technical foundations to support
democratic governance. The provider pattern enables different governance
models. The sealed macro protects governance rules from unilateral change.
The AuthClient enables testing of democratic processes.

But the project is currently framed entirely around ownership, not
governance. For worker cooperatives, this framing is exclusionary. The
word "owner" carries specific political meaning -- it implies a hierarchy
that cooperatives reject.

To serve the cooperative economy, the project needs:

1. Governance-specific traits and providers
2. Plain-language constitution generation
3. One-person-one-vote as a structural pattern
4. Delegation support
5. Power-aware documentation

The foundation is right. The politics need work.

**Rating:** 6/10 -- Technically capable of supporting democratic governance,
but culturally and documentationally oriented toward hierarchical ownership.

---

*"Every line of code is a political decision. #[auth(Self::owner)] means
one person decides. #[auth(voter)] means the people decide. The difference
is democracy."*
