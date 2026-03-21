# Review: Yael -- Kibbutz Manager Exploring Cooperative Economics On-Chain

**Reviewer Profile:** Israeli kibbutz manager investigating how collective ownership, democratic decision-making, and shared resource management can be encoded in Soroban smart contracts.

**Materials Reviewed:**
- `docs/oz-comparison.md`
- `docs/blog-post-composable-contracts.md`
- `examples/trait-test/src/lib.rs`
- `soroban-sdk-tools-macro/src/contract.rs`

---

## Overall Impression

I manage a kibbutz of 400 members. Every week we vote on resource allocation: how many hours go to agriculture, how many to the factory, how much goes to the communal dining hall budget. The governance is messy, democratic, and deeply human. When I look at soroban-sdk-tools, I see a system designed for *hierarchical* authority (owner, admin, operator) rather than *collective* authority (assembly, quorum, delegation).

The toolkit is technically excellent. The provider pattern is genuinely innovative. But the mental model is "one entity owns, others comply." For cooperative economics, we need "many entities share, consensus governs."

**Rating: 3.5/5** -- Strong technical foundation, but the ownership model needs to be inverted for cooperative use cases.

---

## Strengths

### 1. Provider Swapping is Perfect for Governance Evolution

A kibbutz's governance evolves. We started with direct democracy (all members vote on everything). As we grew, we moved to representative councils. Some decisions are still made by full assembly vote.

The provider pattern maps beautifully to this evolution:

```rust
// Year 1: Direct democracy
impl_resource_allocator!(KibbutzContract, DirectDemocracyProvider);

// Year 5: Council-based
impl_resource_allocator!(KibbutzContract, CouncilProvider);

// Year 10: Hybrid (some decisions by council, others by assembly)
impl_resource_allocator!(KibbutzContract, HybridGovernanceProvider);
```

Changing ONE line to evolve governance without rewriting the entire contract is exactly what cooperatives need. This is the standout feature for my use case.

### 2. Supertrait Composition Models Real Cooperative Structure

A kibbutz has layered governance: the general assembly elects a secretariat, the secretariat appoints committee chairs, committees manage specific domains (agriculture, education, housing). The supertrait pattern models this:

```rust
#[contracttrait]
pub trait GeneralAssembly {
    fn member_count(env: &Env) -> u32;
    fn is_member(env: &Env, addr: Address) -> bool;

    #[auth(Self::secretariat)]
    fn add_member(env: &Env, new_member: Address);
}

#[contracttrait]
pub trait ResourceCommittee: GeneralAssembly {
    fn budget(env: &Env) -> i128;

    #[auth(Self::committee_chair)]
    fn allocate(env: &Env, recipient: Address, amount: i128);
}
```

The supertrait chain enforces that a `ResourceCommittee` can only exist within a `GeneralAssembly` context. This is structurally correct for cooperative governance.

### 3. Sealed Macros Prevent Governance Capture

The biggest risk in any cooperative is governance capture: a small group seizing control by manipulating the decision-making process. In on-chain cooperatives, this could mean overriding auth to bypass quorum requirements.

The sealed macro prevents this. If `impl_general_assembly!(KibbutzContract, QuorumProvider)` requires a 2/3 majority for constitutional changes, no individual developer can override that requirement. The auth is baked into the compiled contract.

### 4. AuthClient Enables Democratic Accountability Testing

Testing that "member X authorized this vote" is exactly what cooperative governance needs. The `AuthClient` pattern:

```rust
assembly_auth.add_member(&new_member)
    .authorize(&secretariat_address)
    .invoke();
```

This is not just a test convenience -- it is a model of democratic accountability. Each action traces back to a specific authorized entity. In a kibbutz, every decision has a paper trail. On-chain, the AuthClient ensures every test has an auth trail.

---

## Concerns

### 1. Single-Owner Model is Antithetical to Cooperatives

**Severity: Critical**

The entire example and documentation ecosystem revolves around `Ownable` -- a pattern where ONE address controls the contract. The blog post, the OZ comparison, and the trait-test example all center on single-owner authority.

Cooperatives do not have owners. They have members. Decisions are made collectively, not individually. The `#[auth(Self::owner)]` pattern structurally encodes hierarchy, not cooperation.

For cooperative use cases, the fundamental auth primitive should not be "does this address match the owner?" but "does this proposal have sufficient votes?"

**Recommendation:** Create a `Governable` trait alongside `Ownable` that structurally supports collective decision-making:

```rust
#[contracttrait]
pub trait Governable {
    fn is_member(env: &Env, addr: Address) -> bool;
    fn quorum(env: &Env) -> u32;
    fn proposal_approved(env: &Env, proposal_id: u64) -> bool;

    #[auth_proposal(Self::quorum)]  // requires quorum votes
    fn execute_proposal(env: &Env, proposal_id: u64);
}
```

This would require a new auth primitive beyond `address.require_auth()` -- something like "require N of M members to have previously authorized this proposal."

### 2. No Delegation or Liquid Democracy Support

**Severity: High**

In modern kibbutzim and cooperatives, members can delegate their voting power to trusted representatives. This is "liquid democracy" -- you can vote directly or delegate, and you can revoke delegation at any time.

The current auth model is binary: you either have auth or you do not. There is no concept of delegated authority, where member A authorizes member B to act on their behalf for specific traits or time periods.

**Recommendation:** Support delegated auth in the `#[auth]` attribute:

```rust
#[auth(Self::member_or_delegate)]
fn vote(env: &Env, voter: Address, proposal_id: u64, choice: bool);
```

Where `member_or_delegate` checks both direct membership and delegation chains.

### 3. No Threshold or Weighted Voting

**Severity: High**

Cooperative decisions often require different thresholds for different types of decisions:
- Day-to-day operations: simple majority
- Budget changes: 60% majority
- Constitutional amendments: 2/3 supermajority
- Dissolution: 3/4 or unanimous

The `#[auth]` attribute resolves to a single address. There is no mechanism for threshold-based authorization where the auth check is "have enough members signed this?"

The `MultisigOwner` provider mentioned in the blog post is a step in the right direction, but multisig is not the same as democratic voting. Multisig is "any 3 of 5 keyholders," while democratic voting is "51% of 400 members with a 7-day voting period."

**Recommendation:** The trait system should support temporal, threshold-based auth as a first-class concept. This likely requires a proposal/execution split where proposals accumulate votes over time and execution happens after quorum is reached.

### 4. Resource Pooling Has No Structural Pattern

**Severity: Medium**

A kibbutz pools resources. Members contribute labor, the cooperative generates revenue, and the proceeds are distributed according to need (not ownership). The current trait patterns model *access control* (who can call what) but not *resource pooling* (who contributes what, how is the pool managed, how are proceeds distributed).

The `FungibleToken` trait in the comparison document models individual balances (`balance(addr) -> i128`), not shared pools. For cooperative economics, the primitive is the shared pool, not the individual balance.

**Recommendation:** Document a cooperative token pattern where the fundamental unit is the pool, not the individual:

```rust
#[contracttrait]
pub trait ResourcePool: Governable {
    fn pool_balance(env: &Env) -> i128;
    fn member_contribution(env: &Env, member: Address) -> i128;

    #[auth(member)]
    fn contribute(env: &Env, member: Address, amount: i128);

    #[auth_proposal(Self::quorum)]
    fn distribute(env: &Env, proposal_id: u64, recipients: Vec<(Address, i128)>);
}
```

### 5. No Democratic Upgradeability

**Severity: Medium**

When a cooperative needs to upgrade its smart contract (change providers, add new traits, modify parameters), who authorizes the upgrade? The current model assumes the "owner" does. In a cooperative, contract upgrades should require democratic approval.

The provider swap pattern (`impl_ownable!(Contract, NewProvider)`) happens at compile time, which means the decision to change providers is made by whoever controls the source code and deployment pipeline -- not by the members.

**Recommendation:** Consider a runtime provider selection mechanism where the active provider can be changed via an on-chain governance vote, not just a recompilation.

---

## Use Case Exploration: Kibbutz Resource Management

Here is how I would want to model a kibbutz economy:

```rust
// Membership governed by democratic vote
#[contracttrait]
pub trait Membership {
    fn is_member(env: &Env, addr: Address) -> bool;
    fn member_count(env: &Env) -> u32;

    // Adding a member requires existing member proposal + vote
    #[auth(Self::secretariat)]
    fn add_member(env: &Env, new_member: Address);

    // Any member can propose to remove (subject to vote)
    #[auth(proposer)]
    fn propose_removal(env: &Env, proposer: Address, member: Address);
}

// Work allocation managed by work committee
#[contracttrait]
pub trait WorkAllocation: Membership {
    fn hours_assigned(env: &Env, member: Address) -> u32;

    #[auth(Self::work_coordinator)]
    fn assign_hours(env: &Env, member: Address, department: Symbol, hours: u32);

    #[auth(member)]
    fn log_hours(env: &Env, member: Address, department: Symbol, hours: u32);
}

// Revenue distribution governed by general assembly
#[contracttrait]
pub trait Distribution: Membership {
    fn pool_balance(env: &Env) -> i128;

    // Distribution requires general assembly approval
    #[auth(Self::secretariat)]  // but should really be quorum-based
    fn distribute_surplus(env: &Env, allocations: Vec<(Address, i128)>);
}
```

The toolkit can model the *structure* of this system (traits, supertraits, providers). What it cannot currently model is the *decision-making process* (proposals, votes, quorums, delegation). The auth primitive (`address.require_auth()`) is too atomic for collective decision-making.

---

## Comparison Analysis Feedback

The OZ comparison focuses on "who can call what" -- a hierarchical access control question. For cooperatives, the question is "how do we collectively decide what gets called" -- a governance question.

The blog post mentions "multisig" as a provider option, but multisig is not governance. A 3-of-5 multisig is an oligarchy, not a democracy. True cooperative governance requires:

1. Proposal submission by any member
2. A voting period with quorum requirements
3. Vote tallying (possibly weighted by contribution or seniority)
4. Automated execution upon approval
5. Transparency of all votes

None of these are modeled in the current trait system. They could be built as providers, but the auth primitive itself (`address.require_auth()`) does not naturally extend to collective authorization.

---

## The Cooperative Perspective on Security

The blog post frames security as "preventing unauthorized access." In a cooperative, security also means:

1. **No single point of failure** -- no one member can unilaterally control the contract
2. **Transparency** -- all members can see all actions and their authorizations
3. **Reversibility** -- bad decisions can be reversed by democratic vote
4. **Inclusivity** -- all members have equal access to governance functions

The sealed macro addresses point 1 (no one can bypass auth), but the other three points are not structurally supported.

---

## Summary

soroban-sdk-tools is built on a fundamentally individualist model: one owner, one auth check, one decision-maker. For cooperatives, the model needs to be inverted: many members, collective auth, democratic decisions.

The *architecture* is excellent -- providers, supertraits, sealed macros are all the right building blocks. But the *primitives* assume hierarchy. To serve cooperative economics, the toolkit needs:

1. **Collective auth primitives** (quorum-based authorization, not just single-address)
2. **Delegation support** (liquid democracy, revocable delegation)
3. **Threshold voting** (different quorum requirements for different decision types)
4. **Resource pooling patterns** (shared pools, not just individual balances)
5. **Democratic upgradeability** (governance-controlled provider swaps)

The kibbutz has survived for over a century because it adapts its governance to its community's needs. Give us the tools to encode that adaptability on-chain, and you will have a user base that knows the value of shared infrastructure.
