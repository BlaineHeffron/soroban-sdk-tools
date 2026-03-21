# Review: Nadia -- Peace-Building Technologist

**Reviewer:** Nadia, former child soldier turned peace-building technologist
**Focus:** Trust without institutions, reconciliation systems, power accountability
**Date:** 2026-03-21
**Materials Reviewed:**
- `docs/oz-comparison.md`
- `docs/blog-post-composable-contracts.md`
- `examples/trait-test/src/lib.rs`
- `soroban-sdk-tools-macro/src/contract.rs`

---

## Context: Why I Care About Smart Contract Authorization

I spent my childhood in a place where institutions failed. The people who were
supposed to protect us were the ones with the guns. When I say "trust without
institutions," I do not mean it as an abstract design principle. I mean it as
a survival requirement.

Smart contracts interest me because they offer something I never had growing
up: accountability that does not depend on the goodwill of the person in power.
The authorization system in `soroban-sdk-tools` is, at its core, a system for
constraining power. And constraining power is the most important thing
technology can do.

I review this codebase through the lens of: could this be used to build systems
that prevent abuse of power, enable reconciliation, and create accountability
in contexts where institutions have collapsed?

---

## 1. Structural Auth Enforcement: Power That Cannot Be Secretly Expanded

The blog post's central claim is:

> "The developer never writes auth code. They implement OwnableInternal with
> pure business logic, and the generated Ownable trait handles auth enforcement
> structurally."

This is the most important sentence in the entire documentation.

### Why Structural Enforcement Matters for Peace-Building

In post-conflict settings, we build governance systems. Transitional authorities.
Truth commissions. Reparation funds. Every one of these requires clear answers
to: "Who can authorize what?"

The traditional answer is: "The person we appointed." The structural answer is:
"The code enforces it, and the code is auditable."

The `#[auth(Self::owner)]` annotation creates a structural constraint:

```rust
fn transfer_ownership(env: &Env, new_owner: Address) {
    let __auth_addr = Self::Provider::owner(env);
    __auth_addr.require_auth();
    Self::Provider::transfer_ownership(env, new_owner)
}
```

The `require_auth()` call is not optional. It is not a guideline. It is not a
"best practice." It is generated code that is part of the function body. To
bypass it, you would have to rewrite the contract, redeploy it, and that
redeployment is visible on-chain.

**This is accountability infrastructure.**

### The Override Problem Is a Power Problem

The OZ comparison document describes how trait defaults can be overridden:

```rust
#[contractimpl(contracttrait)]
impl Ownable for MyContract {
    fn transfer_ownership(e: &Env, new_owner: Address, live_until_ledger: u32) {
        // Override: forgot the auth check!
    }
}
```

In a peace-building context, "forgot the auth check" is not an accidental bug.
It is a backdoor. When a transitional authority's tech team can quietly deploy
a contract that omits authorization on fund transfers, that is not a software
defect -- it is corruption.

The sealed macro (`impl_ownable!`) prevents this. Inherent methods cannot be
overridden. The auth check is baked in.

**Assessment: The sealed macro is essential for governance applications.**

---

## 2. The Provider Pattern and Power Transition

### Peaceful Transfer of Power

The `transfer_ownership` method is, fundamentally, a model of power transfer.
The current holder authorizes the transfer. The new holder receives authority.

In peace-building, power transitions are the most dangerous moments. Coups
happen at transitions. Elections are contested at transitions. The moment of
handover is when things break.

The current implementation is a one-step transfer:

```rust
#[auth(Self::owner)]
fn transfer_ownership(env: &Env, new_owner: Address);
```

The OZ comparison correctly notes that two-step transfers (transfer + accept)
are safer. But for governance applications, even two-step is not enough. I
need:

1. **Nomination** -- The current holder nominates a successor
2. **Vetting period** -- A waiting period during which objections can be raised
3. **Community consent** -- A quorum of stakeholders must approve
4. **Acceptance** -- The nominated successor accepts
5. **Handover** -- Authority transfers

This is a five-step process. It maps to a state machine:

```
Nominated -> VettingPeriod -> Approved -> Accepted -> Transferred
         \-> Vetoed (returns to current holder)
```

### How the Provider Pattern Helps

The provider pattern makes this implementable without changing the trait:

```rust
pub struct GovernanceTransfer;
impl OwnableInternal for GovernanceTransfer {
    fn transfer_ownership(env: &Env, new_owner: Address) {
        // Does not actually transfer -- initiates the process
        GovernanceStorage::set_nominee(env, &new_owner);
        GovernanceStorage::set_vetting_start(env, env.ledger().timestamp());
    }
}
```

But this is a hack. The `transfer_ownership` method name implies immediate
transfer. The provider makes it mean something different. This is a semantic
mismatch.

**Recommendation:** For governance applications, provide a separate trait:

```rust
#[contracttrait]
pub trait GovernanceTransition {
    fn current_authority(env: &Env) -> Address;
    fn nominee(env: &Env) -> Option<Address>;
    fn transition_status(env: &Env) -> TransitionStatus;

    #[auth(Self::current_authority)]
    fn nominate(env: &Env, nominee: Address);

    fn approve(env: &Env, approver: Address);  // requires quorum logic

    #[auth(nominee)]  // the nominee themselves must accept
    fn accept(env: &Env, nominee: Address);

    #[auth(Self::current_authority)]
    fn veto(env: &Env);
}
```

---

## 3. Multi-Party Authorization: The Missing Piece

In every peace process I have worked on, decisions are not made by one person.
They are made by committees, councils, quorums. The UN Security Council needs
9 of 15 members (with no P5 veto) to act. Truth commissions require
commissioner consensus. Reparation boards require community representatives.

The current `#[auth]` system supports only single-party authorization:

```rust
#[auth(Self::owner)]  // one address
```

For peace-building, I need:

```rust
#[auth(quorum: Self::council_members, threshold: 2/3)]
fn authorize_reparation(env: &Env, recipient: Address, amount: i128);
```

Or at minimum:

```rust
#[auth(Self::chair, Self::vice_chair)]  // both must sign
fn certify_finding(env: &Env, finding_id: u64);
```

### Provider-Level Workaround

The provider pattern allows a workaround -- the provider can check for
multi-party auth internally:

```rust
impl TruthCommissionInternal for QuorumProvider {
    fn certify_finding(env: &Env, finding_id: u64) {
        let votes = CommissionStorage::get_votes(env, finding_id);
        assert!(votes.len() >= quorum_threshold(), "insufficient votes");
        CommissionStorage::mark_certified(env, finding_id);
    }
}
```

But this puts auth logic in the provider, which defeats the purpose of
structural auth enforcement. The `#[auth]` annotation should handle
multi-party authorization in the outer trait, not push it to the provider.

**Strong Recommendation:** Multi-party auth must be a first-class feature
of `#[contracttrait]` for this system to be useful in governance contexts.

---

## 4. Transparency and Accountability: Event Emission

### Truth Commissions Need Records

A truth commission's work is meaningless without records. Every testimony
received, every finding certified, every reparation authorized must be
recorded in an immutable, publicly accessible format.

On Soroban, this means events. Every state change must emit an event.

The OZ comparison notes:

> "Event emission -- OZ emits events for every state change. We should
> generate events in the outer trait defaults."

This is not a "should." For governance applications, it is a "must."

### What Events Should Be Generated

For the `Ownable` trait:

```
OwnerQueried { queried_by: Address }  // optional, for audit
OwnershipTransferred { from: Address, to: Address, timestamp: u64 }
```

For a governance trait:

```
NominationMade { nominator: Address, nominee: Address, timestamp: u64 }
VettingStarted { nominee: Address, end_date: u64 }
ApprovalVoted { approver: Address, nominee: Address, vote: bool }
TransitionCompleted { old_authority: Address, new_authority: Address }
TransitionVetoed { authority: Address, nominee: Address, reason: String }
```

Without auto-generated events, every provider developer must remember to
emit them. In a governance context, a missing event is a gap in the
accountability record. That gap can be exploited.

**Assessment: Event generation is the single most critical missing feature
for governance applications.**

---

## 5. Role-Based Access Control: Who Guards the Guards?

The OZ comparison mentions role-based access control:

> "Role-based access control -- OZ's RBAC with role enumeration, role
> admins, and two-step admin transfer is comprehensive. This should be
> a provider in our system."

For peace-building, RBAC is fundamental. But it needs to go beyond
traditional RBAC in several ways:

### 5.1 Temporal Roles

In transitional justice, roles have time limits:

```rust
#[contracttrait]
pub trait TemporalRBAC {
    fn has_role(env: &Env, account: Address, role: Symbol) -> bool;
    fn role_expiry(env: &Env, account: Address, role: Symbol) -> Option<u64>;

    #[auth(Self::role_admin)]
    fn grant_role(env: &Env, account: Address, role: Symbol, expiry: u64);
}
```

A truth commissioner's mandate expires. A transitional authority's term ends.
The role system must enforce these temporal boundaries.

### 5.2 Negative Roles (Exclusions)

Sometimes, the critical constraint is who CANNOT act:

```rust
fn is_excluded(env: &Env, account: Address, action: Symbol) -> bool;
```

In reconciliation, perpetrators may be excluded from certain governance
roles. The role system must support exclusions, not just inclusions.

### 5.3 Role Accountability

Every role assignment and revocation must be traceable:

```
RoleGranted { granter: Address, grantee: Address, role: Symbol, expiry: u64 }
RoleRevoked { revoker: Address, revoked: Address, role: Symbol, reason: String }
```

The `#[auth]` system handles the "who can grant" question. But the "why was it
granted" and "when does it expire" questions need additional mechanisms.

---

## 6. The Trust Model: What Do We Trust and Why?

### 6.1 Trust in Code vs Trust in People

The blog post frames the system as providing "trust without institutions."
This is partially true. Let me be precise about what the system trusts:

| Component | Trust Required |
|-----------|---------------|
| Soroban runtime | Full trust -- it executes the WASM |
| Stellar validators | Trust in consensus -- they order transactions |
| Macro output | Trust in correctness -- the generated code must be right |
| Provider implementation | Trust in developer -- they write business logic |
| Contract deployer | Trust in deployment -- they deploy the correct code |
| Key management | Trust in operational security -- private keys must not leak |

The structural auth enforcement removes trust in one specific area: "trust
that the developer remembered to add auth checks." This is valuable. But
it is one layer of a multi-layer trust problem.

### 6.2 The Deployer Problem

The sealed macro prevents override after compilation. But who compiles
and deploys the contract? If the deployer uses the flexible path instead
of the sealed path, the auth guarantees disappear.

For governance applications, deployment itself should be a governed process:

```rust
#[contracttrait]
pub trait GovernedDeployment {
    fn deployment_authority(env: &Env) -> Address;
    fn deployment_hash(env: &Env) -> BytesN<32>;

    #[auth(Self::deployment_authority)]
    fn verify_deployment(env: &Env, expected_hash: BytesN<32>);
}
```

The contract should be able to verify that its own deployment matches an
expected hash, certified by the governance authority.

---

## 7. Reconciliation Systems: A Concrete Use Case

### The Scenario

A post-conflict society needs:
1. A truth commission to document abuses
2. A reparation fund to compensate victims
3. A vetting board to screen individuals for government positions
4. A memorial registry to record the names of those who were killed

### Trait Composition

Using `soroban-sdk-tools`, this could be modeled as:

```rust
#[contracttrait]
pub trait TruthCommission: GovernanceTransition {
    fn submit_testimony(env: &Env, witness: Address, testimony_hash: BytesN<32>);

    #[auth(Self::current_authority)]
    fn certify_finding(env: &Env, finding_id: u64);
}

#[contracttrait]
pub trait ReparationFund: TruthCommission {
    fn fund_balance(env: &Env) -> i128;

    #[auth(Self::current_authority)]
    fn authorize_payment(env: &Env, recipient: Address, amount: i128, finding_id: u64);
}
```

The supertrait chain is meaningful: reparation payments require truth
commission findings, which require governance authority. This is the
actual dependency chain in transitional justice.

### The Provider Pattern for Different Contexts

Different countries have different reconciliation models:

```rust
// South Africa-style: truth for amnesty
pub struct TruthForAmnesty;

// Rwanda-style: community courts (gacaca)
pub struct CommunityJustice;

// Colombia-style: special jurisdiction for peace
pub struct SpecialJurisdiction;
```

Each provider implements the same traits differently. The contract interface
remains the same. This is genuinely useful -- international organizations
working across multiple post-conflict contexts could deploy the same
contract framework with different providers for each country.

---

## 8. Power Asymmetry Concerns

### 8.1 The Owner Is Not Always the Good Actor

The entire auth model assumes that the `owner` is the legitimate authority.
But in conflict zones, the "owner" may be a warlord who seized control.

The system needs a mechanism for disputing ownership:

```rust
#[contracttrait]
pub trait DisputeResolution: Ownable {
    fn dispute_status(env: &Env) -> DisputeStatus;

    // Anyone can file a dispute
    fn file_dispute(env: &Env, claimant: Address, evidence_hash: BytesN<32>);

    // Only the arbitrator can resolve
    #[auth(Self::arbitrator)]
    fn resolve_dispute(env: &Env, in_favor_of: Address);
}
```

### 8.2 Emergency Override

What if the owner's key is compromised? What if the owner is killed or
imprisoned? The system needs emergency override capabilities:

```rust
#[contracttrait]
pub trait EmergencyOverride: Ownable {
    fn emergency_council(env: &Env) -> Vec<Address>;

    // Requires supermajority of emergency council
    #[auth(quorum: Self::emergency_council, threshold: 3/4)]
    fn emergency_transfer(env: &Env, new_owner: Address);
}
```

This is multi-party auth again. It keeps coming back to this.

### 8.3 Transparency Guarantees

In peace-building, transparency is not optional. Every decision must be
auditable. The `AuthClient` testing pattern is excellent for development,
but production systems need:

- On-chain event logs for every authorized action
- Off-chain monitoring dashboards
- Alerting for unusual authorization patterns

The macro could generate standardized monitoring hooks alongside the
auth enforcement.

---

## 9. The AuthClient: Testing Power Constraints

The `AuthClient` pattern is valuable for governance applications because
it allows testing specific authorization scenarios:

```rust
// Test: only the truth commission chair can certify findings
auth_client.certify_finding(&finding_id)
    .authorize(&chair)
    .invoke();  // succeeds

auth_client.certify_finding(&finding_id)
    .authorize(&random_person)
    .try_invoke();  // fails
```

This is how you test that power constraints actually work. `mock_all_auths()`
would not catch a missing `require_auth()` call. In a governance context,
that missing call could allow unauthorized reparation payments.

**Assessment: AuthClient is the right approach for governance testing.**

---

## 10. The Collaboration Invitation: A Model for Peacemaking

The blog post ends with:

> "We deeply respect OpenZeppelin's work on stellar-contracts... We would
> love to explore how these approaches could be integrated."

This tone matters. In peace-building, the how of engagement is as important
as the what. Approaching a established player (OZ) with respect, acknowledging
their contributions, and proposing collaboration rather than competition --
this is how you build coalitions.

If the Soroban ecosystem's composability layer is built through collaboration
between OZ and soroban-sdk-tools, it will be stronger than either could build
alone. This is the lesson of every successful peace process: shared ownership
of the solution.

---

## 11. Recommendations for Governance Applications

| Priority | Recommendation | Rationale |
|----------|---------------|-----------|
| Critical | Multi-party auth (quorum/threshold) | Every governance decision requires it |
| Critical | Event emission in generated code | Accountability requires records |
| High | Temporal roles (expiring permissions) | Transitional mandates have end dates |
| High | Dispute resolution trait | Ownership claims can be illegitimate |
| High | Two-step (or N-step) transfers | Power transitions are dangerous moments |
| Medium | Emergency override mechanism | Key compromise / incapacitation |
| Medium | Negative roles (exclusions) | Perpetrator vetting |
| Medium | Deployment verification | Governance of the deployment process |
| Low | Monitoring hooks | Production accountability |
| Low | Role accountability events | "Why was this role granted?" |

---

## 12. Closing Reflection

I have seen what happens when power is unconstrained. I have seen what happens
when the person who controls the resources answers to no one. Smart contracts
will not end conflict. They will not heal trauma. They will not bring back
the dead.

But they can make one specific promise: the rules that were agreed upon will
be enforced, and any violation of those rules will be visible. This is not
peace. But it is a foundation upon which peace can be built.

The `#[contracttrait]` macro's structural auth enforcement is a small step
toward that foundation. The sealed macro is a slightly larger step. What is
missing -- multi-party auth, event emission, temporal roles -- represents
the distance still to travel.

The distance is not insurmountable. The direction is correct.

---

**Overall Assessment:** The structural auth model is fundamentally aligned with
governance and accountability needs. The sealed macro pattern is essential for
high-stakes applications. Multi-party authorization and event emission are
critical gaps that must be addressed for governance use cases.

**Verdict:** The right architecture for accountability systems. Not yet complete,
but the foundation is sound and the values are aligned.
