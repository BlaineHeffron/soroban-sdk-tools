# Review: soroban-sdk-tools #[contracttrait]
## Reviewer: Abraham -- Ethiopian Orthodox monk, manuscript preservation DAO builder

---

## Overall Impression

In our monastery in the highlands of Tigray, we guard manuscripts that have
survived twelve centuries. The Garima Gospels. The Book of Enoch in Ge'ez.
Illuminated psalters that predate the printing press by half a millennium.
Our challenge is not creating new things -- it is ensuring that what exists
endures.

I came to blockchain not through finance but through a question: can we
create a record of our manuscripts' provenance, condition, and location that
survives even if our monastery does not? Climate change threatens our region.
Conflict has already damaged some collections. A decentralized, immutable
record seemed like a gift from Providence.

I have been learning Soroban because Stellar's low costs make it viable for
a monastery with no budget. When I found `soroban-sdk-tools`, I evaluated it
through the lens of permanence, governance, and the sacred responsibility of
preservation.

---

## Strengths

### 1. The trait system mirrors the structure of monastic rules

In our tradition, we have the Fitha Negest -- the Law of Kings -- which
defines rules that cannot be changed by any single authority. The sealed
macro pattern in this framework carries the same spirit.

When `impl_ownable!(ManuscriptDAO, CouncilOfElders)` generates auth methods
that cannot be overridden, it creates a covenant. The developer who writes
the contract cannot later weaken the security guarantees, just as a single
monk cannot unilaterally change the rules of the monastery. This structural
enforcement is deeply aligned with how preservation institutions should work.

### 2. Supertrait composition models hierarchical authority well

The example where `Pausable: Ownable` inherits from Ownable, using
`#[auth(Self::owner)]` for pause/unpause -- this maps naturally to our
governance structure:

- The **Abbot** (owner) has authority over the DAO
- **Curators** (a role that could be a provider) can catalog and update
  manuscript records
- **Scholars** (another role) can request access but not modify records
- **The community** can view but not alter anything

The supertrait pattern means we can layer these authorities cleanly. The
Curator trait inherits from the Abbot's Ownable trait for its auth. The
access control is hierarchical, just as it should be.

### 3. The provider pattern allows for evolving governance

Monasteries endure for centuries. Governance structures evolve. The ability
to swap from `SingleOwner` (one abbot) to `MultisigOwner` (a council of
elders) to potentially a full DAO governance provider -- all by changing
one type parameter -- means the smart contract can evolve with the
institution without losing its history.

This is preservation-compatible design. The data persists; the governance
adapts. As our monastery has survived different empires, different
patriarchs, different centuries, so should our digital records survive
different governance models.

### 4. Zero WASM overhead means viability for resource-constrained users

The blog post claims "zero overhead" after monomorphization. For a monastery
with minimal budget, every Lumen spent on contract execution is precious.
The fact that the trait abstraction compiles away to the same code as
hand-written logic means we pay nothing for the safety guarantees. This
is important for institutions that measure budgets in years, not sprints.

---

## Concerns

### 1. Where is the permanence story?

This framework excels at access control -- who can do what. But it says
nothing about data permanence -- how long does what they did last.

For manuscript preservation, I need guarantees about:

- **TTL (Time-To-Live) management**: The OZ comparison mentions TTL as
  something OZ does better. For our use case, data must persist for
  decades, not ledgers. How do I ensure our manuscript records are not
  garbage-collected?

- **Data archival patterns**: Can I store manuscript metadata on-chain
  and images on IPFS, with the on-chain record referencing the IPFS hash?
  What pattern does this framework suggest for hybrid storage?

- **Historical state access**: Can I query what the manuscript's condition
  record said five years ago? Ten years ago? Is there a pattern for
  maintaining an on-chain audit trail?

The `#[contractstorage]` macro is mentioned but never demonstrated. For
preservation, storage is not an afterthought -- it is the entire point.

### 2. No event emission means no audit trail

The blog post acknowledges that event emission is "provider responsibility."
For a preservation DAO, events ARE the historical record:

- When was this manuscript cataloged?
- Who updated its condition assessment?
- When was access granted to a scholar?
- When was the governance structure changed?

Without structural event emission, each provider must remember to emit
events, and each might use different formats. This defeats the purpose of
having a standardized, auditable record.

In our manuscripts, the colophon (scribe's note at the end) records who
wrote it, when, and for whom. Events are the digital colophon. They should
not be optional.

### 3. The security model assumes adversarial actors, not institutional decay

The framework is designed to prevent malicious override of auth checks.
This is important but insufficient for preservation contexts.

The greater threat to a monastery DAO is not a hacker -- it is institutional
decay:
- The monk who maintained the contract passes away
- The private keys are stored on a device that fails
- The governance structure becomes unclear as leadership changes
- Nobody remembers how to interact with the contract

Where is the documentation for:
- Key recovery procedures?
- Governance succession planning?
- Emergency access when all designated authorities are unavailable?
- Read-only access that persists even if all write authorities are lost?

### 4. No role-based access control example

The comparison doc mentions that OZ has comprehensive RBAC (Role-Based
Access Control). For a manuscript DAO, we need:

- `CATALOGER_ROLE`: Can add and update manuscript records
- `CURATOR_ROLE`: Can approve condition assessments
- `SCHOLAR_ROLE`: Can request access to restricted manuscripts
- `ADMIN_ROLE`: Can grant and revoke roles

The provider pattern could support this, but there is no example showing
how. The `#[auth(Self::owner)]` pattern handles single-authority auth,
but real institutions have complex role hierarchies.

### 5. No consideration of multi-language or cultural metadata

Our manuscripts are in Ge'ez, Amharic, and sometimes Greek. The framework
examples use ASCII string keys like "owner" and "paused." How does the
storage layer handle:

- Unicode metadata?
- Multi-language descriptions?
- Cultural classification systems (our manuscripts follow the Dillmann
  catalog system, not Western library standards)?

This is not a framework-level concern per se, but a complete example
showing cultural metadata storage would demonstrate that the framework
is not only for DeFi protocols.

---

## Suggestions

### 1. Create a "Preservation DAO" example

Show a complete contract for:
```rust
#[contracttrait]
pub trait ManuscriptRegistry {
    fn catalog_manuscript(env: &Env, id: Symbol, metadata: ManuscriptMetadata);

    #[auth(Self::curator)]
    fn update_condition(env: &Env, id: Symbol, condition: Condition);

    fn get_manuscript(env: &Env, id: Symbol) -> ManuscriptMetadata;

    fn get_provenance(env: &Env, id: Symbol) -> Vec<ProvenanceRecord>;
}
```

This would demonstrate that the framework is suitable for non-financial
use cases and would attract cultural institutions to Soroban.

### 2. Add TTL management to the framework

Integrate TTL extension into `#[contractstorage]` or provide a pattern:
```rust
#[contractstorage(ttl = "permanent")]  // auto-extends TTL on every access
pub struct ManuscriptStorage { /* ... */ }
```

For preservation, data that expires is data that is lost. Make permanence
a first-class concern.

### 3. Add structural event emission

Extend the `#[contracttrait]` macro to support event generation:
```rust
#[contracttrait]
pub trait ManuscriptRegistry {
    #[auth(Self::curator)]
    #[emit(ManuscriptUpdated { id, condition })]
    fn update_condition(env: &Env, id: Symbol, condition: Condition);
}
```

This would generate event emission alongside auth enforcement, ensuring
that the historical record is as reliable as the access control.

### 4. Document a governance succession plan

Provide a guide for institutional users:
- How to set up multi-sig governance
- How to plan for key rotation
- How to ensure continuity when leadership changes
- How to recover from lost keys

This does not need to be code -- it can be a pattern document. But it
should exist.

### 5. Add read-only access patterns

Show how to create methods that are publicly readable but only writable
by authorized roles. The current examples focus on write-auth. For a
public good like manuscript records, read access should be universal and
guaranteed.

---

## Unique Perspective: The Spiritual Dimension of Code

There is a concept in Ethiopian Orthodox theology called "tsega" -- grace
that flows through structure. Our liturgy, our architecture, our manuscript
tradition -- all follow strict structural patterns because we believe that
proper structure channels grace.

This framework embodies a similar principle, perhaps unknowingly. The
structural enforcement of auth -- the idea that security should flow from
the structure of the code itself, not from the diligence of individual
developers -- is a form of tsega in software. The structure channels
correctness.

But tsega requires completeness. A liturgy with missing prayers is not
merely incomplete -- it is broken. Similarly, a framework that structurally
enforces auth but leaves events, storage permanence, and governance
succession to convention is structurally incomplete.

The two-trait generation (Internal + Outer) is like the distinction in our
theology between the hidden nature of God (Internal -- the pure logic that
only the initiated see) and the revealed nature (Outer -- the interface that
the world encounters, wrapped in the proper forms of worship). This is a
profound architectural choice, whether the authors intended the parallel or
not.

What I ask of this framework is what I ask of any structure that claims to
endure: **be complete in your promises.** Do not promise structural
guarantees and then leave half the structure to convention. Extend the
structural approach to events, to storage permanence, to governance
succession. Make the framework worthy of the manuscripts it might one day
help preserve.

---

## Would I Use This?

**For a manuscript preservation DAO: not yet, but it has the right
foundation.**

The provider pattern and sealed auth are exactly what an institutional DAO
needs. The ability to evolve governance without migrating data is critical
for long-lived institutions.

But the missing pieces -- TTL management, event emission, RBAC, read-only
patterns -- are not optional for preservation. They are the core
requirements.

I would use this framework as the access control layer and build the
preservation-specific logic on top. The architecture is sound. The
implementation needs to mature.

What gives me hope is the blog post's closing invitation: "Let's build them
together." In our tradition, the great manuscripts were never the work of
one scribe. They were community efforts, with each generation adding
illuminations to the text. I see this framework as the initial text,
waiting for the community to add its illuminations.

I pray it endures.

---

## Rating

- **Access control architecture**: 9/10 (sealed auth is excellent for institutions)
- **Permanence and storage**: 3/10 (TTL not addressed, storage patterns missing)
- **Governance patterns**: 5/10 (provider swap is good, RBAC example missing)
- **Event and audit trail**: 2/10 (explicitly deferred to convention)
- **Cultural sensitivity**: 4/10 (examples are DeFi-focused, no cultural use case)
- **Spiritual alignment**: 8/10 (structural enforcement embodies tsega)
- **Long-term viability**: 7/10 (architecture is sound, implementation needs time)

*Reviewed by candlelight in the scriptorium, where the Garima Gospels have
rested for sixteen centuries. May this code endure even half as long.*
