# Review: Petra -- Archaeologist Preserving Heritage Sites with Blockchain Provenance

**Reviewer Profile:** Archaeologist specializing in digital preservation of cultural heritage, using blockchain for immutable provenance records, artifact tracking, and time capsule contracts for future generations.

**Materials Reviewed:**
- `docs/oz-comparison.md`
- `docs/blog-post-composable-contracts.md`
- `examples/trait-test/src/lib.rs`
- `soroban-sdk-tools-macro/src/contract.rs`

---

## Overall Impression

Archaeology teaches you to think in centuries. When I document a Bronze Age settlement, I create records that must be legible to researchers 500 years from now. When I look at soroban-sdk-tools, I think about permanence: will these contracts still make sense when the Soroban network is as ancient as the protocols it replaces?

The structural auth pattern is excellent for provenance -- you want immutable guarantees about who certified an artifact's authenticity. The provider pattern is dangerous for provenance -- you want records that do not change when the implementation is swapped. The sealed macro is the closest thing to inscribing a record in stone.

**Rating: 3.5/5** -- Good immutability guarantees for auth, but the mutable provider pattern conflicts with the permanence requirements of heritage preservation.

---

## Strengths

### 1. Sealed Macros are Digital Inscriptions

In archaeology, the most reliable records are inscriptions -- text carved into stone or fired into clay. They cannot be altered after creation. The sealed macro creates digital inscriptions:

```rust
impl_ownable!(HeritageRegistry, CuratorAuthority);
```

Once compiled and deployed, the auth logic is carved into the WASM binary. No future developer can override it. No governance vote can change it. The curator authority is inscribed.

For heritage preservation, this is exactly right. When a museum curator certifies that artifact #4572 is an authentic 3rd-century Roman lamp, that certification should be permanent and tamper-proof. The sealed macro makes the certification process immutable.

### 2. Two-Trait Structure Separates Policy from Record

In archival science, there is a fundamental distinction between the *record* (the artifact or document itself) and the *policy* governing the record (who can access it, how it is stored, when it can be deaccessioned).

The two-trait structure maps directly:
- `HeritageInternal` = the record operations (create, read, update provenance)
- `Heritage` = the policy (who can perform these operations)

This separation means the provenance record is independent of the access control policy. If the museum changes its governance structure (new director, new board), the records themselves are unaffected. Only the policy wrapper changes.

### 3. AuthClient Enables Provenance Chain Testing

Every artifact has a provenance chain: who found it, who authenticated it, who owned it, who donated it to the museum. The `AuthClient` pattern lets you test each link in the provenance chain independently:

```rust
let auth = HeritageAuthClient::new(&env, &registry_id);

// Link 1: Excavation director certifies discovery
auth.record_discovery(&artifact_id, &site_id, &date)
    .authorize(&excavation_director)
    .invoke();

// Link 2: Lab specialist authenticates
auth.authenticate(&artifact_id, &methodology, &results)
    .authorize(&lab_specialist)
    .invoke();

// Link 3: Museum registrar accessions
auth.accession(&artifact_id, &collection_id)
    .authorize(&registrar)
    .invoke();
```

Each step in the provenance chain is independently authorized and testable. This is archival best practice encoded in test infrastructure.

### 4. Supertrait Composition Models Institutional Hierarchy

Heritage institutions have clear hierarchical authority: national heritage ministry oversees regional offices, which oversee individual site managers, who oversee field archaeologists. The supertrait pattern encodes this:

```rust
#[contracttrait]
pub trait NationalHeritage {
    fn minister(env: &Env) -> Address;

    #[auth(Self::minister)]
    fn designate_protected_site(env: &Env, site_id: Symbol);
}

#[contracttrait]
pub trait SiteManagement: NationalHeritage {
    fn site_manager(env: &Env) -> Address;

    #[auth(Self::site_manager)]
    fn record_excavation(env: &Env, site_id: Symbol, trench_id: Symbol);
}
```

The supertrait ensures that `SiteManagement` only exists within the authority of `NationalHeritage`. You cannot manage a site that has not been nationally designated. This is structurally correct for heritage governance.

---

## Concerns

### 1. Provider Swapping Undermines Immutability

**Severity: Critical**

The provider pattern allows swapping implementations: change `SingleOwner` to `MultisigOwner` with one line. For heritage preservation, this is a problem. If the provenance logic changes, historical records may become inconsistent.

Consider: a heritage registry uses `StrictProvenance` provider that requires DNA-tested authentication. Later, the museum swaps to `RelaxedProvenance` that accepts visual inspection. All future certifications use the new standard, but old certifications were made under the old standard. The provenance chain now has an invisible discontinuity.

In archaeology, we call this a *stratigraphy break* -- a disruption in the depositional sequence that makes interpretation unreliable. Provider swapping creates digital stratigraphy breaks.

**Recommendation:** For heritage use cases, providers should be append-only. Instead of swapping providers, add new ones alongside the old:

```rust
// Version 1 provenance (pre-2025)
impl_heritage_v1!(Registry, StrictProvenance);

// Version 2 provenance (post-2025, different methodology)
impl_heritage_v2!(Registry, RelaxedProvenance);
```

Each version has its own method set. Historical records retain their original provenance methodology. New records use the new methodology. The provenance chain is unbroken.

### 2. No Temporal Metadata in the Trait System

**Severity: High**

Archaeology is the study of time. Every record needs temporal metadata: when was it created, by whom, under what circumstances. The current trait system has no temporal dimension.

The `transfer_ownership` function takes `new_owner: Address` but not `when`, `why`, or `context`. For heritage records, every state change needs:
- Timestamp (ledger sequence or Unix time)
- Reason (acquisition, conservation, loan, deaccession)
- Context (excavation season, conservation project, exhibition)
- Previous state (what was the record before this change?)

**Recommendation:** Consider adding a `#[timestamped]` attribute that automatically records the ledger sequence and caller address for every state change:

```rust
#[contracttrait]
pub trait Heritage {
    #[auth(Self::registrar)]
    #[timestamped]  // auto-records env.ledger().sequence() + caller
    fn update_provenance(env: &Env, artifact_id: Symbol, note: String);
}
```

This would generate a provenance log alongside each state change -- an automatic archaeological record of the contract's own history.

### 3. No Immutable Records Pattern

**Severity: High**

The example traits are all mutable: `transfer_ownership` changes the owner, `pause` changes the state, etc. Heritage records should be *append-only*. Once a provenance entry is created, it should never be modified -- only superseded by a new entry.

The Soroban storage model (`instance().set()`) overwrites previous values. For heritage preservation, the storage pattern should be:

```rust
// Instead of: set owner to new_owner (overwriting old)
// Do: append entry to provenance log (preserving history)
```

The current trait system does not distinguish between mutable and append-only methods. All methods can read and write freely.

**Recommendation:** Add an `#[append_only]` attribute that constrains the provider to only use `set` with new keys (never overwriting existing data):

```rust
#[contracttrait]
pub trait ProvenanceLog {
    #[auth(Self::registrar)]
    #[append_only]
    fn add_entry(env: &Env, artifact_id: Symbol, entry: ProvenanceEntry);

    // No update or delete methods -- the trait structurally prevents modification
    fn get_entries(env: &Env, artifact_id: Symbol) -> Vec<ProvenanceEntry>;
}
```

### 4. TTL Conflicts with Permanent Preservation

**Severity: High**

The OZ comparison mentions TTL management as something OZ does better. But for heritage preservation, TTLs are an existential threat. Heritage records must persist *indefinitely*. If a storage entry expires because its TTL was not extended, the provenance chain is broken.

The blog post mentions `#[contractstorage]` could integrate TTL management. For heritage use cases, certain storage entries should be marked as *permanent* -- their TTL should be automatically extended to the maximum at every interaction.

**Recommendation:** Support a `#[permanent]` storage attribute that auto-extends TTL on every read:

```rust
#[contractstorage]
pub enum HeritageStorage {
    #[permanent]  // auto-extend TTL on every access
    ProvenanceLog(Symbol),  // artifact_id -> Vec<ProvenanceEntry>

    #[ttl(30_days)]  // normal TTL for operational data
    ActiveExcavations(Symbol),
}
```

### 5. No Cross-Contract Provenance

**Severity: Medium**

Artifacts move between institutions: from an excavation site to a university lab, from the lab to a national museum, from the museum to a traveling exhibition. Each institution might have its own heritage contract. The current system has no pattern for cross-contract provenance chains.

The supertrait system works within a single contract. But provenance chains span multiple contracts, each with its own auth structure. How does the receiving institution verify the sending institution's provenance records?

**Recommendation:** Document a pattern for inter-contract provenance verification, using Soroban's cross-contract invocation:

```rust
#[contracttrait]
pub trait ProvenanceVerifier {
    fn verify_origin(env: &Env, source_contract: Address, artifact_id: Symbol) -> bool;
}
```

---

## Use Case Exploration: Archaeological Site Registry

Here is how I would structure a heritage site registry:

```rust
#[contracttrait]
pub trait SiteRegistry: Ownable {
    fn heritage_authority(env: &Env) -> Address;

    #[auth(Self::heritage_authority)]
    fn register_site(env: &Env, site_id: Symbol, location: GeoCoord, period: HistoricalPeriod);

    #[auth(Self::heritage_authority)]
    fn add_artifact(env: &Env, site_id: Symbol, artifact: ArtifactRecord);

    // Anyone can query -- transparency is paramount
    fn get_site(env: &Env, site_id: Symbol) -> SiteRecord;
    fn get_artifacts(env: &Env, site_id: Symbol) -> Vec<ArtifactRecord>;

    // Provenance is append-only
    #[auth(Self::heritage_authority)]
    fn add_provenance(env: &Env, artifact_id: Symbol, entry: ProvenanceEntry);

    fn get_provenance(env: &Env, artifact_id: Symbol) -> Vec<ProvenanceEntry>;
}
```

The sealed macro ensures that only the heritage authority can register sites and add artifacts. The provider pattern allows different institutions to have different internal processes (some use lab analysis, others use field assessment) while maintaining the same public interface.

But the critical missing piece is permanence. The trait system must guarantee that provenance records, once written, can never be overwritten or deleted. This is the difference between a heritage record and a database entry.

---

## Reflections on the Documentation

### The Blog Post

The blog post frames soroban-sdk-tools as an improvement over OpenZeppelin. From a heritage perspective, the framing should also address *permanence* as a design goal. "Structural auth enforcement" is about security in the present. Heritage preservation is about security across time.

The section on zero overhead is relevant -- heritage records should be as compact as possible to minimize storage costs over long time periods. But there is no discussion of long-term storage costs, TTL extension strategies, or data format migration.

### The OZ Comparison

The comparison mentions that OZ handles TTL management better. For heritage use cases, this is the most important line in the document. TTL management is not about performance -- it is about survival. If heritage records expire, they are lost.

The comparison should include a "permanence" row in the comparison table:

| Aspect | OpenZeppelin | soroban-sdk-tools |
|--------|-------------|-------------------|
| Data permanence | TTL constants with extension | Not addressed |

### The Example Code

The example in `trait-test/src/lib.rs` stores values with `Symbol::new(env, "owner")` as the key. For heritage records, using string-based keys is fragile -- a typo in the key string silently creates a different storage entry. The `#[contractstorage]` approach with typed enums is safer for long-lived records.

---

## The Long View

Archaeology teaches patience. The contracts we deploy today will be studied by future researchers -- not as software artifacts, but as records of how our generation organized its digital life. The design decisions in soroban-sdk-tools will outlive all of us.

The structural auth pattern is the right foundation. It creates records that can be trusted because the auth cannot be circumvented. But trust alone is not enough for heritage. Heritage requires:

1. **Permanence** -- records that survive beyond their creators
2. **Provenance** -- unbroken chains of custody and certification
3. **Transparency** -- anyone can verify the record's authenticity
4. **Immutability** -- records that cannot be altered after creation
5. **Interoperability** -- records that can be read by future systems

soroban-sdk-tools addresses #3 (public trait methods for querying) and partially addresses #1 and #4 (sealed macros). But #2 and #5 need dedicated architectural support.

Build for the ages. The Bronze Age potters did not know their work would be studied 3,000 years later, but their craftsmanship ensured it survived. Give our digital records the same chance.
