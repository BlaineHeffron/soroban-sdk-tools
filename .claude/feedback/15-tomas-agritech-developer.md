# Review: soroban-sdk-tools `#[contracttrait]` Macro

**Reviewer:** Tomas -- Agricultural tech developer building supply chain tracking in rural areas
**Focus:** Low-bandwidth environments, simplicity for non-crypto-native users, real-world asset tracking

---

## Overall Impression

I build systems that track coffee beans from a smallholder farmer in Oaxaca to
a roaster in Portland. My users include farmers who have never used a
blockchain, cooperative managers who use feature phones, and buyers who care
about provenance but not about cryptography. My infrastructure runs over
spotty cellular connections and sometimes goes offline for hours.

When I evaluate a blockchain development tool, I ask: will this make it easier
or harder to build systems that work for people who do not know what a
"provider" or "trait" is? Will the contracts I build with this tool be simpler
to audit for my non-technical stakeholders? Will the resulting system be
resilient enough for environments where connectivity is unreliable?

The `#[contracttrait]` macro is clearly designed by and for Rust developers. It
solves real problems in contract composition. But there is a gap between the
developer-facing tool and the end-user-facing system I need to build, and I
want to explore that gap honestly. The tool is closer to my needs than OZ
for supply chain use cases, but it needs specific improvements to serve
developers working outside the FinTech bubble.

---

## Strengths

### 1. Provider Pattern Maps Well to Real-World Role Models

In agricultural supply chains, the authorization model is not "owner" -- it is
a web of roles:
- **Farmer:** Can register a harvest lot, update field conditions
- **Cooperative manager:** Can aggregate lots, certify quality
- **Transporter:** Can update shipment status, record temperature
- **Buyer:** Can release payment, confirm receipt
- **Certifier:** Can attest organic/fair-trade status

The provider pattern lets me define these roles cleanly:

```rust
#[contracttrait]
pub trait SupplyChain {
    fn farmer(env: &Env) -> Address;
    fn cooperative(env: &Env) -> Address;

    #[auth(Self::farmer)]
    fn register_lot(env: &Env, lot_id: Symbol, weight_kg: u32);

    #[auth(Self::cooperative)]
    fn certify_lot(env: &Env, lot_id: Symbol, grade: Symbol);
}
```

This is readable. Even a non-developer can look at this trait and understand
"the farmer registers lots, the cooperative certifies them." The `#[auth]`
annotation makes the permission model visible. When I present this to a
certification body (Fair Trade, Rainforest Alliance), they can verify that
the contract enforces the rules they care about without reading implementation
code.

The provider pattern also maps to the reality that different cooperatives have
different governance structures. Some have a designated manager. Some make
decisions democratically. Some delegate to regional coordinators. With a
swappable provider, I can support all of these without changing the contract
interface.

### 2. Sealed Pattern Prevents Unauthorized Modifications

In my domain, the biggest risk is not a hacker -- it is a middleman who
modifies records to misrepresent quality or origin. Coffee graded as "specialty"
commands a 200% premium over "commercial" grade. The incentive to falsify
records is enormous.

The sealed pattern (`impl_supply_chain!`) ensures that the auth model cannot be
accidentally weakened. When I tell a buyer "only the certified farmer can
register a lot on the blockchain," the sealed pattern means that is
architecturally true, not just a promise. This distinction matters for
regulatory compliance -- the EU Deforestation Regulation (EUDR) requires that
custody chain integrity cannot be tampered with.

### 3. Less Code Means Cheaper Audits

My supply chain contracts need to be audited by third parties for certification
bodies. Auditors charge by complexity. A contract with 35 lines of auth logic
is cheaper to audit than one with 80 lines across 3 files. The reduction in
code surface directly reduces my costs -- and for a small agritech company
operating on thin margins, audit costs are a significant line item.

### 4. Supertrait Composition Models Real Hierarchies

Supply chains have hierarchical relationships. A cooperative has authority over
its member farmers. A certification body has authority over cooperatives. An
exporter has authority over logistics.

```rust
#[contracttrait]
pub trait Certifiable: SupplyChain {
    fn certifier(env: &Env) -> Address;

    #[auth(Self::certifier)]
    fn issue_certificate(env: &Env, lot_id: Symbol, cert_type: Symbol);
}
```

The supertrait system naturally models these hierarchies. A `Certifiable`
contract inherits the `SupplyChain` roles and adds certification authority.
This is intuitive and maps directly to the real-world relationships.

### 5. The Custody Chain Provider Pattern

The swap from `SingleOwner` to any custom provider is exactly what we need.
In our cooperatives, the "owner" of a shipment contract changes as goods
move through the chain: farmer -> cooperative -> transporter -> exporter.
Each handoff happens at a physical location that may lack connectivity.

```rust
pub struct CooperativeOwner;
impl OwnableInternal for CooperativeOwner {
    fn owner(env: &Env) -> Address {
        ShipmentStorage::get_current_custodian(env)
    }
    fn transfer_ownership(env: &Env, new_owner: Address) {
        ShipmentStorage::transfer_custody(env, &new_owner);
    }
}
```

The structural auth via `#[auth(Self::owner)]` means the custody transfer
is always authorized by the current holder. This is the right primitive for
supply chain traceability.

---

## Concerns

### 1. WASM Size Matters for Low-Bandwidth Deployment

In rural areas, deploying a contract means uploading WASM over a cellular
connection that might be 2G (EDGE). Every kilobyte matters. A 50KB WASM file
takes 10+ seconds to upload on a 40kbps connection, and if the connection
drops midway, the upload must restart.

The blog post claims "zero overhead" in WASM size, but I want specifics:

- What is the WASM size of the `trait-test` example?
- How does it compare to the equivalent OZ implementation?
- Does the `extern crate alloc` per trait add to WASM size?
- Do the AuthClient and sealed macro add to the production WASM?

The `#[cfg(not(target_family = "wasm"))]` guard on the AuthClient is
reassuring -- it means the test code does not ship in the WASM. But I need
empirical WASM size benchmarks to justify adoption to my team.

### 2. Storage Key Management for Asset Tracking

My supply chain contracts store a lot of data: lot IDs, weights, timestamps,
GPS coordinates, temperature readings, certificate hashes. The trait-test
example uses simple string keys:

```rust
env.storage().instance().set(&Symbol::new(env, "owner"), &owner);
```

For a supply chain with thousands of lots, this approach does not scale:
- Instance storage is loaded on every invocation (expensive for large datasets)
- String-based keys are bandwidth-inefficient and prone to typos
- No key namespacing means potential collisions between composed traits

I need:
- Key namespacing to avoid collisions between traits
- Efficient storage patterns for collections (maps of lot_id -> data)
- TTL management for data that expires (temperature readings have a 7-day
  relevance window; lot records must persist for 7 years for EUDR compliance)

The documentation mentions `#[contractstorage]` for storage management but
does not show how it integrates with `#[contracttrait]`. For my use case, the
storage pattern is as important as the auth pattern.

### 3. Offline-First Considerations

My systems need to work when connectivity is interrupted. Farmers register
lots in the field where there is no cell signal. The data is queued locally
on a mobile app and submitted when connectivity returns -- sometimes hours
later, sometimes the next day when the truck reaches a town with signal.

This means:
- Multiple transactions may arrive in a different order than they were created
- A farmer might try to register a lot that was already registered (duplicate)
- Auth checks need to work with state that might have changed since the
  transaction was created

The `#[auth]` pattern assumes synchronous, online authorization. The Soroban
protocol handles auth at the network level (the auth entry is bound to a
specific state), but the documentation should explicitly address the
implications for developers building offline-first systems:

- If the `owner()` changed between transaction creation and submission, the
  auth check will use the *current* owner, potentially rejecting a legitimate
  transaction signed by the *previous* owner
- Batch submissions (multiple transactions in one ledger close) may have
  ordering dependencies that the framework does not model

### 4. No Localization of Error Messages

My users speak Spanish, Mixtec, and Zapotec. When a transaction fails, the
error needs to be understandable to a cooperative manager who speaks limited
English. Soroban error codes are numbers, which is fine for machines but not
for people.

The `#[scerr]` macro generates error codes, but there is no mechanism for
associating human-readable messages with those codes. I need a mapping layer:

```
Error 2101: "El propietario no esta configurado" / "Owner not set"
Error 2102: "No autorizado" / "Unauthorized"
Error 2103: "Lote ya registrado" / "Lot already registered"
```

This is partly an application-layer concern, but the tool should recommend
that providers use numeric error codes (via `#[contracterror]` / `#[scerr]`)
rather than string panics. The example uses `.expect("not initialized")` --
a string panic that is only useful to English-speaking developers reading logs.
Numeric error codes can be decoded locally by the mobile app and mapped to
any language without additional network requests.

### 5. No Event Pattern for Supply Chain Tracking

Supply chain tracking is fundamentally event-driven. "Lot registered,"
"Lot certified," "Shipment departed," "Temperature recorded." Every state
change must emit an event that can be indexed and displayed on dashboards,
mobile apps, and buyer portals.

The OZ comparison notes that OZ handles events better. For my use case, events
are not optional -- they are the primary interface between the blockchain and
the user-facing application. Frontend applications and mobile apps subscribe
to events to show farmers and buyers what is happening in real time.

The macro should either:
1. Auto-generate events for auth-guarded methods (preferred)
2. Provide a clear pattern for event emission in providers
3. Support an `#[event]` annotation alongside `#[auth]`

Without events, I must add event emission code to every provider method,
which is exactly the kind of boilerplate the tool is designed to eliminate.

### 6. No Batch Operation Support

A farmer delivering 47 bags of coffee needs to register 47 custody transfers
in one transaction (or as few transactions as possible). The current
`#[auth(Self::owner)]` pattern calls `require_auth()` per method invocation.
For batch operations, this means 47 separate auth checks for the same address.

Soroban supports `require_auth_for_args()` which can batch, but the macro does
not expose this. A `#[auth(Self::owner, batch)]` variant would generate
efficient batch auth:

```rust
#[auth(Self::farm_owner)]
fn register_batch(env: &Env, lots: Vec<(Symbol, u32, Bytes)>);
```

One auth check for the entire batch. The farmer signs once at the weighing
station, and the transaction covers all bags delivered.

### 7. Testing Without Full Soroban Infrastructure

In rural development environments, running a full Stellar test node is not
always feasible. The compilation toolchain alone requires 2+ GB of downloads.
The AuthClient pattern requires the full Soroban testing infrastructure.

Can the auth logic be tested without the full Soroban environment? A pure Rust
test of the `Internal` trait (without `Env`) would be valuable for developers
working in resource-constrained environments with limited bandwidth for
downloading build dependencies.

---

## Suggestions

### 1. Add a Supply Chain Example

The current example is abstract (Ownable, Pausable). A concrete supply chain
example would demonstrate real-world applicability:

```rust
#[contracttrait]
pub trait CoffeeSupplyChain {
    fn farm_owner(env: &Env) -> Address;
    fn cooperative(env: &Env) -> Address;

    #[auth(Self::farm_owner)]
    fn register_harvest(env: &Env, lot_id: Symbol, weight_kg: u32, gps: Bytes);

    #[auth(Self::cooperative)]
    fn certify_quality(env: &Env, lot_id: Symbol, grade: Symbol);

    fn get_lot(env: &Env, lot_id: Symbol) -> LotData;
}
```

### 2. Document Storage Patterns for Collections

Show how `#[contractstorage]` works with `#[contracttrait]` for storing
collections of records, including TTL management for different data retention
requirements.

### 3. Add WASM Size Benchmarks

Include a CI check that reports WASM size for the example contracts. Publish
these numbers in the README. For developers deploying over low-bandwidth
connections, this is a critical metric.

### 4. Provide an Event Integration Pattern

Show how providers should emit events and recommend this as a standard pattern.
Better yet, generate events automatically in the outer trait for all
state-modifying methods.

### 5. Support Batch Operations

Add a `batch` flag to the `#[auth]` annotation that generates a single auth
check for batch operations instead of per-invocation auth.

### 6. Recommend Numeric Error Codes

Update the example to use `#[contracterror]` enums instead of `.expect()`
strings. Document why numeric error codes are preferable for internationalized
applications.

### 7. Add TTL Lifecycle Hooks

Consider adding optional lifecycle hooks to the Internal trait:

```rust
pub trait OwnableInternal {
    fn owner(env: &Env) -> Address;
    fn transfer_ownership(env: &Env, new_owner: Address);

    // Optional: called on every interaction to extend TTL
    fn on_access(env: &Env) {
        env.storage().instance().extend_ttl(100, 1000);
    }
}
```

---

## Unique Perspective: Technology for the Margins

Most blockchain development tools are designed for developers in major tech
hubs building financial products for other tech-savvy users. My users are
farmers, cooperative managers, and small-scale processors who need blockchain
for provenance, not for speculation.

The `#[contracttrait]` macro's greatest strength from my perspective is its
*readability*. When I show a coffee buyer the trait definition:

```rust
#[auth(Self::farm_owner)]
fn register_harvest(env: &Env, lot_id: Symbol, weight_kg: u32);
```

They can understand, even without knowing Rust, that "the farm owner
authenticates to register a harvest." That transparency builds trust, which is
the fundamental requirement for supply chain systems. Trust is not an abstract
concept in my world -- it is the difference between a farmer receiving a fair
price and being exploited by a middleman.

The greatest weakness is the assumption of always-on connectivity and modern
development infrastructure. The Soroban ecosystem itself has this assumption,
so it is not unique to this tool. But as blockchain technology reaches into
agricultural supply chains, rural healthcare, and other "last mile" use cases,
the tools need to account for the environments where they will be deployed.

I do not need the tool to solve offline operation -- that is a protocol-level
concern. But I need the documentation to acknowledge these environments and
provide guidance for developers working in them. A section on "deploying
contracts in low-bandwidth environments" would signal that the tool's ambitions
extend beyond Silicon Valley.

---

## Would I Use This?

Yes, with caveats.

The auth model maps well to supply chain roles. The provider pattern lets me
define different authorization models for different cooperatives (some are
democratic, some have a designated manager). The sealed pattern ensures that
the provenance guarantees I promise to buyers are architecturally enforced.

But I would need:
1. Storage patterns for collections of records (not just singleton ownership)
2. Event emission guidance or automation
3. WASM size benchmarks
4. A concrete example in a domain beyond financial ownership
5. Numeric error codes recommended over string panics
6. Batch operation support for bulk registrations

The tool is closer to my needs than OZ because my contracts are more about
role-based authorization for data operations than about token management. The
provider pattern is a natural fit for the varied governance structures I
encounter in agricultural cooperatives.

**Verdict:** A good fit for non-financial blockchain applications that need
role-based authorization. The trait readability and provider flexibility are
genuine advantages. Needs ecosystem support for storage patterns, events,
batch operations, and documentation for developers working outside the
FinTech mainstream. The cooperative farmers I work with will never see this
code, but the contracts built with it will track their livelihoods. Getting
the details right matters.
