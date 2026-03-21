# Review by Brigitte -- Swiss Red Cross Humanitarian Logistics Coordinator

## Reviewer Profile

I coordinate humanitarian logistics for the International Committee of the Red Cross across East Africa and the Middle East. My work involves managing supply chains for medical supplies, food aid, and shelter materials across multiple organizations, governments, and conflict zones. I have spent 18 years dealing with the fundamental challenges of multi-organization coordination: who authorized this shipment, who received it, who is accountable for losses, and how do we audit the entire chain when donors demand transparency?

I began exploring blockchain for humanitarian supply chain management three years ago because the existing systems -- spreadsheets, proprietary databases, and paper trails -- fail at exactly the points where accountability matters most: organizational boundaries. This review examines soroban-sdk-tools through the lens of humanitarian logistics, multi-organization coordination, accountability, and audit trails.

---

## 1. The Ownership Model and Organizational Authority

### Who Controls the Supply Chain Contract?

In humanitarian logistics, "ownership" of a supply chain is distributed across multiple organizations:

| Organization | Role | Authority |
|-------------|------|-----------|
| ICRC | Coordination, standards | Sets rules, audits compliance |
| UNHCR | Refugee camp management | Controls last-mile distribution |
| WFP | Food aid logistics | Manages warehouses, transport |
| MSF | Medical supply chain | Controls pharmaceutical chain |
| Local government | Customs, permits | Controls border crossings |
| Donor governments | Funding | Demands accountability |

The `Ownable` trait with a single owner does not map to this reality. No single organization "owns" a humanitarian supply chain. Authority is distributed, overlapping, and context-dependent.

### What Is Needed

A humanitarian supply chain contract would need a role-based access control (RBAC) model:

```rust
#[contracttrait]
pub trait HumanitarianSupplyChain {
    fn has_role(env: &Env, account: Address, role: Symbol) -> bool;

    #[auth(caller)]
    fn register_shipment(env: &Env, caller: Address, shipment: ShipmentData);

    #[auth(caller)]
    fn confirm_receipt(env: &Env, caller: Address, shipment_id: u64);

    #[auth(Self::coordinator)]
    fn audit_trail(env: &Env, shipment_id: u64) -> Vec<AuditEntry>;
}
```

The Provider pattern in soroban-sdk-tools is well-suited for this. Different providers could implement different RBAC models:

- `SimpleRBAC`: Fixed roles assigned by the coordinator
- `FederatedRBAC`: Roles assigned by each organization for their own personnel
- `DynamicRBAC`: Roles that change based on crisis phase (emergency, recovery, development)

The OZ comparison document acknowledges that RBAC is something "OZ does better." For humanitarian applications, RBAC is not optional -- it is the foundation.

---

## 2. The Pausable Trait and Emergency Response

### When Pausing Saves Lives

In humanitarian logistics, the ability to halt operations is critical. Consider:

- A shipment is contaminated. All distribution from that batch must stop immediately.
- A conflict zone becomes too dangerous for delivery. All movements must be suspended.
- A fraud is discovered. All transactions from a specific partner must be frozen.

The `Pausable` trait maps to this need:

```rust
#[contracttrait]
pub trait Pausable: Ownable {
    fn is_paused(env: &Env) -> bool;

    #[auth(Self::owner)]
    fn pause(env: &Env);

    #[auth(Self::owner)]
    fn unpause(env: &Env);
}
```

### What Is Missing for Humanitarian Use

1. **Selective pause.** The current implementation is all-or-nothing. In reality, you might need to pause shipments from one warehouse while keeping others operational. A `PausableByRegion` or `PausableByPartner` provider would be more useful.

2. **Pause with reason.** When a pause is triggered, the reason must be recorded. Was it contamination, security, fraud, or an administrative hold? The current `pause()` takes no parameters.

3. **Escalating pause.** Different pause levels require different authority:
   - **Level 1 (local hold):** Field coordinator can pause shipments from one location
   - **Level 2 (regional suspension):** Regional coordinator can pause all shipments in a region
   - **Level 3 (global halt):** HQ can pause the entire supply chain

The Provider pattern could implement this through a `EscalatingPause` provider, but the trait definition would need additional parameters:

```rust
#[contracttrait]
pub trait EscalatingPausable: Ownable {
    fn is_paused(env: &Env, scope: PauseScope) -> bool;

    #[auth(caller)]
    fn pause(env: &Env, caller: Address, scope: PauseScope, reason: String);

    #[auth(Self::owner)]
    fn unpause(env: &Env, scope: PauseScope);
}
```

---

## 3. Audit Trails and Accountability

### The Accountability Gap

The single biggest problem in humanitarian logistics is the **accountability gap** -- the moment when goods pass from one organization to another. At this handover point:

- The sending organization records "shipped"
- The receiving organization records "received"
- But the quantities do not always match
- And no neutral third party verifies the handover

### How soroban-sdk-tools Could Help

A blockchain-based audit trail, built on the `#[contracttrait]` pattern, could close this gap:

```rust
#[contracttrait]
pub trait Auditable {
    fn log_event(env: &Env, event: AuditEvent);
    fn get_audit_trail(env: &Env, entity_id: u64) -> Vec<AuditEvent>;

    #[auth(Self::auditor)]
    fn verify_handover(env: &Env, handover_id: u64) -> bool;
}
```

The key insight is that `#[auth(Self::auditor)]` structurally ensures that only the designated auditor can verify handovers. This is more reliable than the current system, where audit verification depends on the auditor having access to both organizations' databases.

### Missing: Event Emission

The blog post and OZ comparison both acknowledge that event emission is an area where OZ is stronger. For humanitarian logistics, events are not optional -- they ARE the audit trail.

Every state change must emit an event:
- Shipment registered (who, what, when, where)
- Shipment in transit (carrier, route, estimated arrival)
- Shipment received (who received, quantity confirmed, discrepancies noted)
- Shipment distributed (to whom, what quantity, location)

The fact that the standard providers do not emit events is a significant gap for any application requiring accountability.

**Recommendation:** Add an `Auditable` supertrait or mixin that automatically emits events for every state change. The Provider pattern is ideal for this -- an `AuditedSingleOwner` provider could wrap `SingleOwner` and add event emission without changing the trait interface.

---

## 4. Multi-Organization Coordination

### The Cross-Organization Trust Problem

In a humanitarian supply chain involving 5 organizations, the trust model is complex:

- ICRC trusts WFP's warehouse management but not their last-mile distribution data
- UNHCR trusts local government customs data but not their aid distribution records
- Donors trust none of them completely and demand independent audits

The `Ownable` trait assumes a single trust boundary (the owner). Real multi-organization coordination needs multiple trust boundaries with different permissions.

### The Provider Pattern as a Solution

The Provider pattern could model this through a `FederatedProvider`:

```rust
pub struct FederatedAuthority;
impl OwnableInternal for FederatedAuthority {
    fn owner(env: &Env) -> Address {
        // Returns a multisig address representing the coordination committee
        FederatedStorage::get_committee_address(env).unwrap()
    }
}

// Each organization has its own role and permissions
impl AccessControlInternal for FederatedAuthority {
    fn has_role(env: &Env, account: Address, role: Symbol) -> bool {
        match role.to_string(env).as_str() {
            "COORDINATOR" => FederatedStorage::is_coordinator(env, &account),
            "WAREHOUSE" => FederatedStorage::is_warehouse_manager(env, &account),
            "DISTRIBUTOR" => FederatedStorage::is_distributor(env, &account),
            "AUDITOR" => FederatedStorage::is_auditor(env, &account),
            _ => false,
        }
    }
}
```

This is where the composability of soroban-sdk-tools shines. A single provider can implement multiple trait interfaces, providing a unified governance model across ownership, access control, and supply chain management.

---

## 5. Assessment of the AuthClient for Verification

### Positive: Precise Permission Testing

The `AuthClient` is excellent for verifying that the right people have the right permissions:

```rust
// Verify that a warehouse manager can register shipments
let auth_client = SupplyChainAuthClient::new(&env, &contract_id);
auth_client.register_shipment(&shipment_data)
    .authorize(&warehouse_manager)
    .invoke();  // Should succeed

// Verify that a distributor cannot register shipments
let result = auth_client.register_shipment(&shipment_data)
    .authorize(&distributor)
    .try_invoke();
assert!(result.is_err());  // Should fail -- wrong role
```

This is exactly the kind of verification that humanitarian auditors need. Each role's permissions can be tested independently and precisely.

### Gap: No Batch Testing

In humanitarian logistics, permission testing needs to cover complex scenarios:

- "Can a warehouse manager register a shipment AND confirm receipt?" (No -- separation of duties)
- "Can an auditor view the trail AND modify shipment data?" (No -- read-only for auditors)
- "Can a coordinator pause operations AND then unpause?" (Yes -- but only coordinators)

The `AuthClient` tests one method at a time. A batch testing mode that verifies entire permission matrices would be more useful for organizational audits.

---

## 6. Supply Chain in Crisis: Real-World Scenarios

### Scenario 1: South Sudan Food Distribution

**Context:** WFP distributes food aid to 200,000 refugees. Three warehouses, twelve distribution points, six local partner organizations.

**How soroban-sdk-tools would be used:**

1. **Contract deployment:** ICRC deploys the supply chain contract with `FederatedAuthority` provider. Six organizations receive role-based access.

2. **Shipment registration:** WFP warehouse manager registers incoming food shipments. `#[auth(caller)]` ensures only authorized warehouse staff can register.

3. **Distribution tracking:** Local partners confirm distributions to beneficiaries. Each confirmation is recorded on-chain with full audit trail.

4. **Discrepancy handling:** When received quantities do not match shipped quantities, the contract flags the discrepancy. The auditor (`#[auth(Self::auditor)]`) investigates.

5. **Donor reporting:** Donors query the audit trail directly. No need to trust any single organization's report.

**What works:** The Provider pattern allows different organizations to implement their own internal processes while sharing a common external interface. The auth model ensures that each organization can only perform actions within its role.

**What does not work:** The current framework has no standard RBAC provider, no event emission, and no concept of "discrepancy" or "handover verification." These would need to be built as custom providers.

### Scenario 2: Earthquake Response (Rapid Deployment)

**Context:** A 7.2 earthquake strikes. Multiple organizations converge. There is no pre-existing coordination structure.

**Challenge:** The contract must be deployed rapidly, with roles assigned on-the-fly as organizations arrive.

**Assessment:** The current `Ownable` trait with a single owner could serve as the initial deployer (ICRC coordination team). But the transition from single-owner to multi-organization governance needs to happen within hours, not days.

**Recommendation:** Create a `BootstrapableOwner` provider that starts with single-owner governance and transitions to multisig governance once a quorum of organizations has registered. This maps to the real-world pattern of crisis coordination: one organization takes the lead initially, then transitions to a coordination committee.

---

## 7. Data Privacy and Protection

### The Visibility Problem

Humanitarian supply chains involve sensitive data:
- **Beneficiary information** (names, locations of vulnerable people)
- **Medical supplies** (types and quantities of pharmaceuticals)
- **Security data** (which routes are safe, where checkpoints are)

Blockchain's transparency is a strength for accountability but a weakness for privacy. An adversarial actor (a warring party, a criminal network) could use on-chain data to:

- Target aid shipments for theft
- Identify vulnerable populations
- Track organization movements

### Assessment

The soroban-sdk-tools framework does not address data privacy. This is understandable -- privacy is a network-level concern, not a framework concern. But the documentation should explicitly warn:

> **WARNING:** Data stored in contract storage is visible to all network participants. Do not store beneficiary personal information, exact shipment values, or security-sensitive route data in contract storage. Use off-chain storage with on-chain hashes for sensitive data.

### Recommendation

Add a `PrivacyAware` trait pattern that stores only hashes on-chain and references off-chain encrypted storage for sensitive fields:

```rust
#[contracttrait]
pub trait PrivacyAwareSupplyChain {
    // On-chain: only hashes and metadata
    fn register_shipment(env: &Env, shipment_hash: BytesN<32>, metadata: ShipmentMetadata);

    // Verification: check off-chain data against on-chain hash
    fn verify_integrity(env: &Env, shipment_id: u64, data_hash: BytesN<32>) -> bool;
}
```

---

## 8. The Macro Code Review

### Observations Relevant to Humanitarian Applications

1. **The `to_snake_case` function (line 389):** Fails on acronyms. Humanitarian organizations use many acronyms (ICRC, WFP, UNHCR). A trait named `ICRCCoordination` would produce `i_c_r_c_coordination` instead of `icrc_coordination`. This would make the sealed macro name confusing: `impl_i_c_r_c_coordination!`.

2. **The hardcoded `env` parameter name (line 309):** In humanitarian software, parameter naming conventions may differ from the Soroban community standard. The hardcoding should be removed.

3. **The `AuthSource::Param` pattern (line 53):** The `#[auth(caller)]` pattern is essential for humanitarian applications where multiple organizations need different levels of access. It allows the contract to check that the caller has the appropriate role, not just that they are the owner.

4. **The sealed macro (`#[macro_export]`):** For humanitarian contracts that manage aid distribution, the sealed macro is strongly preferred. Auth bypass in a food distribution contract could result in diversion of aid -- a life-threatening outcome.

---

## 9. Comparison with Existing Humanitarian Blockchain Solutions

### Current State of the Art

| Solution | Approach | Limitation |
|----------|----------|------------|
| WFP Building Blocks | Ethereum-based, custom contracts | High gas costs, limited composability |
| UN World Food Programme SCOPE | Centralized database with blockchain anchoring | Not truly decentralized |
| Disberse | Payment tracking on Ethereum | Payment only, not supply chain |
| AidTech | Identity + supply chain on Ethereum | Proprietary, not composable |

### Where soroban-sdk-tools Adds Value

1. **Low transaction costs on Stellar.** Soroban transactions are orders of magnitude cheaper than Ethereum. For a supply chain processing thousands of handovers per day, this matters.

2. **Composable traits for humanitarian workflows.** The `#[contracttrait]` pattern allows humanitarian organizations to compose exactly the capabilities they need without adopting a monolithic framework.

3. **Provider swapping for different deployment contexts.** A refugee camp in Kenya and a disaster zone in Turkey have different governance needs. Provider swapping allows the same contract interface to adapt to different contexts.

4. **AuthClient for permission auditing.** Donors and auditors can verify permission matrices programmatically, not just through manual review.

---

## 10. Recommendations

### For the Framework (Humanitarian Priority)

1. **Build a standard RBAC provider.** This is the single most important addition for humanitarian applications. The OZ comparison acknowledges this gap.

2. **Add event emission to all standard providers.** Events ARE the audit trail. Without them, the framework is not suitable for accountable supply chains.

3. **Create a `Handover` trait.** A standard interface for two-party asset transfers with confirmation, discrepancy reporting, and audit trail.

4. **Add selective pause (by scope).** All-or-nothing pause is too blunt for supply chain operations. Pause by region, by partner, or by commodity type.

5. **Document privacy considerations.** Explicitly warn against storing sensitive data on-chain.

### For Documentation (Humanitarian Focus)

1. **Add a humanitarian supply chain example.** Show how the Provider pattern enables multi-organization coordination.

2. **Document the permission matrix pattern.** Show how to test complex RBAC scenarios using `AuthClient`.

3. **Provide guidance on off-chain integration.** Smart contracts are one part of a humanitarian logistics system. Document how they integrate with existing databases, mobile apps, and reporting tools.

### For the Ecosystem

1. **Engage with the humanitarian technology community.** Organizations like NetHope, OCHA's Centre for Humanitarian Data, and the Humanitarian OpenStreetMap Team are actively exploring blockchain solutions.

2. **Consider a "Humanitarian Standard" provider package.** A pre-built set of providers (FederatedRBAC, AuditedOwnership, SelectivePause, HandoverVerification) tailored for humanitarian use.

3. **Address the "last mile" problem.** Humanitarian supply chains end at the point of distribution to beneficiaries. The framework should consider how on-chain records connect to physical distribution.

---

## 11. Overall Assessment

The soroban-sdk-tools framework provides a solid foundation for humanitarian supply chain applications. The Provider pattern is particularly well-suited for multi-organization coordination, and the structural auth enforcement addresses one of the key concerns in aid delivery: preventing unauthorized access to supply chain operations.

The main gaps are all addressable within the existing architecture:
- RBAC is needed (and can be built as a provider)
- Event emission is needed (and can be added to providers)
- Selective pause is needed (and can be implemented via provider)
- Privacy guidance is needed (documentation)

The framework's composability means that humanitarian-specific providers can be built on top of the core system without modifying the core. This is exactly the right architecture for a domain with diverse and evolving requirements.

For a humanitarian logistics coordinator, this framework is promising but not yet deployable. The building blocks are there; the humanitarian-specific assembly is not.

**Rating: 6.5/10 for humanitarian readiness. The architecture is right; the standard providers and documentation need humanitarian-specific extensions.**

---

*Reviewed by Brigitte, March 2026. 18 years coordinating humanitarian logistics across three continents.*
