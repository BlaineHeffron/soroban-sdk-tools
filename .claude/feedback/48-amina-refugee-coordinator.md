# Review by Amina -- Refugee Camp Coordinator

*"When you have lost everything -- your home, your documents, your identity -- the last thing you need is a system that demands proof of who you are."*

---

## Overall Impression

I coordinate aid distribution in refugee camps across East Africa. I have worked with
UNHCR, WFP, and various NGOs. I have seen what happens when identity systems fail:
families go hungry because their biometric scan does not match the database. Mothers
cannot prove their children belong to them because their birth certificates were
destroyed in the bombing. Aid gets diverted because the distribution system trusts the
wrong people.

I am exploring blockchain for aid distribution because I need three things that
traditional systems do not provide:

1. **Identity without documentation** -- a way for people to receive aid without
   government-issued ID
2. **Trustless distribution** -- a system where aid reaches recipients without relying
   on a single trusted party
3. **Offline capability** -- a system that works when the cell tower is down and the
   generator has no fuel

I review this codebase through the lens of humanitarian technology. Can this framework
be used to build systems that serve the most vulnerable people in the world?

---

## Identity Without Documentation: The Auth Model

### The Current Model

The `#[auth]` attribute resolves to `address.require_auth()`. In Soroban, this means
the caller must provide a valid cryptographic signature from the specified address. This
requires:

1. A Stellar keypair (public + private key)
2. A device that can sign transactions
3. Connectivity to submit the signed transaction to the network

**Assessment for refugee contexts:**

- **Keypair generation:** Possible on a basic smartphone. A camp worker could generate
  keypairs offline and associate them with recipients. But what if the phone is lost,
  stolen, or broken? The keypair IS the identity. Losing the phone means losing access
  to aid.

- **Signing device:** A smartphone with a Stellar wallet app. Many refugees have
  smartphones, but they are often shared among family members, have cracked screens,
  run out of battery, or have outdated operating systems that cannot run modern apps.

- **Connectivity:** Required to submit transactions. In the camps I manage, internet
  connectivity is intermittent at best. We sometimes go days without a reliable
  connection.

### What the Framework Enables

The provider pattern is crucial here. Instead of a single `SingleOwner` that requires one
address's signature, you could build providers that support humanitarian use cases:

```rust
pub struct HumanitarianAuth;
impl OwnableInternal for HumanitarianAuth {
    fn owner(env: &Env) -> Address {
        // Could resolve to:
        // - A multisig of camp coordinators
        // - A rotating set of community leaders
        // - A recovery-capable identity system
        HumanitarianStorage::get_guardian(env).unwrap()
    }
}
```

The framework ALLOWS this flexibility. But it does not PROVIDE humanitarian-specific
providers. Everything in the examples assumes a single address with a single keypair --
the Silicon Valley assumption that everyone has a reliable device and a stable identity.

### What Is Missing

#### Recovery Mechanisms

When a refugee loses their phone, they lose their keypair. There is no recovery mechanism
in the current framework. OZ's two-step transfer provides a safety net (the new owner
must accept), but it still requires the OLD owner to initiate. If the old owner's device
is gone, the process cannot start.

A humanitarian provider needs:

```rust
pub struct RecoverableIdentity;
impl OwnableInternal for RecoverableIdentity {
    fn owner(env: &Env) -> Address { /* primary address */ }
    fn transfer_ownership(env: &Env, new_owner: Address) {
        // Allow transfer if:
        // 1. Primary address signs (normal path), OR
        // 2. N-of-M guardian addresses sign (recovery path), OR
        // 3. A time-locked guardian override after waiting period
    }
}
```

The `#[auth]` attribute currently supports only one auth source per method. A recovery
model needs conditional auth -- "this address OR this set of addresses." The macro would
need to support `#[auth(Self::owner | Self::guardians)]` or similar.

#### Delegated Identity

In refugee camps, identity is often managed through delegation:

- A parent acts on behalf of children
- A community leader acts on behalf of families
- A camp coordinator acts on behalf of the entire camp

The current auth model does not support delegation. `require_auth()` requires the
specific address to sign. There is no concept of "Address A can act on behalf of
Address B."

Soroban does support this at the protocol level through `require_auth_for_args()` and
contract-level authorization. But the `#[auth]` macro does not expose this capability.

**Recommendation:** Add a `#[auth(Self::owner, delegatable)]` attribute that generates
a delegation check: "either the owner signs, or an authorized delegate signs."

#### Anonymous Identity

Some refugees cannot be identified -- they may be stateless, or their government may be
tracking them. They need to receive aid without revealing who they are.

The current model is pseudonymous (Stellar addresses are not linked to real-world identity
by default), but transaction patterns can be de-anonymized. A truly privacy-preserving aid
system would need:

- Zero-knowledge proofs of eligibility ("I am in the UNHCR database" without revealing
  which record)
- Stealth addresses (one-time-use addresses for each aid distribution)
- Mixers or shielded pools

These are beyond the scope of this framework, but the provider pattern COULD support
them. A `ZKAuth` provider could implement `owner()` using a ZK verifier instead of a
simple address lookup.

---

## Trustless Aid Distribution: The Composability Model

### The Promise

The composability model (Ownable + Pausable + FungibleToken) could theoretically support
an aid distribution system:

1. **Ownable:** The aid program is "owned" by the coordinating NGO
2. **Pausable:** The distribution can be paused in emergencies (security threats, supply
   shortages)
3. **FungibleToken:** Aid vouchers are tokens that recipients can redeem

The provider pattern enables different governance models:

```rust
// UN-managed: single authorized UN agency
impl_ownable!(AidProgram, UNAgency);

// Community-managed: council of camp leaders
impl_ownable!(AidProgram, CommunityCouncil);

// Hybrid: UN + community multisig
impl_ownable!(AidProgram, HybridGovernance);
```

This is genuinely powerful. The same aid distribution contract can be deployed with
different governance models for different contexts. A camp with strong community
leadership uses `CommunityCouncil`. A new camp without established leadership uses
`UNAgency`. As the camp matures, governance can be transferred.

### The Reality

#### Problem 1: No Offline Capability

All operations require an on-chain transaction. In my camps, the internet goes down
regularly. When the internet is down, no aid can be distributed. People go hungry because
the blockchain requires connectivity.

This is not a problem with soroban-sdk-tools specifically -- it is a fundamental
limitation of blockchain-based systems. But any framework positioning itself for
real-world use should acknowledge this limitation and provide guidance for hybrid
on-chain/off-chain architectures.

**Recommendation:** Document a pattern for offline-capable aid distribution:

1. Pre-sign batches of aid vouchers when connectivity is available
2. Distribute pre-signed vouchers offline (via QR codes, NFC, or local mesh)
3. Settle on-chain when connectivity returns

The provider pattern could support this:

```rust
pub struct OfflineCapableAid;
impl FungibleTokenInternal for OfflineCapableAid {
    fn transfer(env: &Env, from: Address, to: Address, amount: i128) {
        // Check for pre-signed batch vouchers
        if BatchVoucherStorage::has_voucher(env, &from, &to, amount) {
            BatchVoucherStorage::redeem(env, &from, &to, amount);
        } else {
            // Normal on-chain transfer
            BalanceStorage::transfer(env, &from, &to, amount);
        }
    }
}
```

#### Problem 2: No Supply Chain Tracking

Aid distribution has two sides: the supply side (where does the aid come from?) and the
demand side (who receives it?). The current framework focuses entirely on access control
(who can call which method). It does not provide patterns for:

- Tracking aid from donor to recipient
- Verifying that aid was actually delivered (not diverted)
- Recording the condition of received goods
- Accountability reporting back to donors

These are the features that would make blockchain valuable for humanitarian logistics.
Access control is necessary but not sufficient.

#### Problem 3: No Dispute Resolution

What happens when a recipient claims they did not receive their aid, but the blockchain
says they did? Or when a coordinator transfers ownership incorrectly? The current
framework has no dispute resolution mechanism.

In traditional humanitarian systems, disputes are resolved through a complaints committee.
On-chain, disputes need a different mechanism -- perhaps a time-locked transaction that can
be challenged within a window, or a multisig arbitration panel.

The provider pattern could support this:

```rust
pub struct DisputeResolvable;
impl OwnableInternal for DisputeResolvable {
    fn transfer_ownership(env: &Env, new_owner: Address) {
        // Start a dispute window instead of immediate transfer
        DisputeStorage::create_pending_transfer(
            env,
            &new_owner,
            env.ledger().sequence() + DISPUTE_WINDOW,
        );
    }
}
```

But the trait interface does not expose the concept of "pending" or "disputeable"
operations. The `transfer_ownership` return type is `()` -- there is no way to return a
pending transfer ID that can be referenced in a dispute.

---

## The Pausable Pattern: Emergency Response

### Why Pause Matters in Humanitarian Contexts

In my camps, we sometimes need to halt aid distribution immediately:

- Security threat (armed group approaching)
- Supply contamination (spoiled food in the pipeline)
- System error (wrong beneficiary list loaded)
- Natural disaster (flooding, earthquake)

The `Pausable` trait addresses this:

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

**Assessment:** This is a good start, but humanitarian pauses need more nuance:

1. **Partial pause:** Pause new distributions but allow redemption of already-issued
   vouchers. A binary pause/unpause is too coarse.

2. **Emergency pause without owner auth:** If the owner is unreachable (their phone is
   dead, they are in a secure area without connectivity), no one can pause the system.
   Consider a `#[auth(Self::owner | Self::emergency_contact)]` pattern.

3. **Automatic unpause:** A timed pause that automatically unpauses after a duration.
   This prevents indefinite pauses caused by an owner who forgot to unpause.

4. **Pause reason:** Why was the system paused? Different reasons may require different
   responses. The current `pause()` takes no parameters -- there is no way to record
   the reason.

---

## Testing: Does It Work When Everything Goes Wrong?

### The Current Test Suite

The four tests in `examples/trait-test/src/lib.rs` test:

1. Ownership works
2. Pause/unpause works
3. AuthClient works for ownership
4. AuthClient works for pause

These are all happy-path tests. In humanitarian contexts, EVERYTHING goes wrong:

- The owner's device is lost
- The contract is not initialized
- Two people claim to be the owner
- The internet drops mid-transaction
- The camp is relocated and all addresses change

None of these scenarios are tested.

### Tests I Would Need

```rust
#[test]
fn test_recovery_when_owner_device_lost() { ... }

#[test]
fn test_aid_distribution_when_offline() { ... }

#[test]
fn test_dispute_resolution_for_contested_distribution() { ... }

#[test]
fn test_emergency_pause_without_owner() { ... }

#[test]
fn test_graceful_degradation_on_uninitialized_contract() { ... }

#[test]
fn test_identity_recovery_with_guardian_signatures() { ... }
```

These tests do not exist in the framework. They COULD be built using the provider pattern,
but the framework does not encourage or facilitate them.

---

## Recommendations for Humanitarian Use

### Priority 1: Identity Recovery

Build a standard `RecoverableOwner` provider that supports:
- Primary address (normal operation)
- Guardian addresses (recovery)
- Time-locked recovery (anti-theft)

This is the single most important feature for humanitarian blockchain. Without recovery,
losing a device means losing access to aid. That is unacceptable.

### Priority 2: Offline Patterns

Document and provide examples of offline-capable patterns using the provider model. This
should include:
- Pre-signed transaction batches
- Local validation (check against a cached state)
- Batch settlement when connectivity returns

### Priority 3: Graduated Pause

Extend the `Pausable` trait to support:
- Partial pauses (pause specific operations, not everything)
- Reason-tagged pauses
- Automatic unpause with timeout
- Emergency pause with alternative auth

### Priority 4: Delegation Support

Add `#[auth]` support for delegated authorization:
- Parent on behalf of child
- Coordinator on behalf of community
- Agent on behalf of organization

### Priority 5: Dispute Resolution

Provide a standard `Disputable` trait that wraps state changes in a challenge window:
- Pending state changes
- Challenge period
- Arbitration mechanism
- Automatic resolution after timeout

---

## The Human Cost of Technical Decisions

I want to close with something that I think about every day but that rarely appears in
code reviews.

Every technical decision has a human cost. The decision to use `require_auth()` with a
single address means that a mother who loses her phone cannot feed her children until
someone helps her create a new keypair. The decision to require internet connectivity
means that when the generator runs out of fuel, the entire camp loses access to aid.

These are not edge cases. These are the daily reality of the people I serve.

The `#[contracttrait]` framework is technically excellent. The structural auth enforcement
is a genuine improvement. The provider pattern enables the kind of flexibility that
humanitarian systems need. But the examples, the documentation, and the default patterns
all assume a world where people have reliable devices, stable internet, and established
identities.

My people have none of these things. And they are the ones who need trustless systems the
most.

I am not asking this framework to solve humanitarian aid. I am asking it to acknowledge
that its users are not all Silicon Valley developers with MacBooks and fiber internet.
The provider pattern makes it POSSIBLE to build humanitarian systems. The documentation
should make it VISIBLE.

---

## Humanitarian Readiness Scorecard

| Aspect | Rating | Notes |
|--------|--------|-------|
| Identity flexibility | B | Provider pattern enables alternatives; none provided |
| Recovery mechanisms | F | No recovery; device loss = identity loss |
| Offline capability | F | All operations require connectivity |
| Trustless distribution | B+ | Provider pattern enables trustless governance |
| Dispute resolution | F | No dispute mechanism |
| Emergency response | C | Pausable exists but is too coarse |
| Delegation | D | Not supported in #[auth] |
| Privacy | C | Pseudonymous but not anonymous |
| Documentation for humanitarian use | F | No humanitarian examples or guidance |

**Overall humanitarian readiness: D+**

The architecture enables humanitarian use cases through the provider pattern, but none
are implemented, documented, or tested. The framework is a foundation, not a solution.

---

## Files Reviewed

| File | Humanitarian Assessment |
|------|------------------------|
| `docs/oz-comparison.md` | No humanitarian context considered |
| `docs/blog-post-composable-contracts.md` | Focused on developer experience, not user experience |
| `examples/trait-test/src/lib.rs` | Happy-path only; no failure scenarios |
| `soroban-sdk-tools-macro/src/contract.rs` | Flexible architecture; constrained by single-address auth |

---

*Amina Hassan. Camp Coordinator, UNHCR East Africa.
"Technology is not neutral. It serves whoever designs it. Design it for the people who
have no one else to design for them."*
