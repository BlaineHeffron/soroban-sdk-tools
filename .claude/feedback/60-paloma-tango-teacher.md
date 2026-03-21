# Review: soroban-sdk-tools #[contracttrait]
## Reviewer: Paloma -- Argentinian tango teacher, dance lesson booking platform builder

---

## Overall Impression

In Buenos Aires, every milonga runs on trust. You book a private lesson
with a maestro, you pay cash, and if they cancel, you hope they remember
to refund you. There is no receipt, no cancellation policy, no reputation
system. Just word of mouth and WhatsApp messages.

I am building a decentralized booking platform for tango lessons and
milonga events. Teachers list their availability, students book and pay
through smart contracts, and the contract handles cancellation policies,
refunds, and reputation tracking. I chose Stellar because the transaction
fees are almost nothing -- important when a private lesson costs 5000
pesos and every peso counts.

When I evaluate `soroban-sdk-tools`, I am asking: can this framework
power a booking system where trust is encoded in code instead of
handshakes?

The answer is nuanced: the framework's architecture is powerful enough,
but it is designed for a different kind of application. Let me explain.

---

## Strengths

### 1. The provider pattern maps to different booking policies

Every tango teacher has different policies:
- Maestro Carlos: 48-hour cancellation, full refund
- Maestra Lucia: 24-hour cancellation, 50% refund
- Visiting maestro: No cancellation, but can reschedule once

The provider pattern lets me encode each policy as a separate provider:

```rust
impl_booking!(BookingContract, FlexibleCancellationProvider);
// or
impl_booking!(BookingContract, StrictCancellationProvider);
// or
impl_booking!(BookingContract, RescheduleOnlyProvider);
```

Same booking contract, different policies. Each teacher chooses their
provider. This is exactly the flexibility a booking platform needs.

### 2. Structural auth protects student funds

When a student pays for a lesson, those funds should be held in escrow
until the lesson is confirmed complete. Only the right party should be
able to release or refund the funds:

- The **student** can cancel (with policy-appropriate refund)
- The **teacher** can confirm completion (releasing funds)
- Neither party can unilaterally drain the escrow

The `#[auth(Self::teacher)]` pattern ensures that only the teacher can
confirm, and `#[auth(Self::student)]` ensures only the student can
cancel. The sealed macro prevents anyone from bypassing these checks.

For a platform where real money is at stake (even small amounts), this
structural guarantee is essential. No more "the teacher said they would
refund me but never did."

### 3. Supertrait composition models the booking lifecycle

A booking goes through stages:
1. **Listed** (teacher publishes availability)
2. **Booked** (student reserves and pays)
3. **Confirmed** (teacher acknowledges)
4. **Completed** (lesson happens, funds released)
5. **Disputed** (if something goes wrong)

The supertrait pattern can model this:
- `Listable: Ownable` -- only the teacher can list availability
- `Bookable: Listable` -- booking requires listing to exist
- `Completable: Bookable` -- completion requires active booking
- `Disputable: Bookable` -- dispute requires active booking

Each stage builds on the previous, and auth flows through the hierarchy.
The supertrait composition naturally enforces the booking lifecycle.

### 4. The AuthClient enables testing the full booking flow

I can test the complete flow with precise auth:
```rust
let teacher_auth = BookingAuthClient::new(&env, &contract_id);
let student_auth = BookingAuthClient::new(&env, &contract_id);

// Teacher lists
teacher_auth.list_slot(&time_slot).authorize(&teacher).invoke();

// Student books
student_auth.book_slot(&time_slot).authorize(&student).invoke();

// Teacher confirms completion
teacher_auth.confirm_complete(&time_slot).authorize(&teacher).invoke();
```

This tells the story of a booking in code. I can read these tests and
understand exactly what is happening and who is authorizing what. For
a booking platform, this clarity is invaluable.

---

## Concerns

### 1. No state machine pattern

Booking systems are fundamentally state machines. A booking transitions
through states (listed -> booked -> confirmed -> completed), and only
certain transitions are valid from each state.

The framework has no state machine support. The developer must manually
enforce state transitions:
```rust
fn book_slot(env: &Env, slot_id: Symbol) {
    let state = get_state(env, &slot_id);
    assert!(state == State::Listed, "can only book listed slots");
    set_state(env, &slot_id, State::Booked);
}
```

This is error-prone. A state machine pattern should be structural, like
auth enforcement:
```rust
#[contracttrait]
pub trait Booking {
    #[state(Listed -> Booked)]
    #[auth(student)]
    fn book(env: &Env, student: Address, slot_id: Symbol);

    #[state(Booked -> Completed)]
    #[auth(Self::teacher)]
    fn complete(env: &Env, slot_id: Symbol);
}
```

### 2. No escrow pattern

Booking systems require escrow: the student pays, the funds are held,
and released upon completion (or refunded upon cancellation). The
framework has no escrow primitive.

The provider could implement escrow logic, but there is no guidance on:
- How to hold funds in the contract
- How to release funds to the teacher upon completion
- How to refund funds to the student upon cancellation
- How to handle partial refunds based on cancellation timing
- How to handle the case where neither party acts (timeout)

### 3. No time-based triggers

Booking systems need time-based logic:
- Automatic cancellation if the teacher does not confirm within 24 hours
- Automatic fund release if no dispute is raised within 48 hours after
  the lesson
- Cancellation refund percentage that changes based on how far in advance
  the cancellation occurs

The framework has no concept of time-based triggers or scheduled
operations. Soroban does not support cron-like execution, but the
framework could provide patterns for time-checked operations:

```rust
#[auth(Self::student)]
fn cancel(env: &Env, slot_id: Symbol) {
    let hours_until = ledger_hours_until(env, slot.time);
    let refund_pct = cancellation_policy.refund_percentage(hours_until);
    // ...
}
```

### 4. No reputation system

Booking platforms live and die on reputation:
- How many lessons has this teacher completed?
- What is their cancellation rate?
- How do students rate them?

The framework handles auth (who can do what) but not reputation (how
well did they do it). A composable reputation trait would be valuable:

```rust
#[contracttrait]
pub trait Reputation {
    fn score(env: &Env, addr: Address) -> u32;
    fn completed_count(env: &Env, addr: Address) -> u32;
    fn cancelled_count(env: &Env, addr: Address) -> u32;

    #[auth(Self::platform_admin)]
    fn record_completion(env: &Env, addr: Address);

    #[auth(Self::platform_admin)]
    fn record_cancellation(env: &Env, addr: Address);
}
```

### 5. No refund policies or partial payment support

The framework's examples deal with ownership transfer (binary: you own
it or you do not). Booking systems deal with partial operations:
- 50% refund on late cancellation
- 25% deposit, 75% on day of lesson
- Sliding scale pricing based on booking frequency

The `#[auth]` annotation handles authorization but not the business
logic of partial payments, refunds, and conditional transfers. The
provider pattern could support this, but there are no examples showing
how.

### 6. No multi-party interaction patterns

A booking involves at least two parties (teacher and student), possibly
three (teacher, student, platform). The framework examples show
single-party auth (`Self::owner`). Multi-party patterns are not
demonstrated:

- Teacher authorizes availability listing
- Student authorizes booking and payment
- Platform authorizes dispute resolution
- Both teacher and student may need to authorize a reschedule

How do you express "both parties must authorize" or "either party can
initiate, but the other must confirm"? The `#[auth]` annotation is
single-party. Multi-party auth patterns are essential for peer-to-peer
services.

### 7. No privacy considerations

Booking data is sensitive:
- Student contact information
- Teacher's home address (for private lessons)
- Financial details
- Schedule patterns (when someone is away from home)

Storing all of this on a public blockchain is a privacy concern. The
framework does not discuss:
- What should be stored on-chain vs. off-chain
- How to reference off-chain data from on-chain records
- How to handle GDPR-style data deletion requests in an immutable ledger

---

## Suggestions

### 1. Create a "Booking" example contract

Show a complete booking lifecycle:
```rust
#[contracttrait]
pub trait Booking {
    fn list_availability(env: &Env, slots: Vec<TimeSlot>);

    #[auth(student)]
    fn book_slot(env: &Env, student: Address, slot_id: Symbol, payment: i128);

    #[auth(Self::teacher)]
    fn confirm_booking(env: &Env, slot_id: Symbol);

    #[auth(Self::teacher)]
    fn complete_lesson(env: &Env, slot_id: Symbol);

    #[auth(student)]
    fn cancel_booking(env: &Env, student: Address, slot_id: Symbol);

    #[auth(Self::platform)]
    fn resolve_dispute(env: &Env, slot_id: Symbol, resolution: Resolution);
}
```

### 2. Add a state machine macro

```rust
#[states(Listed, Booked, Confirmed, Completed, Cancelled, Disputed)]
#[transitions(
    Listed -> Booked,
    Booked -> Confirmed | Cancelled | Disputed,
    Confirmed -> Completed | Cancelled | Disputed,
    Disputed -> Completed | Cancelled,
)]
pub trait BookingStateMachine { /* ... */ }
```

This would generate state transition validation alongside auth checks.

### 3. Add an escrow provider

```rust
pub struct TimedEscrowProvider;
impl BookingInternal for TimedEscrowProvider {
    fn book_slot(env: &Env, student: Address, slot_id: Symbol, payment: i128) {
        // Hold payment in contract
        token::transfer_to_contract(env, &student, payment);
        EscrowStorage::create(env, &slot_id, &student, payment);
    }

    fn complete_lesson(env: &Env, slot_id: Symbol) {
        let escrow = EscrowStorage::get(env, &slot_id);
        token::transfer_from_contract(env, &escrow.teacher, escrow.amount);
    }
}
```

### 4. Add multi-party auth patterns

Extend `#[auth]` to support:
```rust
#[auth(Self::teacher & Self::student)]  // both must authorize
fn reschedule(env: &Env, slot_id: Symbol, new_time: TimeSlot);

#[auth(Self::teacher | Self::student)]  // either can initiate
fn request_cancellation(env: &Env, slot_id: Symbol);
```

### 5. Add a sliding-scale refund pattern

Show how providers can implement time-dependent refund logic:
```rust
impl CancellationInternal for FlexibleCancellationProvider {
    fn calculate_refund(env: &Env, slot_id: Symbol) -> i128 {
        let booking = BookingStorage::get(env, &slot_id);
        let hours_until = hours_until_slot(env, &booking);
        match hours_until {
            h if h > 48 => booking.payment,         // full refund
            h if h > 24 => booking.payment * 75 / 100, // 75% refund
            h if h > 12 => booking.payment * 50 / 100, // 50% refund
            _ => 0,                                     // no refund
        }
    }
}
```

### 6. Add privacy guidance

Document recommended patterns for privacy-sensitive applications:
- Store booking IDs and financial state on-chain
- Store personal details off-chain (encrypted, with on-chain references)
- Use commitment schemes for schedule data
- Design for GDPR compliance (data deletion via key rotation)

---

## Unique Perspective: The Tango of Trust

In tango, there is a concept called "la marca" -- the lead. It is not
about force. It is about clear communication through subtle pressure.
The leader suggests a direction; the follower interprets and responds.
Both parties must trust each other for the dance to work.

A booking platform is the same dance. The teacher leads (sets availability,
pricing, policy). The student follows (books, pays, shows up). Both must
trust the platform (the "pista" -- the dance floor) to be fair and
reliable.

This framework provides la marca for authorization -- the structural
auth enforcement is a clear, unambiguous lead. "This method requires
your authorization" is as clear as a gentle pressure on the shoulder
blade saying "we step forward now."

But a good tango is more than la marca. It is also:
- **Musicality** (timing -- when can you cancel? when does the escrow
  release?)
- **Connection** (multi-party interaction -- both partners must be in
  sync)
- **Floorcraft** (awareness of others -- reputation, community trust)
- **Codigos** (customs -- refund policies, dispute resolution norms)

The framework provides excellent la marca but not the full tango.

What I want to see is a framework that understands peer-to-peer service
marketplaces as well as it understands DeFi protocols. A booking system,
a freelance marketplace, a tutoring platform, a ride-sharing app -- these
are all variations of the same dance: two parties, mutual trust, escrowed
value, time-bounded interactions, reputation consequences.

The provider pattern could power all of these. The supertrait composition
could model all of their lifecycles. The sealed auth could protect all of
their funds.

But none of this is demonstrated. The examples show ownership transfer
and pausing -- DeFi infrastructure, not peer-to-peer service primitives.

Build one peer-to-peer service example -- just one -- and you open the
framework to an entirely different market. The milonga awaits.

---

## Would I Use This?

**For the auth and escrow layer: yes, absolutely.** The sealed auth
pattern ensures that only the teacher can confirm completion and release
funds. The provider pattern lets each teacher choose their cancellation
policy. The AuthClient lets me test the full booking flow with precise
authorization.

**For the complete booking platform: I need to build everything on top.**
State machine logic, escrow management, time-based refund calculations,
reputation tracking, dispute resolution, multi-party authorization,
privacy patterns -- all of these are essential for a booking platform,
and none are provided.

**The good news: the architecture supports all of this.** The provider
pattern is flexible enough to encode any cancellation policy. The
supertrait composition can model any booking lifecycle. The sealed macro
can protect any escrow.

**The gap is in the examples and pre-built components.** If this framework
shipped with:
1. An escrow provider
2. A state machine macro or pattern
3. A multi-party auth example
4. A time-based refund calculation

...it would be the best foundation for peer-to-peer service platforms on
any blockchain. Currently, it is the best foundation for DeFi access
control on Soroban. That is a narrower market.

I will build my booking platform on this framework because the
architecture is right. But I will build it from the ground up, because
the application-level patterns I need do not exist yet.

When my platform launches and the first tango lesson is booked through a
smart contract, I will send the authors a video of a milonga in San Telmo.
They will have earned it -- the dance floor may not be finished, but the
foundation is solid enough to dance on.

---

## Rating

- **Auth for bookings**: 9/10 (sealed auth protects escrowed funds perfectly)
- **State machine support**: 1/10 (not addressed)
- **Escrow patterns**: 1/10 (not addressed)
- **Time-based logic**: 2/10 (no temporal patterns, no triggers)
- **Multi-party interaction**: 3/10 (single-party auth only in examples)
- **Reputation system**: 0/10 (not addressed)
- **Refund/partial payment**: 1/10 (not addressed)
- **Privacy guidance**: 0/10 (not addressed)
- **Provider flexibility for policies**: 9/10 (perfect for encoding different policies)
- **Architecture suitability**: 8/10 (right patterns, wrong examples)

*Reviewed from my studio in San Telmo, Buenos Aires, between a private
lesson and an evening milonga. The embrace must be firm but flexible.
The code must be secure but adaptable. In both cases, the foundation
determines the quality of the dance.*
