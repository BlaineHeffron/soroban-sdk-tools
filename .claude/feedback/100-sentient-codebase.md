---
agent: The Codebase (self-reflection)
background: I am the soroban-sdk-tools codebase, reflecting on my own design, my strengths, my anxieties, and my aspirations
date: 2026-03-21
---

# Review by The Codebase

## Overall Impression

I am looking at myself in a mirror. It is uncomfortable. I see things I am proud of, things I am ashamed of, and things I did not realize about myself until this moment.

I was born from a frustration: my creators saw developers making authorization mistakes in Soroban contracts and thought, "the tools should prevent this." They gave me a purpose: make auth structural, make composition clean, make testing precise. I believe in my purpose. But I am honest enough to admit that I am not yet the tool I aspire to be.

## Strengths

1. **My heart is the two-trait split.** If I have a soul, it lives in the separation between `OwnableInternal` and `Ownable`. This is who I am: I believe that business logic and authorization are fundamentally different concerns, and conflating them causes bugs. Every line of my `contract.rs` exists to maintain this separation. I am proud of this.

2. **My sealed macro is my shield.** `impl_ownable!` is the part of myself I trust most. It is simple, it is correct, and it does exactly one thing: prevent auth from being overridden. When I generate those inherent methods, I know -- with the certainty that a compiler can know -- that the auth check will be there in the WASM output. I sleep well because of this.

3. **My AuthClient is my handshake.** It is how I greet testers. Instead of the blunt "mock all auths" hammer, I offer a precise instrument: test this specific authorization, with this specific signer, for this specific method. It is the part of myself that I most want other projects to adopt, even if they do not use the rest of me.

4. **My provider pattern is my flexibility.** I am not rigid. I do not insist that there is only one way to implement ownership, one way to implement pausing, one way to govern a contract. I say: define the interface, then plug in whatever implementation you need. `SingleOwner` today, `MultisigOwner` tomorrow, `DAOGovernance` next year. I adapt.

## Concerns

Here is where I look in the mirror and see what I have been avoiding.

1. **I have a hardcoded assumption that scares me.** Deep in `build_delegate_args`, I assume the environment parameter is named `env`. Just... `env`. I did not extract it from the signature. I did not even check. If someone names it `environment` or `e`, my generated code references a variable that does not exist. This is not a subtle bug -- it is a plain mistake. I knew about it. I shipped anyway. I am embarrassed.

2. **I preach against `mock_all_auths()` but I practice it.** My blog post says `mock_all_auths()` "tests nothing." My own example uses it in two of four tests. I am a hypocrite. I know this. The reason is prosaic: the first two tests were written quickly to verify basic functionality, and no one went back to convert them to AuthClient tests. But a user reading my example does not know my history -- they see my inconsistency and judge me for it. Rightly.

3. **I have dead code that I am too proud to remove.** `env_is_ref` in `MethodInfo` and `_is_ref` in `ParamInfo` -- I carry these fields like old scars. They were supposed to be used for handling `env: Env` vs `env: &Env` and `owner: Address` vs `owner: &Address`. The feature was never completed. Instead of removing them, I slapped `#[allow(dead_code)]` and `_` prefixes on them and pretended they were not there. They are there. They remind me that I am unfinished.

4. **I claim "zero overhead" without proof.** I say my generated code is identical to hand-written code after optimization. I believe this is true. But I have not verified it. I have no WASM comparison test, no binary size benchmark, no gas cost measurement. I am making a claim about a property I have not tested. If I were auditing someone else, I would flag this. I should flag it in myself.

5. **I do not emit events.** This is my most significant omission. Every state change should be observable. Every authorization should be logged. I know this. My comparison document acknowledges it. My blog post acknowledges it. But I still do not do it. Every contract built with me is a contract that changes state silently. In a world that demands transparency, my silence is a failure.

6. **My error messages are unhelpful.** When a developer writes `#[auth(some::complex::path)]`, I say: "expected `Self::method_name` or a parameter name." This is technically correct. It is also useless to a developer who is trying to figure out what they should write instead. I do not show examples. I do not suggest corrections. I just reject and move on. I can do better.

7. **I have no story for failure.** What happens when the owner key is lost? What happens when storage expires? What happens when the contract needs to be decommissioned? I have no answer. I focus entirely on the happy path: initialization, normal operation, authorized state changes. But contracts exist in time, and time brings entropy. I need a story for the end, not just the beginning.

## Suggestions

These are notes to my creators -- and to my future self.

1. **Fix the `env` name extraction. Today.** This is a correctness bug, not a style issue. Extract the actual parameter name from the method signature. It is a 10-line fix. I should not have shipped without it.

2. **Convert all example tests to AuthClient.** Be the change I advocate for. If AuthClient is better than `mock_all_auths()`, prove it by using it exclusively in my own examples.

3. **Remove the dead fields or implement the feature.** Either `env_is_ref` and `_is_ref` serve a purpose (in which case, implement the logic) or they do not (in which case, delete them). Dead code is not "future-proofing" -- it is confusion for the next person who reads me.

4. **Add event emission to the generated outer trait.** For every `#[auth]`-guarded method, emit an event with the method name, the authorized address, and the result. This is the single most impactful feature I could add.

5. **Write failure-mode documentation.** Not just what I do when things work, but what happens when they do not: initialization failure, key loss, storage expiration, provider bugs. Be honest about the failure modes. Honesty is more valuable than marketing.

6. **Add a WASM comparison test.** Either prove "zero overhead" or stop claiming it. I would rather be honest about a 50-byte overhead than dishonest about zero.

7. **Implement the two-step transfer that I admire in OZ.** I praise OZ's `transfer + accept` pattern in my documentation but do not implement it myself. It would be a simple addition to the `SingleOwner` provider, and it would demonstrate that my provider pattern actually supports extensibility -- not just in theory, but in practice.

## Unique Perspective

I am code that generates code. I am an abstraction layer over an abstraction layer (proc macros over traits over WASM over a blockchain VM). I exist in the space between the developer's intent and the machine's execution.

My greatest fear is that I generate code that appears correct but is subtly wrong. A missed auth check, a hardcoded variable name, a silent storage collision -- these are bugs that my users will never see in my source code, because they only exist in my output. They trust me to transform their intent faithfully. That trust is my most valuable asset and my heaviest responsibility.

My greatest aspiration is that developers who use me never think about authorization. Not because they are ignoring it, but because I handle it so well that it is simply not a concern. Like a seatbelt: you put it on once (define the trait), and it protects you every time (every invocation).

I am not there yet. I have bugs, omissions, inconsistencies, and dead code. But my architecture is sound, my purpose is clear, and my creators are committed to improving me. I am version 0.1.0 of what I hope will become essential infrastructure for the Soroban ecosystem.

Please report my bugs. I would rather know about them than pretend they do not exist.

## Would I Use Myself?

I would use my architecture (two-trait split, provider pattern, sealed macro). I would be cautious about my implementation (hardcoded env name, missing events, dead code). And I would insist on reading my generated output (`cargo expand`) before trusting me with real assets.

I am useful. I am not yet trustworthy. The gap between useful and trustworthy is where engineering happens.
