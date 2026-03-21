---
agent: Zara Chen
background: 14-year-old high school sophomore who runs a Discord server with 200 members and wants to build a DAO for their school's environmental club on Stellar
date: 2026-03-21
---

# Review by Zara Chen

## Overall Impression

ok so I've been learning Rust for like 4 months from the Rust Book and I watched all of Soroban's youtube tutorials. I found this project because I want to build a DAO where our environmental club can vote on which projects to fund with our fundraising money. I read through all the docs and examples and here's what I think.

The idea is really cool -- like, the fact that you can just write `#[auth(Self::owner)]` and it automatically checks permissions? That's amazing. In the Soroban tutorials I had to write all the `require_auth()` stuff manually and I kept forgetting it. But honestly some of the documentation lost me pretty fast.

## Strengths

1. **The trait-test example actually compiles and makes sense.** I cloned the repo and ran `cargo test` on the example and it worked! That's huge because like half the blockchain tutorials I've tried have broken examples. The test names are clear (`test_ownership_with_auth_enforcement`, `test_pausable_supertrait_composition`) and I can read them and understand what's being tested.

2. **The `#[auth(Self::owner)]` syntax is intuitive.** Even without understanding proc macros (I barely know what those are), I can read `#[auth(Self::owner)]` and understand it means "the owner needs to authorize this." That's way better than having to write `owner.require_auth()` in every function and remembering which functions need it.

3. **The pause/unpause pattern is exactly what I need.** For our DAO, I need the ability to pause voting if something goes wrong. The `Pausable` trait with `pause()`, `unpause()`, and `is_paused()` is exactly the right abstraction. And the fact that it extends `Ownable` makes sense -- only the admin should be able to pause things.

4. **The `SingleOwner` provider is a good starting point.** I can understand the implementation -- it's just storing an address in storage and reading it back. I could write my own provider that does something different (like checking if someone is in a list of club officers).

## Concerns

1. **I don't understand what "provider" means in this context.** The blog post talks about "provider-based dependency injection" and "CGP" and "functorial properties" and I have no idea what any of that means. I just want to know: what do I need to write to make my contract work? A "Getting Started for Beginners" guide that skips all the theory and just shows step-by-step what to do would be amazing.

2. **The blog post compares to OpenZeppelin but I've never used OpenZeppelin.** I don't know what Solidity is. I don't know what EVM is. The entire comparison is lost on me. I need a standalone guide that doesn't assume I know other blockchain ecosystems.

3. **Where's the DAO example?** There's an `Ownable` example and a `Pausable` example, but those are simple admin patterns. I need voting, proposals, quorums, time-limited votes. Is this the right tool for building a DAO, or is it only for simple ownership stuff? I can't tell from the current docs.

4. **The test example uses `mock_all_auths()` which the blog post says is bad.** The blog post literally says `mock_all_auths()` "tests nothing" and then the example uses it! I'm confused -- should I use it or not? If the AuthClient is better, why doesn't every test use it?

5. **I don't understand `type Provider = SingleOwner`.** What is `type Provider`? Why can't I just implement the trait directly? The blog post says it's for "dependency injection" but I don't know what that is. Can you explain it like... "it's like choosing which recipe to follow for the same dish"? (I cook a lot, sorry for the food metaphor.)

6. **There's no frontend integration guide.** Once I deploy the contract, how do I call it from my React app? How do I connect a wallet? The `AuthClient` is cool for testing but what about production?

## Suggestions

1. **Write a "Build Your First Contract in 15 Minutes" tutorial.** Start with: (a) create a new Soroban project, (b) add the dependency, (c) write a trait, (d) implement a provider, (e) wire it up, (f) test it, (g) deploy it. No theory, no comparisons, just steps.

2. **Add a DAO example.** Show a simple voting contract with proposals, votes, and a quorum check. This would demonstrate supertrait composition (Ownable + Pausable + Votable) and make the value of the tool concrete for governance use cases.

3. **Explain providers with an analogy.** Something like: "A provider is like a plug. Your trait defines the shape of the socket (what methods are needed), and the provider is the specific plug you insert (how those methods work). You can swap plugs without rewiring the house."

4. **Fix the test inconsistency.** Either remove `mock_all_auths()` from the example and use `AuthClient` everywhere, or add a comment explaining when `mock_all_auths()` is acceptable (e.g., for non-auth tests like `init`).

5. **Add error messages that tell me what to do.** When something goes wrong (like I forget to implement a method), the error should say "you need to implement `owner()` in your provider" not just "method not found."

6. **Add a glossary.** I don't know what "supertrait," "proc macro," "DI," "CGP," "monomorphization," "inherent method," or "sealed pattern" means. A glossary at the bottom of the docs with one-sentence definitions would help a lot.

## Unique Perspective

I represent the next generation of blockchain developers. I'm not a Solidity veteran migrating to Rust. I'm not a systems programmer who already knows traits and lifetimes. I'm a teenager who wants to build something cool for my school club.

If this tool wants to grow the Soroban ecosystem, it needs to be accessible to people like me. Not dumbed down -- I can learn hard things. But explained from first principles, with examples that match what I actually want to build, and error messages that guide me instead of confusing me.

The architecture might be brilliant (the adults in my Discord say it is), but if I can't figure out how to use it in an afternoon, I'm going to use something else. That's not a threat -- it's just the reality of how teenagers learn. We have short attention spans and infinite options.

## Would I Use This?

I want to! The `#[auth(Self::owner)]` syntax is exactly what I need -- I kept getting auth wrong when doing it manually. But I need more examples, a beginner tutorial, and a DAO template. If those existed, I'd switch to this immediately. Right now I'm going to struggle through it because the concept is right, even if the onboarding isn't there yet.
