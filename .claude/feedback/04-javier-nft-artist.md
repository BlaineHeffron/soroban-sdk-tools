---
persona: Javier
age: 29
background: Street artist turned NFT coder, self-taught Rust from YouTube, sold art on Ethereum and Tezos
focus: Accessibility for non-CS artists who want to code their own contracts
tone: Warm, visual thinker, gets frustrated by academic abstractions, uses metaphors from art
---

# Review: soroban-sdk-tools -- Can an Artist Actually Use This?

## First Impression: The Wall

I read the blog post and the example code. My honest first reaction: this is
beautiful architecture dressed in intimidating language. "Provider-based
dependency injection." "Context-Generic Programming." "Structural auth
enforcement." "Sealed macro patterns."

These are walls. I paint walls. But these walls keep people out instead of
inviting them in.

The actual CODE is not that complicated. Look at the trait-test example:

```rust
#[contracttrait]
pub trait Ownable {
    fn owner(env: &Env) -> Address;

    #[auth(Self::owner)]
    fn transfer_ownership(env: &Env, new_owner: Address);
}
```

This is five lines. I can read this. "There is an owner. To transfer ownership,
the owner must authorize it." The `#[auth(Self::owner)]` is actually genius
because it reads like English: "auth requires self's owner."

But then I look at the generated code explanation and I get lost. The blog post
shows `OwnableInternal`, `Ownable` outer trait, `type Provider`, `impl_ownable!`,
`OwnableAuthClient` -- that is five concepts generated from five lines of input.
For an artist who just wants to make their NFT contract ownable, this is a lot.

## What I Want as an Artist

I want to write something like this:

```rust
#[contracttrait]
pub trait NFTOwnable {
    fn artist(env: &Env) -> Address;

    #[auth(Self::artist)]
    fn set_royalty(env: &Env, percentage: u32);

    #[auth(Self::artist)]
    fn update_metadata(env: &Env, token_id: u64, uri: String);
}
```

And I want the framework to just... handle it. I should not need to know what a
"Provider" is. I should not need to choose between `impl_nft_ownable!` and
`#[contractimpl(contracttrait)]`. I should not need to understand the security
implications of "sealed vs flexible."

## The "Default Provider" Problem

In the OZ comparison, you show that switching from `SingleOwner` to `MultisigOwner`
is a one-line change. That is cool for DAOs and DeFi protocols. But 90% of NFT
artists just want single ownership. We should not need to think about multisig.

**Suggestion**: Provide a `#[contracttrait(default_provider = SingleOwner)]`
attribute so that the simple case requires zero provider boilerplate:

```rust
impl_ownable!(MyNFT);  // uses SingleOwner automatically
```

Or even better: a higher-level `#[ownable]` attribute macro that does everything
in one shot:

```rust
#[contract]
#[ownable]  // just... make it ownable. That is all I want.
pub struct MyNFT;
```

## Error Messages Are the UX

When I forget `require_auth()` in vanilla Soroban, the error message is a runtime
panic that says something about "missing auth." When I use your framework and make
a mistake, what happens?

If I implement `OwnableInternal` wrong -- say I return a hardcoded address instead
of reading from storage -- will the compiler tell me? Will the test catch it? What
does the error look like?

The blog post talks about what the framework prevents but never shows what happens
when things go wrong. For artists learning to code, the error path IS the learning
experience. Show us the errors. Show us how to fix them.

## The Naming Convention Barrier

`OwnableInternal` vs `Ownable` vs `OwnableAuthClient` vs `impl_ownable!`

Four names for one concept. I would constantly confuse which one I need to use
where. A quick-reference card or cheat sheet would help enormously:

| I want to...                  | Use this              |
|-------------------------------|-----------------------|
| Define a new behavior         | `#[contracttrait]`    |
| Write the logic               | `impl XInternal for`  |
| Wire it to my contract        | `impl_x!` or `impl X for` |
| Test auth                     | `XAuthClient`         |

That table should be in the README, the doc comments, and ideally printed by the
macro when it detects a common mistake.

## The Art of Testing

The AuthClient pattern is actually the most artist-friendly part. Compare:

```rust
// Old way (what does this even test?)
env.mock_all_auths();
client.transfer_ownership(&new_owner);

// New way (I can read this!)
auth_client.transfer_ownership(&new_owner)
    .authorize(&owner)
    .invoke();
```

The new way reads like a sentence: "Transfer ownership to new_owner, authorized
by owner, invoke." This is good. This is how artists think -- in sequences of
actions, not in mock configurations.

But the test example in `trait-test/src/lib.rs` still uses `mock_all_auths()` for
the `init()` call. This inconsistency is confusing. If the whole point is to move
away from `mock_all_auths()`, then the example should show the alternative for
every call, including init.

## Visual Documentation Matters

The blog post is text-heavy. Artists are visual. A single diagram showing:

```
[Your Trait] --> macro --> [Internal] + [Outer] + [AuthClient] + [Sealed Macro]
                              |            |           |
                          implement     auto-auth   test with
                          pure logic    enforced    .authorize()
```

...would communicate in 5 seconds what the blog post takes 5 minutes to explain.

## What I Love

1. The `#[auth(Self::owner)]` syntax is poetic. It says what it means.
2. The AuthClient fluent API is intuitive.
3. The claim of zero WASM overhead matters -- my NFTs are already near the size limit.
4. The sealed macro protecting artists from accidentally removing auth checks.

## What Worries Me

1. The vocabulary barrier will scare away 80% of artist-coders before they try it.
2. No "getting started in 30 seconds" path for the simplest use case.
3. No visual docs or interactive examples.
4. The example file has 180 lines. My entire first NFT contract was 90 lines.
   The framework should make contracts shorter, not longer.

## Verdict

This is a power tool built by engineers for engineers, with potential to be an
artist's tool if the onboarding is redesigned. The architecture is sound. The
syntax is actually readable once you learn the vocabulary. But the vocabulary
itself is the main barrier.

Make the simple case simple. Let the complex case be possible. Right now, both
cases require understanding the full system.

**Rating: 6/10 for accessibility** -- brilliant core, needs an artist-friendly
wrapper layer and visual documentation.
