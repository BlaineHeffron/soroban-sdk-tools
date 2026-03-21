# Review by Tatiana -- Ballet Choreographer

*"In ballet, every movement serves the whole. In code, every function should serve the composition."*

---

## Overall Impression

I am a principal choreographer with twenty-three years of professional ballet experience.
I have created original works for the Bolshoi, the Royal Ballet, and the Paris Opera
Ballet. For the past two years, I have been exploring smart contracts for managing dance
intellectual property -- choreography rights, performance royalties, and collaboration
agreements between dancers, composers, and production companies.

When I look at code, I see choreography. I see the corps de ballet (the helper functions)
supporting the principals (the main logic). I see pas de deux (two traits dancing
together). I see the choreographic structure that determines whether a performance feels
unified or disjointed.

This codebase is a well-choreographed piece. But it is not yet ready for opening night.

---

## The Corps de Ballet: Helper Functions

In ballet, the corps de ballet is the ensemble that provides the structural foundation
for the principals. They must be perfectly synchronized, invisible in their precision,
and absolutely reliable. The helper functions in `contract.rs` are the corps.

### `extract_auth_attr` -- The Preparation

```rust
fn extract_auth_attr(attrs: &[Attribute]) -> syn::Result<Option<AuthSource>> {
    for attr in attrs {
        if attr.path().is_ident("auth") {
            let expr: Expr = attr.parse_args()?;
            match &expr {
                Expr::Path(ep) if ep.path.segments.len() == 2 => { ... }
                Expr::Path(ep) if ep.path.segments.len() == 1 => { ... }
                _ => { return Err(...) }
            }
        }
    }
    Ok(None)
}
```

This function prepares the stage -- it reads the `#[auth]` annotation and determines the
source of authority. The pattern matching is clean, like a preparation exercise at the
barre. Two valid forms (`Self::method` and `param_name`), one rejection. Three positions,
one function.

**Choreographic assessment:** Elegant. No wasted movement.

### `extract_method_info` -- The Rehearsal

```rust
fn extract_method_info(method: &TraitItemFn) -> syn::Result<MethodInfo> { ... }
```

This function rehearses each method signature -- extracting the name, signature,
attributes, auth source, and parameters. It is a thorough rehearsal that prepares
everything the choreography needs.

But there is a blemish: the function skips the first parameter if it is `Env`, but it
uses a `first` boolean flag to track this:

```rust
let mut first = true;
for arg in &sig.inputs {
    if let FnArg::Typed(pat_type) = arg {
        if first && is_env_type(&pat_type.ty) {
            first = false;
            env_is_ref = matches!(*pat_type.ty, syn::Type::Reference(_));
            continue;
        }
        first = false;
        // ...
    }
}
```

This is like a dancer counting steps with their fingers -- it works but it is not elegant.
A more balletic approach:

```rust
let mut args_iter = sig.inputs.iter();
// Handle env parameter
if let Some(FnArg::Typed(pat_type)) = args_iter.next() {
    if is_env_type(&pat_type.ty) {
        env_is_ref = matches!(*pat_type.ty, syn::Type::Reference(_));
    } else {
        // First arg is not env -- process it as a regular param
        // (handle this edge case)
    }
}
// Process remaining parameters
for arg in args_iter { ... }
```

Using the iterator directly eliminates the boolean flag and makes the "first parameter is
special" logic explicit. In ballet, we call this **epaulement** -- the precise positioning
that makes a movement look effortless.

### `to_snake_case` -- The Pirouette

```rust
fn to_snake_case(s: &str) -> String {
    let mut result = String::new();
    for (i, c) in s.chars().enumerate() {
        if c.is_uppercase() {
            if i > 0 {
                result.push('_');
            }
            result.push(c.to_lowercase().next().unwrap());
        } else {
            result.push(c);
        }
    }
    result
}
```

A simple pirouette -- one turn, nothing fancy. It converts PascalCase to snake_case
character by character. But like a pirouette that wobbles, it does not handle consecutive
uppercase letters well. `HTTPAuth` becomes `h_t_t_p_auth` instead of `http_auth`.

For trait names, this is unlikely to be an issue (who names a trait `HTTPAuth`?). But in
choreography, even unlikely movements are rehearsed. Consider using the `heck` crate or
handling the edge case.

---

## The Pas de Deux: Two-Trait Generation

A pas de deux is the dance between two partners. In classical ballet, the structure is:
entree (introduction), adagio (slow, sustained movements together), variations (solos),
and coda (finale together).

The two-trait generation follows this structure:

### Entree: The Original Trait

```rust
#[contracttrait]
pub trait Ownable {
    fn owner(env: &Env) -> Address;
    #[auth(Self::owner)]
    fn transfer_ownership(env: &Env, new_owner: Address);
}
```

The two dancers are introduced: `owner` and `transfer_ownership`. One is a query
(no auth), one is a command (auth required). They are defined together, establishing
their relationship.

### Adagio: The Internal Trait

```rust
pub trait OwnableInternal {
    fn owner(env: &Env) -> Address;
    fn transfer_ownership(env: &Env, new_owner: Address);
}
```

The slow, sustained movement. The Internal trait strips away the auth decoration,
revealing the pure movement vocabulary. This is the adagio -- sustained, essential,
showing the line of each movement without embellishment.

### Variation: The Provider Solo

```rust
pub struct SingleOwner;
impl OwnableInternal for SingleOwner {
    fn owner(env: &Env) -> Address { ... }
    fn transfer_ownership(env: &Env, new_owner: Address) { ... }
}
```

The male variation (in classical terminology): powerful, direct, technically demanding.
The provider performs the actual work -- storage reads and writes -- without the support
of the auth framework. It dances alone.

### Coda: The Outer Trait

```rust
pub trait Ownable {
    type Provider: OwnableInternal;
    fn transfer_ownership(env: &Env, new_owner: Address) {
        let __auth_addr = Self::Provider::owner(env);
        __auth_addr.require_auth();
        Self::Provider::transfer_ownership(env, new_owner)
    }
}
```

The finale. Both dancers come together. The auth wrapper (the female variation, in
classical terms -- elegant, contextualizing) wraps around the provider's business logic,
creating the complete movement.

**Choreographic assessment:** This is a well-structured pas de deux. The partners
complement each other without redundancy. The Internal trait and Outer trait have distinct
roles that combine into a complete picture.

---

## The Grand Pas: Supertrait Composition

A grand pas includes multiple dancers in coordinated movement. The supertrait composition
pattern (`Pausable: Ownable`) is a grand pas where multiple traits dance together.

### The Choreographic Structure

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

`Pausable` references `Self::owner` from `Ownable`. This is a choreographic reference --
a moment in the Pausable choreography that quotes a movement from the Ownable
choreography. In ballet, we call this **leitmotif** (borrowing from music) -- a recurring
theme that connects different scenes.

The generated Internal trait maintains this connection:

```rust
pub trait PausableInternal: OwnableInternal { ... }
```

The supertrait constraint ensures that any dancer who performs the Pausable choreography
can also perform the Ownable choreography. You cannot dance the Pausable role without
knowing the Ownable vocabulary. This is type-level casting.

### The Single-Provider Solo

```rust
impl PausableInternal for SingleOwner { ... }
```

One dancer performs both roles. `SingleOwner` implements both `OwnableInternal` and
`PausableInternal`. This is like a dancer who performs in both Act I and Act II -- the
same technique, applied to different choreographic contexts.

**Assessment:** Elegant composition. The supertrait system prevents "miscasting" -- you
cannot assign a provider that knows only Pausable to a role that requires both Pausable
and Ownable. The type system is the casting director.

---

## Creative Rights Management: My Use Case

### The Problem

When I create a new ballet, the intellectual property is complex:

1. **Choreography IP:** The movement sequence I create. This is my primary work.
2. **Music IP:** The composition, typically created by a collaborator.
3. **Costume/Set IP:** Visual design, another collaborator.
4. **Performance Rights:** Each company that performs my work pays a royalty.
5. **Recording Rights:** Video recordings of performances have separate rights.
6. **Dancer Likeness Rights:** Individual dancers in recordings have their own rights.

A single performance involves at least six different IP claims. Currently, these are
managed through paper contracts negotiated individually. There is no standard, no
automation, and no transparency.

### How This Framework Could Help

The `#[contracttrait]` pattern maps directly to IP management:

```rust
#[contracttrait]
pub trait ChoreographyRights {
    fn choreographer(env: &Env) -> Address;
    fn collaborators(env: &Env) -> Vec<Address>;
    fn royalty_split(env: &Env) -> Vec<(Address, u32)>;  // address, basis points

    #[auth(Self::choreographer)]
    fn register_performance(env: &Env, company: Address, date: u64, venue: Symbol);

    #[auth(Self::choreographer)]
    fn update_royalty_split(env: &Env, new_split: Vec<(Address, u32)>);
}

#[contracttrait]
pub trait PerformanceRoyalties: ChoreographyRights {
    fn performance_fee(env: &Env, performance_id: u64) -> i128;

    #[auth(company)]
    fn pay_royalties(env: &Env, company: Address, performance_id: u64, amount: i128);
}

#[contracttrait]
pub trait RecordingRights: ChoreographyRights {
    fn recording_license(env: &Env, recording_id: u64) -> RecordingLicense;

    #[auth(Self::choreographer)]
    fn grant_recording_license(
        env: &Env,
        licensee: Address,
        performance_id: u64,
        terms: RecordingTerms,
    );
}
```

Three traits composing to form a complete IP management system:

1. `ChoreographyRights` -- the base (who created it, who gets paid)
2. `PerformanceRoyalties` -- extends base with live performance payments
3. `RecordingRights` -- extends base with recording licenses

The provider pattern enables different governance models:

```rust
// Solo choreographer
impl_choreography_rights!(MyBallet, SoloChoreographer);

// Collaborative creation (multiple choreographers)
impl_choreography_rights!(MyBallet, CollaborativeCreation);

// Company-commissioned (company owns, choreographer gets royalties)
impl_choreography_rights!(MyBallet, CommissionedWork);
```

### What the Framework Needs for This Use Case

#### 1. Multi-Party Auth

Currently, `#[auth]` supports one auth source. Choreography often involves multiple
parties who must all agree:

```rust
// Both choreographer AND music composer must approve a recording license
#[auth(Self::choreographer, Self::composer)]
fn grant_recording_license(env: &Env, ...);
```

The macro would need to generate:

```rust
fn grant_recording_license(env: &Env, ...) {
    Self::Provider::choreographer(env).require_auth();
    Self::Provider::composer(env).require_auth();
    Self::Provider::grant_recording_license(env, ...)
}
```

#### 2. Royalty Distribution

The current framework handles auth but not payment distribution. A `#[royalty]` attribute
could automate royalty splits:

```rust
#[auth(company)]
#[royalty(Self::royalty_split)]
fn pay_royalties(env: &Env, company: Address, performance_id: u64, amount: i128);
```

The generated code would split the payment according to the royalty schedule before
executing the method.

#### 3. Time-Limited Licenses

Performance and recording licenses have expiration dates. The provider pattern supports
this, but the trait interface does not make it explicit:

```rust
fn grant_recording_license(
    env: &Env,
    licensee: Address,
    performance_id: u64,
    terms: RecordingTerms,
    expires_at: u64,  // ledger sequence number
);
```

A `#[expires]` attribute could generate automatic expiration checks:

```rust
#[auth(Self::choreographer)]
#[expires(expires_at)]
fn grant_recording_license(env: &Env, ..., expires_at: u64);
```

#### 4. Collaboration Contracts

The most complex choreographic relationships involve multiple parties with different roles:

- Choreographer: creates the movement
- Composer: creates the music
- Dancers: perform the work
- Company: produces and markets
- Venue: provides the space

Each has different rights, different payment structures, and different approval
requirements. A full collaboration contract would compose multiple traits:

```rust
pub trait CollaborationContract:
    ChoreographyRights +
    PerformanceRoyalties +
    RecordingRights +
    DancerLikenessRights +
    VenueAgreement
{
    // Methods specific to the overall collaboration
}
```

The diamond problem becomes relevant here: if multiple traits share the
`ChoreographyRights` supertrait, the sealed macro must handle deduplication.

---

## The Rehearsal Notes: Testing

### What Is Tested

The four tests demonstrate basic choreographic vocabulary:

1. Ownership transfer -- a simple movement (bourree)
2. Pause/unpause -- a preparation and recovery (plie/releve)
3. Auth client for ownership -- a supported movement (partnered turn)
4. Auth client for pause -- another supported movement

### What Is Not Tested

In ballet, we rehearse not just the movements but the transitions. What happens between
steps matters as much as the steps themselves. Missing tests:

1. **Transition from uninitialized to initialized:** What happens if you dance before the
   music starts?

2. **Failed auth:** What happens when the wrong dancer tries to lead? The test suite
   proves that the right dancer CAN lead, but not that the wrong dancer CANNOT.

3. **Composition under stress:** What happens when Ownable and Pausable interact? Can you
   transfer ownership while paused? Can you pause during a transfer?

4. **The curtain call:** What happens at the end? Is there a clean shutdown? A
   `renounce_ownership` equivalent?

### The `mock_all_auths` Problem

Two of four tests use `mock_all_auths()`. This is the choreographic equivalent of
marking through a piece instead of dancing full-out. You go through the positions but
you skip the transitions, the dynamics, the artistry. You know where to stand, but you do
not know if you can actually get there.

The blog post rightly criticizes this approach:

> "Testing authorization in Soroban currently means either mock_all_auths() (which tests
> nothing)"

Yet the example uses it. This is like a choreographer saying "always dance full-out in
rehearsal" and then marking through their own demonstration. Lead by example. Replace the
`mock_all_auths` tests with AuthClient tests.

---

## The Program Notes: Documentation

### The Blog Post as Program Notes

Program notes for a ballet explain the choreographer's intent, the historical context,
and the narrative arc. The blog post serves this purpose well:

1. **Context:** The history of contract composability (OZ, Solidity, EVM)
2. **Intent:** Structural auth, provider DI, sealed patterns
3. **Narrative:** From problem to solution to invitation

**Assessment:** Well-written program notes. The audience (developers) understands the
choreographic intent. The comparison with OZ provides the historical context. The
invitation to collaborate provides the forward-looking narrative.

### The Missing Notation

In ballet, choreographic notation (Benesh notation or Labanotation) provides a precise,
visual record of every movement. Code does not have an equivalent -- but it could.

The `#[contracttrait]` macro transforms one definition into multiple artifacts. The
transformation itself is not documented visually. A "notation" showing the transformation
would help:

```
INPUT:                          OUTPUT:
                               +-------------------+
#[contracttrait]               | OwnableInternal   |
pub trait Ownable {            | (pure logic)      |
  fn owner(...)                +-------------------+
  #[auth(Self::owner)]    =>          |
  fn transfer_ownership(...)   +-------------------+
}                              | Ownable           |
                               | (auth-wrapped)    |
                               +-------------------+
                                      |
                               +-------------------+
                               | OwnableAuthClient |
                               | (test helper)     |
                               +-------------------+
                                      |
                               +-------------------+
                               | impl_ownable!     |
                               | (sealed macro)    |
                               +-------------------+
```

This visual notation should be in the documentation, the doc comments, and ideally
generated by the macro itself.

---

## Recommendations for the Final Rehearsal

### 1. Solve the Diamond Problem

For composition of multiple traits sharing a supertrait (essential for collaboration
contracts), the sealed macro must handle method deduplication. This is the most critical
choreographic challenge.

### 2. Add Multi-Party Auth

`#[auth(Self::choreographer, Self::composer)]` for methods requiring multiple approvals.
This unlocks collaboration contracts.

### 3. Replace mock_all_auths in Examples

Lead by example. Every test should use the AuthClient. The example is the first
rehearsal a new developer sees -- make it full-out, not marked.

### 4. Add Visual Documentation

Transformation diagrams, composition diagrams, and supertrait relationship diagrams.
Show the choreography, do not just describe it.

### 5. Generate Events

Every `#[auth]` method should emit an event. The audience (block explorers, front-ends,
analytics) needs to see the performance, not just the final position.

### 6. Trim Unused Code

Remove `env_is_ref` and `_is_ref`. DRY the clone generation. Clean the studio before
opening night.

---

## Closing: The Opening Night

This framework is in final rehearsals. The choreography is strong -- the two-trait pas
de deux, the supertrait grand pas, the sealed coda. The technique is clean -- well-
organized code, consistent naming, clear structure.

But opening night demands perfection. The unused code is a costume wrinkle. The missing
events are a lighting cue that was not programmed. The mock_all_auths tests are a dancer
who is still marking.

Fix these before the curtain rises, and this will be a piece that the Soroban ecosystem
remembers.

**Rating: 4 curtain calls out of 5.**

---

## Files Reviewed

| File | Choreographic Assessment |
|------|--------------------------|
| `docs/oz-comparison.md` | Well-structured competitive analysis; clear choreographic intent |
| `docs/blog-post-composable-contracts.md` | Excellent program notes; complete narrative arc |
| `examples/trait-test/src/lib.rs` | Basic vocabulary demonstrated; needs transitions and failures |
| `soroban-sdk-tools-macro/src/contract.rs` | Strong technique; trim and polish for opening night |

---

*Tatiana Volkov. Principal Choreographer. Bolshoi Ballet, Royal Ballet, Paris Opera Ballet.
"A perfect performance is not one where nothing can be added, but one where nothing can
be removed."*
