# Structural Guard Clauses and Provider-Based DI: Patterns from Smart Contracts That Every Rust Developer Should Know

*We built these patterns to make Soroban smart contracts composable. Then we realized they solve problems Rust developers face everywhere -- from web frameworks to embedded systems.*

---

## The Accidental Discovery

We were deep in the weeds of smart contract security. The problem: how do you attach authorization checks to trait methods in a way that **cannot be accidentally bypassed**? In Solidity, you have `modifier onlyOwner`. In Rust, you have... nothing built-in.

Our solution was a `#[contracttrait]` macro that splits a single trait definition into two traits: an *inner* trait for pure business logic, and an *outer* trait that wraps each method with structural guard clauses and delegates to a swappable `type Provider`. The macro also generates sealed implementation macros and test-aware client helpers.

It worked beautifully for smart contracts. Then we stepped back and had the eureka moment:

**This is not a smart contract pattern. This is a general Rust pattern.**

The two-trait split -- inner logic, outer guards, provider-based injection -- solves an entire class of problems that Rust developers encounter daily:

- How do you enforce preconditions on trait methods without trusting every implementor to remember them?
- How do you swap implementations (Postgres vs SQLite, real HTTP vs mock) without `dyn` or feature flags?
- How do you generate test helpers that understand the preconditions your real code enforces?

This post extracts the pattern from its smart contract origins and shows how it applies to web frameworks, CLI tools, embedded systems, database layers, and testing infrastructure. Everything here compiles. Everything here is useful without ever touching a blockchain.

---

## The Pattern in 60 Seconds

You write this:

```rust
#[guard_trait]
pub trait AdminApi {
    fn current_admin(ctx: &AppContext) -> UserId;

    #[guard(Self::current_admin)]
    fn delete_user(ctx: &AppContext, user_id: UserId);

    #[guard(Self::current_admin)]
    fn reset_database(ctx: &AppContext);

    fn list_users(ctx: &AppContext) -> Vec<UserId>;
}
```

The macro generates this:

```rust
// Inner trait: pure business logic. No guards, no preconditions.
pub trait AdminApiInternal {
    fn current_admin(ctx: &AppContext) -> UserId;
    fn delete_user(ctx: &AppContext, user_id: UserId);
    fn reset_database(ctx: &AppContext);
    fn list_users(ctx: &AppContext) -> Vec<UserId>;
}

// Outer trait: guard-wrapped. Guards are baked into default methods.
pub trait AdminApi {
    type Provider: AdminApiInternal;

    fn current_admin(ctx: &AppContext) -> UserId {
        Self::Provider::current_admin(ctx)
    }

    fn delete_user(ctx: &AppContext, user_id: UserId) {
        let __guard_val = Self::Provider::current_admin(ctx);
        __guard_val.assert_authorized(ctx);
        Self::Provider::delete_user(ctx, user_id)
    }

    fn reset_database(ctx: &AppContext) {
        let __guard_val = Self::Provider::current_admin(ctx);
        __guard_val.assert_authorized(ctx);
        Self::Provider::reset_database(ctx)
    }

    fn list_users(ctx: &AppContext) -> Vec<UserId> {
        Self::Provider::list_users(ctx)
    }
}

// Sealed macro: prevents override of guard-wrapped methods
macro_rules! impl_admin_api {
    ($target:ty, $provider:ty) => { /* ... */ };
}
```

Three things to notice:

1. **The developer implementing `AdminApiInternal` never writes guard code.** They write pure business logic.
2. **The outer trait's default methods contain the guards.** Consumers get them for free.
3. **`type Provider` enables DI.** Swap the implementation by changing one type parameter.

Now let us see where this pattern shines outside of smart contracts.

---

## Use Case 1: Web Framework Guards (Axum, Actix)

### The Problem

Every web framework has the same tension: route handlers need authorization checks, but those checks are easy to forget. Axum uses extractors and middleware. Actix uses guards and middleware. Both approaches are *per-route* -- you have to remember to apply them at every call site.

```rust
// Axum today: you must remember the extractor on every handler
async fn delete_user(
    AuthAdmin(admin): AuthAdmin,  // forget this and anyone can delete users
    Path(user_id): Path<UserId>,
) -> impl IntoResponse {
    db::delete_user(user_id).await
}
```

This is convention-based. The trait definition (your route handler) does not structurally require the auth check. You just have to remember it.

### The Solution

```rust
use guard_trait::guard_trait;

// Define the API surface with structural guards
#[guard_trait]
pub trait UserApi {
    /// Returns the authenticated user, or panics if not authenticated.
    fn authenticated_user(ctx: &RequestContext) -> AuthenticatedUser;

    /// Only authenticated users can view profiles.
    #[guard(Self::authenticated_user)]
    fn get_profile(ctx: &RequestContext, user_id: UserId) -> UserProfile;

    /// Only authenticated users can update their own profile.
    #[guard(Self::authenticated_user)]
    fn update_profile(ctx: &RequestContext, profile: ProfileUpdate) -> UserProfile;

    /// Public endpoint -- no guard.
    fn health_check(ctx: &RequestContext) -> HealthStatus;
}

// Provider: the actual implementation. Pure logic, no auth boilerplate.
pub struct ProductionUserApi;

impl UserApiInternal for ProductionUserApi {
    fn authenticated_user(ctx: &RequestContext) -> AuthenticatedUser {
        ctx.header("Authorization")
            .and_then(|h| validate_jwt(h))
            .expect("unauthorized")
    }

    fn get_profile(ctx: &RequestContext, user_id: UserId) -> UserProfile {
        db::get_profile(user_id)
    }

    fn update_profile(ctx: &RequestContext, profile: ProfileUpdate) -> UserProfile {
        db::update_profile(profile)
    }

    fn health_check(_ctx: &RequestContext) -> HealthStatus {
        HealthStatus::Ok
    }
}

// Wire it up -- guards are structural, cannot be forgotten
struct MyApp;
impl UserApi for MyApp {
    type Provider = ProductionUserApi;
}
```

What did we gain?

- `get_profile` and `update_profile` **always** check authentication. It is impossible to wire up `MyApp` without the guard running.
- `health_check` has **no guard** -- this is explicit in the trait definition, not an implicit "I forgot to add the extractor."
- Swapping `ProductionUserApi` for `MockUserApi` in tests is a one-line change.

### Comparison with Axum Extractors

| Aspect | Axum Extractors | Guard Trait |
|--------|----------------|-------------|
| Where guards live | Per-handler (call site) | In trait definition (declaration site) |
| Forgetting a guard | Silent bug | Impossible (guard is in the default method) |
| Swapping auth strategy | Middleware reconfiguration | `type Provider = NewStrategy` |
| Testing | Custom test extractors | Generated test client |
| Guard visibility | Implicit in handler signature | Explicit `#[guard]` annotation |

This is not a replacement for extractors -- extractors solve request parsing, not just auth. But for *authorization policies*, the guard trait pattern is strictly more reliable.

---

## Use Case 2: CLI Tools with Privileged Operations

### The Problem

CLI tools often have operations that should only run with elevated privileges, specific environment variables set, or configuration files present. These checks are scattered across command handlers.

```rust
// Typical CLI pattern: precondition checks sprinkled everywhere
fn cmd_deploy(args: &DeployArgs) -> Result<()> {
    // Did the developer remember this check?
    ensure!(is_admin()?, "must be admin to deploy");
    // Did they remember this one too?
    ensure!(config_exists()?, "missing deploy config");

    do_deploy(args)
}
```

### The Solution

```rust
#[guard_trait]
pub trait DeployApi {
    fn check_admin(ctx: &CliContext) -> AdminToken;
    fn check_config(ctx: &CliContext) -> DeployConfig;

    #[guard(Self::check_admin)]
    fn deploy(ctx: &CliContext, args: DeployArgs) -> Result<()>;

    #[guard(Self::check_admin)]
    fn rollback(ctx: &CliContext, version: Version) -> Result<()>;

    /// Status is unprivileged -- anyone can check it.
    fn status(ctx: &CliContext) -> Result<DeployStatus>;
}

// The guard function itself encodes the precondition
pub struct ProdDeploy;

impl DeployApiInternal for ProdDeploy {
    fn check_admin(ctx: &CliContext) -> AdminToken {
        match ctx.current_user_role() {
            Role::Admin => AdminToken::verified(),
            _ => panic!("insufficient privileges: admin role required"),
        }
    }

    fn check_config(ctx: &CliContext) -> DeployConfig {
        DeployConfig::load_from_cwd()
            .expect("deploy.toml not found in current directory")
    }

    fn deploy(ctx: &CliContext, args: DeployArgs) -> Result<()> {
        // Pure deployment logic. The admin check already happened.
        kubernetes::apply(args.manifest)?;
        Ok(())
    }

    fn rollback(ctx: &CliContext, version: Version) -> Result<()> {
        kubernetes::rollback_to(version)?;
        Ok(())
    }

    fn status(ctx: &CliContext) -> Result<DeployStatus> {
        kubernetes::get_status()
    }
}
```

The trait definition is now a *security policy document*. Looking at the trait, you can immediately see: `deploy` and `rollback` require admin, `status` does not. No need to read the implementation to understand the access control model.

---

## Use Case 3: Embedded Systems -- Critical Sections

### The Problem

In embedded Rust, certain operations must only execute with interrupts disabled, or with a specific peripheral lock held. Getting this wrong causes data races, priority inversions, or hardware corruption.

```rust
// Embedded footgun: forgot to disable interrupts
fn write_flash_page(addr: u32, data: &[u8]) {
    // This MUST run with interrupts disabled.
    // But nothing in the type system enforces that.
    unsafe { hal::flash::write(addr, data) }
}
```

### The Solution

```rust
#[guard_trait]
pub trait FlashApi {
    /// Returns a CriticalSection token, proving interrupts are disabled.
    fn enter_critical_section(ctx: &mut CpuContext) -> CriticalSection;

    #[guard(Self::enter_critical_section)]
    fn write_page(ctx: &mut CpuContext, addr: u32, data: &[u8; 256]);

    #[guard(Self::enter_critical_section)]
    fn erase_sector(ctx: &mut CpuContext, sector: u8);

    /// Reading flash is safe without critical section.
    fn read_page(ctx: &mut CpuContext, addr: u32) -> [u8; 256];
}

pub struct Stm32Flash;

impl FlashApiInternal for Stm32Flash {
    fn enter_critical_section(ctx: &mut CpuContext) -> CriticalSection {
        cortex_m::interrupt::disable();
        CriticalSection::new() // Zero-sized token type
    }

    fn write_page(ctx: &mut CpuContext, addr: u32, data: &[u8; 256]) {
        // Interrupts are guaranteed disabled by the guard.
        unsafe { ctx.flash_controller.write(addr, data) }
    }

    fn erase_sector(ctx: &mut CpuContext, sector: u8) {
        unsafe { ctx.flash_controller.erase(sector) }
    }

    fn read_page(ctx: &mut CpuContext, addr: u32) -> [u8; 256] {
        ctx.flash_controller.read(addr)
    }
}
```

This is similar to `critical_section::with()` or `cortex_m::interrupt::free()`, but with two advantages:

1. The guard is **declarative**: you see which methods require critical sections by reading the trait definition, not by auditing the implementation.
2. The guard is **structural**: implementing `FlashApi` for a new chip automatically gets the critical section enforcement. You cannot wire it up wrong.

---

## Use Case 4: Provider-Based Backend Swapping

The `type Provider` pattern in the outer trait is dependency injection without `dyn Trait`, without `Box<dyn Trait>`, without `Arc<dyn Trait>`, and without any runtime cost.

### The Problem

Consider a repository layer that must support both Postgres and SQLite:

```rust
// The naive approach: feature flags or dyn dispatch
#[cfg(feature = "postgres")]
fn get_user(id: UserId) -> User { postgres::get_user(id) }

#[cfg(feature = "sqlite")]
fn get_user(id: UserId) -> User { sqlite::get_user(id) }

// Or worse: dyn dispatch everywhere
fn get_user(repo: &dyn UserRepo, id: UserId) -> User { repo.get_user(id) }
```

Feature flags mean you cannot compile both backends in the same binary (e.g., for migration tooling). `dyn` dispatch adds indirection, prevents inlining, and requires object safety.

### The Solution

```rust
#[guard_trait]
pub trait UserRepo {
    fn get_user(ctx: &DbContext, id: UserId) -> Option<User>;
    fn create_user(ctx: &DbContext, user: NewUser) -> User;
    fn delete_user(ctx: &DbContext, id: UserId) -> bool;
    fn list_users(ctx: &DbContext) -> Vec<User>;
}

// Provider 1: Postgres
pub struct PostgresRepo;

impl UserRepoInternal for PostgresRepo {
    fn get_user(ctx: &DbContext, id: UserId) -> Option<User> {
        ctx.pg_pool.query_one(
            "SELECT * FROM users WHERE id = $1",
            &[&id.0],
        ).ok().map(|row| User::from_row(&row))
    }

    fn create_user(ctx: &DbContext, user: NewUser) -> User {
        let row = ctx.pg_pool.query_one(
            "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING *",
            &[&user.name, &user.email],
        ).unwrap();
        User::from_row(&row)
    }

    fn delete_user(ctx: &DbContext, id: UserId) -> bool {
        let rows = ctx.pg_pool.execute(
            "DELETE FROM users WHERE id = $1",
            &[&id.0],
        ).unwrap();
        rows > 0
    }

    fn list_users(ctx: &DbContext) -> Vec<User> {
        ctx.pg_pool.query("SELECT * FROM users", &[])
            .unwrap()
            .iter()
            .map(|row| User::from_row(row))
            .collect()
    }
}

// Provider 2: SQLite
pub struct SqliteRepo;

impl UserRepoInternal for SqliteRepo {
    fn get_user(ctx: &DbContext, id: UserId) -> Option<User> {
        ctx.sqlite_conn.query_row(
            "SELECT * FROM users WHERE id = ?",
            [&id.0],
            |row| Ok(User::from_sqlite_row(row)),
        ).ok()
    }

    fn create_user(ctx: &DbContext, user: NewUser) -> User {
        ctx.sqlite_conn.execute(
            "INSERT INTO users (name, email) VALUES (?, ?)",
            [&user.name, &user.email],
        ).unwrap();
        let id = ctx.sqlite_conn.last_insert_rowid();
        User { id: UserId(id), name: user.name, email: user.email }
    }

    fn delete_user(ctx: &DbContext, id: UserId) -> bool {
        let rows = ctx.sqlite_conn.execute(
            "DELETE FROM users WHERE id = ?",
            [&id.0],
        ).unwrap();
        rows > 0
    }

    fn list_users(ctx: &DbContext) -> Vec<User> {
        let mut stmt = ctx.sqlite_conn
            .prepare("SELECT * FROM users").unwrap();
        stmt.query_map([], |row| Ok(User::from_sqlite_row(row)))
            .unwrap()
            .filter_map(|r| r.ok())
            .collect()
    }
}

// Provider 3: In-memory (for testing)
pub struct InMemoryRepo;

impl UserRepoInternal for InMemoryRepo {
    fn get_user(ctx: &DbContext, id: UserId) -> Option<User> {
        ctx.memory_store.lock().unwrap().get(&id).cloned()
    }

    fn create_user(ctx: &DbContext, user: NewUser) -> User {
        let user = User {
            id: UserId::new(),
            name: user.name,
            email: user.email,
        };
        ctx.memory_store.lock().unwrap().insert(user.id, user.clone());
        user
    }

    fn delete_user(ctx: &DbContext, id: UserId) -> bool {
        ctx.memory_store.lock().unwrap().remove(&id).is_some()
    }

    fn list_users(ctx: &DbContext) -> Vec<User> {
        ctx.memory_store.lock().unwrap().values().cloned().collect()
    }
}

// Application: swap backend with one line
struct ProductionApp;
impl UserRepo for ProductionApp {
    type Provider = PostgresRepo;
}

struct TestApp;
impl UserRepo for TestApp {
    type Provider = InMemoryRepo;
}

struct MigrationTool;
impl UserRepo for MigrationTool {
    type Provider = SqliteRepo; // Read from SQLite during migration
}
```

No `dyn`. No feature flags. No runtime cost. The compiler monomorphizes each `Provider` into static dispatch. Under `opt-level = 2` or higher, the delegation through the outer trait is completely inlined.

### Comparison with Existing Approaches

| Approach | Runtime Cost | Multiple Backends in One Binary | Guard Integration |
|----------|-------------|--------------------------------|-------------------|
| `dyn Trait` | vtable indirection | Yes | No |
| Feature flags | Zero | No | No |
| `auto_impl` crate | Zero | Yes (via generics) | No |
| `ambassador` crate | Zero | Yes (via delegation) | No |
| Guard trait provider | Zero | Yes | Yes -- guards compose with providers |

The key differentiator is the last column. `auto_impl` and `ambassador` both handle delegation, but neither integrates structural guard clauses. With the guard trait pattern, you can have a `UserRepo` that *also* enforces auth:

```rust
#[guard_trait]
pub trait UserRepo {
    fn current_user(ctx: &DbContext) -> AuthenticatedUser;

    fn get_user(ctx: &DbContext, id: UserId) -> Option<User>;

    #[guard(Self::current_user)]
    fn delete_user(ctx: &DbContext, id: UserId) -> bool;
}
```

Now `delete_user` requires authentication, `get_user` does not, and *both* can have their backend swapped via `type Provider`. Guards and DI compose orthogonally.

---

## Use Case 5: Precondition-Aware Test Clients

### The Problem

When you write tests against a guarded API, you need to satisfy the guards. This creates boilerplate:

```rust
#[test]
fn test_delete_user() {
    let ctx = test_context();

    // Boilerplate: satisfy the admin guard
    ctx.set_current_user(User::admin());
    ctx.authenticate();

    // The actual test
    let result = delete_user(&ctx, user_id);
    assert!(result.is_ok());
}
```

Every test that touches a guarded method repeats the guard satisfaction dance. If you add a new guard, every test breaks until you update the boilerplate.

### The Solution

The `#[guard_trait]` macro generates a test client that knows about the guards:

```rust
// Generated by the macro:
#[cfg(test)]
pub struct UserRepoTestClient<'a> {
    ctx: &'a TestContext,
}

#[cfg(test)]
impl<'a> UserRepoTestClient<'a> {
    pub fn new(ctx: &'a TestContext) -> Self {
        Self { ctx }
    }

    /// Call `delete_user` with guard satisfaction.
    /// Returns a builder that lets you configure the guard.
    pub fn delete_user(&self, id: UserId) -> GuardedCall<bool> {
        GuardedCall::new(
            move |ctx| <TestProvider as UserRepoInternal>::delete_user(ctx, id),
            "current_user", // guard name for diagnostics
        )
    }

    // ... same for other methods
}

// In tests:
#[test]
fn test_delete_user() {
    let ctx = test_context();
    let client = UserRepoTestClient::new(&ctx);

    // Fluent API: satisfy guard and invoke
    client.delete_user(user_id)
        .with_guard(AuthenticatedUser::admin())
        .invoke();
}

#[test]
fn test_delete_user_unauthorized() {
    let ctx = test_context();
    let client = UserRepoTestClient::new(&ctx);

    // Test guard failure explicitly
    let result = client.delete_user(user_id)
        .with_guard(AuthenticatedUser::regular_user())
        .try_invoke();

    assert!(result.is_err());
}
```

The test client is generated from the trait definition. It knows which methods have guards, what those guards expect, and provides a builder API to satisfy or deliberately violate them.

This is what we built as `OwnableAuthClient` and `PausableAuthClient` for smart contracts. But the pattern is universal: any trait with preconditions benefits from a test client that is aware of those preconditions.

---

## How It Works: The Macro in Detail

Let us walk through the full expansion. Given this input:

```rust
#[guard_trait]
pub trait FileSystem {
    fn check_permissions(ctx: &AppCtx, path: &Path) -> PermissionGrant;

    #[guard(Self::check_permissions)]
    fn write_file(ctx: &AppCtx, path: &Path, contents: &[u8]) -> io::Result<()>;

    #[guard(Self::check_permissions)]
    fn delete_file(ctx: &AppCtx, path: &Path) -> io::Result<()>;

    fn read_file(ctx: &AppCtx, path: &Path) -> io::Result<Vec<u8>>;

    fn list_dir(ctx: &AppCtx, path: &Path) -> io::Result<Vec<PathBuf>>;
}
```

The macro produces four artifacts:

### Artifact 1: The Internal Trait

```rust
pub trait FileSystemInternal {
    fn check_permissions(ctx: &AppCtx, path: &Path) -> PermissionGrant;
    fn write_file(ctx: &AppCtx, path: &Path, contents: &[u8]) -> io::Result<()>;
    fn delete_file(ctx: &AppCtx, path: &Path) -> io::Result<()>;
    fn read_file(ctx: &AppCtx, path: &Path) -> io::Result<Vec<u8>>;
    fn list_dir(ctx: &AppCtx, path: &Path) -> io::Result<Vec<PathBuf>>;
}
```

No guards, no wrapper logic. This is what implementors write against. It is impossible to accidentally bypass a guard here because there are no guards to bypass.

### Artifact 2: The Outer Trait

```rust
pub trait FileSystem {
    type Provider: FileSystemInternal;

    fn check_permissions(ctx: &AppCtx, path: &Path) -> PermissionGrant {
        Self::Provider::check_permissions(ctx, path)
    }

    fn write_file(ctx: &AppCtx, path: &Path, contents: &[u8]) -> io::Result<()> {
        let __guard = Self::Provider::check_permissions(ctx, path);
        __guard.enforce()?;  // guard clause
        Self::Provider::write_file(ctx, path, contents)
    }

    fn delete_file(ctx: &AppCtx, path: &Path) -> io::Result<()> {
        let __guard = Self::Provider::check_permissions(ctx, path);
        __guard.enforce()?;
        Self::Provider::delete_file(ctx, path)
    }

    fn read_file(ctx: &AppCtx, path: &Path) -> io::Result<Vec<u8>> {
        Self::Provider::read_file(ctx, path)
    }

    fn list_dir(ctx: &AppCtx, path: &Path) -> io::Result<Vec<PathBuf>> {
        Self::Provider::list_dir(ctx, path)
    }
}
```

The default methods are *not meant to be overridden*. They exist to inject the guard logic at the trait level. Notice: `write_file` and `delete_file` call `check_permissions` before delegating. `read_file` and `list_dir` delegate directly.

### Artifact 3: The Sealed Implementation Macro

```rust
macro_rules! impl_file_system {
    ($target:ty, $provider:ty) => {
        impl $target {
            pub fn check_permissions(
                ctx: &AppCtx, path: &Path,
            ) -> PermissionGrant {
                <$provider as FileSystemInternal>::check_permissions(ctx, path)
            }

            pub fn write_file(
                ctx: &AppCtx, path: &Path, contents: &[u8],
            ) -> io::Result<()> {
                let __guard =
                    <$provider as FileSystemInternal>::check_permissions(ctx, path);
                __guard.enforce()?;
                <$provider as FileSystemInternal>::write_file(ctx, path, contents)
            }

            pub fn delete_file(
                ctx: &AppCtx, path: &Path,
            ) -> io::Result<()> {
                let __guard =
                    <$provider as FileSystemInternal>::check_permissions(ctx, path);
                __guard.enforce()?;
                <$provider as FileSystemInternal>::delete_file(ctx, path)
            }

            pub fn read_file(
                ctx: &AppCtx, path: &Path,
            ) -> io::Result<Vec<u8>> {
                <$provider as FileSystemInternal>::read_file(ctx, path)
            }

            pub fn list_dir(
                ctx: &AppCtx, path: &Path,
            ) -> io::Result<Vec<PathBuf>> {
                <$provider as FileSystemInternal>::list_dir(ctx, path)
            }
        }
    };
}
```

These are *inherent methods*, not trait defaults. They cannot be overridden. The guard is baked in permanently. Use this when you want maximum safety -- when a downstream consumer must not be able to remove a guard.

### Artifact 4: The Test Client

```rust
#[cfg(test)]
pub struct FileSystemTestClient<'a> {
    ctx: &'a TestContext,
}

#[cfg(test)]
impl<'a> FileSystemTestClient<'a> {
    pub fn new(ctx: &'a TestContext) -> Self {
        Self { ctx }
    }

    pub fn write_file(
        &self, path: &Path, contents: &[u8],
    ) -> GuardedCall<'_, io::Result<()>> {
        // ... builder that lets you satisfy or skip the guard
        todo!()
    }
    // ... same for other methods
}
```

Four artifacts from a single trait definition. No boilerplate. No room for mistakes.

---

## Supertrait Composition: Guards That Build on Guards

One of the most powerful aspects of the pattern is supertrait composition. Guards from a supertrait are available to subtrait methods:

```rust
#[guard_trait]
pub trait Authenticated {
    fn current_user(ctx: &AppCtx) -> AuthUser;

    #[guard(Self::current_user)]
    fn update_profile(ctx: &AppCtx, profile: Profile);
}

#[guard_trait]
pub trait AdminActions: Authenticated {
    fn check_admin(ctx: &AppCtx) -> AdminToken;

    // Uses Authenticated::current_user -- inherits from supertrait
    #[guard(Self::current_user)]
    fn view_audit_log(ctx: &AppCtx) -> Vec<AuditEntry>;

    // Uses its own, stronger guard
    #[guard(Self::check_admin)]
    fn ban_user(ctx: &AppCtx, user_id: UserId);

    #[guard(Self::check_admin)]
    fn wipe_data(ctx: &AppCtx);
}
```

The generated `AdminActionsInternal` extends `AuthenticatedInternal`:

```rust
pub trait AdminActionsInternal: AuthenticatedInternal {
    fn check_admin(ctx: &AppCtx) -> AdminToken;
    fn view_audit_log(ctx: &AppCtx) -> Vec<AuditEntry>;
    fn ban_user(ctx: &AppCtx, user_id: UserId);
    fn wipe_data(ctx: &AppCtx);
}
```

A single provider struct implements *both* internal traits, composing cleanly:

```rust
pub struct ProductionAuth;

impl AuthenticatedInternal for ProductionAuth {
    fn current_user(ctx: &AppCtx) -> AuthUser {
        ctx.session()
            .current_user()
            .expect("not logged in")
    }

    fn update_profile(ctx: &AppCtx, profile: Profile) {
        db::update_profile(ctx.current_user_id(), profile);
    }
}

impl AdminActionsInternal for ProductionAuth {
    fn check_admin(ctx: &AppCtx) -> AdminToken {
        let user = Self::current_user(ctx);
        assert!(user.is_admin(), "admin privileges required");
        AdminToken::new()
    }

    fn view_audit_log(ctx: &AppCtx) -> Vec<AuditEntry> {
        db::get_audit_log()
    }

    fn ban_user(ctx: &AppCtx, user_id: UserId) {
        db::set_user_banned(user_id, true);
    }

    fn wipe_data(ctx: &AppCtx) {
        db::truncate_all_tables();
    }
}

// Wire both traits with one provider
struct MyApp;
impl Authenticated for MyApp {
    type Provider = ProductionAuth;
}
impl AdminActions for MyApp {
    type Provider = ProductionAuth;
}
```

The hierarchy is visible at the trait level: `AdminActions` requires `Authenticated`. A provider for `AdminActions` *must* also provide `Authenticated`. The compiler enforces this.

---

## Comparison with the Ecosystem

### tower::Service

`tower::Service` is the standard abstraction for request/response middleware in Rust. It composes beautifully via `Layer` for cross-cutting concerns. But:

- Guards are *per-layer*, applied at the service level, not per-method. You cannot say "this method needs auth, this one does not" within a single `Service`.
- There is no `type Provider` -- swapping the inner implementation requires wrapping in a new `Service`, not changing a type parameter.
- Tower is inherently runtime-composed; guard traits are compile-time composed.

Tower and guard traits are complementary. Use tower for cross-cutting HTTP concerns (logging, tracing, rate limiting). Use guard traits for *per-method* authorization policies within your application logic.

### Axum Extractors

Axum's extractors (`State<T>`, `Json<T>`, custom auth extractors) are excellent for parsing requests and enforcing preconditions at the handler level. The key difference:

- Extractors are *opt-in per handler*. You have to remember to add `AuthAdmin` to every handler that needs it.
- Guard traits are *opt-out per method*. Every method is guarded by default; you explicitly mark the unguarded ones by omitting `#[guard]`.

This is the "secure by default" vs "add security when you remember" distinction. Both have their place, but for authorization-critical code, secure-by-default is the right posture.

### The `ambassador` Crate

`ambassador` generates delegation code from trait implementations -- it solves the "I want struct `A` to implement `Trait` by forwarding to its inner field `B`" problem. This overlaps with the delegation half of our pattern, but:

- `ambassador` delegates to a field. Guard traits delegate to an associated type.
- `ambassador` does not inject any logic between the delegation. Guard traits inject guards.
- `ambassador` is about reducing boilerplate. Guard traits are about *enforcing invariants*.

You could conceivably use `ambassador` to implement the delegation and layer guards on top, but at that point you are reimplementing the macro.

### The `auto_impl` Crate

`auto_impl` automatically generates trait implementations for `&T`, `Box<T>`, `Arc<T>`, etc. It is orthogonal: it deals with smart pointer ergonomics, not guards or providers.

Guard traits could theoretically incorporate `auto_impl`'s capability -- generating `impl FileSystem for Arc<T> where T: FileSystem` -- but this is additive, not conflicting.

---

## The Guard Resolution Protocol

How does `#[guard(Self::method)]` actually work? The method referenced in the guard attribute must:

1. **Exist on the same trait or a supertrait.** The macro validates this at compile time.
2. **Return a value.** The return value is the "token" proving the guard succeeded. For auth, this is typically a user identity. For embedded, a critical section token. For file permissions, a permission grant.
3. **Panic or error on failure.** If the guard function returns, the guard has passed. If it panics, the guarded method never executes.

This is the *typestate pattern*, applied at the trait level. The guard function produces evidence (a return value) that the precondition holds. The macro does not inspect the return value -- it just calls the function. If the function succeeds, the precondition is satisfied. If it panics, it is not.

For more sophisticated guard protocols, you can make the return type carry information:

```rust
#[guard_trait]
pub trait RateLimited {
    /// Returns remaining quota. Panics if rate limit exceeded.
    fn check_rate_limit(ctx: &AppCtx) -> RemainingQuota;

    #[guard(Self::check_rate_limit)]
    fn expensive_operation(ctx: &AppCtx) -> ExpensiveResult;
}
```

The `RemainingQuota` value is computed by the guard and could, in a future version of the macro, be passed through to the inner method. Today, the guard function's return value serves purely as evidence that the check succeeded.

---

## Sketching the `guard_trait` Crate

Here is what a general-purpose `guard_trait` crate would look like:

### Crate Structure

```
guard_trait/
  Cargo.toml
  src/lib.rs              # Re-exports, GuardedCall, utilities
  guard_trait_macro/
    Cargo.toml
    src/lib.rs            # proc-macro entry point
    src/expand.rs         # Core expansion logic (adapted from contracttrait)
```

```toml
# guard_trait/Cargo.toml
[package]
name = "guard_trait"
version = "0.1.0"
edition = "2021"

[dependencies]
guard_trait_macro = { version = "0.1.0", path = "./guard_trait_macro" }
```

### The Attribute Macro

```rust
// guard_trait_macro/src/lib.rs
use proc_macro::TokenStream;

/// Transforms a trait into a two-trait split with structural guards.
///
/// # Attributes
///
/// - `#[guard(Self::method)]`: The specified method is called before the
///   annotated method. If the guard method panics, the guarded method
///   is never executed.
///
/// # Generated Items
///
/// Given `trait Foo`:
/// - `FooInternal` -- the inner trait (implement this)
/// - `Foo` -- the outer trait (use this, with `type Provider`)
/// - `impl_foo!` -- sealed implementation macro
/// - `FooTestClient` -- test client (cfg(test) only)
#[proc_macro_attribute]
pub fn guard_trait(attr: TokenStream, item: TokenStream) -> TokenStream {
    guard_trait_impl::expand(attr, item)
}
```

### Runtime Support

```rust
// guard_trait/src/lib.rs
pub use guard_trait_macro::guard_trait;

/// A call builder for testing guarded methods.
///
/// Provides a fluent API to satisfy guards and invoke the underlying method.
pub struct GuardedCall<'a, T> {
    invoke_fn: Box<dyn FnOnce() -> T + 'a>,
    guard_name: &'static str,
}

impl<'a, T> GuardedCall<'a, T> {
    pub fn new(
        f: impl FnOnce() -> T + 'a,
        guard_name: &'static str,
    ) -> Self {
        Self {
            invoke_fn: Box::new(f),
            guard_name,
        }
    }

    /// The name of the guard protecting this method.
    pub fn guard_name(&self) -> &'static str {
        self.guard_name
    }

    /// Invoke the guarded method. Panics if the guard fails.
    pub fn invoke(self) -> T {
        (self.invoke_fn)()
    }

    /// Try to invoke, catching panics from guard failures.
    pub fn try_invoke(self) -> std::thread::Result<T> {
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            (self.invoke_fn)()
        }))
    }
}
```

### What Changes from the Smart Contract Version

The core algorithm in `soroban-sdk-tools-macro/src/contract.rs` is ~700 lines. Here is what changes for a general-purpose version:

| Smart Contract Version | General Version |
|----------------------|----------------|
| `require_auth()` on `Address` | Guard function panics on failure (no special method call) |
| `soroban_sdk::contractimpl` | Standard `impl` block |
| `soroban_sdk::contracttrait` on outer trait | No special attribute needed |
| `#[cfg(not(target_family = "wasm"))]` for test client | `#[cfg(test)]` for test client |
| `soroban_sdk::Vec<Val>` for args | Standard Rust types |
| `extern crate alloc` (no_std) | Not needed (std available) |
| `#[auth(Self::method)]` attribute name | `#[guard(Self::method)]` attribute name |

The algorithm itself -- parse trait items, extract guard attributes, split into internal + outer, generate sealed macro and test client -- is identical. The transformation is mechanical.

---

## Performance: Zero Cost

Let us be precise about what "zero cost" means here.

**Monomorphization.** When you write `type Provider = PostgresRepo`, the compiler generates a specialized version of every outer-trait method that calls `PostgresRepo` methods directly. There is no vtable, no indirection, no `dyn`.

**Inlining.** The outer trait's default methods are trivial: call guard, call inner method. Under `opt-level >= 1`, LLVM inlines both calls. The final binary is identical to hand-written code that calls the guard and then the implementation directly.

**Binary size.** Each `type Provider = X` generates one monomorphized copy. If you use three different providers for the same trait (Postgres, SQLite, InMemory), you get three copies of the outer trait methods. This is the same cost as writing three separate implementations by hand -- no more, no less.

**Compile time.** The macro generates approximately 30-50 lines of code per trait method. For a trait with 10 methods, that is 300-500 lines of generated code. This is trivial compared to what `serde` or `diesel` generate for a single struct.

---

## When NOT to Use This Pattern

The guard trait pattern is not appropriate for every situation. Honesty about limitations builds trust:

1. **When guards need runtime configuration.** If your guard logic depends on runtime-loaded policy files or dynamically registered middleware, you want tower or a middleware stack, not compile-time trait dispatch.

2. **When you need multiple guard strategies per method.** The current pattern supports one `#[guard]` per method. If you need "check auth AND check rate limit AND check feature flag," you would need to compose these into a single guard function or extend the macro to support `#[guard(Self::check_auth, Self::check_rate_limit)]`.

3. **When your trait has `&self` / `&mut self` methods.** The current pattern uses associated functions (no `self` parameter) with an explicit context argument. This is a deliberate design choice from the smart contract world (Soroban contracts are stateless structs). For `self`-based traits, the pattern would need adaptation -- the `type Provider` would likely become a field rather than an associated type.

4. **When you want dynamic dispatch.** `type Provider` is monomorphic. If you need to select the provider at runtime (`if use_postgres { ... } else { ... }`), you need `dyn Trait` or an enum dispatch.

5. **When the ecosystem already solves your problem.** If you are writing a pure Axum app and tower middleware covers your needs, adding another abstraction layer is not worth the complexity. Use guard traits when the per-method granularity matters and when "forgot the middleware" is a real risk.

---

## The Bigger Picture: Structural Enforcement in Rust

Rust already has powerful mechanisms for structural enforcement:

- **Ownership and borrowing** enforce memory safety.
- **`Send` / `Sync` marker traits** enforce thread safety.
- **The typestate pattern** enforces protocol state machines.
- **Sealed traits** enforce that only approved types implement an interface.

The guard trait pattern adds **precondition enforcement** to this toolkit. It says: "this method requires this precondition, and the structure of the generated code ensures that every implementation and every call site satisfies it."

This is the Rust philosophy, extended to application-level invariants. Not just "memory safe" but "authorization safe." Not just "data-race free" but "precondition-verified."

The gap we are filling is real. Consider the hierarchy of enforcement:

```
Manual checks         → "Please remember to call is_admin()"
Linting               → "clippy warns if you forget"
Extractors/middleware  → "The framework checks, but you must opt in"
Guard traits           → "The trait definition guarantees the check runs"
Type-level proofs      → "The type system makes invalid states unrepresentable"
```

Guard traits sit between extractors and full typestate proofs. They are stronger than middleware (which is opt-in) but more pragmatic than encoding your entire authorization model in the type system (which is often impractical).

---

## A Roadmap for `guard_trait`

If we were to publish a general-purpose `guard_trait` crate, here is what the roadmap would look like:

### v0.1: Core Pattern
- `#[guard_trait]` attribute macro
- `#[guard(Self::method)]` for method-level guards
- Internal trait + outer trait generation
- `impl_{trait_snake}!` sealed macro
- Basic test client generation

### v0.2: Multiple Guards and Parameter Guards
- `#[guard(Self::check_auth, Self::check_rate_limit)]` -- multiple guards per method
- `#[guard(caller)]` -- use a parameter as the guard subject
- Guard execution order guarantees (left-to-right)
- Short-circuit semantics (first failing guard stops execution)

### v0.3: Async Support
- `async fn` in both internal and outer traits (using `async_trait` or native `async fn in traits`)
- Async guard functions
- Integration examples with tower and axum

### v0.4: `&self` Support
- Adaptation for `&self` / `&mut self` methods
- Provider becomes a field or generic parameter rather than an associated type
- Support for stateful guards (e.g., connection pooling, circuit breakers)

### v0.5: Guard Introspection
- `FooGuards::list()` -- runtime-queryable guard metadata
- Integration with OpenAPI generators (guards become security requirements)
- Guard documentation in `rustdoc` output

### v1.0: Stable API
- Stabilized macro expansion format
- Formal specification of guard resolution protocol
- Comprehensive test suite and documentation
- Published on crates.io

---

## From Smart Contracts to Everywhere

Let me show you where we started. This is an actual trait definition from our smart contract toolkit:

```rust
#[contracttrait]
pub trait Ownable {
    fn owner(env: &Env) -> Address;

    #[auth(Self::owner)]
    fn transfer_ownership(env: &Env, new_owner: Address);
}
```

And here is the same pattern, stripped of blockchain-specific types, ready for any Rust project:

```rust
#[guard_trait]
pub trait Ownable {
    fn owner(ctx: &AppCtx) -> OwnerId;

    #[guard(Self::owner)]
    fn transfer_ownership(ctx: &AppCtx, new_owner: OwnerId);
}
```

The difference is cosmetic. The pattern is identical. `Env` becomes `AppCtx`. `Address` becomes `OwnerId`. `#[auth]` becomes `#[guard]`. `require_auth()` becomes a panic-on-failure check.

Everything else -- the two-trait split, the provider-based DI, the sealed implementation macro, the test client generation -- transfers directly.

The macro implementation in our smart contract crate is ~700 lines of procedural macro code. Roughly 600 of those lines are domain-independent. The remaining ~100 are Soroban-specific (`contractimpl`, `require_auth`, WASM target gating). Extracting the general pattern is not speculative; the code already exists and works.

---

## The Invitation

We built `soroban-sdk-tools` to make smart contracts on Stellar composable and secure. Along the way, we stumbled into a pattern that we believe has much wider applicability.

The two-trait split with structural guard injection is not just clever -- it fills a genuine gap in Rust's ecosystem. Tower handles cross-cutting middleware. Extractors handle request-level parsing. Sealed traits control implementation. But nobody has a clean, macro-driven solution for *per-method structural preconditions with provider-based DI*.

If you are a web framework author thinking about authentication guarantees -- this pattern is relevant. If you are an embedded developer thinking about critical section enforcement -- this pattern is relevant. If you are a database library author thinking about backend abstraction -- the provider half of this pattern is relevant. If you maintain any crate where "the user forgot to add the auth check" is a category of bug -- this entire pattern is for you.

We would love collaborators. The `#[contracttrait]` macro is MIT-licensed, the source is at [soroban-sdk-tools-macro/src/contract.rs](https://github.com/blaineheffron/soroban-sdk-tools/blob/main/soroban-sdk-tools-macro/src/contract.rs), and the generalization to `guard_trait` is a tractable weekend project for anyone comfortable with `syn` and `quote`.

We discovered this building smart contracts, but it belongs in your Rust toolkit too.

---

*Willem Wyndham and Blaine Heffron are the authors of `soroban-sdk-tools`. For discussion about generalizing this pattern, reach out via [GitHub Issues](https://github.com/blaineheffron/soroban-sdk-tools/issues) or find us at RustConf.*
