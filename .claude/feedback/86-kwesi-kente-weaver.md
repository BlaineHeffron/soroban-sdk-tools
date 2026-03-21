# Review: Kwesi -- Ghanaian Kente Weaver Tracking Artisan Craft Provenance

**Reviewer Profile:** Master kente weaver from the Ashanti region of Ghana, exploring how blockchain provenance can protect artisan craft authenticity, ensure fair pricing, and preserve cultural heritage in a globalized marketplace.

**Materials Reviewed:**
- `docs/oz-comparison.md`
- `docs/blog-post-composable-contracts.md`
- `examples/trait-test/src/lib.rs`
- `soroban-sdk-tools-macro/src/contract.rs`

---

## Overall Impression

I weave kente cloth. Each piece takes weeks. The patterns have names and meanings passed down through generations -- "Sika Futuro" means "gold dust," "Oyokoman" represents the royal clan. My cloth sells for a fair price when buyers know it is authentic. But mass-produced imitations from factories flood the market, selling for a fraction of the cost with no attribution to the tradition.

soroban-sdk-tools could solve this. The structural auth pattern could ensure that only verified artisans can register authentic pieces. The provider pattern could adapt to different craft traditions (kente, mudcloth, adinkra, batik) without rewriting the provenance system. But the toolkit as documented is built for DeFi and ownership transfer, not for craft economies. It needs to be shown through the lens of the artisan.

**Rating: 3.5/5** -- Strong technical foundation for craft provenance, but the documentation and examples do not speak to artisan use cases, and critical features for craft economies are missing.

---

## Strengths

### 1. Structural Auth Protects Artisan Identity

The biggest problem in the craft economy is impersonation. Factories produce machine-made "kente" and label it as handwoven. The `#[auth]` pattern can prevent this at the contract level:

```rust
#[contracttrait]
pub trait CraftRegistry {
    fn master_weaver(env: &Env) -> Address;

    #[auth(Self::master_weaver)]
    fn certify_artisan(env: &Env, artisan: Address, craft: Symbol, region: Symbol);

    #[auth(artisan)]
    fn register_piece(env: &Env, artisan: Address, piece_id: Symbol, metadata: PieceMetadata);
}
```

Only a certified master weaver can register other artisans. Only a certified artisan can register their own pieces. The sealed macro ensures no one can bypass this certification chain. This is the digital equivalent of the weaving guild's quality mark.

### 2. Provider Pattern Supports Multiple Craft Traditions

Different crafts have different certification processes:
- Kente: certified by master weavers in the Ashanti region
- Mudcloth: certified by the Bamana artisan cooperatives in Mali
- Batik: certified by UNESCO intangible cultural heritage registries

The provider pattern handles this without separate codebases:

```rust
pub struct KenteCertification;
impl CraftRegistryInternal for KenteCertification {
    fn certify_artisan(env: &Env, artisan: Address, craft: Symbol, region: Symbol) {
        // Kente-specific: must be from Ashanti region
        assert!(region == Symbol::new(env, "ashanti"), "kente must originate from Ashanti");
        // ... additional kente-specific checks
    }
}

pub struct MudclothCertification;
impl CraftRegistryInternal for MudclothCertification {
    fn certify_artisan(env: &Env, artisan: Address, craft: Symbol, region: Symbol) {
        // Mudcloth-specific: must be endorsed by cooperative
        // ... mudcloth-specific checks
    }
}
```

Same registry interface, different cultural rules. This respects the diversity of craft traditions while maintaining a unified verification system. This is exactly what the artisan economy needs.

### 3. AuthClient Verifies the Certification Chain

When a buyer purchases a kente cloth, they want to verify the chain: "Was this piece registered by a certified artisan? Was the artisan certified by a recognized master weaver?" The `AuthClient` pattern lets you test each link:

```rust
// Master weaver certifies artisan
craft_auth.certify_artisan(&artisan_addr, &kente_symbol, &ashanti_symbol)
    .authorize(&master_weaver_addr)
    .invoke();

// Artisan registers their piece
craft_auth.register_piece(&artisan_addr, &piece_id, &metadata)
    .authorize(&artisan_addr)
    .invoke();
```

Each step in the certification chain is testable and verifiable. This builds the trust infrastructure that the artisan economy desperately needs.

### 4. Composable Error Handling for Cultural Validation

The `#[scerr]` error chaining could provide meaningful error messages for cultural validation failures:

```rust
#[scerr]
pub enum CraftError {
    ArtisanNotCertified,
    RegionMismatch,
    PatternNotRecognized,
    PieceAlreadyRegistered,
    #[transparent]
    Auth(#[from] AuthError),
}
```

When a factory tries to register a mass-produced piece as handwoven kente, the contract does not just say "unauthorized" -- it says "artisan not certified" or "region mismatch." Specific errors make the system more transparent and educational.

---

## Concerns

### 1. No Concept of Craft Lineage or Apprenticeship

**Severity: High**

Kente weaving is passed from master to apprentice over years of training. A master weaver certifies their apprentice, and the apprentice's work carries the lineage of their master. This is a multi-generational provenance chain.

The current `Ownable` pattern has `transfer_ownership` -- a single owner who can transfer to a single new owner. But craft lineage is not ownership transfer. A master does not *stop* being a master when they certify an apprentice. The relationship is additive, not transferive.

**Recommendation:** Add a lineage trait pattern:

```rust
#[contracttrait]
pub trait CraftLineage {
    fn master(env: &Env, artisan: Address) -> Option<Address>;
    fn apprentices(env: &Env, master: Address) -> Vec<Address>;

    #[auth(master)]
    fn certify_apprentice(env: &Env, master: Address, apprentice: Address);

    // Apprentice's pieces carry master's lineage
    fn get_lineage(env: &Env, artisan: Address) -> Vec<Address>;
}
```

This is fundamentally different from ownership. Multiple apprentices can share the same master. The master does not lose authority when certifying apprentices. And the lineage chain is an immutable historical record.

### 2. No Fair Pricing or Royalty Distribution

**Severity: High**

The artisan economy's biggest challenge is not authentication -- it is fair compensation. When a kente cloth is resold, the original weaver receives nothing. When a pattern is copied, the cultural community receives nothing.

The `FungibleToken` trait in the comparison document models transfers but not royalties. For craft economies, every secondary sale should route a percentage back to the artisan and their community.

**Recommendation:** Add royalty support to the trait system:

```rust
#[contracttrait]
pub trait CraftMarketplace: CraftRegistry {
    fn royalty_rate(env: &Env, piece_id: Symbol) -> u32; // basis points

    #[auth(seller)]
    fn list_piece(env: &Env, seller: Address, piece_id: Symbol, price: i128);

    #[auth(buyer)]
    fn purchase(env: &Env, buyer: Address, piece_id: Symbol);
    // Auto-distributes: artisan gets royalty, community fund gets share, seller gets remainder
}
```

The sealed macro would ensure the royalty distribution logic cannot be bypassed. This is the killer feature for craft economies.

### 3. No Metadata Standards for Physical Artifacts

**Severity: Medium**

A kente cloth has metadata that digital tokens do not: physical dimensions, thread count, weave pattern, dye types (natural vs. synthetic), region of origin, approximate weaving time. The current trait system passes `Symbol` and `Address` types but has no standard for rich metadata.

The example code uses `Symbol::new(env, "owner")` for storage keys. For craft provenance, the metadata schema needs to be standardized so that different marketplaces, museums, and authentication services can interoperate.

**Recommendation:** Define standard metadata types for physical craft objects:

```rust
#[contracttype]
pub struct CraftMetadata {
    pub craft_type: Symbol,      // "kente", "mudcloth", "batik"
    pub region: Symbol,          // "ashanti", "bamana", "java"
    pub artisan: Address,        // certified artisan
    pub creation_date: u64,      // timestamp
    pub materials: Vec<Symbol>,  // "silk", "cotton", "natural_dye"
    pub pattern_name: Symbol,    // "sika_futuro", "oyokoman"
    pub dimensions_cm: (u32, u32), // width x length
    pub certification_hash: BytesN<32>, // hash of physical inspection report
}
```

### 4. No Physical-Digital Bridge

**Severity: Medium**

The fundamental challenge of craft provenance is bridging the physical and digital worlds. A blockchain record says "this kente cloth is authentic," but how do you verify that the physical cloth in your hands corresponds to the digital record?

The current system has no mechanism for:
- NFC tags embedded in cloth linking to on-chain records
- QR codes woven into the selvedge edge
- Photo hash verification comparing physical appearance to registered images
- Third-party physical inspection certification

**Recommendation:** Add a physical verification trait:

```rust
#[contracttrait]
pub trait PhysicalVerification: CraftRegistry {
    #[auth(Self::inspector)]
    fn link_physical_id(env: &Env, piece_id: Symbol, nfc_hash: BytesN<32>);

    #[auth(Self::inspector)]
    fn add_photo_hash(env: &Env, piece_id: Symbol, photo_hash: BytesN<32>);

    fn verify_nfc(env: &Env, nfc_hash: BytesN<32>) -> Option<Symbol>; // returns piece_id
}
```

### 5. No Community Governance for Cultural Standards

**Severity: Medium**

Who decides what constitutes "authentic" kente? It is not a single owner -- it is the weaving community. Pattern recognition, material standards, and regional designations are collective decisions made by the community of practitioners.

The `Ownable` pattern centers authority in one address. Craft authentication requires community consensus. This echoes the concerns raised by cooperative economics (see Yael's review), but with a cultural dimension: the community is not just making economic decisions, they are maintaining cultural standards.

**Recommendation:** The `CraftRegistry` should be governed by a council of master weavers, not a single owner. See the cooperative governance patterns, but adapted for cultural authority:

```rust
#[contracttrait]
pub trait CraftCouncil {
    fn is_master(env: &Env, addr: Address) -> bool;
    fn council_size(env: &Env) -> u32;

    // Requires majority of masters to approve new patterns
    #[auth(proposer)]
    fn propose_pattern(env: &Env, proposer: Address, pattern_name: Symbol, description: String);

    // Voting by masters
    #[auth(voter)]
    fn vote_pattern(env: &Env, voter: Address, proposal_id: u64, approve: bool);
}
```

---

## Use Case Exploration: Kente Cloth Provenance System

Here is the full system I would build:

```rust
// Layer 1: Artisan Registry (who is certified to weave)
#[contracttrait]
pub trait ArtisanRegistry {
    fn registry_authority(env: &Env) -> Address;

    #[auth(Self::registry_authority)]
    fn register_master(env: &Env, master: Address, region: Symbol);

    #[auth(master)]
    fn register_apprentice(env: &Env, master: Address, apprentice: Address);

    fn is_certified(env: &Env, artisan: Address) -> bool;
    fn get_lineage(env: &Env, artisan: Address) -> Vec<Address>;
}

// Layer 2: Piece Registry (what has been woven)
#[contracttrait]
pub trait PieceRegistry: ArtisanRegistry {
    #[auth(artisan)]
    fn register_piece(env: &Env, artisan: Address, metadata: CraftMetadata) -> Symbol;

    fn get_piece(env: &Env, piece_id: Symbol) -> CraftMetadata;
    fn pieces_by_artisan(env: &Env, artisan: Address) -> Vec<Symbol>;
}

// Layer 3: Marketplace (buying and selling with royalties)
#[contracttrait]
pub trait CraftMarketplace: PieceRegistry {
    #[auth(seller)]
    fn list(env: &Env, seller: Address, piece_id: Symbol, price: i128);

    #[auth(buyer)]
    fn buy(env: &Env, buyer: Address, piece_id: Symbol);

    fn royalty_rate(env: &Env) -> u32; // basis points to artisan
    fn community_rate(env: &Env) -> u32; // basis points to community fund
}
```

The supertrait chain ensures that pieces can only be registered by certified artisans, and marketplace sales can only happen for registered pieces. The sealed macro ensures that royalty distribution cannot be bypassed.

This is a system that protects the weaver, honors the tradition, and creates a fair market.

---

## Documentation Feedback

### The Blog Post

The blog post is well-written but speaks exclusively to developers and the DeFi community. For artisan adoption, the documentation needs to show non-technical stakeholders *what the system means for them*:

- "Structural auth" means "only real weavers can register cloth"
- "Provider swapping" means "different communities can have different rules"
- "Sealed macros" means "no one can cheat the system, not even the programmers"

### The OZ Comparison

The comparison is purely technical. For craft economies, the comparison should also address: "Which system better protects physical artisans from exploitation?" Neither system currently has artisan-specific features, but the provider pattern in soroban-sdk-tools is more adaptable to cultural use cases.

### The Example Code

The example (`trait-test/src/lib.rs`) uses `SingleOwner` -- a corporate pattern. Consider adding an artisan-themed example that shows craft provenance. This would:
1. Demonstrate the toolkit's versatility beyond DeFi
2. Attract a different developer community
3. Show that Soroban can serve real-world economies, not just financial speculation

---

## The Weaver's Perspective

A loom has a warp (the fixed vertical threads) and a weft (the moving horizontal threads). The warp provides structure; the weft provides pattern and color. soroban-sdk-tools is the warp -- the structural framework. The providers are the weft -- the creative expression within that structure.

But a kente cloth is judged by its weft, not its warp. The structure must serve the pattern, not the other way around. Right now, the documentation asks: "How do you enforce auth?" It should also ask: "How do you honor the artisan?"

Give us the tools to weave our provenance into the blockchain, and we will create something more beautiful than any DeFi protocol. We have been weaving for centuries. We just need a new loom.
