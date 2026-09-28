//! Error handling utilities for Soroban contracts.
//!
//! Provides traits and helper types for composable error handling
//! with the `#[scerr]` macro. Error codes are assigned sequentially
//! starting at 1, with wrapped inner types flattened at their position
//! via const-chaining. The `Aborted` variant always uses code 0, and
//! the `UnknownError` sentinel always uses [`UNKNOWN_ERROR_CODE`].

// Re-export contracterror for users
pub use soroban_sdk::contracterror;

/// The error code used for the unknown/sentinel error variant.
/// Uses `i32::MAX as u32` for platform-independent consistency (matches
/// `isize::MAX as u32` on wasm32 which is the Soroban target).
pub const UNKNOWN_ERROR_CODE: u32 = i32::MAX as u32;

/// Base trait for contract errors
pub trait ContractError: Sized {
    /// Convert this error into a u32 code
    fn into_code(self) -> u32;

    /// Try to construct this error from a u32 code
    fn from_code(code: u32) -> Option<Self>;
}

/// Trait for mapping error variants to/from a 0-based sequential index.
///
/// This enables composable error flattening: wrapped inner types are mapped
/// into the outer enum's code space using `offset + inner.to_seq()` without
/// any off-by-one arithmetic. Types implementing this trait can be used as
/// inner types in `#[transparent]` and `#[from_contract_client]` variants.
///
/// For `#[scerr]` types, `to_seq()` returns `into_code() - 1` (since codes
/// start at 1). For `contractimport!` types, the mapping is generated from
/// the variant order regardless of native error codes.
pub trait SequentialError: Sized {
    /// Convert this error to a 0-based sequential index in `[0, TOTAL_CODES)`.
    fn to_seq(&self) -> u32;

    /// Construct this error from a 0-based sequential index.
    fn from_seq(seq: u32) -> Option<Self>;
}

/// Spec entry for a single error variant.
/// Used for flattening inner error types into outer contract specs.
#[derive(Debug, Clone, Copy)]
pub struct ErrorSpecEntry {
    /// The error code (u32)
    pub code: u32,
    /// The variant name (e.g., "DivisionByZero")
    pub name: &'static str,
    /// Human-readable description
    pub description: &'static str,
}

/// Node in the error spec tree for recursive flattening.
///
/// - **Leaf** (`children` is empty): a single error variant with a code,
///   name, and description.
/// - **Group** (`children` is non-empty): a wrapped inner error type whose
///   children should be flattened with a name prefix.
#[derive(Clone, Copy)]
pub struct SpecNode {
    /// Leaf: the error code.  Group: the offset in the parent's code space.
    pub code: u32,
    /// Leaf: the variant name.  Group: the prefix for flattened names.
    pub name: &'static str,
    /// Leaf: doc string.  Group: unused (empty).
    pub description: &'static str,
    /// Leaf: empty.  Group: inner type's `SPEC_TREE`.
    pub children: &'static [SpecNode],
}

/// Trait providing spec metadata for error types.
///
/// Automatically implemented by `#[scerr]` and `contractimport!`.
/// Used by root error enums for const-chaining: the length of `SPEC_ENTRIES`
/// determines how many sequential codes an inner type occupies in the outer
/// enum's code space.
pub trait ContractErrorSpec {
    /// Array of spec entries for all variants in this error type.
    const SPEC_ENTRIES: &'static [ErrorSpecEntry];

    /// Total number of sequential codes this type occupies.
    ///
    /// For basic-mode enums this equals `SPEC_ENTRIES.len()`.
    /// For root-mode enums this may be larger because wrapped inner types
    /// occupy multiple sequential codes that are not individually listed
    /// in `SPEC_ENTRIES`.
    const TOTAL_CODES: u32 = Self::SPEC_ENTRIES.len() as u32;

    /// Tree of all variants (leaves and groups) for recursive XDR
    /// flattening.  Empty by default for backward compatibility with
    /// types that haven't been recompiled yet.
    const SPEC_TREE: &'static [SpecNode] = &[];
}

// -----------------------------------------------------------------------------
// Const-fn XDR builder – runs entirely at compile time
// -----------------------------------------------------------------------------

/// Compute the total byte size of a `ScSpecUdtErrorEnumV0` entry encoded as
/// XDR, including the 4-byte union discriminant.
///
/// The XDR layout is:
/// ```text
///   4  bytes   union discriminant (4 = UdtErrorEnumV0)
///   string     doc
///   string     lib (always empty → 4 bytes)
///   string     name
///   4  bytes   cases count
///   per case:
///     string   doc
///     string   name (with accumulated prefix)
///     4 bytes  value (u32)
/// ```
pub const fn xdr_error_enum_size(name: &str, doc: &str, tree: &[SpecNode]) -> usize {
    4                                // union discriminant
    + xdr_string_size(doc.len())     // doc
    + xdr_string_size(0)             // lib (empty)
    + xdr_string_size(name.len())    // name
    + 4                              // cases count
    + tree_cases_size(tree, 0) // cases
}

/// Build the complete XDR bytes for a `ScSpecEntry::UdtErrorEnumV0`.
///
/// `N` must equal `xdr_error_enum_size(name, doc, tree)`.
pub const fn build_error_enum_xdr<const N: usize>(
    name: &str,
    doc: &str,
    tree: &[SpecNode],
) -> [u8; N] {
    let mut buf = [0u8; N];
    let n_cases = count_tree_leaves(tree) as u32;
    let prefix_buf = [0u8; 256];

    let mut pos = write_u32_be(&mut buf, 0, 4); // union discriminant
    pos = write_xdr_string(&mut buf, pos, doc.as_bytes());
    pos = write_xdr_string(&mut buf, pos, &[]); // lib (empty)
    pos = write_xdr_string(&mut buf, pos, name.as_bytes());
    pos = write_u32_be(&mut buf, pos, n_cases);
    pos = write_tree_cases(&mut buf, pos, tree, &prefix_buf, 0, 0);

    // The array size constraint `N` enforces that we wrote exactly the
    // right number of bytes; verify at compile time on supported toolchains.
    assert!(pos == N, "XDR size mismatch");

    buf
}

// --- Internal helpers --------------------------------------------------------

/// Count the total number of leaf nodes in a tree (recursively).
const fn count_tree_leaves(nodes: &[SpecNode]) -> usize {
    let mut total = 0usize;
    let mut idx = 0usize;
    while idx < nodes.len() {
        if nodes[idx].children.is_empty() {
            total += 1;
        } else {
            total += count_tree_leaves(nodes[idx].children);
        }
        idx += 1;
    }
    total
}

/// Compute the XDR-padded size of a string (4-byte length prefix + content
/// padded to 4-byte boundary).
const fn xdr_string_size(len: usize) -> usize {
    4 + ((len + 3) & !3)
}

/// Recursively compute byte sizes of all leaf cases in the tree, accounting
/// for name-prefix accumulation.
const fn tree_cases_size(nodes: &[SpecNode], prefix_len: usize) -> usize {
    let mut size = 0usize;
    let mut idx = 0usize;
    while idx < nodes.len() {
        if nodes[idx].children.is_empty() {
            // Leaf: doc + name (with prefix) + value
            size += xdr_string_size(nodes[idx].description.len());
            size += xdr_string_size(prefix_len + nodes[idx].name.len());
            size += 4;
        } else {
            // Group: recurse with extended prefix (name + '_')
            size += tree_cases_size(nodes[idx].children, prefix_len + nodes[idx].name.len() + 1);
        }
        idx += 1;
    }
    size
}

/// Write a big-endian u32, returning the new write position.
const fn write_u32_be(buf: &mut [u8], pos: usize, val: u32) -> usize {
    buf[pos] = (val >> 24) as u8;
    buf[pos + 1] = (val >> 16) as u8;
    buf[pos + 2] = (val >> 8) as u8;
    buf[pos + 3] = val as u8;
    pos + 4
}

/// Copy `src` bytes into `buf` at `pos`, returning the new write position.
const fn write_bytes(buf: &mut [u8], pos: usize, src: &[u8]) -> usize {
    let mut idx = 0usize;
    while idx < src.len() {
        buf[pos + idx] = src[idx];
        idx += 1;
    }
    pos + src.len()
}

/// Write zero-padding bytes to reach 4-byte alignment after `content_len`
/// bytes of content, returning the new write position.
const fn write_xdr_padding(buf: &mut [u8], pos: usize, content_len: usize) -> usize {
    let rem = content_len % 4;
    if rem == 0 {
        return pos;
    }
    let pad = 4 - rem;
    let mut idx = 0usize;
    while idx < pad {
        buf[pos + idx] = 0;
        idx += 1;
    }
    pos + pad
}

/// Write an XDR string: 4-byte BE length + content + zero-padding to 4-byte
/// alignment.
const fn write_xdr_string(buf: &mut [u8], pos: usize, s: &[u8]) -> usize {
    let pos = write_u32_be(buf, pos, s.len() as u32);
    let pos = write_bytes(buf, pos, s);
    write_xdr_padding(buf, pos, s.len())
}

/// Write an XDR string that is the concatenation of `prefix[0..prefix_len]`
/// and `name`, without allocating.
///
/// Const fn cannot take `&[u8]` sub-slices, so we accept the fixed-size
/// prefix buffer and a length instead.
const fn write_xdr_prefixed_string(
    buf: &mut [u8],
    pos: usize,
    prefix_buf: &[u8; 256],
    prefix_len: usize,
    name: &[u8],
) -> usize {
    let total_len = prefix_len + name.len();
    let mut pos = write_u32_be(buf, pos, total_len as u32);
    // Copy prefix bytes
    let mut idx = 0usize;
    while idx < prefix_len {
        buf[pos + idx] = prefix_buf[idx];
        idx += 1;
    }
    pos += prefix_len;
    // Copy name bytes
    pos = write_bytes(buf, pos, name);
    write_xdr_padding(buf, pos, total_len)
}

/// Recursively write tree cases into the XDR buffer.
///
/// For leaves: `code = base_offset + leaf.code`.
/// For groups: children get `new_base = base_offset + group.code - 1`
/// (because leaf codes start at 1 within their inner type).
const fn write_tree_cases(
    buf: &mut [u8],
    pos: usize,
    nodes: &[SpecNode],
    prefix_buf: &[u8; 256],
    prefix_len: usize,
    base_offset: u32,
) -> usize {
    let mut pos = pos;
    let mut idx = 0usize;
    while idx < nodes.len() {
        if nodes[idx].children.is_empty() {
            // Leaf: doc, name (with prefix), value
            pos = write_xdr_string(buf, pos, nodes[idx].description.as_bytes());
            pos = write_xdr_prefixed_string(
                buf,
                pos,
                prefix_buf,
                prefix_len,
                nodes[idx].name.as_bytes(),
            );
            pos = write_u32_be(buf, pos, base_offset + nodes[idx].code);
        } else {
            // Group: extend prefix with "Name_" and recurse
            let name_bytes = nodes[idx].name.as_bytes();
            let mut new_prefix = *prefix_buf;
            new_prefix = copy_into(new_prefix, prefix_len, name_bytes);
            new_prefix[prefix_len + name_bytes.len()] = b'_';

            pos = write_tree_cases(
                buf,
                pos,
                nodes[idx].children,
                &new_prefix,
                prefix_len + name_bytes.len() + 1,
                base_offset + nodes[idx].code - 1,
            );
        }
        idx += 1;
    }
    pos
}

/// Copy `src` into `dst` starting at `offset`, returning the modified array.
/// Used instead of `write_bytes` when we need to build a new prefix buffer
/// without mutating in place.
const fn copy_into(mut dst: [u8; 256], offset: usize, src: &[u8]) -> [u8; 256] {
    let mut idx = 0usize;
    while idx < src.len() {
        dst[offset + idx] = src[idx];
        idx += 1;
    }
    dst
}

// -----------------------------------------------------------------------------
// Spec shaking marker – const SHA-256 over the spec entry XDR
// -----------------------------------------------------------------------------

/// Length in bytes of a spec shaking marker (`"SpEcV1"` + 8 hash bytes).
///
/// Mirrors `soroban_spec::shaking::Marker`.
pub const SPEC_SHAKING_MARKER_LEN: usize = 14;

/// Build the spec shaking marker for a spec entry's XDR bytes, at compile time.
///
/// soroban-sdk 28 requires every type that appears in a contract function
/// signature to implement `SpecShakingMarker`, and `stellar contract build`
/// strips any `contractspecv0` entry that has no matching marker in the WASM
/// data section. The marker layout is the six-byte magic `"SpEcV1"` followed
/// by the first eight bytes of the SHA-256 of the entry's XDR, matching
/// `soroban_spec::shaking::generate_marker_for_xdr`.
///
/// Root-mode `#[scerr]` enums build their spec XDR with [`build_error_enum_xdr`]
/// rather than the SDK's `#[contracterror]`, so they compute the marker here.
pub const fn spec_shaking_marker(spec_entry_xdr: &[u8]) -> [u8; SPEC_SHAKING_MARKER_LEN] {
    let hash = sha256(spec_entry_xdr);
    [
        b'S', b'p', b'E', b'c', b'V', b'1', hash[0], hash[1], hash[2], hash[3], hash[4], hash[5],
        hash[6], hash[7],
    ]
}

const SHA256_K: [u32; 64] = [
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
];

/// SHA-256 as a `const fn`, so a marker can be computed from XDR bytes that
/// are themselves produced by const evaluation.
pub const fn sha256(input: &[u8]) -> [u8; 32] {
    let mut state: [u32; 8] = [
        0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab,
        0x5be0cd19,
    ];

    // Process every complete 64-byte block of the input.
    let mut offset = 0usize;
    while offset + 64 <= input.len() {
        let mut block = [0u8; 64];
        let mut i = 0usize;
        while i < 64 {
            block[i] = input[offset + i];
            i += 1;
        }
        state = sha256_compress(state, &block);
        offset += 64;
    }

    // Pad the tail: remaining bytes, 0x80, zeros, then the 64-bit bit length.
    let rem = input.len() - offset;
    let mut tail = [0u8; 128];
    let mut i = 0usize;
    while i < rem {
        tail[i] = input[offset + i];
        i += 1;
    }
    tail[rem] = 0x80;
    let tail_len = if rem < 56 { 64 } else { 128 };
    let bit_len = (input.len() as u64) * 8;
    let mut i = 0usize;
    while i < 8 {
        tail[tail_len - 1 - i] = (bit_len >> (8 * i)) as u8;
        i += 1;
    }

    let mut block = [0u8; 64];
    let mut i = 0usize;
    while i < 64 {
        block[i] = tail[i];
        i += 1;
    }
    state = sha256_compress(state, &block);
    if tail_len == 128 {
        let mut i = 0usize;
        while i < 64 {
            block[i] = tail[64 + i];
            i += 1;
        }
        state = sha256_compress(state, &block);
    }

    let mut out = [0u8; 32];
    let mut i = 0usize;
    while i < 8 {
        let word = state[i].to_be_bytes();
        out[4 * i] = word[0];
        out[4 * i + 1] = word[1];
        out[4 * i + 2] = word[2];
        out[4 * i + 3] = word[3];
        i += 1;
    }
    out
}

const fn sha256_compress(state: [u32; 8], block: &[u8; 64]) -> [u32; 8] {
    let mut w = [0u32; 64];
    let mut i = 0usize;
    while i < 16 {
        w[i] = u32::from_be_bytes([
            block[4 * i],
            block[4 * i + 1],
            block[4 * i + 2],
            block[4 * i + 3],
        ]);
        i += 1;
    }
    while i < 64 {
        let s0 = w[i - 15].rotate_right(7) ^ w[i - 15].rotate_right(18) ^ (w[i - 15] >> 3);
        let s1 = w[i - 2].rotate_right(17) ^ w[i - 2].rotate_right(19) ^ (w[i - 2] >> 10);
        w[i] = w[i - 16]
            .wrapping_add(s0)
            .wrapping_add(w[i - 7])
            .wrapping_add(s1);
        i += 1;
    }

    let mut a = state[0];
    let mut b = state[1];
    let mut c = state[2];
    let mut d = state[3];
    let mut e = state[4];
    let mut f = state[5];
    let mut g = state[6];
    let mut h = state[7];

    let mut i = 0usize;
    while i < 64 {
        let s1 = e.rotate_right(6) ^ e.rotate_right(11) ^ e.rotate_right(25);
        let ch = (e & f) ^ (!e & g);
        let t1 = h
            .wrapping_add(s1)
            .wrapping_add(ch)
            .wrapping_add(SHA256_K[i])
            .wrapping_add(w[i]);
        let s0 = a.rotate_right(2) ^ a.rotate_right(13) ^ a.rotate_right(22);
        let maj = (a & b) ^ (a & c) ^ (b & c);
        let t2 = s0.wrapping_add(maj);

        h = g;
        g = f;
        f = e;
        e = d.wrapping_add(t1);
        d = c;
        c = b;
        b = a;
        a = t1.wrapping_add(t2);
        i += 1;
    }

    [
        state[0].wrapping_add(a),
        state[1].wrapping_add(b),
        state[2].wrapping_add(c),
        state[3].wrapping_add(d),
        state[4].wrapping_add(e),
        state[5].wrapping_add(f),
        state[6].wrapping_add(g),
        state[7].wrapping_add(h),
    ]
}
