//! Error handling utilities for Soroban contracts.
//!
//! Provides traits and helper types for composable error handling
//! with the `#[scerr]` macro. Error codes are assigned sequentially
//! starting at 1, with wrapped inner types flattened at their position
//! via const-chaining.

// Re-export contracterror for users
pub use soroban_sdk::contracterror;

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
pub const fn xdr_error_enum_size(
    name: &str,
    doc: &str,
    tree: &[SpecNode],
) -> usize {
    4                                // union discriminant
    + xdr_string_size(doc.len())     // doc
    + xdr_string_size(0)             // lib (empty)
    + xdr_string_size(name.len())    // name
    + 4                              // cases count
    + tree_cases_size(tree, 0)       // cases
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
                buf, pos, prefix_buf, prefix_len, nodes[idx].name.as_bytes(),
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

/// Helper to panic with an error and optional context
#[macro_export]
macro_rules! panic_with_error {
    ($env:expr, $error:expr) => {{
        let env: &soroban_sdk::Env = $env;
        env.panic_with_error($error)
    }};
    ($env:expr, $error:expr, $msg:literal) => {{
        let env: &soroban_sdk::Env = $env;
        // In the future we could log `$msg` via events or debug logging
        let _ = $msg;
        env.panic_with_error($error)
    }};
}
