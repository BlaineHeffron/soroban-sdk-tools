//! Tests for the const-fn XDR builder in `soroban_sdk_tools::error`.
//!
//! These tests verify that `xdr_error_enum_size` and `build_error_enum_xdr`
//! produce correct XDR bytes for `ScSpecUdtErrorEnumV0` entries, exercising
//! leaf-only trees, nested group trees, and edge cases.

use soroban_sdk_tools::error::{build_error_enum_xdr, xdr_error_enum_size, SpecNode};

// ---------------------------------------------------------------------------
// Helpers for reading XDR bytes
// ---------------------------------------------------------------------------

/// Read a big-endian u32 from `buf` at `pos`.
fn read_u32(buf: &[u8], pos: usize) -> (u32, usize) {
    let val = u32::from_be_bytes([buf[pos], buf[pos + 1], buf[pos + 2], buf[pos + 3]]);
    (val, pos + 4)
}

/// Read an XDR string from `buf` at `pos`: 4-byte length + padded content.
fn read_xdr_string(buf: &[u8], pos: usize) -> (String, usize) {
    let (len, pos) = read_u32(buf, pos);
    let len = len as usize;
    let s = std::str::from_utf8(&buf[pos..pos + len]).unwrap().to_string();
    let padded = (len + 3) & !3;
    (s, pos + padded)
}

/// Parsed representation of a single error case in XDR.
#[derive(Debug, PartialEq)]
struct XdrCase {
    doc: String,
    name: String,
    value: u32,
}

/// Parse a complete XDR error enum entry and return its fields.
fn parse_xdr_error_enum(buf: &[u8]) -> (u32, String, String, String, Vec<XdrCase>) {
    let (discriminant, pos) = read_u32(buf, 0);
    let (doc, pos) = read_xdr_string(buf, pos);
    let (lib, pos) = read_xdr_string(buf, pos);
    let (name, pos) = read_xdr_string(buf, pos);
    let (n_cases, mut pos) = read_u32(buf, pos);

    let mut cases = Vec::new();
    for _ in 0..n_cases {
        let (case_doc, p) = read_xdr_string(buf, pos);
        let (case_name, p) = read_xdr_string(buf, p);
        let (case_value, p) = read_u32(buf, p);
        cases.push(XdrCase {
            doc: case_doc,
            name: case_name,
            value: case_value,
        });
        pos = p;
    }

    assert_eq!(pos, buf.len(), "Did not consume all XDR bytes");
    (discriminant, doc, lib, name, cases)
}

// ---------------------------------------------------------------------------
// Tests: leaf-only tree (basic mode)
// ---------------------------------------------------------------------------

const BASIC_TREE: &[SpecNode] = &[
    SpecNode {
        code: 1,
        name: "DivisionByZero",
        description: "division by zero",
        children: &[],
    },
    SpecNode {
        code: 2,
        name: "NegativeInput",
        description: "negative input",
        children: &[],
    },
];

#[test]
fn leaf_only_size_and_build() {
    const SIZE: usize = xdr_error_enum_size("MathError", "math errors", BASIC_TREE);
    let xdr: [u8; SIZE] = build_error_enum_xdr("MathError", "math errors", BASIC_TREE);

    let (disc, doc, lib, name, cases) = parse_xdr_error_enum(&xdr);

    assert_eq!(disc, 4); // UdtErrorEnumV0 discriminant
    assert_eq!(doc, "math errors");
    assert_eq!(lib, "");
    assert_eq!(name, "MathError");
    assert_eq!(cases.len(), 2);
    assert_eq!(
        cases[0],
        XdrCase {
            doc: "division by zero".into(),
            name: "DivisionByZero".into(),
            value: 1,
        }
    );
    assert_eq!(
        cases[1],
        XdrCase {
            doc: "negative input".into(),
            name: "NegativeInput".into(),
            value: 2,
        }
    );
}

// ---------------------------------------------------------------------------
// Tests: single-level nested tree (root mode with one inner type)
// ---------------------------------------------------------------------------

const INNER_TREE: &[SpecNode] = &[
    SpecNode {
        code: 1,
        name: "DeepFailureOne",
        description: "deep failure one",
        children: &[],
    },
    SpecNode {
        code: 2,
        name: "DeepFailureTwo",
        description: "deep failure two",
        children: &[],
    },
];

const MIDDLE_TREE: &[SpecNode] = &[
    SpecNode {
        code: 1,
        name: "MiddleFailure",
        description: "middle-level failure",
        children: &[],
    },
    SpecNode {
        code: 2,
        name: "MiddleOther",
        description: "another middle error",
        children: &[],
    },
    SpecNode {
        code: 3, // offset where inner starts
        name: "Deep",
        description: "",
        children: INNER_TREE,
    },
    SpecNode {
        code: 5,
        name: "Aborted",
        description: "Cross-contract call aborted",
        children: &[],
    },
    SpecNode {
        code: 6,
        name: "UnknownError",
        description: "Unknown error from cross-contract call",
        children: &[],
    },
];

#[test]
fn single_nesting_size_and_build() {
    const SIZE: usize = xdr_error_enum_size("MiddleError", "", MIDDLE_TREE);
    let xdr: [u8; SIZE] = build_error_enum_xdr("MiddleError", "", MIDDLE_TREE);

    let (disc, _doc, _lib, name, cases) = parse_xdr_error_enum(&xdr);

    assert_eq!(disc, 4);
    assert_eq!(name, "MiddleError");
    assert_eq!(cases.len(), 6);

    // Unit variants: codes as-is
    assert_eq!(cases[0].name, "MiddleFailure");
    assert_eq!(cases[0].value, 1);
    assert_eq!(cases[1].name, "MiddleOther");
    assert_eq!(cases[1].value, 2);

    // Nested Deep variants: base_offset = 0 + 3 - 1 = 2, leaf codes 1,2 → 3,4
    assert_eq!(cases[2].name, "Deep_DeepFailureOne");
    assert_eq!(cases[2].value, 3);
    assert_eq!(cases[2].doc, "deep failure one");
    assert_eq!(cases[3].name, "Deep_DeepFailureTwo");
    assert_eq!(cases[3].value, 4);

    // Remaining unit variants
    assert_eq!(cases[4].name, "Aborted");
    assert_eq!(cases[4].value, 5);
    assert_eq!(cases[5].name, "UnknownError");
    assert_eq!(cases[5].value, 6);
}

// ---------------------------------------------------------------------------
// Tests: double-nested tree (root wrapping root)
// ---------------------------------------------------------------------------

const NESTED_TREE: &[SpecNode] = &[
    SpecNode {
        code: 1,
        name: "NestedFailure",
        description: "nested-level failure",
        children: &[],
    },
    SpecNode {
        code: 2, // offset where middle starts
        name: "Middle",
        description: "",
        children: MIDDLE_TREE,
    },
    SpecNode {
        code: 8,
        name: "Aborted",
        description: "Cross-contract call aborted",
        children: &[],
    },
    SpecNode {
        code: 9,
        name: "UnknownError",
        description: "Unknown error from cross-contract call",
        children: &[],
    },
];

#[test]
fn double_nesting_size_and_build() {
    const SIZE: usize = xdr_error_enum_size("NestedError", "", NESTED_TREE);
    let xdr: [u8; SIZE] = build_error_enum_xdr("NestedError", "", NESTED_TREE);

    let (_, _doc, _lib, name, cases) = parse_xdr_error_enum(&xdr);

    assert_eq!(name, "NestedError");
    assert_eq!(cases.len(), 9);

    // All 9 flattened variants with correct names and sequential codes
    let expected: &[(&str, u32)] = &[
        ("NestedFailure", 1),
        ("Middle_MiddleFailure", 2),
        ("Middle_MiddleOther", 3),
        ("Middle_Deep_DeepFailureOne", 4),
        ("Middle_Deep_DeepFailureTwo", 5),
        ("Middle_Aborted", 6),
        ("Middle_UnknownError", 7),
        ("Aborted", 8),
        ("UnknownError", 9),
    ];

    for (i, (exp_name, exp_code)) in expected.iter().enumerate() {
        assert_eq!(
            cases[i].name, *exp_name,
            "case[{}] name mismatch",
            i
        );
        assert_eq!(
            cases[i].value, *exp_code,
            "case[{}] ({}) value mismatch",
            i, exp_name
        );
    }
}

// ---------------------------------------------------------------------------
// Tests: edge cases
// ---------------------------------------------------------------------------

#[test]
fn empty_tree_produces_zero_cases() {
    const SIZE: usize = xdr_error_enum_size("EmptyError", "no variants", &[]);
    let xdr: [u8; SIZE] = build_error_enum_xdr("EmptyError", "no variants", &[]);

    let (disc, doc, _lib, name, cases) = parse_xdr_error_enum(&xdr);

    assert_eq!(disc, 4);
    assert_eq!(doc, "no variants");
    assert_eq!(name, "EmptyError");
    assert_eq!(cases.len(), 0);
}

#[test]
fn single_leaf_tree() {
    const TREE: &[SpecNode] = &[SpecNode {
        code: 1,
        name: "OnlyVariant",
        description: "the only one",
        children: &[],
    }];

    const SIZE: usize = xdr_error_enum_size("SingleError", "", TREE);
    let xdr: [u8; SIZE] = build_error_enum_xdr("SingleError", "", TREE);

    let (_, _, _, name, cases) = parse_xdr_error_enum(&xdr);
    assert_eq!(name, "SingleError");
    assert_eq!(cases.len(), 1);
    assert_eq!(cases[0].name, "OnlyVariant");
    assert_eq!(cases[0].value, 1);
    assert_eq!(cases[0].doc, "the only one");
}

#[test]
fn xdr_string_padding_alignment() {
    // Test with names of various lengths to exercise XDR padding:
    // len 1 → 4 bytes padded, len 2 → 4, len 3 → 4, len 4 → 4, len 5 → 8
    const TREE: &[SpecNode] = &[
        SpecNode {
            code: 1,
            name: "A",
            description: "x",
            children: &[],
        },
        SpecNode {
            code: 2,
            name: "AB",
            description: "xy",
            children: &[],
        },
        SpecNode {
            code: 3,
            name: "ABC",
            description: "xyz",
            children: &[],
        },
        SpecNode {
            code: 4,
            name: "ABCD",
            description: "wxyz",
            children: &[],
        },
        SpecNode {
            code: 5,
            name: "ABCDE",
            description: "vwxyz",
            children: &[],
        },
    ];

    const SIZE: usize = xdr_error_enum_size("PadTest", "", TREE);
    let xdr: [u8; SIZE] = build_error_enum_xdr("PadTest", "", TREE);

    let (_, _, _, _, cases) = parse_xdr_error_enum(&xdr);

    assert_eq!(cases.len(), 5);
    assert_eq!(cases[0].name, "A");
    assert_eq!(cases[1].name, "AB");
    assert_eq!(cases[2].name, "ABC");
    assert_eq!(cases[3].name, "ABCD");
    assert_eq!(cases[4].name, "ABCDE");

    // Verify round-trip: all XDR bytes consumed correctly
    // (the parse_xdr_error_enum assert at the end guarantees this)
}

#[test]
fn group_only_tree_no_leaves_at_top() {
    // Tree with only a group node (no top-level leaves)
    const INNER: &[SpecNode] = &[
        SpecNode {
            code: 1,
            name: "InnerA",
            description: "a",
            children: &[],
        },
        SpecNode {
            code: 2,
            name: "InnerB",
            description: "b",
            children: &[],
        },
    ];
    const TREE: &[SpecNode] = &[SpecNode {
        code: 1,
        name: "Wrapped",
        description: "",
        children: INNER,
    }];

    const SIZE: usize = xdr_error_enum_size("GroupOnly", "", TREE);
    let xdr: [u8; SIZE] = build_error_enum_xdr("GroupOnly", "", TREE);

    let (_, _, _, _, cases) = parse_xdr_error_enum(&xdr);

    assert_eq!(cases.len(), 2);
    // base_offset = 0 + 1 - 1 = 0, leaf codes 1,2 → 1,2
    assert_eq!(cases[0].name, "Wrapped_InnerA");
    assert_eq!(cases[0].value, 1);
    assert_eq!(cases[1].name, "Wrapped_InnerB");
    assert_eq!(cases[1].value, 2);
}

#[test]
fn long_doc_string() {
    let doc = "This is a very long documentation string that tests XDR encoding \
               of longer content to make sure padding works correctly even with \
               content that spans many bytes and requires proper alignment.";

    const TREE: &[SpecNode] = &[SpecNode {
        code: 1,
        name: "Variant",
        description: "This is a very long documentation string that tests XDR encoding \
                      of longer content to make sure padding works correctly even with \
                      content that spans many bytes and requires proper alignment.",
        children: &[],
    }];

    const SIZE: usize = xdr_error_enum_size(
        "LongDoc",
        "enum-level doc",
        TREE,
    );
    let xdr: [u8; SIZE] = build_error_enum_xdr("LongDoc", "enum-level doc", TREE);

    let (_, enum_doc, _, name, cases) = parse_xdr_error_enum(&xdr);
    assert_eq!(name, "LongDoc");
    assert_eq!(enum_doc, "enum-level doc");
    assert_eq!(cases.len(), 1);
    assert_eq!(cases[0].doc, doc);
}
