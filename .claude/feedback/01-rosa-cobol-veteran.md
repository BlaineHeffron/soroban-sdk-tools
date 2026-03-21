---
agent: Rosa Delgado
background: Retired COBOL programmer with 40 years on IBM mainframes at Citibank, now auditing open-source smart contracts for fun
date: 2026-03-21
---

# Review by Rosa Delgado

## Overall Impression

I have spent four decades reading code that moves real money -- batch settlement systems, wire transfer engines, CICS transaction programs. After forty years, I know one thing: the code that survives is the code that can be read by the person who did not write it, at 2 AM, during an incident.

This codebase has good bones. The separation of concerns between the "Internal" trait (business logic) and the outer trait (auth enforcement) reminds me of how we separated validation subroutines from processing paragraphs in COBOL. But some of the readability choices concern me.

## Strengths

1. **The two-trait split is instantly comprehensible.** When I look at the trait-test example, I can see that `OwnableInternal` is the "what" and `Ownable` is the "who is allowed." In COBOL terms, this is like separating the PROCEDURE DIVISION from the security PARAGRAPH. Anyone reading this at 2 AM will immediately understand the architecture.

2. **The example file reads top-to-bottom.** `lib.rs` in the trait-test example follows a natural narrative: define the trait, implement the provider, compose the contract, test. This is exactly how a COBOL program flows -- IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE. Do not underestimate how important this ordering is.

3. **Method names are verbs acting on nouns.** `transfer_ownership`, `is_paused`, `pause`, `unpause` -- these read like English. In my era we called this "self-documenting code" and it was considered the highest craft.

4. **The sealed macro pattern is a safety net I wish we had.** In mainframe systems, we used to have "protected copy members" that could not be modified by application programmers. `impl_ownable!` serves the same purpose -- it prevents the junior developer from accidentally removing the security check.

## Concerns

1. **The `__auth_addr` variable name is a code smell.** Double underscores are a convention for "compiler internals, do not touch." But this variable appears in generated code that developers will see in error messages and debugger output. When something goes wrong at 2 AM, the operator needs to understand every symbol. Call it `authorized_caller` or `auth_resolved_address`. Be explicit. Be kind to the person who comes after you.

2. **The `alloc_alias` trick (`__alloc_{Trait}`) is clever but opaque.** `extern crate alloc as __alloc_Ownable` -- I had to read the macro three times to understand why this exists (namespace collision avoidance when multiple traits are composed). A comment at the generation site explaining this would save the next reader thirty minutes.

3. **The blog post uses "we" and "our" but the code has no inline documentation.** The `contract.rs` file has excellent module-level doc comments, but individual functions like `build_delegate_args` and `extract_return_types` have no doc comments explaining their purpose. The module comment says what the macro does; the function comments should say how.

4. **Error messages in the macro are terse.** `"expected Self::method_name or a parameter name"` -- this is correct but not helpful. What did the user actually write? What should they write instead? Show an example in the error message. In COBOL, every ABEND code had a corresponding manual entry with examples. The error message IS your documentation for the failure case.

5. **Magic strings in the example.** `Symbol::new(env, "owner")` and `Symbol::new(env, "paused")` are string literals used as storage keys. In forty years I have seen string-key collisions cause more production incidents than any other bug class. The `#[contractstorage]` macro mentioned in the OZ comparison document should be shown in the example, not raw string keys.

## Suggestions

1. Add a "Reading Guide" comment block at the top of `contract.rs` that says: "This file is organized in five sections: auth parsing, method extraction, inner trait generation, outer trait generation, and client generation. Start at `contracttrait_impl` at the bottom and work upward."

2. Every generated identifier (`__auth_addr`, `__alloc_*`, `_clone`, `_try_clone`) should be documented in a single "Generated Names" table in the module doc comment. When a developer sees these in compiler errors, they need a reference.

3. The `to_snake_case` function handles basic PascalCase but will fail on acronyms like `HTTPServer` (produces `h_t_t_p_server`). This is the kind of edge case that bites you five years later when someone names a trait `DAOGovernance`. Test it. Document its limitations.

4. Consider adding `#[must_use]` to the `CallBuilder` return from `AuthClient` methods. A forgotten `.invoke()` call is a silent bug -- the authorization is set up but never executed.

## Unique Perspective

In 1987, I wrote a COBOL copybook that was included in over 400 programs across Citibank's wire transfer system. It validated SWIFT message fields. I learned that any reusable code module must optimize for one thing above all else: the speed at which a stranger can understand it during a crisis.

This codebase is building exactly that kind of reusable module -- one that will be included in many contracts, handling authorization for real money. The macro generates code that developers will never read... until something goes wrong. And then they will read every line, under pressure, with money on the line.

Every generated variable name, every error message, every comment that was not written -- these are debts that will be paid during an incident. Pay them now, while it is cheap.

## Would I Use This?

If I were still building systems, yes -- but only after the generated code is made more readable and every error message includes an example of the correct usage. The architecture is sound. The discipline of separating auth from logic is exactly right. The readability of the generated output needs the same care that went into the macro itself.
