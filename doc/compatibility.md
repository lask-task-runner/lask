# Lask Compatibility and Migration Policy

This document defines the criteria for determining compatibility with respect to specification changes in Lask, and the policy for staged migration from the specification of the current implementation to this specification.
The language specification itself is documented in `spec.md`. References to chapter and section numbers in this document (e.g., 11.3, Chapter 14) refer to the chapter numbers of `spec.md` unless otherwise noted.

## 1. Purpose and Scope

The purpose of this document is to balance specification evolution with user protection.

Effective date:

- Until the first release, all features are treated as being in the `experimental` stage (Chapter 4), and breaking changes may be introduced without migration procedures.
- The compatibility guarantees of this document (the procedures and constraints in Chapters 5-9) take effect for `stable` features from the first release onward.

Scope:

- Language syntax (Chapters 3, 6)
- Type system and static semantics (Chapters 4, 7)
- Dynamic semantics and execution environments (Chapters 8, 10)
- CLI, observability, serialization, and the error system (Chapters 11, 12, 13, 14)
- Built-in library (Chapter 15)

This document prescribes backward compatibility requirements for implementations, tooling, and operational guides.

## 2. Compatibility Levels

Specification changes must be classified into the following compatibility levels.

- `compatible`: Changes that do not break the meaning, types, or execution results of existing code
- `source-breaking`: Changes that require source modifications
- `behavior-breaking`: Changes where execution results or failure classification change even with identical source
- `tooling-breaking`: Changes that break external contracts such as LSP/CLI/serialization

When multiple levels apply, the strictest level is used.

## 3. Compatibility Criteria

Compatibility is determined from the following perspectives.

1. Syntax compatibility
2. Type compatibility
3. Behavioral compatibility
4. External contract compatibility

Syntax compatibility:

- Existing legal syntax must not be made illegal.
- When changing the expansion target of syntactic sugar, the post-expansion semantics must be equivalent.

Type compatibility:

- Existing well-typed programs must not produce type errors under the same conditions.
- Signature changes to existing functions in the built-in library are prohibited in principle.

Behavioral compatibility:

- The success/failure classification and the exit code classification (11.3, 14.8) must not change.
- When changing a failure code, a compatibility alias or migration rule must be provided.

External contract compatibility:

- CLI options, JSON fields, and the minimum requirements for execution events must not be broken.
- Non-removable required fields must not be reduced.

## 4. Staged Introduction Policy

It is recommended that changes be introduced in the following stages.

1. `experimental`: Enabled only via explicit opt-in (flags, settings)
2. `preview`: Disabled by default, but intended for real-world validation
3. `stable`: Enabled by default, subject to compatibility guarantees

Rules:

- `experimental` features may undergo breaking changes before stabilization.
- When promoting from `preview` to `stable`, the compatibility impact must be documented.
- Breaking changes to `stable` features must not be introduced without following the procedures in Chapter 5 of this document (Deprecation and Removal).

Implementations should make the feature state identifiable via the CLI or diagnostic output.

## 5. Deprecation and Removal

Deprecation is carried out according to the following procedure.

1. Deprecation notice (specification, CLI help, release notes)
2. Presentation of alternatives (notation, functions, options)
3. Migration grace period of at least one release cycle
4. Removal and update of the migration guide

Rules:

- During the deprecation period, existing features must be kept in a functioning state.
- When a deprecated feature is used, a warning may be output, but it must not fail by default.
- Upon removal, a corresponding error code and remediation guidance must be provided.

## 6. Migration Guide Requirements

For each breaking change, a migration guide containing at least the following must be provided.

- Background and purpose of the change
- Scope of impact (syntax/types/execution/CLI/serialization)
- Side-by-side examples of old and new notation
- Rules that allow automatic conversion (where possible)
- Cases requiring manual fixes
- Rollback procedure or compatibility mode

Examples must be written uniformly in the current environment notation, including `#local`, `#docker(...)`, `#remote(...)`, and the syntactic sugar `#image-name`.

## 7. Compatibility Verification

As compatibility verification, implementations should continuously perform the following.

- Re-evaluation of sample sets from previous versions (syntax, types, execution)
- CLI contract tests (exit codes, stdout/stderr format)
- Execution event JSON schema conformance tests
- Tests that deprecated-feature warnings are triggered

Verification results must include at least the following.

- Compatible/incompatible determination
- List of breaking differences
- Recommended migration deadline

## 8. Operational Procedure for Specification Changes

Specification change proposals are operated in the following order.

1. Registration of the change proposal (target chapters, compatibility level, expected impact)
2. Impact analysis on types, semantics, CLI, and serialization
3. Update of samples and the migration guide
4. Execution of compatibility verification
5. Publication of release notes and the deprecation plan

Acceptance criteria:

- If the compatibility level is not `compatible`, the change must not be adopted without migration information.
- The error system of Chapter 14 and the exit code mapping of 11.3 must not be broken.
- The minimum required items for observability (Chapter 12) and serialization (Chapter 13) must be maintained.

## 9. Stability of the Built-in Library and Error Codes

Built-in library (specification Chapter 15):

- Backward compatibility of existing symbols is maintained.
- The types and basic semantics of existing functions must not be changed incompatibly.
- Adding new functions is considered backward compatible.
- When deprecating, it is desirable to provide a migration grace period of at least one release cycle.

Error codes (specification Chapter 14):

- Existing codes must not be reused in a way that breaks their meaning.
- For compatibility, deprecation is preferred over removal.
