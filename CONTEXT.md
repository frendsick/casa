# Casa

Casa is a self-hosted programming language and compiler. Its CI vocabulary describes how compiler changes are proven bootstrappable and stable.

## Language

**Type AST**:
The compiler-owned structural representation of a Casa type after parsing.
_Avoid_: Type string, source type text

**Source type syntax**:
The user-written type expression before the parser converts it into the **Type AST**.
_Avoid_: Type annotation metadata, internal type string

**Bootstrap compiler**:
The latest stable released `casac` binary used to compile the compiler source on a branch.
_Avoid_: Temporary compiler, release compiler

**Branch compiler**:
The `casac` binary built from the current branch by the **Bootstrap compiler**.
_Avoid_: Stage compiler

**Fixed point**:
The state where recompiling the compiler with itself produces equivalent assembly output.
_Avoid_: Idempotence

**Temporary compiler release**:
An exceptional prerelease `casac` asset used only to unblock CI during staged bootstrap repair.
_Avoid_: CI bootstrap path, normal release

**Staged bootstrap repair**:
A sequence of changes that restores compatibility with the latest stable **Bootstrap compiler** after a branch crosses the compatibility line.
_Avoid_: Permanent temporary release

**Bootstrap override**:
An explicit CI exception that permits a **Temporary compiler release** during **Staged bootstrap repair**.
_Avoid_: Hidden fallback, alternate bootstrap path

**bootstrap-override label**:
The pull request label that makes a **Bootstrap override** visible and intentional.
_Avoid_: Override flag, magic branch

**casa-release.env**:
The tracked environment file that names the stable Casa release used as the **Bootstrap compiler**.
_Avoid_: Duplicated workflow tag, repository variable

**Bootstrap policy check**:
The CI check that rejects prerelease or temporary **Bootstrap compiler** tags unless a **Bootstrap override** is present.
_Avoid_: Release lint, tag lint

## Relationships

- **Source type syntax** is parsed into the **Type AST** before compiler analysis.
- Compiler analysis should operate on the **Type AST**, not reparsed or reformatted type strings.
- A **Bootstrap compiler** builds exactly one **Branch compiler** at the start of CI.
- A **Branch compiler** must self-compile and reach a **Fixed point** before the branch is considered releasable.
- A **Temporary compiler release** must not replace the **Bootstrap compiler** as the normal PR CI input.
- A **Temporary compiler release** is valid only as part of **Staged bootstrap repair** with explicit cleanup back to a stable **Bootstrap compiler**.
- A **Bootstrap override** must be visible in CI and must not be enabled by default.
- A **bootstrap-override label** is the only normal way to enable a **Bootstrap override** on a pull request.
- **casa-release.env** is the single tracked source for the **Bootstrap compiler** release tag used by CI.
- The **Bootstrap policy check** validates both the **casa-release.env** tag name and GitHub release metadata.

## Example Dialogue

> **Dev:** "Can this PR depend on a temporary release so CI passes?"
> **Domain expert:** "No. By default, the **Bootstrap compiler** must build the **Branch compiler** from the latest stable release, and that compiler must reach a **Fixed point**."

## Flagged Ambiguities

- "`Op.type_annotation` / `Op.deferred_return_type` as source text" was used to justify keeping parsed type metadata as strings. Resolved: user-written type expressions are **Source type syntax** only before parsing; after parsing, compiler-owned metadata should use the **Type AST**.
- "release compiler" was used to mean both the stable compiler downloaded by CI and an ad hoc temporary compiler. Resolved: use **Bootstrap compiler** for the stable CI input; temporary releases are exceptional escape hatches.
- "temporary release" was considered as a normal CI mechanism. Resolved: use **Temporary compiler release** only as an exception, not as the default bootstrap path.
