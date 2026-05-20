# Casa

Casa is a self-hosted programming language and compiler. Its glossary captures project-specific terms that prevent documentation drift.

## Standard library

**IoError**:
The single error enum for all fallible OS syscall operations (file, directory, environment). Wraps errno values into named variants (`NotFound`, `PermissionDenied`, `AlreadyExists`, `IsDirectory`, `NotDirectory`, `BadFd`, `NotEmpty`, `Other(int)`).
_Avoid_: FileError, DirError, OsError

**os module**:
The stdlib module (`lib/os.casa`) that consolidates all OS syscall wrappers: file I/O (`impl file`), directory operations (`impl dir`), environment variable access (`impl env`), path manipulation (`impl path`), and file metadata (`FileStat`).
_Avoid_: putting OS operations in std.casa

**FileStat**:
A struct returned by `file::stat` containing file metadata fields (`size`, `mode`, `mtime`, `atime`, `ctime` as raw integers) with helper methods for type checks (`is_dir`, `is_file`, `is_symlink`) and permission checks (`is_readable`, `is_writable`, `is_executable`).
_Avoid_: stat buffer, metadata tuple

## Language

### Compiler architecture

**Functional compiler pass**:
A compiler phase that takes its context as explicit input and returns its output, diagnostics, and updated context explicitly while keeping mutation local to phase-internal builders.
_Avoid_: Pure compiler rewrite, immutable compiler

**Typecheck result**:
The explicit output of typechecking: updated symbols, checked global stack effect, and any resolved operation changes produced during typechecking.
_Avoid_: Hidden typechecker side effects, global typechecker state

**Pass result**:
An explicit compiler phase output struct used only when a phase has multiple meaningful outputs.
_Avoid_: Wrapper struct, single-field result

**Compiler dependency**:
An explicit state value returned from a compiler pass when later passes need that state, instead of passing a phase-owned object across boundaries.
_Avoid_: Leaking parser object, omnibus context

**Parse-and-resolve boundary**:
The first explicit compiler boundary that keeps parser internals private while returning resolved operations and symbols needed by later phases.
_Avoid_: Parser result, resolver-owned parser

**Default parser**:
The current process-global parser instance used as implicit compiler state before explicit pass boundaries replace it.
_Avoid_: Shared compiler context, hidden parser dependency

**Compiler diagnostics schema**:
The shared representation and flow for compiler diagnostics across lexer, parser, typechecker, and later phases.
_Avoid_: Typechecker-only diagnostics refactor, phase-local error schema

### Type representation

**Type AST**:
The compiler-owned structural representation of a Casa type after parsing.
_Avoid_: Type string, source type text

**Source type syntax**:
The user-written type expression before the parser converts it into the **Type AST**.
_Avoid_: Type annotation metadata, internal type string

### Documentation terminology

**Documentation glossary**:
The project-specific vocabulary in this file for concepts whose names must stay stable across docs, code review, and issues.
_Avoid_: Language keyword reference, complete syntax catalog

**Operand order**:
The Casa rule that maps consumed stack values to operation operands: the topmost stack value is the first argument, with arithmetic operators as the explicit exception.
_Avoid_: Evaluation order, precedence

**Stack effect**:
The type-only input and output shape of the Casa value stack for a function, operator, intrinsic, or expression.
_Avoid_: Function signature, operator signature

**Function type**:
The `fn[...]` type form whose brackets contain a **Stack effect**.
_Avoid_: Function signature

**Function declaration**:
The written `fn` header that gives a function's name, type variables, parameter names and types, and return types without its body.
_Avoid_: Function signature

**Method declaration**:
The written `fn` header inside an `impl` or `trait` block that gives a method's name, type variables, parameter names and types, and return types without its body.
_Avoid_: Method signature

**Function definition**:
A **Function declaration** together with its body.
_Avoid_: Function signature

**Method definition**:
A **Method declaration** together with its body.
_Avoid_: Method signature

### Traits

**Trait method requirement**:
A bodyless `fn` line in a `trait` block that an implementing type must satisfy.
_Avoid_: Required method signature

**Default method definition**:
A method body in a `trait` block that is available when a type implements trait and does not provide its own **Method definition**.
_Avoid_: Default method signature

**Implements trait**:
A type has available methods whose names and **Stack effects** satisfy a trait under Casa's structural trait rules.
_Avoid_: `impl Trait for Type`, nominal trait implementation

### Bootstrap

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
- A **Functional compiler pass** may use local mutation internally, but pass boundaries should make compiler context and diagnostics explicit.
- A **Pass result** should be introduced only when the compiler phase returns more than one meaningful output; single-output phases should return the value directly.
- A **Compiler dependency** should be returned as its own value only when a later boundary uses it now; unused phase-private state should stay private until needed.
- The **Parse-and-resolve boundary** should hide `Parser` and return only resolved operations plus symbols until parsing and identifier resolution can be split cleanly.
- The **Default parser** should trend toward zero use as explicit pass boundaries mature; if a slice can remove it fully, it should.
- A **Typecheck result** may return the same **SymbolStore** reference it received, as long as mutations are represented at the pass boundary.
- The **Compiler diagnostics schema** should be refactored once across the compiler, not as part of the first **Typecheck result** boundary.
- The **Documentation glossary** names project concepts that prevent drift; language keywords and ordinary programming concepts belong in reference docs.
- Function, operator, intrinsic, and expression docs should use **Stack effect**; **Operand order** explains how stack values map to operands.
- Public reference docs should use one **Stack effect** line for an operation instead of separate signature and stack-effect lines.
- **Stack effect** contains types and optional type bounds only; semantic operand names belong in concise prose only when they add information not derivable from the types.
- In **Stack effect** notation, input types are listed from topmost consumed value downward; output types are listed in push order, so the last output type becomes topmost after the operation.
- `int str -> bool char` means `int` is consumed from the top of the stack, `str` is consumed below it, `bool` is pushed first, and `char` is pushed last/topmost.
- `None` in **Stack effect** notation means no stack values, not a Casa type and not `Option::None`.
- **Stack effect** notation should write `None -> T` for no inputs and `T -> None` for no outputs.
- Generic bounds appear before the **Stack effect**, such as `[T: Display] T -> None`; bounds are constraints, not stack values.
- Only constrained type variables appear in the bounds prefix; unbounded type variables that appear in stack types are not repeated there.
- Unbounded type variables should not use a bracket prefix in **Stack effect** notation; write `T -> T T`, not `[T] T -> T T`.
- A **Function type** contains a **Stack effect** inside `fn[...]`; the whole `fn[...]` form is not called a signature.
- Lambda docs should use **Function type** for the lambda's type and **Stack effect** for what the lambda consumes and produces.
- Existing diagnostic names such as `SIGNATURE_MISMATCH` may remain until an internal rename, but explanatory prose should describe declared and inferred **Stack effects**.
- In normal calls, the topmost stack value maps to the first argument; avoid describing this as "rightmost" because stack values may have existed before the immediate call expression.
- Arithmetic operators use the same **Stack effect** notation as other operations, but their **Operand order** maps the topmost consumed value to the right operand instead of the first/left operand.
- Comparison operators follow normal **Operand order**; for `a b <`, `b` is topmost and therefore the left operand, so the expression means `b < a`.
- A **Function declaration** excludes the body; a **Function definition** includes the body.
- A **Method declaration** excludes the body; a **Method definition** includes the body.
- A **Trait method requirement** is satisfied by a matching method name and **Stack effect** on the implementing type.
- A **Default method definition** belongs to the trait and is available to types that implement trait while omitting their own **Method definition**.
- A type implements trait structurally; Casa has no `impl Trait for Type` syntax.
- A **Function declaration** describes written source only, not compiler-injected hidden parameters.
- A **Bootstrap compiler** builds exactly one **Branch compiler** at the start of CI.
- A **Branch compiler** must self-compile and reach a **Fixed point** before the branch is considered releasable.
- A **Temporary compiler release** must not replace the **Bootstrap compiler** as the normal PR CI input.
- A **Temporary compiler release** is valid only as part of **Staged bootstrap repair** with explicit cleanup back to a stable **Bootstrap compiler**.
- A **Bootstrap override** must be visible in CI and must not be enabled by default.
- A **bootstrap-override label** is the only normal way to enable a **Bootstrap override** on a pull request.
- **casa-release.env** is the single tracked source for the **Bootstrap compiler** release tag used by CI.
- The **Bootstrap policy check** validates both the **casa-release.env** tag name and GitHub release metadata.
- All OS syscall wrappers live in the **os module**, not in `std.casa`.
- All fallible OS operations return `Result[T IoError]`; **IoError** is the single error type for file, directory, and environment failures.
- **FileStat** is returned by `file::stat` and provides both raw metadata fields and convenience query methods.
- `env::get` returns `Option[str]`, not `Result` — a missing environment variable is absence, not an error.

## Example Dialogue

> **Dev:** "Can this PR depend on a temporary release so CI passes?"
> **Domain expert:** "No. By default, the **Bootstrap compiler** must build the **Branch compiler** from the latest stable release, and that compiler must reach a **Fixed point**."

> **Dev:** "Should I document `fn foo a:int b:str -> bool` as the signature?"
> **Domain expert:** "No. That is the **Function declaration**. The public stack contract is the **Stack effect**: `int str -> bool`, where `int` is consumed from the top of the stack and `str` below it."

## Flagged Ambiguities

- "`Op.type_annotation` / `Op.deferred_return_type` as source text" was used to justify keeping parsed type metadata as strings. Resolved: user-written type expressions are **Source type syntax** only before parsing; after parsing, compiler-owned metadata should use the **Type AST**.
- "functional programming concepts" was broad enough to imply a full immutable rewrite. Resolved: the target is **Functional compiler pass** boundaries, with local mutation still allowed inside phases.
- "`LexResult`" was proposed as a first slice even though it would only wrap `List[Token]`. Resolved: avoid single-field **Pass result** structs; `lex_file` should keep returning tokens until lexing has multiple explicit outputs.
- "`ParseResult` returning `Parser`" leaked a parser-owned object past parsing, while returning every parser field exposed unused state. Resolved: return only the **Compiler dependencies** later boundaries use now, and migrate call sites instead of keeping the old API.
- Parse and identifier resolution both need import state today, so a standalone parse result is premature. Resolved: start with a **Parse-and-resolve boundary** that keeps import state private.
- "`DEFAULT_PARSER`" was treated as convenient shared context. Resolved: call it the **Default parser** and remove uses as explicit pass boundaries replace hidden compiler state.
- "return updated SymbolStore" could imply deep-copying the symbol table. Resolved: **Typecheck result** may return the same reference after mutation; explicit pass output is the important boundary.
- "typechecker diagnostics" was treated as a typechecker-specific refactor. Resolved: diagnostics belong to a compiler-wide **Compiler diagnostics schema** refactor, tracked in issue #219.
- "glossary" was considered as a complete keyword or syntax catalog. Resolved: the **Documentation glossary** covers only project-specific concepts whose terminology must stay stable.
- "function signature" and "stack effect" were treated as interchangeable in docs. Resolved: use **Stack effect** for public stack contracts; reserve "signature" only where the compiler's internal function type model is meant.
- "`fn foo a:int b:str -> bool`" was called a signature. Resolved: call it a **Function declaration** when bodyless, and a **Function definition** when paired with a body.
- "top on right" was used to explain **Stack effect** notation. Resolved: inputs are topmost-first and outputs are push-order.
- "`None`" in **Stack effect** notation can be confused with `Option::None`. Resolved: `None` means no stack values in notation only.
- "release compiler" was used to mean both the stable compiler downloaded by CI and an ad hoc temporary compiler. Resolved: use **Bootstrap compiler** for the stable CI input; temporary releases are exceptional escape hatches.
- "temporary release" was considered as a normal CI mechanism. Resolved: use **Temporary compiler release** only as an exception, not as the default bootstrap path.
