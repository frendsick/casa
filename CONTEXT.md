# Casa

Casa is a self-hosted programming language and compiler. Its glossary captures project-specific terms that prevent documentation drift.

## Standard library

**IoError**:
The single error enum for all fallible OS syscall operations (file, directory, environment). Wraps errno values into named variants (`NotFound`, `PermissionDenied`, `AlreadyExists`, `IsDirectory`, `NotDirectory`, `BadFd`, `NotEmpty`, `Other(i64)`).
_Avoid_: FileError, DirError, OsError

**os module**:
The stdlib module (`lib/os.casa`) that consolidates all OS syscall wrappers: file I/O (`impl file`), directory operations (`impl dir`), environment variable access (`impl env`), path manipulation (`impl path`), and file metadata (`FileStat`).
_Avoid_: putting OS operations in std.casa

**FileStat**:
A struct returned by `file::stat` containing file metadata fields (`size`, `mode`, `mtime`, `atime`, `ctime` as raw integers) with helper methods for type checks (`is_dir`, `is_file`, `is_symlink`) and permission checks (`is_readable`, `is_writable`, `is_executable`).
_Avoid_: stat buffer, metadata tuple

## Language

### Design identity

**Composition-first programming**:
Casa programs are built primarily by composing small, statically typed functions through the value stack. New features should strengthen that style without sacrificing a minimal language or fast compilation.
_Avoid_: General-purpose feature parity, syntax-first design

### Compiler architecture

**Functional compiler pass**:
A compiler phase that takes its context as explicit input and returns its output, diagnostics, and updated context explicitly while keeping mutation local to phase-internal builders.
_Avoid_: Pure compiler rewrite, immutable compiler

**Typecheck result**:
The explicit output of typechecking: updated symbols, checked global stack effect, and any resolved operation changes produced during typechecking.
_Avoid_: Hidden typechecker side effects, global typechecker state

**Operation semantics**:
The phase-independent meaning of a resolved operation: its typed-stack transition, canonical rewrite or dispatch target, and declaration, type, or trait dependencies.
_Avoid_: Typechecker-only operation meaning, selective-import simulation

**Lex result**:
The explicit output of lexing: a structurally usable token stream plus any recoverable lexical diagnostics.
_Avoid_: File read result, generic token pass result

**Pass result**:
A phase-specific compiler output struct used only when a phase has multiple meaningful outputs.
_Avoid_: Generic pass wrapper, single-field result

**Compiler dependency**:
An explicit state value returned from a compiler pass when later passes need that state, instead of passing a phase-owned object across boundaries.
_Avoid_: Leaking parser object, omnibus context

**Compiler source store**:
The compiler dependency mapping file paths to the exact source text used by a compilation, including imported files and in-memory document overrides.
_Avoid_: global source cache, diagnostics cache

**Parse-and-resolve boundary**:
The first explicit compiler boundary that keeps parser internals private while returning resolved operations and symbols needed by later phases.
_Avoid_: Parser result, resolver-owned parser

**Default parser**:
The current process-global parser instance used as implicit compiler state before explicit pass boundaries replace it.
_Avoid_: Shared compiler context, hidden parser dependency

**Compiler diagnostics schema**:
The data-only representation and flow for compiler diagnostics across lexer, parser, typechecker, and later phases. Each phase owns recovery; callers decide whether usable output permits the next phase.
_Avoid_: Typechecker-only diagnostics refactor, phase-local error schema

**Analysis result**:
The shared front-end compiler output containing exact compiler sources, collected diagnostics, and optional typechecked output for CLI, LSP, and tests.
_Avoid_: CLI compile result, LSP compiler snapshot, global compiler result

**Array literal**:
A bracket-delimited list of values (`[1, 2, 3]`, `["a", "b"]`, `[[1], [2]]`, `[my_fn]`) that produces an independent owned `array[T N]`, where the element count `N` is part of the type. Elements may be primitive literals, enum variants, nested array literals, function references, lambda expressions, or struct literals. Storage placement and behavior-preserving sharing remain compiler decisions.
_Avoid_: Static array, const array, inline array

**Text view**:
A `str` value that provides immutable UTF-8 access without owning or freeing its storage. String literals are static text views. `String.as_str` borrows owned text as a text view without allocation.
_Avoid_: Immutable string, owned str

**Owned text**:
A non-`Copy` `String` value that owns growable UTF-8 storage, moves by default, and releases dynamic storage during destruction.
_Avoid_: StringBuilder, growable str

### Type representation

**Type AST**:
The compiler-owned structural representation of a Casa type after parsing.
_Avoid_: Type string, source type text

**Source type syntax**:
The user-written type expression before the parser converts it into the **Type AST**.
_Avoid_: Type annotation metadata, internal type string

**Fully parameterized type**:
A generic type where all type parameters are specified, either as concrete types or as declared type variables. `Option[i64]` and `Option[T]` (where `T` is declared) are fully parameterized; bare `Option` is not.
_Avoid_: Raw type, unparameterized generic

**Unresolved type variable**:
A `Type::TypeVar` node in the **Type AST** that has not yet been bound to a concrete type during type inference. The type unifier treats types containing unresolved type variables as flexible; fully resolved types must match structurally.
_Avoid_: Unknown type, type placeholder (which means `TYPE_UNKNOWN_T`)

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
A type explicitly implements a trait with `impl Type: Trait`, and its available methods satisfy every required **Stack effect**.
_Avoid_: Accidental implementation from matching method names

**Language trait method**:
A method that a reserved language-integrated trait must provide with a compiler-validated **Stack effect**. Name its role directly, such as operator method or formatting method.

**Trait implementation rules**:
The rules that control where a trait implementation may be declared and reject duplicate or overlapping implementations.

**Borrowed value**:
The live `T` accessed through a shared `$T` or exclusive `mut$T` borrow.

**Safety comment**:
A `# SAFETY:` comment immediately before an `unsafe` block or `unsafe fn`. A
block comment states the concrete invariants that make its unchecked operations
valid. A function comment states the caller contract.
_Avoid_: Operation description, generic unsafe warning

**Compiler-called cleanup method**:
The reserved inherent `drop` method that the compiler calls during destruction and source code cannot call directly.

### Bootstrap

**Stable Casa release**:
An official, non-prerelease Casa version that distributes the `casac` compiler for general use.
_Avoid_: Bootstrap release, Stable bootstrap compiler (as a release identity)

**Bootstrap compiler**:
The `casac` binary used to compile the compiler source on a branch. This is a CI role, normally filled by a **Stable Casa release**.
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
- Generic types must be **Fully parameterized types** at all compiler boundaries; bare generic names are not valid type expressions.
- The type unifier treats types containing **Unresolved type variables** as flexible; fully resolved types must match structurally.
- Whether a type is "still flexible" is determined by the presence of `Type::TypeVar` nodes, not by checking whether the base name is a known enum or struct.
- A **Functional compiler pass** may use local mutation internally, but pass boundaries should make compiler context and diagnostics explicit.
- Within a **Functional compiler pass**, diagnostics collection belongs to the phase-owned implementation; nested helpers report through that owner instead of compiler-global state.
- A **Pass result** should be introduced only when the compiler phase returns more than one meaningful output; single-output phases should return the value directly.
- A **Pass result** names its phase outputs directly; a generic result abstraction requires repeated identical pass semantics that reduce caller knowledge.
- A **Lex result** always contains tokens; file loading remains a separate fallible OS operation returning **IoError**.
- A **Compiler dependency** should be returned as its own value only when a later boundary uses it now; unused phase-private state should stay private until needed.
- The **Compiler source store** is separate from the **Compiler diagnostics schema**; diagnostics carry locations, while reporting adapters use the store to resolve source text.
- CLI, LSP, and tests share one compiler-analysis seam returning an **Analysis result**; root source is explicit input, while root file I/O remains owned by the caller.
- The **Parse-and-resolve boundary** should hide `Parser` and return only resolved operations plus symbols until parsing and identifier resolution can be split cleanly.
- The **Parse-and-resolve boundary** returns partial output only after recovery at a known structural delimiter; ambiguous parser state produces no usable output.
- The **Default parser** should trend toward zero use as explicit pass boundaries mature; if a slice can remove it fully, it should.
- A **Typecheck result** may return the same **SymbolStore** reference it received, as long as mutations are represented at the pass boundary.
- **Operation semantics** are computed once below the **Parse-and-resolve boundary** and typechecking; each phase consumes data-only facts while keeping its mutable state private.
- A **Typecheck result** with diagnostics may remain usable by editor adapters, but any type error prevents bytecode compilation.
- Bytecode compilation runs only after error-free typechecking and produces either a complete program or an internal compiler failure; user-facing validation belongs in earlier phases.
- The **Compiler diagnostics schema** should be refactored once across the compiler, not as part of the first **Typecheck result** boundary.
- A **Functional compiler pass** may return partial output after recoverable diagnostics only when that phase guarantees the output remains usable; unrecoverable diagnostics produce no usable output.
- Unrecoverable phase state is represented by the **Pass result**, not by a separate diagnostic severity.
- Diagnostics produced by completed or recoverable work remain visible regardless of severity; unusable output prevents later compiler phases and therefore their diagnostics.
- Imported-file diagnostics join the same compilation diagnostic stream at the import encounter point, preserving emission order; core diagnostics are not sorted by file or severity.
- A failed import ends the **Parse-and-resolve boundary** with no usable output after its diagnostics are merged; import expansion and identifier resolution do not continue with incomplete symbols.
- Recording a diagnostic does not control compiler flow; fallible phase helpers represent missing or failed values explicitly with `Option` or `Result`.
- Diagnostics collection does not decide whether to print, exit, or run another compiler phase.
- The **Compiler diagnostics schema** preserves emission order in one diagnostic list; error and warning variants encode severity without a separate severity field while retaining their different payload shapes.
- CLI reporting, LSP conversion, and test inspection are adapters outside the **Compiler diagnostics schema**.
- Diagnostics migration is complete only when compiler-global diagnostics, source, and mode state is deleted and all adapters consume explicit results; snapshot or copy bridges do not satisfy the deletion test.
- Each evaluation of an **Array literal** has independent owned-value semantics. The compiler may share or statically emit backing data when mutation and destruction still behave independently. Raw address equality is a representation detail.
- A **Text view** never releases storage. An **Owned text** value releases its storage exactly once.
- Converting **Owned text** to a **Text view** borrows without allocation. Converting a **Text view** to **Owned text** allocates and copies.
- The **Documentation glossary** names project concepts that prevent drift; language keywords and ordinary programming concepts belong in reference docs.
- Function, operator, intrinsic, and expression docs should use **Stack effect**; **Operand order** explains how stack values map to operands.
- Public reference docs should use one **Stack effect** line for an operation instead of separate signature and stack-effect lines.
- **Stack effect** contains types and optional type bounds only; semantic operand names belong in concise prose only when they add information not derivable from the types.
- In **Stack effect** notation, input types are listed from topmost consumed value downward; output types are listed in push order, so the last output type becomes topmost after the operation.
- `i64 str -> bool char` means `i64` is consumed from the top of the stack, `str` is consumed below it, `bool` is pushed first, and `char` is pushed last/topmost.
- `None` in **Stack effect** notation means no stack values, not a Casa type and not `Option::None`.
- **Stack effect** notation should write `None -> T` for no inputs and `T -> None` for no outputs.
- Generic bounds appear before the **Stack effect**, such as `[T: Display] T -> None`; bounds are constraints, not stack values.
- Multiple bounds on one type variable use `+`, such as `[T: Copy + Display]`; every listed bound is required and their order has no semantic meaning.
- An owned binding is definitely available at a control-flow join only when every continuing incoming path owns or reinitialized it; terminating paths do not participate.
- `ptr` is a nullable, non-owning, `Copy` raw address. Existing load/store intrinsics and pointer arithmetic require `unsafe`; only explicit `ptr::as_ref[T]` and `ptr::as_mut[T]` form typed borrows.
- An inherent method wins over defaults from explicitly implemented traits. Without one, more than one applicable default-method declaration is a compile-time ambiguity rather than a declaration-order choice.
- A trait implementation may be declared only by the module defining the type or the trait; duplicate and overlapping implementations are rejected without specialization.
- First-class function values are monomorphic. A generic named function reference supplies all type arguments explicitly, such as `&id[i64]`; direct generic calls still infer them.
- Every closure is repeatable. Invoking it may consume explicit arguments but may not leave a captured non-`Copy` owner consumed; Casa has no single-use function type.
- Standard `Copy` is a methodless marker extending Clone; it may be used implicitly and never allocates or calls user code. `derives Copy` and `impl Type: Copy { }` establish the same compiler-validated implementation and supply missing fieldwise Clone behavior. A freestanding Copy declaration may omit that supertrait and relationship.
- A Copy type may provide a customized Clone implementation, which takes precedence over the fieldwise fallback. Generated aggregate Clone calls field Clone methods and preserves stored shared borrows with their origins, while implicit Copy remains allocation-free. The implementation author controls explicit Clone cost and semantics.
- Reserved language-integrated traits use minimum compiler-validated **Language trait method** contracts while allowing additional default methods and supertraits. Primitive operations remain available without importing those declarations.
- `!=` lowers to the active equality trait's `ne` operator method. The standard default negates `eq`; overrides must preserve that semantic inverse.
- PartialEq owns the shared `eq` and `ne` operator methods; Eq extends PartialEq as the explicit lawful-total marker. The compiler validates Eq's effective inherited shape, and `derives Eq` implements both traits.
- PartialOrd owns `partial_cmp` and the `lt`, `le`, `gt`, and `ge` operator methods; Ord extends PartialOrd and Eq, adds `cmp`, and provides the inherited `partial_cmp` default. The compiler validates the complete effective inherited shape.
- `Clone` is an explicit, infallible trait operation that may allocate or run user code; allocation failure terminates. Stack operations and implicit reuse never invoke it.
- `Clone` is declared in `std`, not injected by the compiler. The compiler recognizes its canonical identity for explicit `derives Clone` generation and for the fieldwise Clone fallback that standard `Copy` requires. It generates no other Clone bodies.
- `array[T N]` owns exactly `N` elements, stores no length word, implements `Copy` when `T: Copy`, and implements `Clone` when `T: Clone`. A runtime-length range is a separate borrowed view, and `List[T]` remains the growable sequence.
- Standard value owners implement `Clone` when their owned contents do; identity-bearing resources and exclusive borrows do not receive automatic implementations.
- Structs and enums may opt into generated explicit duplication with `derives Clone`; derivation is conditional on every owned field or payload implementing `Clone` and never implies `Copy`.
- Derived trait methods are fallbacks: one handwritten customization block may override generated methods and merge into the same trait implementation. Multiple handwritten implementations remain errors, and source order has no effect.
- Custom `eq` requires explicit `hash` when Hashable is derived and explicit `cmp` when Ord is derived; compiler generation cannot infer those consistency laws.
- Finite recursive owned types may derive Clone. Trait implementation checking resolves recursive obligations as one dependency cycle; runtime cloning traverses and may allocate for the complete structure.
- Finite recursive owned types may also derive Eq, Ord, and Hashable through cycle-aware trait implementation checking. Generated operations recursively traverse finite payloads.
- Recursive destruction initially uses call-stack recursion and preserves the order of the **Compiler-called cleanup method** and reverse-field destruction. Deep-chain tests and benchmarks report the practical stack limit before iterative lowering is considered.
- `Ordering` is the ordinary standard enum `Less`, `Equal`, `Greater`; it is compiler-validated only when generated Ord behavior needs it. Option remains ordinary library code.
- Standard Ordering initially derives Eq and Copy, but not Ord or Hashable.
- A subtrait may provide a matching default body for an inherited bodyless requirement; Ord uses this to adapt `cmp` into PartialOrd's `partial_cmp` without compiler knowledge of Option.
- Trait inheritance must be acyclic. Direct and indirect cycles are rejected before inherited methods or trait satisfaction is computed.
- Diamond inheritance deduplicates a shared ancestor's methods by declaration identity; only distinct competing defaults are ambiguous.
- Implementing a subtrait satisfies every transitive supertrait once inherited requirements are met; an explicit supertrait implementation is reused rather than duplicated.
- A trait declaration is rejected when distinct inherited methods share a name but have incompatible stack effects; Casa does not overload methods.
- One inherited default method satisfies every compatible bodyless requirement; zero bodies require an implementation and multiple distinct bodies require an override.
- A type may implement multiple distinct instantiations of one generic trait; the **Trait implementation rules** identify implementations by receiver type and fully instantiated trait.
- Ambiguous trait implementation method calls use postfix trait qualification such as `token Convert[i64]::convert`; return types never drive overload resolution.
- Ambiguous trait implementation method pointers fully qualify receiver and trait, such as `&Token::Convert[i64]::convert`; unambiguous pointers retain `&Token::method`.
- Inside a trait default, an unqualified call to one of that trait's methods resolves within the same instantiated trait implementation; another trait requires explicit qualification.
- Borrow checking distinguishes statically disjoint named struct fields, while dynamic indexes and other unproven projections conservatively overlap.
- A `mut$self` receiver exclusively borrows the complete object according to its public stack effect; method bodies do not produce narrower inferred field effects.
- A safe function may return multiple exclusive borrows when its body proves them disjoint; callers may rely on that non-aliasing contract.
- Borrows returned through an opaque function keep each complete source input loaned until all derived outputs expire; body-derived field paths are not part of public function types.
- An available `mut$T` implicitly reborrows as `$T` in any shared typed context, suspending exclusive use until the shared reborrow expires; the reverse conversion is forbidden.
- A shared `$T` borrow permits no safe mutation of its reachable owned storage; Casa initially has no interior-mutability container or exception.
- Unsafe raw access must preserve all live borrow invariants; violating lifetime, validity, or aliasing requirements is undefined behavior and receives no runtime check.
- A typed borrow formed from `ptr` may escape only when tied conservatively to existing borrowed inputs; raw pointers never invent an originless returned lifetime.
- An escaping `mut$T` formed from `ptr` requires an exclusive `mut$` input origin; unsafe code cannot upgrade a shared input into mutable access.
- An owner cannot move, be replaced, or be destroyed while any derived borrow remains live; Casa does not automatically pin owners or transfer loans across moves.
- Safe code cannot construct an owner containing a borrow into itself; use offsets or on-demand views instead of pinning and staged initialization.
- `$T` and `mut$T` are always non-null live references; absence uses ordinary enums such as stdlib `Option`, without compiler special-casing.
- Equality, ordering, hashing, and display on a borrow use the **Borrowed value**'s traits rather than observing its address; raw `ptr` equality remains address equality.
- Shared `$T` can be duplicated with `dup` and `over`, but it does not satisfy `Copy` or `Clone`. When `T: Clone`, `$T.clone` calls the **Borrowed value** implementation and returns an owned `T`.
- Method availability follows receiver capability uniformly: `self` requires ownership, `$self` accepts owned/shared/exclusive access, and `mut$self` accepts owned/exclusive access.
- `mut$T` is affine and not Copy. When `T: Clone`, `mut$T.clone` calls the **Borrowed value** implementation through a temporary shared reborrow and returns an owned `T`.
- `ptr::from_ref` safely obtains `ptr` from `$T`; owned and exclusive values may reborrow, and no separate `from_mut` exists because raw pointers have no mutability.
- Unsafe `ptr::into_raw` transfers a heap-indirect owner to its allocation address, and `ptr::from_raw[T]` reconstructs that owner when the caller proves the address is the complete live allocation and has no other owner.
- Unsafe `ptr::read[T]` and `ptr::write[T]` move ownership out of and into initialized generic storage; their caller maintains validity and initialization state.
- Unsafe raw storage uses `u64 alloc -> ptr` and `ptr free -> None`; `free` releases bytes only and never replaces typed destruction.
- `0 alloc` returns `ptr::null`, `ptr::null free` is a no-op, and every positive allocation returns non-null or terminates.
- Casa targets only x86-64 and has no `usize` or `isize`; in-memory sizes and indexes use `u64`, while signed offsets use `i64`.
- `size_of[T] -> u64` is a safe compile-time query for padded inline storage size, enabling dense generic containers without compiler-known collection layouts.
- Ordinary aggregate layout has no stable ABI or persistence contract; `size_of[T]` describes only the current compiler's x86-64 layout.
- `size_of[T]` is the only initial layout query; Casa has no `align_of`, `offset_of`, packed-layout, or explicit-alignment surface without a concrete need.
- Every inhabited concrete type has `size_of[T] >= 1`; empty structs occupy one byte so generic storage has no zero-sized-value exceptions.
- Enum declarations require at least one variant; empty enums are rejected rather than acting as an implicit `Never` type.
- Unsafe raw pointer `+` and `-` use `u64` byte offsets within one live allocation or its one-past address; Casa has no pointer-pointer subtraction.
- Raw `load8` through `load64` and matching stores use their exact unsigned integer widths; other typed values use unsafe `ptr::read[T]` and `ptr::write[T]`.
- Fixed-width raw integer loads and stores allow unaligned addresses on x86-64 but still require every accessed byte to be valid; typed raw operations retain natural alignment.
- Multibyte raw integer loads and stores use explicit x86-64 little-endian byte order; other protocol orders require library conversion.
- `memcpy` remains an ordinary unsafe stdlib function for non-overlapping initialized byte regions; typed ownership moves do not use raw byte copying.
- The compiler intrinsic `copy` has `[T: Copy] $T -> T`, materializing an owned allocation-free copy of the **Borrowed value** without invoking Clone.
- Aggregate padding is unspecified and may be uninitialized; safe operations use fields, and `size_of[T]` does not make padding readable byte data.
- Unsafe `ptr::read[T]` requires an already valid initialized `T`; invalid booleans, Unicode scalars, enum tags, borrows, or owners cause undefined behavior rather than runtime validation.
- Character conversion uses safe `character.codepoint` and stdlib `char::from_codepoint -> Option[char]`; only the validating wrapper knows `Option`, while the compiler supplies narrow lossless and unsafe unchecked code-point primitives.
- Floating-point types use the canonical names `f32` and `f64`; `float` is removed without an alias, and both widths retain IEEE NaN, infinity, and partial-comparison semantics.
- Floating-point literals are context-typed as `f32` or `f64`, default to `f64`, round directly to their target width, and have no suffix syntax initially.
- `f32` and `f64` expose safe, allocation-free `from_bits` and `to_bits` primitives using width-matched unsigned integers; these preserve representation and are not numeric casts.
- Numeric conversion names expose their loss contract: `from` is universally exact, `try_from` succeeds only for an exactly representable value, `round_from` permits IEEE floating-point rounding, and integer-only `wrapping_from` keeps low bits.
- Float `+`, `-`, `*`, and `/` require equal widths and retain that width; division follows IEEE behavior, mixed widths are rejected, and `%` remains integer-only.
- Floating-point execution fixes rounding to nearest with ties-to-even, preserves subnormals and signed zero, and initially forbids reassociation, silent fused operations, ambient rounding modes, and fast-math assumptions.
- Floating-point literals use decimal-point or exponent notation, require digits on both sides of a decimal point, and add no hexadecimal, suffix, NaN, or infinity syntax.
- Safe `Target::trunc_from` converts floats to integers by truncating toward zero and terminates on invalid or out-of-range inputs; stdlib validation builds exact `try_from -> Option[Target]` without compiler knowledge of Option.
- Initial stdlib floats expose special values, classification, absolute value, and basic rounding only; transcendental math, `total_cmp`, and a comprehensive math module remain deferred.
- Float parsing is locale-independent and returns ordinary Option; formatting emits shortest same-width round-trippable decimal text, preserves negative zero, and canonicalizes special-value spellings without preserving NaN payloads.
- Custom destruction is the reserved inherent `drop` method, not a trait implementation. It is compiler-invoked, cannot be called directly, and makes the type non-`Copy`.
- Borrow types use prefix sigils: `T` is owned, `$T` is shared, and `mut$T` is exclusive. Receivers use `self`, `$self`, and `mut$self` respectively.
- A returned borrow is conservatively tied to every compatible borrowed input; Casa exposes no named lifetime syntax initially.
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
- A **Trait method requirement** is satisfied by a matching method name and **Stack effect** on a type that explicitly implements the trait.
- A **Default method definition** belongs to the trait and is available to types that implement trait while omitting their own **Method definition**.
- A type implements a user-defined trait only through `impl Type: Trait`; matching methods alone do not declare an implementation.
- A **Function declaration** describes written source only, not compiler-injected hidden parameters.
- A **Stable Casa release** may provide the `casac` binary that serves as the **Bootstrap compiler**; the role does not define the release.
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
- `env::get` returns `Option[Bytes]`, not `Result` — a missing environment variable is absence, not an error, and a present Linux value is not guaranteed to be UTF-8.

## Example Dialogue

> **Dev:** "Can this PR depend on a temporary release so CI passes?"
> **Domain expert:** "No. By default, the **Bootstrap compiler** must build the **Branch compiler** from the latest stable release, and that compiler must reach a **Fixed point**."

> **Dev:** "Should I document `fn foo a:i64 b:str -> bool` as the signature?"
> **Domain expert:** "No. That is the **Function declaration**. The public stack contract is the **Stack effect**: `i64 str -> bool`, where `i64` is consumed from the top of the stack and `str` below it."

## Flagged Ambiguities

- "`Op.type_annotation` / `Op.deferred_return_type` as source text" was used to justify keeping parsed type metadata as strings. Resolved: user-written type expressions are **Source type syntax** only before parsing; after parsing, compiler-owned metadata should use the **Type AST**.
- "functional programming concepts" was broad enough to imply a full immutable rewrite. Resolved: the target is **Functional compiler pass** boundaries, with local mutation still allowed inside phases.
- "`LexResult`" was proposed while it would only wrap `List[Token]`. Resolved: **Lex result** became justified only once recoverable lexical diagnostics were made explicit alongside tokens; file loading remains separate.
- "`ParseResult` returning `Parser`" leaked a parser-owned object past parsing, while returning every parser field exposed unused state. Resolved: return only the **Compiler dependencies** later boundaries use now, and migrate call sites instead of keeping the old API.
- Parse and identifier resolution both need import state today, so a standalone parse result is premature. Resolved: start with a **Parse-and-resolve boundary** that keeps import state private.
- "`DEFAULT_PARSER`" was treated as convenient shared context. Resolved: call it the **Default parser** and remove uses as explicit pass boundaries replace hidden compiler state.
- "return updated SymbolStore" could imply deep-copying the symbol table. Resolved: **Typecheck result** may return the same reference after mutation; explicit pass output is the important boundary.
- "typechecker diagnostics" was treated as a typechecker-specific refactor. Resolved: diagnostics belong to a compiler-wide **Compiler diagnostics schema** refactor, tracked in issue #219.
- "glossary" was considered as a complete keyword or syntax catalog. Resolved: the **Documentation glossary** covers only project-specific concepts whose terminology must stay stable.
- "function signature" and "stack effect" were treated as interchangeable in docs. Resolved: use **Stack effect** for public stack contracts; reserve "signature" only where the compiler's internal function type model is meant.
- "`fn foo a:i64 b:str -> bool`" was called a signature. Resolved: call it a **Function declaration** when bodyless, and a **Function definition** when paired with a body.
- "top on right" was used to explain **Stack effect** notation. Resolved: inputs are topmost-first and outputs are push-order.
- "`None`" in **Stack effect** notation can be confused with `Option::None`. Resolved: `None` means no stack values in notation only.
- "release compiler" was used to mean both the stable compiler downloaded by CI and an ad hoc temporary compiler. Resolved: use **Bootstrap compiler** for the CI role and **Stable Casa release** for the public release; temporary releases are exceptional escape hatches.
- "temporary release" was considered as a normal CI mechanism. Resolved: use **Temporary compiler release** only as an exception, not as the default bootstrap path.
