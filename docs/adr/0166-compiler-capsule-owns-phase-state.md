# Compiler Capsule owns phase state
related issue: [Confirm Compiler Capsule constraints and tradeoffs](https://github.com/frendsick/casa/issues/645)

The maintainer selected Compiler Capsule in the
[architecture comparison](https://github.com/frendsick/casa/issues/644).
It places compilation behind a deep module whose callers request compiler
products without coordinating mutable phase state. This records the selected
direction. [ADR-0167](0167-compiler-products-own-independent-snapshots.md)
records the selected representations and product interfaces. Production
migration and the remaining seam contracts are unfinished.

[ADR-0172](0172-editor-products-retain-verified-source-facts.md) completes the
tooling contract, including partial results, workspace queries, and edit freshness.

## Verified starting point

At source commit `f07c444`, [TypecheckResult](../../compiler/typechecker.casa)
exposes operations and a symbol store, while
[compile_typechecked](../../compiler/bytecode.casa) checks diagnostics before
lowering. [Semantic analysis](../../compiler/semantics.casa) uses physical
layout in `check_size_of` and x86-64 ABI classification for extern validation.
[Editor documents](../../compiler/document.casa) retain operations and function
maps. Sealing phase state and separating target policy therefore require a
migration, not just new names for the current outputs.

## Accepted constraints

- Keep Casa self-hosted and dependency-light, with direct parsing and x86-64
  emission. Do not add LLVM, a parser generator, or a backend registry.
- Complete source-level checking, ownership validation, concrete semantic
  specialization, generic cycle checks, and trait dispatch before constructing
  a target-neutral `CheckedProgram`. Its consumers must not repeat those
  semantic decisions or mutate the product into an invalid state.
- Use one shared x86-64 backend with a closed platform policy. The policy owns
  physical layout, `size_of` values, ABI, runtime, and assembly spelling.
  Target-specific rejection remains possible after semantic checking.
- Linux is the current target and the immediate blueprint acceptance target.
  Windows is the only planned additional target and remains later work under
  [ADR-0169](0169-backend-plans-and-renders-one-function-at-a-time.md). Expose its
  production target variant only when its policy works.
- Carry the target with `AssemblySource` so the build layer can select the
  assembler, object format, and linker. Native process execution stays outside
  the compiler module.
- The shared analysis path produces source-oriented editor facts. Only analysis
  retains an `EditorIndex`. Assembly discards it. Editor queries must not expose
  compiler operations or reconstruct semantic decisions from them.
- Preserve accumulated diagnostics and exact source context on rejection and
  internal failure. Partial editor facts must not grant access to codegen
  input. Internal failure and native build failure remain distinct from invalid
  source.

Judge the design by locality, explicit state ownership, caller knowledge, and
fewer representable invalid states. A capsule around the existing mutable
store alone does not meet this decision. Private passes and internal seams are
allowed when they consume established facts and hide their protocols.

## Alternatives and costs

The [pinned comparison](https://github.com/frendsick/casa/blob/bd3516658e11b4a1544562a552ae47e37329c073/compiler/compiler_architecture_prototype.html)
presents three designs. Capsule was selected as the simplest and most natural
model. The following tradeoffs explain that choice without treating the
prototype's estimates as measurements.

| Design | Useful property | Cost compared with the selected direction |
| --- | --- | --- |
| Compiler Capsule | Callers obtain compiler products without managing phase transitions | A large private module can still hide shared-state coupling. Internal ownership and validation need concrete evidence. |
| Typed Product Ladder | Distinct phase products make transitions inspectable | More products, conversions, and lifetime rules can preserve the coordination burden inside the compiler. |
| Function-at-a-Time Direct Compiler | Short-lived body state can reduce retained work | Streaming, generic recipes, and separate editor and assembly products add coordination before a complete program is known to be valid. |

These are design risks, not measured performance conclusions. Capsule does not
commit to every private type or removal proposed by the prototype. In
particular, removing inspectable machine state must still support backend
validation and diagnostics.

## Interface comparison

The prototype proposes `run -> CompilerProduct` and `query -> ToolAnswer`.
[ADR-0167](0167-compiler-products-own-independent-snapshots.md) compares these
with typed operations and records the accepted product and ownership contract.
Its caller-knowledge comparison resolves this interface choice. Executable
evidence is still required. ADR-0172 records the tooling guarantees.

Count everything each caller must know: request and response variants, valid
pairings, matching rules, ordering, ownership transfer, retained borrows,
reclamation, and failure states. A broad result requires callers to understand
which variants can occur for their request. Typed operations may remove that
matching burden while exposing more callable names. Neither call counts nor
source-line estimates are fixed quotas.

## Open work and reconsideration

Simplification targets remain open. Tooling follows
[ADR-0172](0172-editor-products-retain-verified-source-facts.md).
Qualified-only imports and the absence of a source-level code-size guarantee are accepted in
[ADR-0168](0168-imports-expose-qualified-names-only.md). Backend and runtime seams
are recorded in [ADR-0169](0169-backend-plans-and-renders-one-function-at-a-time.md).
Constant evaluation follows
[Constants use bounded target-independent expressions](0171-constants-use-bounded-target-independent-expressions.md).
The generic contract follows
[ADR-0170](0170-generics-specialize-after-symbolic-checking.md).
Derivation, trait defaults, runtime-global removal, and ownership use the
accepted contracts linked from the [map](https://github.com/frendsick/casa/issues/638). The older
prototype's behavior examples do not override those decisions. The single
authoritative operation-semantics principle continues in
[ADR-0173](0173-semantic-checking-owns-source-obligations.md). Qualified-only
imports follow ADR-0168. The prototype's older decision identifiers have
surviving destinations in the [ADR index](README.md#retired-records).

Seam design proceeds from ADR-0167 within these limits. Proposals must
label any assumption about an open contract and identify which result depends
on it. Do not infer syntax removal, diagnostic changes, a performance ceiling,
or further interface commitments from the architecture selection alone.

Revisit the direction if the executable slice shows that retained behavior
requires consumers to coordinate mutable phase state, that checked products
cannot prevent invalid backend entry, or that editor and assembly work require
parallel semantic implementations. Also revisit it if measured time or memory
costs miss the subsequently agreed acceptance gates. A more inspectable private
product or a typed operation can be adopted without reopening the whole choice.

[Blueprint validation](https://github.com/frendsick/casa/issues/651) must supply
an executable integrated slice, resolved behavior decisions, agreed comparison
measurements, and an implementation breakdown before final acceptance. It must
define a working bootstrap route and final fixed-point validation. The map
permits a non-incremental cutover, but this documentation does not change the
repository's current stable-release and CI rules. No performance gain or
production implementation is established by this record.
