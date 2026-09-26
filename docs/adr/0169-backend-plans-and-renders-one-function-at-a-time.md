# Backend plans and renders one function at a time

related issue: [Choose the backend and runtime seams](https://github.com/frendsick/casa/issues/649).

The maintainer accepted this contract on 2026-09-26. Keep a private machine
instruction buffer for one function at a time, with complete storage and native
call plans before rendering. Embed the Linux runtime as assembly text in a
separate Casa module. Linux x86-64 is the immediate blueprint acceptance target.
Windows remains later work.

This completes the backend choices left open by
[ADR-0166](0166-compiler-capsule-owns-phase-state.md) and
[ADR-0167](0167-compiler-products-own-independent-snapshots.md). Production
migration and executable blueprint validation remain pending. This decision
establishes no source reduction or performance gain.

## Checked input and target planning

Only the private, target-neutral `CheckedProgram` enters the backend. Name and
type resolution, stack effects, ownership and borrowing, moves and copies,
cleanup obligations, structured control flow, concrete specialization, and
trait dispatch are complete. Bodies reference concrete declarations in the same
product. The backend neither queries trait conformance nor infers source-level
behavior from type names.

The closed platform policy owns physical layout, field and value storage,
`size_of` values, calling conventions, runtime symbols, and assembly spelling.
Source-level extern restrictions, including allowed declaration forms, `Copy`
requirements, and unsafe calls, remain semantic checks. Target ABI eligibility,
physical argument classification, and placement belong to target planning.
Successful semantic checking does not guarantee target support.

Use one storage plan for ordinary and extern fields, consistent with
[ADR-0162](0162-ordinary-and-extern-structs-share-field-storage-planning.md).
Record size, alignment, offsets, inline or indirect placement, value carrier,
and physical actions for projection, load, store, copy, move, and destruction.
All consumers use that plan. Preserve the accepted ownership and layout
contracts rather than selecting a new aggregate model in this work.

Compute a complete native-call plan once per concrete signature and target.
It assigns argument parts and results to registers, stack locations, or hidden
result storage. Include stack alignment, aggregate transfers, clobbers, return
normalization, and temporary-storage obligations. Instruction selection consumes
this plan. The renderer does not classify types or allocate argument registers.

`size_of` remains symbolic until target planning. The subsequently accepted
[constant-expression contract](0168-constants-use-bounded-target-independent-expressions.md)
excludes layout queries from constant initializers and constant type arguments.

Target restrictions produce source diagnostics using retained source context.
Invalid private state produces `CompilerFailure` with the accumulated report.
Either outcome discards incomplete assembly and releases backend work. Neither
is a native build failure.

## Private machine representation

The backend has one entry from compiler orchestration and returns target-tagged
assembly or the failure outcomes above. It exposes no instructions, builders,
pools, or separately supplied symbol store to callers.

A function builder owns its instruction buffer, frame slots, temporary storage,
labels, and source attribution. It lowers structured checked bodies through an
exhaustive dispatch. Machine operands carry selected widths, storage actions,
concrete calls, and label identities. They contain no unresolved source
operations, trait queries, or optional type-name hints.

Finalize frame size, frame restoration at returns, native-call placement, and
local label references before rendering. Semantic cleanup actions already name
the owners and exits involved. The backend assigns storage for conditional
cleanup state and emits those actions without deciding ownership again.
Physical frame restoration and semantic destruction remain separate duties.

Render the completed function through one exhaustive machine-operation dispatch,
then release its buffer. Keep literal pools, layout and ABI caches, and the
function-symbol table at request scope. Concrete function identities exist
before lowering, so recursive calls can refer forward without recursively
building another function buffer. Validate cross-function, pool, and runtime
references before publishing complete assembly. Partial output remains private.

The buffer earns its cost through frame finalization and inspectable target
instructions. Direct text emission would need another way to size frames before
writing prologues and returns. Current lowering discovers locals and then
rewrites returns. A whole-program machine product has no additional demonstrated
consumer. Do not add a second semantic tree, a general control-flow graph, a
backend registry, or an optimization framework.

Remove public `Program` and `InstValue` construction, the whole-program bytecode
list, repeated instruction-family matching, the flat source control-flow
validation prepass, and semantic queries during backend lowering. Backend label
allocation and private invariant checks remain. Representation tests must not
force retention of the removed interface.

## Runtime source and distribution

Store fixed Linux runtime assembly in an ordinary multiline string in a
dedicated Casa source module. Ordinary compilation embeds the text in `casac`.
Append it once to generated assembly, including its fixed code and data. Keep
program-specific pools, function bodies, and root execution generated.

The checkout supplies this module through normal compiler imports. Installed
binaries need no runtime source file, path search, generator, or prebuilt
runtime object. `--keep-asm` retains a complete `.s` file. The native compiler
driver assembles and links that file with requested libraries. Existing release
assets and the bootstrap distribution model remain sufficient.

Quotes and backslashes still need ordinary string escaping. Use assembler
constants and reserved runtime-local labels for fixed text. Current generated
runtime return labels can become fixed labels because each helper is emitted
once. Preserve checked return-stack reservation and keep runtime symbols
distinct from generated function and literal labels.

Preserve allocator alignment, free-list reuse, mapped-chunk growth, zero
allocation, null free, and allocation-failure termination. Preserve output
retry/error handling and return-stack overflow handling. Retain conditional
heap-free guards until every producer and native-call path has sufficient
physical storage provenance to remove them safely. Extraction does not
authorize allocator redesign.

Always include the fixed runtime. No helper reachability analysis is needed.
The extraction relocates maintained source out of compiler control flow. It is
not net repository source deletion.

## Native build and platform scope

Native process execution remains outside Compiler Capsule. The build adapter
consumes target-tagged `AssemblySource`, an output path, `keep_asm`, and ordered
native libraries. Linux uses one invocation of the existing C compiler driver
to assemble and link. Preserve `-nostdlib`, `-no-pie`, `-Wl,-e,_start`,
`-Wl,-z,noexecstack`, and library order. Return write, launch, or nonzero build
failures to the CLI with available tool diagnostics. The CLI owns reporting
and exit status.

Temporary-file creation and cleanup belong to this adapter. The compiler no
longer manages an intermediate object file. Preserve assembly retention and
reconcile installation requirements with the actual driver used.

Immediate acceptance covers Linux host tools, Linux x86-64 output, the Linux
runtime, and the current Linux standard library. Windows is the only planned
additional platform. Add its closed policy only when implemented. Windows ABI
support alone would not establish native build, runtime, OS library, or
self-hosting support. Those Windows capabilities are not required for this
blueprint, and no unimplemented target variant is exposed.

## Validation and reconsideration

[Validate the compiler simplification blueprint](https://github.com/frendsick/casa/issues/651)
must exercise the buffer and plans in the integrated slice. Include scalar
operations, aggregate copies, branches, direct and generic calls, extern calls,
early returns, and conditional cleanup. Show that rendering performs no semantic,
layout, or ABI classification. Reject invalid private references through the
actual constructing interfaces.

Retain execution coverage for cleanup, integer and SSE register exhaustion,
aggregate spills and returns, alignment, and return normalization. Keep focused
storage-plan and call-plan tests and exact assembly checks where execution alone
does not establish the target contract. Preserve runtime behavior tests when
replacing tests that construct public bytecode fields.

Validate an installed binary outside the source checkout, assembly retention,
requested libraries, missing or failing native tools, and output-write failures.
Exercise runtime inclusion without an external asset. The eventual migration
must pass self-hosting and fixed-point checks, including deterministic assembly.
The slice cannot substitute for those full-compiler gates.

Measure request and function lifetimes, time, memory, and maintained source in
the integrated blueprint. Count runtime relocation separately. Reconsider the
buffer if the slice shows that direct emission handles frame finalization and
validation more simply, or that another retained machine form is necessary.
Any alternative must preserve the checked-input and failure contracts.

## Evidence

The source inspected was `62a46a68929ef20860f8a35f49b40ab5c0093949`.

- [Function lowering](https://github.com/frendsick/casa/blob/62a46a68929ef20860f8a35f49b40ab5c0093949/compiler/bytecode.casa#L2528)
  discovers frame size and inserts frame restoration at returns.
- [Native-call emission](https://github.com/frendsick/casa/blob/62a46a68929ef20860f8a35f49b40ab5c0093949/compiler/emitter.casa#L918)
  makes placement decisions after earlier ABI classification.
- [Runtime emission](https://github.com/frendsick/casa/blob/62a46a68929ef20860f8a35f49b40ab5c0093949/compiler/emitter.casa#L336)
  mixes fixed assembly, constants, and generated return labels.
- [String lexing](https://github.com/frendsick/casa/blob/62a46a68929ef20860f8a35f49b40ab5c0093949/compiler/lexer.casa#L622)
  accepts literal newlines. The
  [formatter](https://github.com/frendsick/casa/blob/62a46a68929ef20860f8a35f49b40ab5c0093949/formatter/format.casa#L497)
  preserves literal source spans.
- [Native build](https://github.com/frendsick/casa/blob/62a46a68929ef20860f8a35f49b40ab5c0093949/compiler/build.casa#L8)
  runs the assembler and C compiler driver separately. The
  [installer](https://github.com/frendsick/casa/blob/62a46a68929ef20860f8a35f49b40ab5c0093949/install.sh#L43)
  downloads standalone compiler and formatter binaries.

The [backend audit](../benchmarks/backend-runtime-complexity-audit.md) supplies
the historical baseline. Its estimates are not measurements of this design.
