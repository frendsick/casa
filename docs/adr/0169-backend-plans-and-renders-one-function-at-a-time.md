# Backend plans and renders one function at a time

related issue: [Choose the backend and runtime seams](https://github.com/frendsick/casa/issues/649).

Keep a private machine
instruction buffer for one function at a time, with complete storage and native
call plans before rendering. Embed the Linux runtime as assembly text in a
separate Casa module. Linux x86-64 is the implemented target.

## Checked input and target planning

The private `CheckedProgram` supplies semantic input to the backend. The backend
also borrows the compilation source store to resolve instruction locations for
deterministic line tables. Name and type resolution, stack effects, ownership and
borrowing, moves and copies, cleanup obligations, structured control flow,
concrete specialization, and trait dispatch are complete. Bodies reference
concrete declarations in the same product. The backend neither queries trait
conformance nor infers source-level behavior from type names.

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

The backend lowers `size_of` using its layout plan. The
[constant-expression contract](0171-constants-use-bounded-target-independent-expressions.md)
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

### Bounded explicit inline expansion

An `inline fn` declaration makes resolved named calls eligible for best-effort
expansion. Semantic checking retains this flag on concrete function declarations.
It does not select physical storage or change stack effects, ownership, borrowing,
or cleanup. Generated closure bodies do not inherit the flag. Unmarked functions
bypass the general expansion planner, while the existing eight-instruction leaf
templates remain available.

As an exception to immediate buffer release, the backend can retain a bounded
set of private expansion plans for the current assembly request. A plan contains
selected target actions before frame finalization, the complete local-storage
reservation, and its local label range. Its identity includes the concrete symbol
and the closure specialization context. Matching calls reuse the same immutable
plan. Pending and rejected entries prevent recursive planning and repeated failed
attempts. All entries are released when the request ends.

Planning is limited to 64 candidates, eight nested plans, and 64 checked nodes
per candidate, including branch and match-arm entries, nested pattern nodes, and
pattern and constructor field work. Or-pattern candidates retain ordinary calls
without expanding
their alternatives for eligibility. A retained plan has at most 256 actions and
512 local slots. The request retains at most 4,096 actions. Each caller can add at
most 1,024 actions and 512 local slots through expansion.
Exceeding a limit or encountering a planning cycle retains the ordinary call and
its native-call plan. Unsupported bodies also retain their calls.

Each insertion reserves fresh labels and local slots, including complete
aggregate storage extents. It rewrites local returns to one fresh continuation,
initializes cleanup flags for that invocation, and retains the hidden destination
protocol for Copy aggregate results. Checked cleanup and borrow origins remain
unchanged. Callee source locations survive expansion, and the continuation restores
the caller location. Calls retained inside a reusable plan stay calls, including
recursive fallbacks. Closure specialization continues to use the original capture
record and its mutation state.

The finalized caller still owns one physical instruction buffer. Reusable
expansion plans do not form a whole-program machine product or a second semantic
tree. They remain private target planning under the same backend entry point.

The buffer earns its cost through frame finalization and inspectable target
instructions. Direct text emission would need another way to size frames before
writing prologues and returns. Discovering locals during lowering requires frame finalization before rendering. A whole-program machine product has no additional demonstrated
consumer. Do not add a second semantic tree, a general control-flow graph, a
backend registry, or an optimization framework.

Remove public `Program` and `InstValue` construction, the whole-program bytecode
list, repeated instruction-family matching, the flat source control-flow
validation prepass, and semantic queries during backend lowering. Backend label
allocation and private invariant checks remain. Representation tests must not
force retention of the removed interface.

## Known closure targets

Before function planning, the backend propagates callable target shapes through
checked bindings, aggregate fields, captures, arguments, and returned values.
A shape records target identity and nested callable shapes. It contains no
runtime values or capture storage. Each context uses the original checked body
and its existing ownership, cleanup, and source facts.

The backend emits a specialized function only when its context selects a known
execution target or another specialized call. Capturing calls select the
original closure record as the active environment before calling the proven
target, then restore the previous environment. Separate closure instances keep
separate mutable capture state. Capture-free targets use ordinary direct calls
without environment switching or static-record destruction.

Every specialized symbol inherits the original checked function's native-call
plan for both call sites and function entry. Typed indirect execution uses that
same eligibility rule. Environment setup and restoration preserve the register
value stack and the argument and result registers. Generated destruction entry
points retain the value-stack memory convention.

Context counts, shape counts, recursion depth, and loop analysis have fixed
bounds. Conflicting targets, active recursive contexts, unknown mutations, and
exhausted budgets retain general dispatch. Joins retain only common target
facts. Cleanup analysis consumes checked transfers and inactive-owner facts,
and distinguishes user effects from representation-only destruction. It does
not change borrow origins or the lifetime of yielded values.

## Aggregate storage

Before register emission, the backend can replace non-escaping aggregate bodies
with scalar frame slots. The proof follows aggregate addresses, fixed field offsets, local
aliases, and active allocation lifetimes through the existing instruction labels.
Constructors from separate branches can share scalar slots only when their lifetimes
do not overlap. Stored aggregate addresses and representation copies propagate escape
rejection to their dependent storage. Checked moves, payload transfers, borrow
origins, source locations, and cleanup order remain unchanged.

This proof stays within one physical function. It is limited to 4,096 instructions,
64 aggregate regions, 256 bytes per region, 128 labels, 256 facts per state
component, and 16 analysis passes. Unknown address uses, overlapping live
constructors, unsupported field access, and incomplete convergence retain the
original representation. A closure environment can use scalar slots only when checked
capture cleanup is trivial and all environment accesses are visible within the function.
Nested environment entry and restoration retain their original behavior. This bounded
address analysis does not introduce a general control-flow graph or optimization
framework.

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
