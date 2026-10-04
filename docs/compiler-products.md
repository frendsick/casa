# Compiler request products

`compiler/products.casa` provides three request operations. The CLI requests
assembly, the LSP and workspace request analysis snapshots, and the formatter
requests syntax:

| Operation | Input | Result |
| --- | --- | --- |
| `syntax` | One `SourceUnit` | Tokens, optional structural facts, and a report |
| `analyze` | `CompilationInput` | An independent analysis snapshot with a report and editor index |
| `assembly` | `CompilationInput` and `Target` | `Produced(report, source)` or `Rejected(report)` |

Each operation returns `std::Result` with `CompilerFailure` as its error type. Source errors
return ordinary products. Rejected syntax has no structural facts, and rejected
assembly has no assembly text. `CompilerFailure` retains the accumulated report,
phase, message, and optional location. Current failures cover backend errors
and invalid request bases. Process termination and allocation failure are excluded.

`CompilationInput` owns root text, its file identity, an absolute base directory,
ordered library paths, and source overrides. Relative file identities, library
paths, and override keys resolve against that base without changing the process
directory. Root text takes precedence over overrides for the root file. Import overrides take
precedence over disk text. Casa's path-style and module-style search rules apply.

Reports expose shared borrows through `get_diagnostics` and `get_sources`, with
diagnostics in front-end order and exact root and loaded import text, including
rejected sources. `SourceStore::get_bytes` returns an owned byte copy even for
imports rejected as invalid UTF-8, for which text lookup returns no value.
Completed analysis and assembly reports omit unused overrides. Requests own source stores and
compiler state, so retained results cannot affect other requests.

`AssemblySource` exposes a shared borrow of its text and identifies its target,
`Target::LinuxX86_64`. The caller writes and builds assembly and launches binaries.
Native failures do not consume or change the report.

`compiler/build.casa` provides `NativeDriver::compile_binary`, which borrows an
`AssemblySource`, an output path, and ordered native library names. The default
driver is `/usr/bin/cc`. One invocation assembles and links with `-nostdlib`,
`-no-pie`, `-Wl,-e,_start`, and `-Wl,-z,noexecstack`. Tool diagnostics inherit
the caller's stderr. The adapter returns `Result[bool BuildFailure]` with `true`
on success. Write, launch, wait, nonzero exit, and signal failures return to the
caller. Only the forked child exits after an unsuccessful `execve`.

The fixed Linux runtime code and data are embedded from `compiler/runtime.casa`.
Program-specific pools and bodies remain generated. Installed compilers need no
runtime asset from the checkout. `keep_asm` retains the complete `<output>.s` on
success and native failure. Otherwise the adapter removes it after the driver
returns. The compiler creates no intermediate object file.

The CLI passes the assembly product to the native driver and retains its report
until diagnostics have been presented. It does not coordinate compiler phases.

`SourceProgram` owns declaration metadata and structured source bodies. Function
records contain signatures, variables, captures, and linkage metadata. Source
resolution elaborates names and declarations without checking function bodies.
Pending source and checked recipes belong to private work registries. Active
resolution, checking, and specialization frames own each body until they publish
the completed result. Recursive calls can read a reserved signature while the
body remains private to its frame.

Shared syntax retains unresolved struct, enum, and trait headers, including
member types, generic parameters, derives, and method signatures. Source
construction consumes these facts after constant elaboration. It resolves names
and types, checks duplicates, and creates field accessors. Trait default bodies
use the existing body-construction and specialization paths. Ordinary function
and implementation-block headers still use the source builder's token parser.

`constant_eval.casa` executes complete parsed initializers and validates the
final stack and value type. It stops at the first failed term and resolves
references in encounter order through a source-builder callback. Source
construction owns annotation lookup, source-order visibility, declaration
commit, and failed constant identities. Evaluation reports diagnostics and
returns either a validated value or `None`, so the caller needs no execution
stack or diagnostic-count comparison.

Checking consumes structured source and produces semantic branches, loops,
match arms, and operations. Generic recipes and concrete instances share the
semantic vocabulary. Specialization substitutes types, ownership actions, and
call targets without checking source again. Pattern and loop bindings retain
source names for diagnostics and separate checked storage identities for layout.
Closure signatures receive the concrete argument and result types established
by checking.

`CheckedProgram` can be constructed only after all reachable bodies, signatures,
local and capture types, storage references, and dispatch targets are complete.
It also owns the selected copy behavior, value categories, and concrete drop
hooks for each used type. The backend consumes these facts without trait queries.
Rejected source retains diagnostics and editor facts but cannot produce backend input. Unreachable source still receives
type and structural checks. It retains source occurrences for editor queries,
without executable operands or ownership actions.

Scalar and aggregate programs use one backend entry. Physical instructions,
function plans, pools, selection, and spelling helpers are private to
`backend.casa`. A deterministic worklist
plans each reachable function from its structured body. Each function plan fixes
storage actions, frame size, ownership flags, cleanup, and concrete call targets
before instruction selection. Native calls share one request-owned ABI plan per
concrete extern function. Recursive calls refer to reserved identities.

Source checking validates extern declaration forms, `Copy` requirements, and
unsafe calls without ABI classification. The checked program retains concrete
extern signatures and declaration locations. Before lowering functions, the
Linux x86-64 planner classifies these signatures and fixes native argument and
result placement. Unsupported layouts return `Rejected` with diagnostics and
retained source context. Internal failures remain `CompilerFailure`, and neither
outcome publishes partial assembly. Analysis requires no native target.

Each completed function plan contains finalized frame and storage actions.
Selection validates local labels, storage slots, captures, calls, and literal
references, then renders the plan directly into private assembly text. It does
not retain a second buffer of rendered lines. Bounded leaf templates, literal
pools, native-call plans, and symbol identities live until the request ends.
Publication also checks completed function definitions and static pool references.
An invariant failure discards the private output. It never retries another backend.

Operation handlers record dependencies from the selected target and receiver
used to check the stack. The editor projection retains only source occurrences
and presentation facts. It traverses completed semantic bodies directly and
releases all compiler bodies before returning the index. Verified operation
identities are settled after literal checks and recursive call obligations.
A failed operation withholds facts that depend on its recovered stack. An
independent sibling branch or function can still contribute verified facts.
A return-signature error rejects assembly without erasing established calls.

Ownership checking keeps typed places with a resolved binding name and field
components. Origins distinguish access to a place from dependencies on an
input's ordinary or cleanup-observed origins. Each binding keeps both origin
sets and its diagnostic location in one record. Continuing branch completion
transfers results before checking LIFO cleanup and leaving the scope. Returns
attach cleanup to the exit and do not contribute to the continuing join.

Loop back-edges and `continue` validate stack shape, capability, origins,
callable targets, and binding ownership against the loop header. False conditions
and reachable `break` paths contribute to the exit join. Root block bindings
follow the same lexical cleanup rules as function locals. Match joins retain the
merged origins and callable targets even when their stack types do not change.

Each explicit return checks its declared stack effect. Both early returns and
fallthrough validate borrowed origins and restore owned captures for repeatable
closures. A payload-free enum variant establishes an empty origin set. Recursive
borrowed callable results retain conservative input origins when a recursive
summary is still active. Calls reuse completed callable summaries. Independent
function-analysis queries create their own source snapshots. Session consumers
request completed declarations and borrow checked bodies without managing body
phase transitions.

Semantic operations own their assignment, move, and cleanup actions. Operations
that need none omit them. Specialization substitutes cleanup types within the
body, and backend lowering reads the actions directly. Pending moves from stack values remain private to the body checker
until it attaches them to their source operations. There is no ownership table
in `SymbolStore`.

`AnalysisSnapshot::get_index` borrows the source index. Its five `find_*` queries
use a retained absolute file identity and byte offset. Hover and definition
return `PointAnswer`: `Known`, `Absent`, or `Unavailable`. Completion and semantic
tokens return `ListAnswer`: `Complete`, `Incomplete`, or `Unavailable`.
References use the same availability variants in
`ReferenceAnswer`. Completion items retain candidate presentation, insertion
text, and the identifier replacement range. Reference results retain ranges,
the declaration identity, covered files, and whether rename requires workspace
coverage.

An empty reference list can be complete. Invalid files and offsets are
unavailable. Returned values own their text and locations and survive snapshot
release. Convert locations with the originating report's retained sources.

Projection records checker-verified source facts during construction and
releases the compiler store and bodies before returning. Query execution reads
source facts only. Named parameters retain their written name ranges from parsing,
including method receivers. Type substitution preserves this attribution.
Parameter queries use these retained ranges, and generated parameters have no
source declaration. Established parameter declarations survive independent body
errors. Semantic checking retains each local binding's declaration range,
verified type, and lexical visibility while its scope is open. Resolved uses and
closure captures retain that declaration identity. The editor projects these
facts into definition, reference, hover, and completion results without matching
assignments or replaying scope markers. Root bindings, branches, loops, and
patterns use the same facts. Rejected operations withhold dependent bindings,
while independent verified declarations remain available.
References cover this compilation's root and loaded imports,
with sorted, deduplicated ranges and explicit declaration inclusion. Tokens are
sorted, non-overlapping, and restricted to the requested file. Source failures
preserve independent verified facts. Point availability tracks affected source
regions. Tokens track requested-file coverage. References conservatively report
compilation coverage for top-level symbols and body coverage for local bindings.
A damaged construct cannot contribute an invented operation fact. Lexical token
categories remain available after syntax rejection.

Type references are incomplete while signature and field type occurrences are
not represented. Rename must reject these answers. The
[editor lifetime workload](../tests/benchmarks/editor-lifetime.casa) checks
storage reclamation after repeated replacements.

`workspace.casa` aggregates owned answers across fresh snapshots, retains exact
source revisions, and validates proposed rename bindings through reanalysis.
Workspace discovery and LSP document versions stay outside the compiler products.
The LSP stores `AnalysisSnapshot` values. Position-based queries convert through
the snapshot's retained sources and return owned answers. The caller must check
document versions before applying results to newer text.

`tests/compiler/test_products.casa` checks independent overrides, release order,
source rejection, syntax products, retained failure context, and native output
from requested assembly. The private-field fixture checks the product boundary.

The [native build lifetime workload](../tests/benchmarks/native-build-lifetime.casa)
checks storage reclamation across missing-tool, nonzero-tool, and successful-build
paths. See the [measurement instructions](../tests/benchmarks/README.md).
