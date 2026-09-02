# Backend and runtime complexity audit

Status: complete audit.

Source revision: [`bb6ffa7`](https://github.com/frendsick/casa/commit/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1),
2026-09-02.

Run date: 2026-09-03.

Related issue: #641.

## Answer

Most backend complexity comes from weak phase boundaries, not from x86-64
instruction encoding. The typechecker returns a public, mutable operation list.
Bytecode lowering rechecks semantic facts, makes storage and ABI decisions, and
accepts frontend-only operations. The emitter routes 123 of the resulting 136
instruction variants through a second family match. Tests use eight additional
emitter functions and construct the seven-field `Program` value directly.

The later architecture synthesis should first compare two small models:

1. Keep one compact, typed machine IR whose variants already contain storage,
   ownership, control-flow, and ABI decisions.
2. Lower an opaque checked program directly through one backend module, with no
   persistent bytecode list.

This audit does not choose between those models. It establishes the evidence
that the current `Op` to `InstValue` to assembly pipeline is not a deep module.
Keeping direct x86-64 emission is compatible with either model.

The directly evidenced, model-independent deletion and shrink candidates total
about 500 source lines. Checked-input, storage, and ABI changes can remove more,
but those estimates depend on the later prototype and are not part of this
total. A further 500 lines of fixed runtime emission can move from compiler
logic to a static assembly asset or object without changing runtime behavior.
No major dependency is needed.

## Scope and baseline

The inspected production surface has 6,601 lines:

| File | Lines | Role |
|---|---:|---|
| `compiler/bytecode.casa` | 3,721 | Semantic lowering, storage operations, control-flow targets, and machine IR construction |
| `compiler/abi.casa` | 156 | Per-type System V classification |
| `compiler/emitter.casa` | 2,654 | Runtime source, ABI call placement, instruction dispatch, and assembly text |
| `compiler/build.casa` | 70 | Assembly file, assembler, linker, and cleanup |

The focused test surface has 3,537 lines in
`tests/compiler/test_bytecode.casa`, `tests/compiler/test_emitter.casa`,
`tests/compiler/test_extern.casa`, and `tests/test_compiler.sh`.

The current public surface has one bytecode entry point, two ABI entry points,
one build entry point, and 61 emitter functions. Production code outside
`emitter.casa` calls only `emitter::emit`. `test_emitter.casa` also uses two
top-level text helpers and six instance functions. At least 52 public emitter
functions have no caller outside their own module.

[`Program` has seven public fields](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/common.casa#L1574-L1582).
[`InstValue` is a 136-variant enum](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/common.casa#L241-L396).
[`OpValue` has 128 variants](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/common.casa#L1009-L1161).

### Self-compilation baseline

The stable v1.50.0 compiler compiled `casa.casa` three times after one warm run.
The median wall time was 31.90 seconds. All three runs used 760,564 KiB peak
RSS. The generated assembly had 1,265,225 lines and 26,529,317 bytes. The
binary had 4,777,120 bytes.

One separate verbose run reported these phase boundaries:

| Boundary | Elapsed time | Delta from prior boundary |
|---|---:|---:|
| Analysis complete | 24.819 s | 24.819 s |
| Bytecode complete | 27.804 s | 2.985 s |
| Assembly emission complete | 29.811 s | 2.007 s |
| Binary complete | 32.390 s | 2.579 s |

The complete backend after analysis took 7.57 seconds in that run, or 23% of
wall time. These timings locate meaningful work after analysis. They do not
show that the fixed runtime dominates output. Its 467 assembly lines are less
than 0.04% of the self-compiled assembly.

Commands:

```sh
mkdir /tmp/casa-641
./casac --keep-asm -L lib casa.casa -o /tmp/casa-641/casac-audit
for run in 1 2 3; do
    /usr/bin/time -f "$run %e %M" \
        ./casac --keep-asm -L lib casa.casa -o /tmp/casa-641/casac-audit
done
./casac --verbose --keep-asm -L lib casa.casa -o /tmp/casa-641/casac-verbose
wc -lc /tmp/casa-641/casac-audit.s
./casac --keep-asm -L lib /dev/null -o /tmp/casa-641/empty
wc -lc /tmp/casa-641/empty.s
```

## Ranked findings

### 1. `[shrink]` Make checked backend input represent only checked programs

[`TypecheckResult` exposes its store, operation list, and diagnostics as public
fields](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/typechecker.casa#L7-L12).
`compile_typechecked` rejects diagnostics at run time, but its parameter type
does not prove that typechecking succeeded. Unit tests construct error-free
results from arbitrary `Op` sequences.

Bytecode lowering contains 66 calls that record a failure. They have three
different causes:

- Backend input can contain malformed `if`, `while`, and `match` sequences,
  unresolved identifiers, generic print operations, unlowered method calls,
  unspecialized trait operations, and unspecialized numeric conversions
  ([control-flow validation](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/bytecode.casa#L1137-L1325),
  [frontend-only operations](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/bytecode.casa#L3094-L3100),
  [unlowered calls and identifiers](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/bytecode.casa#L3237-L3280)).
- Internal helpers guard against receiving an operation from the wrong family.
  These are local contract assertions, not invalid backend input
  ([assignment and block helpers](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/bytecode.casa#L1490-L1658),
  [static helpers](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/bytecode.casa#L2218-L2295),
  [function helper](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/bytecode.casa#L2685-L2712)).
- Backend planning can fail to find a symbol, type hint, layout, field plan, or
  value representation. These failures show repeated planning after analysis,
  but an opaque input alone does not remove them
  ([field and layout planning](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/bytecode.casa#L320-L410),
  [typed storage operations](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/bytecode.casa#L2740-L2900)).

The bytecode phase also has a raw control-flow target allocation and a 160-line
prepass that validates flat frontend control flow. Parser and semantic passes
already know this structure. The backend keeps the validation because the
checked-program boundary does not preserve it as an invariant
([target storage](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/bytecode.casa#L38-L130),
[prepass](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/bytecode.casa#L1170-L1330),
[entry point](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/bytecode.casa#L3713-L3721)).

An opaque successful-analysis value should expose only backend-valid operations
or blocks. Invalid source still needs frontend diagnostics. Impossible backend
states should become construction errors or one internal assertion, not normal
branches throughout lowering.

The control-flow table and validation prepass alone occupy about 250 lines.
A structured checked input can remove or relocate that work. Other failure
categories are not included in this estimate.

### 2. `[yagni]` Stress-test the bytecode layer as a phase boundary

Bytecode has one producer and one consumer. The consumer does not gain a small
interface. [`emit_inst` routes 123 variants to helpers that match the same enum
again](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/emitter.casa#L2477-L2642).
It handles the other 13 variants directly. Five public family
helpers terminate the process if they receive a variant outside their family
([stack](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/emitter.casa#L1224-L1295),
[arithmetic](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/emitter.casa#L1302-L1452),
[control flow](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/emitter.casa#L2090-L2301),
[memory](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/emitter.casa#L2308-L2334),
[I/O](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/emitter.casa#L2341-L2425)).

`compile_op` tries six broad operation families in order. Its last call discards
the `false` result from `compile_value_op`. A new valid operation can therefore
reach the end without an instruction or failure
([dispatch](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/bytecode.casa#L3572-L3630)).
No current valid source was found that reaches this path. It is evidence that
the phase contract is not exhaustive.

The synthesis prototype should measure both candidate models against these
requirements:

- one exhaustive place selects each machine operation
- the backend consumes only backend-valid input
- labels and branches do not require a second parse of frontend control flow
- storage and ABI plans are inputs to emission, not queries made during it
- one compile entry point hides instruction-family helpers

Estimated direct effect: remove the 166-line routing match and 20 to 50 lines
of family guards and adapters. Removing the persistent bytecode representation
could save more, but this audit does not count that unproven result.

### 3. `[shrink]` Put value storage decisions in one typed plan

The current plan carries physical field placement only:

```text
FieldStoragePlan = size + alignment + offset + stores_inline
```

Value representation is a separate optional integer with three magic values:
`INDIRECT = 0`, `RAW = 1`, and `SIGNED = 2`. Bytecode combines those values
with trait checks and separate helpers named `has_copy_aggregate_storage`,
`uses_inline_local_storage`, `emit_typed_load`, `emit_typed_borrow`,
`emit_typed_copy`, and `emit_typed_store`
([layout and field plan](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/common.casa#L3839-L3897),
[member and payload plans](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/common.casa#L4059-L4130),
[value representation](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/common.casa#L4190-L4230),
[typed operations](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/bytecode.casa#L2751-L2910)).

This permits combinations that later code must interpret or reject. It also
keeps backend-specific layout policy in the broad `common` module. ADR-0162
already states that one storage plan should define placement and the operations
needed to project, load, store, move, and destroy a value. The current code has
not completed that seam.

A typed storage plan should make indirect, inline raw, inline signed, and
aggregate ownership cases distinct. The backend should consume the plan without
new trait or symbol-store queries.

Estimated effect: remove 100 to 200 lines of repeated decisions. Move the
remaining layout policy behind one interface.

### 4. `[shrink]` Compute a complete native-call plan once

Extern ABI work is split across four places:

1. Semantics calls `abi::classify_extern_type` to validate parameter and return
   types.
2. Bytecode calls the same classifier again for every extern call.
3. `ExternCall` stores only per-type classes, passing kind, and size.
4. The emitter spends about 210 lines assigning registers, spilling stack
   arguments, aligning the stack, placing the return value, and cleaning up
   aggregates.

The relevant code is
[`ExternAbiType` and `ExternCall`](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/common.casa#L192-L228),
[`classify_extern_type`](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/abi.casa#L101-L156),
[`lower_extern_call`](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/bytecode.casa#L2453-L2503),
and [`emit_extern_call`](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/emitter.casa#L918-L1127).

The ABI module is shallow because it hides classification but not call
placement. A complete call plan should assign each argument and return part to
a register, stack slot, or memory result once. Emission should render that plan.
This keeps the System V details together and removes the repeated classifier
call.

Estimated effect: remove 50 to 100 lines of repeated branching and move about
150 lines of ABI policy out of bytecode, semantics, and emitter.

### 5. `[delete]` Remove unreachable static struct and nested static-object emission

`Program.static_structs`, `StaticStruct`, `compile_static_struct`, recursive
static-array branches, and the emitter's `static_struct_N` loop remain from the
old array representation
([data types](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/common.casa#L772-L787),
[lowering](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/bytecode.casa#L2186-L2337),
[emission](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/emitter.casa#L290-L314)).

Valid source cannot reach the struct or nested-array branches. Static array
selection requires a word-sized element with `RAW` or `SIGNED` representation.
Structs and arrays use `INDIRECT` representation. Current dense-storage tests
also assert that struct arrays have no `static_struct_N` label
([selection rule](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/bytecode.casa#L2341-L2380),
[tests](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/tests/compiler/test_array_methods.casa#L205-L233)).

Tests construct only empty static-struct lists. No other producer of
`StaticStruct` exists.

Estimated effect: delete 110 to 150 production lines and one `Program` field.

### 6. `[native]` Move the fixed runtime out of assembly-string construction

`emit_helpers` always appends the allocator, write loop, integer printing,
UTF-8 encoding, primitive conversion, string concatenation, and failure
handlers. This occupies about 500 lines of emitter source
([runtime helpers](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/emitter.casa#L336-L838)).

An empty source file emits 510 assembly lines and a 10,256-byte binary. The
fixed helper block runs from assembly line 29 to line 495, before `_start`.
Moving this stable text to a checked-in assembly asset or prebuilt object would
remove runtime implementation detail from the compiler. The emitter would keep
only a small interface for runtime calls and required data labels.

Do not add helper reachability analysis now. Always linking a small fixed
runtime is simpler than adding a dependency graph and dead-code elimination.
Do not simplify the allocator algorithm without separate performance evidence.
The exact-size free lists and mapped-chunk growth are deliberate recent changes
([exact-size bins](https://github.com/frendsick/casa/commit/a96c6522e78a89085a1edd2b9740e247b9eb7875),
[mapped chunks](https://github.com/frendsick/casa/commit/3c5ce9d6e45770fa11bb326ef20fb76cb0776d12)).

Estimated effect: relocate about 500 compiler lines. This is locality and
interface improvement, not a net repository line reduction.

### 7. `[shrink]` Test the backend contract instead of public implementation steps

`test_emitter.casa` has 89 tests. It constructs `Program` directly about 80
times, although the file already has `make_empty_program` and
`make_simple_program`. Seventy-nine tests call `emitter::emit`. Four unique
emission helpers are called directly. The tests also use `Emitter::new`,
`Emitter::into_asm`, `sanitize_name`, and `escape_string`.

`test_bytecode.casa` has 40 tests and often asserts exact instruction positions
and sequences. The combined 2,852 lines protect the current representation
more than the source-to-binary contract. Native compiler tests already cover
execution. Exact assembly tests remain valuable for ABI rules, stack layout,
failure handlers, and instruction cases that native behavior cannot identify.

After the backend seam is selected, use:

- a compact source-to-binary behavior matrix for language behavior
- small table-driven tests for storage plans and ABI call plans
- a limited set of exact assembly tests for target contracts
- direct unit tests only for the chosen backend's deep public interfaces

The three native extern blocks in `tests/test_compiler.sh` also repeat the same
compile, archive, link, run, and expected-output flow across 122 lines
([native extern tests](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/tests/test_compiler.sh#L215-L336)).

The selected backend seam determines which representation tests remain and what
replaces them. No architecture-dependent test deletion is included in the
reduction total. A model-independent fixture extraction can remove 150 to 250
lines without changing test coverage.

### 8. `[native]` Let the C compiler driver assemble and link

`compile_binary` writes assembly, invokes `/usr/bin/as` to make an object,
invokes `/usr/bin/cc` to link it, then removes both intermediates
([build adapter](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/build.casa#L1-L70)).
The C compiler driver accepts assembly input and can perform both steps in one
call while preserving the current linker arguments.

Estimated effect: remove 15 to 25 lines, one hard-coded tool path, one process
invocation, one temporary object, and one error path. This removes no major
dependency because the compiler driver still invokes an assembler.

### 9. `[shrink]` Consolidate small duplicated machine routines after the seams move

The fixed runtime has three integer decimal loops in `print_int`, `print_uint`,
and `primitive_to_str`. `emit_float_from_decimal` and
`emit_float_significand` each emit a similar power-of-ten loop
([printing and conversion](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/emitter.casa#L504-L774),
[float helpers](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/emitter.casa#L2011-L2086)).

Primitive string conversion is a compiler primitive for f-strings, so moving it
to `std` would create a false simplification. Consolidate the machine routines
inside the runtime or backend only when the shared calling convention is
smaller than the duplicated bodies.

Estimated effect: 30 to 60 lines. This is lower priority than the phase and
storage seams.

## Dependency and ownership observations

The intended dependency direction is not present. `bytecode.casa` imports both
`semantic_rules` and `typechecker`. It formats types into strings to select
machine operations. String equality uses synthetic type names `StringStrEq`
and `StrStringEq` that semantics writes and bytecode reads
([semantic marker](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/semantics.casa#L5254-L5270),
[backend selection](https://github.com/frendsick/casa/blob/bb6ffa784afe7a0a86fa68014c2773e0d6a114e1/compiler/bytecode.casa#L2985-L3024)).
Bytecode also queries `Copy` conformance again to choose aggregate storage.

`common.casa` owns frontend operations, machine instructions, storage plans,
ABI data, and backend output. This makes it a shared-data module rather than a
stable abstraction. The later design should assign each representation to the
phase that constructs and validates it. Cross-phase data should contain final
facts, not names that a later phase must reinterpret.

`HeapFreeIfAllocated` is another symptom. Its emitted code scans mapped chunks
to determine whether a pointer is heap-owned before freeing it. The runtime
needs this test because instruction selection does not carry explicit ownership
for every aggregate cleanup. A complete storage and ownership plan can make
cleanup explicit. Do not remove the runtime guard until all producers carry
that proof.

## Recommended prototype checks for the later design ticket

The backend-seam ticket should compare candidate models with the same small
prototype and record:

- production lines needed for one scalar operation, one aggregate copy, one
  branch, one function call, and one extern call
- number of public backend methods and cross-module data types
- number of symbol-store, trait, type-formatting, and layout queries after
  successful analysis
- number of invalid states expressible at the backend entry
- peak memory and time for self-compilation
- source-to-binary tests retained, exact IR tests retained, and tests deleted
- whether the model keeps fixed runtime code outside compiler control flow

Reject a model that adds LLVM, a parser generator, runtime reachability
analysis, or another major dependency. Reject a model that only renames
`InstValue` while preserving the duplicate semantic decisions and broad public
test surface.

## Reduction ledger

The first table contains only changes that do not depend on the selected
backend model. The figures avoid counting the same lines twice.

| Model-independent change | Production | Tests | Relocated | Dependencies |
|---|---:|---:|---:|---:|
| Single exhaustive instruction selection path | -180 | 0 | 0 | 0 |
| Dead static struct path | -120 | 0 | 0 | 0 |
| Shared emitter test fixtures | 0 | -150 | 0 | 0 |
| One compiler-driver build command | -20 | 0 | 0 | 0 |
| Small runtime and float consolidation | -30 | 0 | 0 | 0 |
| Fixed runtime asset | 0 | 0 | 500 | 0 |
| **Model-independent total** | **-350** | **-150** | **500** | **0** |

The prototype-dependent opportunities are not part of the net figure:

| Prototype-dependent change | Potential reduction | Potential relocation |
|---|---:|---:|
| Structured checked input and control-flow invariants | about 250 | 0 |
| Complete storage and ABI plans | 100 to 200 | about 150 |
| Replacement of representation-coupled tests | not yet estimated | not yet estimated |

The estimates are architecture inputs, not acceptance targets. Validate them
against the selected prototype before implementation tickets use them.

net: -500 lines, -0 deps possible.
