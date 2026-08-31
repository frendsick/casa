# Ordinary struct C-compatibility investigation

Status: complete investigation.

Source-tracing revision: `aa088b4b253d0ac6dd7a67605e493e3d8d207e57`,
2026-08-31.

Prototype and measurement base: `5766c38415b920c7b0a2bef5bfe979ab5e1f32d8`,
2026-08-31.

Run date: 2026-08-31.

Related issue: #610.

## Finding

An ordinary struct has two current representations. Its inline body has aligned
fields and tail padding, but every standalone struct value is one owning pointer
to a heap allocation. Scalar and fixed-array fields can have the same byte layout
as C inside that allocation. Direct nested ordinary struct fields, generic
fields, recursive edges, and owned struct fields use pointer-sized slots instead
of C by-value nesting. Borrowed fields use raw pointer-sized slots. A fixed array
is a special case because it contains complete element bodies inline.

This means that a matching `size_of`, alignment, and offset table is not enough
to make an ordinary struct a C ABI type. Ordinary struct construction allocates,
moves transfer pointer ownership, all ordinary structs are non-`Copy`, and
destruction recursively destroys fields before freeing the allocation. Extern
functions also reject ordinary structs, including borrows of ordinary structs.

The implementation has two independent field-storage decisions. Layout uses
`struct_field_layout`, while generated accessors calculate `stores_inline`
separately. The difference already causes an incorrect read when an ordinary
struct contains an extern struct. Representation work for #610 needs one shared
field-storage policy and concrete-owner-aware offsets before it changes which
fields are inline.

## Scope and method

This investigation used only repository source, tests, documentation, Git
metadata, the local Casa compiler, and the local C compiler. No web search was
used.

The local environment was Linux 6.18.33.2 x86-64 under WSL2, Casa v1.49.0, and
GCC 11.4.0 in strict C11 mode.

The Casa layout probe parsed representative declarations and called
`type_layout`, `struct_member_offset`, and `type_value_representation`. The C
probe compiled matching declarations with strict C11 and `_Static_assert` checks
for `sizeof`, `_Alignof`, and `offsetof`. Runtime probes checked an ordinary
struct that contains an extern struct and replacement of an owned ordinary
struct field. The reported alignment is compiler-internal because Casa does not
provide `align_of`. `size_of[T]` reports the inline body size, not the size of the
runtime pointer that carries an ordinary struct value
([docs/adr/0126 lines 1-24](../adr/0126-size-of-exposes-inline-storage-size.md#L1-L24)).

## Verified facts

### Layout and member offsets

`WORD_SIZE` is 8 bytes. Raw and signed scalar values use direct value
representations. `VALUE_REPR_INDIRECT` identifies a pointer-carried value
([compiler/common.casa lines 1575-1578](../../compiler/common.casa#L1575-L1578)).
Builtin field sizes and alignments are 1, 2, 4, or 8 bytes
([compiler/common.casa lines 3766-3785](../../compiler/common.casa#L3766-L3785)).

For an ordinary struct field, the compiler uses these rules:

1. An unresolved type variable occupies one 8-byte, 8-aligned slot.
2. A fixed array uses its complete inline layout.
3. Any other indirect value occupies one 8-byte, 8-aligned slot.
4. A direct scalar or borrow uses its complete type layout.

These rules are in `aggregate_field_layout`
([compiler/common.casa lines 3787-3808](../../compiler/common.casa#L3787-L3808)).
Extern structs instead validate and use the complete C-layout field type
([compiler/common.casa lines 3810-3888](../../compiler/common.casa#L3810-L3888)).
`struct_field_layout` selects between the policies
([compiler/common.casa lines 3890-3901](../../compiler/common.casa#L3890-L3901)).

The struct layout loop aligns each field, adds its size, tracks maximum
alignment, gives an empty struct size 1, and adds tail padding
([compiler/common.casa lines 3903-3930](../../compiler/common.casa#L3903-L3930)).
Member offsets use the same field-layout function
([compiler/common.casa lines 3961-3983](../../compiler/common.casa#L3961-L3983)).
Fixed arrays use `length * element size`, retain element alignment, and use a
1-byte layout when the length is zero
([compiler/common.casa lines 4038-4058](../../compiler/common.casa#L4038-L4058)).
The element size is its complete type layout. An `array[Inner N]` therefore
contains `N` consecutive `Inner` bodies even though a direct `Inner` field uses
one pointer slot. Array construction moves each indirect element body into the
array allocation through a typed store
([compiler/bytecode.casa lines 2309-2377](../../compiler/bytecode.casa#L2309-L2377),
[compiler/bytecode.casa lines 2746-2777](../../compiler/bytecode.casa#L2746-L2777)).

Borrows have an 8-byte, 8-aligned layout. Recursive layout discovery also uses
one word when it encounters a type that it is already visiting
([compiler/common.casa lines 4060-4092](../../compiler/common.casa#L4060-L4092)).
All structs and fixed arrays have the indirect standalone value representation,
regardless of their inline body layout
([compiler/common.casa lines 4099-4140](../../compiler/common.casa#L4099-L4140)).

Generic struct layout is declaration-based today. The layout loop reads the
unsubstituted member types, and bytecode asks for the size of the bare struct
name rather than a concrete specialization
([compiler/common.casa lines 3903-3930](../../compiler/common.casa#L3903-L3930),
[compiler/bytecode.casa lines 359-380](../../compiler/bytecode.casa#L359-L380)).
A generated ordinary accessor also embeds a parse-time integer offset. Generic
specialization substitutes typed operations, but it does not replace integer
operations with specialization-dependent offsets
([compiler/syntax.casa lines 4098-4108](../../compiler/syntax.casa#L4098-L4108),
[compiler/common.casa lines 598-681](../../compiler/common.casa#L598-L681),
[compiler/typechecker.casa lines 9578-9607](../../compiler/typechecker.casa#L9578-L9607)).

### Compatibility matrix

The table reports the inline allocation body on the local x86-64 target. `S/A`
means size/alignment in bytes. Offsets follow declaration order. Every row is an
ordinary Casa struct and therefore has a separate one-pointer runtime carrier.

| Case | Representative declaration | Casa S/A | Offsets | Field storage in the Casa body | Local strict C11 comparison |
|---|---|---:|---|---|---|
| Scalar | `Scalar { u8, u64, u16 }` | 24/8 | 0, 8, 16 | All fields inline | Exact body match for `uint8_t`, `uint64_t`, `uint16_t`: 24/8 at 0, 8, 16 |
| Empty | `Empty {}` | 1/1 | none | One non-field byte | Standard C11 has no empty struct. A one-byte dummy field is only a representation analogue |
| Nested | `Nested { u8, Inner, u32 }`, `Inner { u16, bool }` | 24/8 | 0, 8, 16 | `Inner` is one pointer to a separate 4/2 allocation | C by-value `Inner` gives 12/4 at 0, 2, 8. An `Inner *` field gives the Casa outer shape |
| Fixed array | `Fixed { u8, array[u16 3], u32 }` | 12/4 | 0, 2, 8 | Six array bytes inline | Exact body match for `uint16_t values[3]`: 12/4 at 0, 2, 8 |
| Generic | `Box[T] { u8, T, u8 }`, for `Box[u16]` and `Box[Inner]` | 24/8 | 0, 8, 16 | `T` is one declaration-time word. Aggregate `T` values use their pointer | C `BoxU16` is 6/2 at 0, 2, 4. A `uintptr_t` field gives the Casa physical shape. C has no generic type equivalence |
| Recursive | `Node { u64, Option[Node] }` | 16/8 | 0, 8 | `Option[Node]` is one pointer to its own allocation. Its recursive payload is also indirect | A C `OptionNode *` field gives the outer 16/8 shape. It does not reproduce Casa tags, ownership, or destruction |
| Borrowed | `View { u8, $u16, u8 }` | 24/8 | 0, 8, 16 | The borrow is one raw pointer | `const uint16_t *` gives the same body shape. C has no Casa lifetime or exclusivity contract |
| Owned field | `Owner { u8, Resource, u8 }`, where `Resource` has a 2-byte body and a drop hook | 24/8 | 0, 8, 16 | `Resource` is one owning pointer to a separate allocation | C by-value `Resource` is 6/2 at 0, 2, 4. A `Resource *` field gives the Casa physical shape but not its ownership contract |

The scalar and fixed-array rows show that parts of the current ordinary layout
already follow the same natural alignment calculation as C. This is only a body
layout match. Ordinary structs remain compiler-owned layouts with no stable ABI
contract
([docs/adr/0127 lines 1-17](../adr/0127-ordinary-layout-has-no-stable-abi-contract.md#L1-L17)).
Padding is also unspecified and can be uninitialized
([docs/adr/0137 lines 1-20](../adr/0137-aggregate-padding-is-unspecified-and-may-be-uninitialized.md#L1-L20)).

The empty case is an explicit one-byte Casa rule
([docs/adr/0132 lines 1-12](../adr/0132-inhabited-types-have-a-minimum-size-of-one-byte.md#L1-L12)).
A zero-length fixed-array field also occupies one inline byte, which has no
standard C array equivalent
([docs/adr/0155 lines 5-18](../adr/0155-a-zero-length-array-is-inhabited-and-occupies-one-byte.md#L5-L18)).

### Construction

Typechecking positional construction consumes the declared fields, binds generic
arguments, combines contained borrow origins, and produces the resolved struct
type. Named literals perform the same field checks in named order
([compiler/typechecker.casa lines 7964-7988](../../compiler/typechecker.casa#L7964-L7988),
[compiler/typechecker.casa lines 7995-8044](../../compiler/typechecker.casa#L7995-L8044)).

Both construction forms always allocate the inline body with `HeapAlloc`.
Positional construction writes fields in declaration order. Named construction
first saves supplied values in temporary locals, then allocates and writes fields
in declaration order
([compiler/bytecode.casa lines 1885-1910](../../compiler/bytecode.casa#L1885-L1910),
[compiler/bytecode.casa lines 2053-2106](../../compiler/bytecode.casa#L2053-L2106)).
The allocator rounds the requested block size to an 8-byte boundary and returns
an aligned pointer
([compiler/emitter.casa lines 314-333](../../compiler/emitter.casa#L314-L333)).
Each allocation also has an 8-byte allocator header. A separately allocated
nested owner therefore costs its rounded body, one header, and the pointer slot
in its parent
([compiler/emitter.casa lines 357-415](../../compiler/emitter.casa#L357-L415),
[compiler/emitter.casa lines 432-449](../../compiler/emitter.casa#L432-L449)).

Construction uses the same ordinary field-storage rule as layout. Scalars and
borrows use typed stores, fixed arrays copy their inline bytes, and indirect or
unresolved fields store one pointer-sized word
([compiler/bytecode.casa lines 285-357](../../compiler/bytecode.casa#L285-L357)).
This gives each nested ordinary owner a separate allocation. A fixed array is the
exception inside an aggregate because its bytes are part of the outer body.

### Access

The parser lowers dot syntax to a method call and generates a getter and setter
for every struct member
([compiler/syntax.casa lines 1373-1391](../../compiler/syntax.casa#L1373-L1391),
[compiler/syntax.casa lines 4088-4224](../../compiler/syntax.casa#L4088-L4224)).
An ordinary getter converts the receiver borrow to a raw pointer, adds its
embedded offset, then chooses one of three operations:

- load a pointer and convert it back to the indirect field type
- return a borrow to inline fixed-array or extern-struct storage
- perform a typed scalar or borrow load

The generated paths are visible in
[compiler/syntax.casa lines 4088-4157](../../compiler/syntax.casa#L4088-L4157).
The fixed-array getter behavior also has a focused parser test
([tests/compiler/test_generic_structs.casa lines 112-120](../../tests/compiler/test_generic_structs.casa#L112-L120)).

The generated getter's Stack effect returns the declared field type. The
typechecker recognizes generated getters and changes a non-`Copy` result into a
shared or exclusive projection of the receiver. It records the projected place
and preserves contained borrow origins
([compiler/typechecker.casa lines 2844-2977](../../compiler/typechecker.casa#L2844-L2977),
[compiler/typechecker.casa lines 3191-3297](../../compiler/typechecker.casa#L3191-L3297)).
A scalar field is copied. A nested owned struct is borrowed through the parent.
Existing tests cover nested reads and nested mutation
([tests/compiler/test_typed_struct_fields.casa lines 33-77](../../tests/compiler/test_typed_struct_fields.casa#L33-L77)).

### Mutation

Field assignment is lowered to a push of the root owner, generated getters for
intermediate fields, and the final generated setter. Compound assignment also
uses the getter before the setter
([compiler/typechecker.casa lines 4487-4552](../../compiler/typechecker.casa#L4487-L4552)).
The setter stores one pointer for an indirect field and uses a typed store for an
inline field
([compiler/syntax.casa lines 4181-4208](../../compiler/syntax.casa#L4181-L4208)).
The replacement value is consumed by the setter. Live borrows prevent moving or
replacing their owner
([docs/adr/0114 lines 1-27](../adr/0114-live-borrows-prevent-moving-their-owner.md#L1-L27)).

### Move, Copy, Clone, and raw-storage movement

An ordinary move transfers the one-pointer runtime value. The typechecker rejects
a move through a borrow or while the owner has an active loan, marks a moved
binding, and annotates the source operation as moving its owner
([compiler/typechecker.casa lines 1715-1774](../../compiler/typechecker.casa#L1715-L1774)).
Owner generations later prevent cleanup of the moved binding
([compiler/bytecode.casa lines 1028-1063](../../compiler/bytecode.casa#L1028-L1063)).

All ordinary structs currently fail `Copy` before field eligibility is relevant
because their value representation is indirect. Shared borrows can be duplicated,
exclusive borrows cannot, and fixed arrays depend on element `Copy`
([compiler/syntax.casa lines 5330-5458](../../compiler/syntax.casa#L5330-L5458)).
The compiler applies the same check to `dup`, `over`, and explicit `copy`
([compiler/typechecker.casa lines 8829-8875](../../compiler/typechecker.casa#L8829-L8875)).
The behavior is intentional: duplicating the struct pointer would create two
apparent owners of one allocation
([docs/adr/0158 lines 1-18](../adr/0158-copy-requires-a-raw-value-representation.md#L1-L18)).
A focused test rejects `Copy` even for a scalar-only struct
([tests/compiler/test_copy_clone.casa lines 551-568](../../tests/compiler/test_copy_clone.casa#L551-L568)).

Generated `Clone` reads each field, calls field `Clone` except for a shared
borrow, and reconstructs a new struct
([compiler/syntax.casa lines 5529-5552](../../compiler/syntax.casa#L5529-L5552)).
It therefore allocates a new outer body. Nested owned fields clone according to
their own implementations. This is semantic independent-owner duplication, not
a raw body copy.

Typed raw-memory operations have a separate movement path. Reading an indirect
type allocates a shell and copies its inline bytes. Writing an indirect type
copies the inline bytes to the destination and frees the source shell when it is
heap allocated
([compiler/bytecode.casa lines 2630-2777](../../compiler/bytecode.casa#L2630-L2777),
[compiler/emitter.casa lines 998-1083](../../compiler/emitter.casa#L998-L1083)).
Fixed-array copying uses an inline byte move into local storage
([compiler/bytecode.casa lines 2711-2727](../../compiler/bytecode.casa#L2711-L2727)).
These operations materialize or move ownership through typed storage. They do not
make an ordinary struct `Copy`.

### Destruction

Borrows need no recursive destruction. A non-`Copy` indirect value or a value
with a custom drop hook does
([compiler/common.casa lines 4155-4172](../../compiler/common.casa#L4155-L4172)).
The compiler calls a custom drop hook first. It then visits struct fields in
reverse declaration order, loads each field with the same storage policy used by
construction, recursively destroys it, and frees the outer allocation
([compiler/bytecode.casa lines 660-758](../../compiler/bytecode.casa#L660-L758),
[compiler/bytecode.casa lines 814-934](../../compiler/bytecode.casa#L814-L934)).
Fixed-array destruction visits elements in reverse order
([compiler/bytecode.casa lines 690-729](../../compiler/bytecode.casa#L690-L729)).

This ordering is part of the language contract. A replaced owner must be
destroyed immediately, a moved owner is not destroyed at its old binding, and
fields are destroyed in reverse order
([docs/adr/0050 lines 1-9](../adr/0050-destruction-is-lifo.md#L1-L9)).
Custom cleanup runs before automatic field destruction
([docs/adr/0044 lines 14-20](../adr/0044-custom-destruction-uses-a-reserved-drop-method.md#L14-L20)).
Recursive ownership transfers and destroys the complete descendant tree once
([docs/adr/0017 lines 12-20](../adr/0017-compiler-managed-recursive-indirection.md#L12-L20)).
The destruction tests exercise custom resources and a 128-level recursive owner
chain
([tests/compiler/test_destruction.casa lines 9-59](../../tests/compiler/test_destruction.casa#L9-L59),
[tests/compiler/test_destruction.casa lines 215-243](../../tests/compiler/test_destruction.casa#L215-L243)).

### C ABI boundary

An extern struct is the current C-layout contract. Its allowed fields have C
field order, alignment, padding, array stride, and tail padding. Native calls only
accept it through `$T` or `mut$T`. By-value aggregate parameters and returns are
not supported
([docs/functions-and-lambdas.md lines 186-235](../functions-and-lambdas.md#L186-L235)).
Compiler validation rejects ordinary struct fields in extern structs and rejects
borrowed ordinary structs in extern function declarations
([compiler/typechecker.casa lines 8740-8822](../../compiler/typechecker.casa#L8740-L8822)).
The corresponding tests check C layout and the rejection cases
([tests/compiler/test_extern.casa lines 78-149](../../tests/compiler/test_extern.casa#L78-L149)).
The native fixture uses C static assertions for the supported extern layout
([tests/compiler/fixtures/extern_struct.c lines 4-30](../../tests/compiler/fixtures/extern_struct.c#L4-L30)).

An ordinary struct whose body matches a C declaration is therefore still not C
equivalent as a value or function argument. It has an owning heap-pointer carrier,
compiler-defined lifetime behavior, and unspecified padding. The compiler rejects
it from native function declarations.

### Target boundary

The numeric compatibility results are target-specific. The current compiler
hard-codes an 8-byte word and its builtin size/alignment table, and extern layout
uses that table
([compiler/common.casa lines 1575-1578](../../compiler/common.casa#L1575-L1578),
[compiler/common.casa lines 3766-3785](../../compiler/common.casa#L3766-L3785)).
Pointer width, scalar alignment, C `_Bool`, and aggregate calling convention must
be checked again for another target. The matrix proves body layout only for the
local x86-64 C implementation.

Declaration order, per-field alignment and tail-padding placement, the distinction
between body layout and runtime carrier, compiler-managed recursive indirection,
borrow and ownership rules, deterministic destruction, and the absence of an
ordinary ABI promise are independent of the measured byte counts. The one-byte
empty and zero-length-array rules are also language decisions rather than C ABI
facts
([docs/adr/0017 lines 1-20](../adr/0017-compiler-managed-recursive-indirection.md#L1-L20),
[docs/adr/0127 lines 1-17](../adr/0127-ordinary-layout-has-no-stable-abi-contract.md#L1-L17),
[docs/adr/0132 lines 1-12](../adr/0132-inhabited-types-have-a-minimum-size-of-one-byte.md#L1-L12),
[docs/adr/0155 lines 5-18](../adr/0155-a-zero-length-array-is-inhabited-and-occupies-one-byte.md#L5-L18)).

## Verified current limitations

### Owned-field replacement loses the old owner

A local drop-counter probe assigned a new owning ordinary struct into an ordinary
struct field. The counter was `0` immediately after assignment and `1` after the
outer owner was dropped. Correct replacement behavior would destroy the old field
at assignment and later destroy the replacement.

The generated setter writes the replacement pointer without loading or destroying
the old pointer
([compiler/syntax.casa lines 4181-4202](../../compiler/syntax.casa#L4181-L4202)).
Outer destruction can reach only the replacement
([compiler/bytecode.casa lines 731-758](../../compiler/bytecode.casa#L731-L758)).
The existing field-assignment test replaces a field that has no observable drop
hook, so it does not detect this leak
([tests/compiler/test_typed_struct_fields.casa lines 40-46](../../tests/compiler/test_typed_struct_fields.casa#L40-L46)).
This is a verified current defect, not a proposed consequence of #610.

### An ordinary struct misreads an extern-struct field

The ordinary layout policy stores a nested extern struct as one indirect word.
Generated accessors independently mark any known extern-struct member as inline,
even when its parent is ordinary
([compiler/common.casa lines 3787-3808](../../compiler/common.casa#L3787-L3808),
[compiler/syntax.casa lines 4110-4128](../../compiler/syntax.casa#L4110-L4128)).
The getter then returns the address of the pointer slot as if it were the extern
struct body
([compiler/syntax.casa lines 4142-4156](../../compiler/syntax.casa#L4142-L4156)).

The local runtime probe stored an extern `RunInner { x: u16, flag: u8 }` in an
ordinary `RunHolder`. Reading `holder.inner.x` returned `40464` instead of the
stored `4660` in that run. The wrong value is allocation-address dependent. The
source also shows that the generated setter would perform a typed inline write at
the pointer slot. This is direct evidence that duplicated field policy is unsafe.

## Verified prototype results

A temporary prototype centralized the field placement decision across common
layout, generated accessors, and bytecode field load/store. It then stored
concrete struct fields inline. Unresolved type variables and enums remained
indirect, and standalone struct values remained one-pointer owners. The patch was
used only for this investigation and is not part of this report change.

The focused workload used these relevant shapes:

```casa
struct Scalar {
    small: u8
    large: u32
}

struct Nested {
    tag:   u8
    inner: Scalar
    tail:  u16
}

struct Box[T] {
    value: T
}

struct GenericNested {
    tag:   u8
    inner: Box[u8]
    tail:  u8
}

struct Tracked {
    value: u32
    count: ptr
}

struct OwnedNested {
    tag:   u8
    inner: Tracked
    tail:  u8
}
```

The workload passed nested access and mutation, generic nested access and
mutation, one-time owned destruction, and recursive destruction. Its body-size
results were:

| Case | Baseline Casa | Prototype Casa | Strict C11 counterpart |
|---|---:|---:|---:|
| `Nested` | 24 | 16 | 16, alignment 4, offsets 0, 4, 12 |
| `GenericNested` | 24 | 24 | Specialized `BoxU8` form: 3, alignment 1, offsets 0, 1, 2 |
| `OwnedNested` | 24 outer + 16 child = 40 live body bytes | 32 outer | 32, alignment 8, offsets 0, 8, 24 |

These sizes exclude allocator headers. The nested and owned cases show the body
compatibility and live-storage reduction that concrete inlining can provide. The
generic case stayed at its uniform declaration-time layout, so it did not match
the specialized C declaration.

The prototype did not pass the focused repository suite. This command reported
four passes and three failures:

```sh
CASA_COMPILER=/tmp/casac-610-prototype \
    tests/test_compiler.sh size_of destruction extern
```

The `size_of` test passed. The destruction filter failed, and `test_extern` and
the native `extern_struct` fixture crashed or failed. Central field placement is
therefore necessary but not sufficient. Existing projection and destruction
paths still assume that an indirect member can be materialized as an owner.

A 5,000,000-iteration scalar nested construction, access, mutation, and
destruction workload had these medians:

| Compiler | Wall time | Peak RSS |
|---|---:|---:|
| Baseline | 0.210 s | 136 KiB |
| Prototype | 0.315 s | 136 KiB |

For a 100-iteration version, debugger breakpoints counted 200 baseline
`heap_alloc` calls and 300 prototype calls. The prototype removed the persistent
nested allocation but added an allocation when destruction materialized the
inline child as an indirect owner. The current typed load does exactly this: it
allocates a shell and copies the inline body into it
([compiler/bytecode.casa lines 2630-2643](../../compiler/bytecode.casa#L2630-L2643),
[compiler/emitter.casa lines 998-1050](../../compiler/emitter.casa#L998-L1050)).
The timing is a result for this prototype, not evidence that inline fields must
be slower.

## Hypotheses and implementation implications

Everything in this section is a hypothesis or design implication. It is not a
description of current behavior.

| Option | Expected gain | Main risk or limitation | Assessment |
|---|---|---|---|
| Keep separate ordinary and extern storage policies | No representation migration | Preserves duplicated policy and gains no ordinary C compatibility | Reject as the long-term design |
| Share one layout and storage engine without changing field choices | Removes policy drift and fixes the ordinary-with-extern failure | Does not inline ordinary nested fields | Recommended first step |
| Automatically use C-compatible inline bodies for eligible ordinary fields | Matches nested C-safe bodies and removes persistent child allocations while the outer value can remain one pointer | Current projection and destruction materialize extra owners. Replacement, destruction, and offset rules must remain exact | Reconsider after address-based projection and destruction work |
| Inline most concrete acyclic fields without an ABI promise | Removes more indirection and allocations | Can enlarge outer bodies and byte moves. Generic specialization and recursive cut points become harder | Defer until measurements justify it |

The prototype supports a shared, target-specific field storage plan as the first
implementation step. The plan must describe placement, projection, load, store,
move, and destruction for a concrete owner type. Address-based projection and
destruction should operate on inline owners without allocating a temporary shell
before default inlining is enabled.

`extern struct` should remain distinct syntax for field validation, FFI
eligibility, and an ABI stability promise. It does not need a separate placement
algorithm for a field graph that an ordinary struct can also lay out inline.
Ordinary layout remains free to change in a later compiler version. Standalone
ordinary structs should remain indirect in this step, so `Copy` behavior does not
change.

1. A narrow C-compatible ordinary-struct subset could use the extern field
   whitelist and layout algorithm for nonempty, non-generic, nonrecursive structs
   with no borrow or field that needs nontrivial destruction. This could make
   scalar, nested eligible leaf, and nonempty fixed-array bodies C-compatible in
   one build. It would not by itself provide C by-value calls because the
   standalone struct value would still be an owning pointer.

2. Giving eligible ordinary structs a direct value representation could make
   by-value C equivalence and aggregate `Copy` possible. This is a broad change.
   Construction needs destination storage instead of unconditional allocation.
   Variables and calls need multiword direct values, accessors need stable
   addresses for borrows, moves need byte movement, and destruction must work for
   direct owners without freeing a shell.

3. Reusing only `extern_struct_field_layout` is insufficient. Nested ordinary
   ownership still needs a decision between inline ownership and an owned pointer.
   Recursive edges still require indirection. Borrowed fields still carry origin
   and exclusivity rules. Owned fields still require deterministic replacement
   and destruction.

4. Any specialization-dependent generic layout needs deferred offsets. Current
   generic accessors embed declaration-time offsets, while typed operations are
   specialized later. A concrete layout without concrete offsets would repeat the
   verified ordinary-with-extern accessor failure in another form.

5. The safest compiler seam is one shared field-storage plan that returns size,
   alignment, inline or indirect storage, and projection, load, store, move, and
   destruction policy for a concrete owner type. Layout, construction, generated
   accessors, raw typed operations, and destruction should consume that result.
   Ordinary accessor offsets should use a deferred, concrete-owner-aware operation
   when they can vary by specialization.

6. A stable ordinary C-layout contract would need a successor or amendment to
   ADR-0127. Tests should distinguish four promises: inline body layout, runtime
   value carrier, native calling convention, and ownership semantics. Equal size
   and offsets prove only the first promise.

7. The owned-field replacement and ordinary-with-extern accessor defects need
   focused regression tests before representation changes. Otherwise a new layout
   can preserve or hide existing incorrect lifecycle behavior.

## Decision and follow-up

[ADR-0162](../adr/0162-ordinary-and-extern-structs-share-field-storage-planning.md)
accepts one target-specific storage plan and keeps `extern struct` as the ABI
validation and stability contract. It limits automatic inlining to eligible C
field graphs after address-based projection and destruction are available.

- [#619](https://github.com/frendsick/casa/issues/619) fixes destruction of a
  replaced owned field.
- [#620](https://github.com/frendsick/casa/issues/620) centralizes aggregate
  field placement and fixes the ordinary-extern policy mismatch.
- [#621](https://github.com/frendsick/casa/issues/621) adds eligible field
  inlining after #619 and #620.
