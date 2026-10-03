# Constants use bounded target-independent expressions

related issue: [Choose the compile-time evaluation surface](https://github.com/frendsick/casa/issues/657).

Bounded constant expressions retain value relationships without executing user
functions. User-defined function calls are excluded from constant expressions.

## Declarations and values

Keep `const NAME VALUE`, earlier-constant references, `pub const`, and postfix
blocks. An optional annotation specifies the type:

```casa
const ELEMENT_COUNT 128
const ELEMENT_BYTES 8
const BUFFER_BYTES:u64 { ELEMENT_COUNT ELEMENT_BYTES * }
const SCALE:f32 { 0.1 2.0 * }
const LABEL "buffer"
const ENABLED true
const MARKER 'B'
```

An annotation names a supported primitive value type: an integer width, `f32`,
`f64`, `bool`, `char`, or `str`. No aggregate, pointer, borrow, callable, or owned
runtime object is a constant value. Strings are static UTF-8 text views.

Unannotated integer and float literal declarations preserve their exact literal
information for contextual typing at each use. An unannotated alias preserves
the referenced constant's value and typing category. An annotation constrains
its initializer. It does not convert an already typed value.
Contextual literals must still be valid and representable in at least one
supported numeric type. Each use checks its selected width, even when another
use of the same constant is valid at a wider width.

A block is checked and evaluated once at its declaration. Its annotation and
typed operands constrain numeric literals through the primitive operations.
Remaining unconstrained integers default to `i64`, and floats to `f64`, before
evaluation. Numeric block results and annotated constants keep their resolved
width at ordinary value uses. They are not converted back to contextual
literals. For example, `const N { 1 2 + }` produces `i64`, while
`const N:u8 { 1 2 + }` produces `u8`. An ordinary use needing another width uses
the existing explicit conversion operations outside the constant block.

## Closed expression surface

A block is a finite postfix sequence of literals, visible constant references,
the operations below, whitespace, and comments. It starts with an empty stack
and must finish with exactly one value.

| Operands | Allowed operations |
| --- | --- |
| Same-width integers | `+`, `-`, `*`, `/`, `%`, `&`, `\|`, `^`, `==`, `!=`, `<`, `<=`, `>`, `>=` |
| One integer | `~` and `!`. Integer `!` tests for zero and produces `bool`. |
| Integer value and `u64` count | `<<`, `>>` |
| Same-width floats | `+`, `-`, `*`, `/`, `==`, `!=`, `<`, `<=`, `>`, `>=` |
| Booleans | `&&`, `\|\|`, `==`, `!=`, `!` |
| Any supported constant values | `dup`, `drop`, `swap`, `over`, `rot` |

Operators have their primitive meaning. They never dispatch to a user-defined
trait implementation. There are no implicit integer/float or width conversions.
String and character values support references and stack manipulation, but no
comparison, concatenation, arithmetic, or conversion inside constant blocks.

Arithmetic retains left-to-right operand order: `10 3 -` is `10 - 3`.
Comparisons retain the topmost value as their first operand: `0 1 >` is
`1 > 0`. Shifts consume the topmost value as the count: `8 1 >>` is `8 >> 1`.
Boolean operators are eager. With stack contents written bottom to top, `rot`
maps `a b c` to `b c a`. The other four stack intrinsics retain their ordinary
effects. All permitted constant values are Copy, and these intrinsics execute
no user-defined cloning or cleanup.

Reject function and method calls, numeric conversion calls, trait dispatch,
control flow, nested blocks, local bindings, runtime variables, aggregate
construction, allocation, foreign operations, and all other intrinsics.
Negative numeric literals remain available. No infix grammar or general
compile-time function execution is added.

## Numeric results and failures

Use the existing integer and floating-point contracts from ADR-0018,
ADR-0028, ADR-0141, ADR-0144, and ADR-0145 at the resolved width.

- Integer `+`, `-`, and `*` overflow is a source error. Division and remainder
  truncate toward zero. Zero divisors and signed minimum divided or remaindered
  by `-1` are source errors. Arithmetic never silently wraps.
- A shift count must be nonnegative and smaller than the value's width. Left
  shift discards shifted-out bits. Signed right shift preserves the sign, and
  unsigned right shift inserts zero bits.
- Float literals round directly to the selected width, with no intermediate
  `f64` rounding for `f32`. Finite source literals that overflow that width are
  errors. Float operations round each result to the operand width using
  round-to-nearest, ties-to-even. Preserve subnormals and signed zero.
- Float arithmetic may produce infinity or NaN, including division by zero.
  Comparisons follow IEEE partial comparison: NaN compares unequal to every
  value, and its ordered comparisons are false. `%` remains integer-only.
  Do not reassociate expressions or fuse multiply-add. Arithmetic NaN payloads
  remain unspecified.

Report unsupported syntax or operations, operand-kind and width mismatches,
literal range errors, integer arithmetic failures, stack underflow, and zero or
multiple final values at their source locations. Range diagnostics name the
value and required type. Arithmetic diagnostics name the operator and operands.
Diagnose removed `const fn` declarations with a runtime migration hint.

Reject a failed declaration without publishing a usable substitute value.
Dependent uses refer to that failed declaration. A diagnostic must not be
followed by an invalid stack access, compiler panic, or executable output from
the failed compilation. Unused constants receive the same checks.

## References, imports, and generic parameters

An initializer may refer only to preceding constants in its own module and
public constants from imports preceding that initializer. References can be
qualified or otherwise visible under the separately selected import contract.
Reject self-reference, forward references, unknown names, non-constant names,
and private imported names. Imported public values may depend on their defining
module's private constants without exposing those names to the importer.

Discover modules and reject import cycles under ADR-0068. Elaborate dependency
constants before importer constants, then evaluate each module's declarations
in source order. Each constant has one resolved value per compilation snapshot.
This requires import and visibility facts before evaluation.
Constant type arguments accept literal values, visible named constants, and
symbolic constant parameters. The admitted kinds remain integers, `bool`, and
`char` under ADR-0153. Float and string type arguments remain rejected even
though those kinds are valid named constants. No inline block or arithmetic
grammar is added inside type arguments.

A named integer type argument supplies its mathematical value, checked against
the parameter's declared width. This value-level rule does not introduce
implicit conversions at ordinary value uses. Accept the complete signed and
unsigned ranges, including negative signed arguments and values above `i64`
maximum for `u64`. Canonical values, not declaration names, identify instances:
`array[u8 BUFFER_BYTES]` and the equivalent literal length denote the same type.
Resolve named arguments from the module's elaborated constants with ordinary
visibility. Initializer source ordering does not restrict ordinary bodies or
type uses to constants preceding those uses.

Generic parameters remain symbolic while checking generic bodies and types.
Preserve forwarding to another generic declaration and substitute concrete
values during specialization, with kind and declared-width checks. Do not force
a symbolic parameter through named-constant initializer evaluation. Arithmetic
on such parameters in ordinary function bodies remains ordinary typed code.
Named constant declarations require concrete initializers and do not introduce
generic-dependent constants or symbolic type-level arithmetic.

## Layout and compiler ownership

Exclude `size_of` and every other target-layout query from constant initializers
and constant type arguments. There is no symbolic layout-dependent constant
expression language. Ordinary code retains `size_of[T]`. Its semantic check
consults the current layout, and the backend supplies its value.

Keep constant checking and evaluation private to the compiler. It consumes
parsed expressions and resolved constant references and either establishes a
value or reports a source failure. Reuse primitive typing and numeric rules.
It needs no function bodies, call frames, runtime state, recursion protocol, or
backend layout. The front-end and semantic seam decisions assign its concrete
interface and ownership within the accepted compilation snapshot.

## Tradeoffs

Literal/reference-only constants would remove more evaluator behavior, but
would replace `ELEMENT_COUNT ELEMENT_BYTES *` with a manually synchronized
number. Bounded expressions preserve that relationship. Keeping `const fn`
would also preserve reusable computations, but retain function-body validation,
parameter binding, calls, cycle tracking, and opportunistic call folding.
The selected contract removes that machinery while accepting the additional
cost of typed integer/float evaluation, stack operations, and named type arguments.
