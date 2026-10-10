# Reference Notation

## Signatures and calls

A function signature names its parameters in consumption order. The first parameter
receives the topmost value. This complete program prints `7`:

```casa
fn subtract left:i64 right:i64 -> i64 { left right - }

3 10 subtract print
```

Save it as `sample.casa` in the repository root and run
`./casac sample.casa -r`. The call pushes `3`, then `10`. `left` receives `10`
and `right` receives `3`.

| Casa expression | Conventional reading | Result |
|---|---|---|
| `3 10 subtract` | `subtract(10, 3)` | `7` |
| `10 3 -` | `10 - 3` | `7` |
| `0 1 <` | `0 < 1` | `true` |

Named functions and receiver methods use the topmost value as their first
argument. Binary symbolic operators use the topmost value as their right
operand. Source evaluation proceeds left to right in each case.

A method signature also describes the receiver. For example, this excerpt
from `impl List` omits the body:

```text
pub fn push [T] self:mut$List[T] item:T
```

`push` consumes `self`, then `item`. Its call form is `item numbers.push`. No `->` means
the method produces no output. For a function with multiple outputs, the types after
`->` use push order. The final output is on top.

## Stack effects

A stack effect contains input and output types without parameter names:

```text
mut$List[T] T -> None
```

Inputs are listed from the topmost consumed value downward. Outputs are listed
in push order. `None` means that side of the effect is empty. It is not an
`Option::None` value or a return type to write in a function signature.

A stack snapshot uses a different convention: the top is on the right.
Immediately before `subtract` runs above, the snapshot is `[3, 10]`.

| After | Stack, top on the right |
|---|---|
| `3` | `[3]` |
| `10` | `[3, 10]` |
| `subtract` | `[7]` |
| `print` | `[]` |

`i64 i64 -> i64` states that two integers are consumed and one is produced.
It does not identify subtraction's left and right operands. Use the named
parameters and concrete call to determine their roles. For binary symbolic
operators, inputs still use consumption order: right operand, then left operand.
For example, `<<: u64 T -> T` describes `value count <<`, where `count` is `u64`.

## Function types

A function value includes its effect inside `fn[...]`. For example,
`&subtract` has type `fn[i64 i64 -> i64]`. The type has no function name or
parameter names. See [Function values](functions-and-lambdas.md#function-values)
for callbacks and `exec`.

## Ownership in parameters

| Parameter type | Contract |
|---|---|
| `T` | Consume a value, moving it when it is a non-`Copy` owner |
| `$T` | Borrow for shared access |
| `mut$T` | Borrow for exclusive mutable access |

Calls borrow available owners automatically when the parameter requires it.
A signature alone does not describe all mutation, failure, or borrowing
conditions. Read those conditions beside the operation. The
[ownership reference](ownership.md) gives the language rules.

## Library names and examples

A module import exposes qualified names. Write `std::List` and `std::Option`
after `import "std"`. Receiver calls such as `numbers.push` use the receiver's
type to find the method. Primitive namespaces such as `str::substring` retain
their spelling. See [Modules](modules.md#qualified-names-and-module-identity).

Method tables list the method name, signature, and description. Signatures use
local library names, such as `List` for `std::List`. The description states
bounds inherited from an `impl` block.
The Signature column omits the body and visibility modifiers such as `pub`.
Each method heading uses the qualified name, such as `array[T N]::clone`.
The signature follows the heading, before the behavior description. The Method
column uses only the method name and links to the automatic heading anchor.
Keep signatures in both places for table lookup and direct heading links.
Explain behavior in complete sentences, with an explicit subject for each
operation.
Method descriptions should state input limits, output ownership, failure,
mutation, or callback order that the signature cannot express. Avoid a second
summary sentence when the following contract already explains the operation.
Method tables and their entries use alphabetical order. Independent lookup
lists also use alphabetical order. Examples keep execution order, and numeric
families keep increasing width or argument count.
Call forms are fragments and name any bindings they assume.

Complete programs include their imports. Save a library example as
`sample.casa` in the repository root and run `./casac sample.casa -L lib -r`.
Fragments that extend an earlier example use its declarations and bindings.
Expected output appears in comments for one value or in a separate block for
several lines. Intentionally invalid examples are identified in their text.
