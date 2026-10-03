# Reference notation

## Declarations

A declaration names the function, its type parameters, its input parameters,
and its outputs. A method declaration also names its receiver. For example,
this excerpt appears inside `impl List` in
[the standard library](../../../../lib/std.casa):

```text
pub fn push [T] self:mut$List[T] item:T
```

The body is omitted. `List` is the type's name inside its defining module.
Imported programs name the type `std::List` after `import "std"`.

Inputs are consumed in declaration order: `self`, then `item`. The call form
is `item numbers.push`. No `->` means the method produces no output.

## Stack effects

```text
mut$List[T] T -> None
```

A stack effect records only input and output types. Inputs are listed from
the top of the stack downward. Outputs are listed in push order, so the final
output is on top. `None` denotes an empty side in this documentation notation.
It is not an `Option::None` value or a return type to add to the declaration.

A stack snapshot, such as `[3, 10]`, shows the top on the right. Snapshots show
the current values in bottom-to-top order, whereas effect inputs show
consumption order.

## Ownership in a declaration

| Form | Meaning |
|---|---|
| `T` | A value parameter. A non-`Copy` owner moves into the call |
| `$T` | A shared borrow |
| `mut$T` | An exclusive mutable borrow |

Calls can borrow an available owner automatically. A returned borrow keeps
its source loaned until the borrow's last use. A stack effect alone does not
describe mutation or failure, so method entries state those contracts in prose.

## Function types

`fn[i64 i64 -> i64]` is a function type containing a stack effect. It has no
function name or parameter names. The type of `&subtract` in the
[call example](../explanation/stack-and-calls.md#follow-one-call) has this form.
