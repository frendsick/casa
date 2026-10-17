# Control Flow and Patterns

Casa conditions consume a `bool`. Branches and loops must keep the value stack
consistent.

## Conditionals

Use `if`, optional `elif` and `else` branches, and `fi`:

```casa
82 = score
if score 90 >= then
    "excellent"
elif score 60 >= then
    "pass"
else
    "retry"
fi
print
```

Each condition is followed by `then`. All continuing branches must leave the
same types on the stack. If there is no `else`, the body must leave the stack
unchanged because the condition can be false.

Bindings created inside a branch exist only in that branch. An assignment
updates an outer binding when one exists. Otherwise it creates a branch-local
binding.

An owned binding remains available after `fi` only when every branch that can
reach `fi` leaves it available. A branch can consume the owner when it returns,
or when it assigns a replacement before the join:

```casa
if should_stop then
    request send
    return
fi
request send
```

Loans also merge at the join. If a borrow can refer to an owner on any branch,
that owner stays loaned until the borrow's last use.

## While loops

`while` evaluates its condition before each iteration:

```casa
0 = index
while index 5 < do
    index print "\n" print
    1 += index
done
```

Use `break` to leave the innermost loop. Use `continue` to start its next
condition check.

The stack at the end of the body, at `break`, and at `continue` must match the
stack before the loop. A loop cannot accumulate values between iterations.
The same rule applies to ownership. Each normal back-edge and `continue` must
restore the availability and loans present at the loop header. Loop exits merge
the condition-false path with every reachable `break`.

## For loops

`for` consumes an iterator and binds each yielded value:

```casa
import "std"

for number in [1, 2, 3].iter do
    number print "\n" print
done
```

The iterable expression is evaluated once. `break` and `continue` work as they
do in a `while` loop. The iterator must provide a `next` method returning
`Option[T]`. This can be an inherent method or an `Iterable[T]` implementation.
The loop borrows its stored iterator for each `next` call and gives the body
each `Some` payload. `None` ends the loop.

An owned iterator is destroyed when the loop ends, before the next statement.
This applies to exhaustion and `break`. `continue` retains the iterator.
Function return and `?` also clean it up once. As elsewhere, panic and process
exit do not unwind owners. A loop over a borrowed iterator ends that borrow
and leaves the caller's iterator available.

A lending cursor such as `ListIterMut[T]` lends each payload through the stored
cursor. Finish using the payload before the next iteration. See
[Collections](collections.md) for standard iterator sources.

## Test and bind an enum variant

`is` reports whether an enum value has a given variant:

```casa
enum Shape {
    Circle(i64)
    Rectangle(i64 i64)
    Point
}

10 Shape::Circle = shape
shape Shape::Circle is print # true
```

In an `if` or `elif` condition, the pattern can bind carried values:

```casa
if shape Shape::Circle(radius) is then
    f"radius: {radius}\n" print
elif shape Shape::Rectangle(width height) is then
    width height * print
fi
```

These bindings exist only in the branch whose pattern created them. They
cannot shadow an accessible local binding. A binding pattern is not valid
outside an `if` or `elif` condition.

The payload binding follows the subject capability. An owned subject gives an
owned binding for a non-`Copy` payload and is consumed only when the check
succeeds. A `$T` subject gives a shared payload borrow. A `mut$T` subject gives
an exclusive payload borrow. A `Copy` payload is copied and does not consume
the subject.

The original owner stays available in `else` and on other unmatched paths. It
is unavailable after `fi` if a continuing matched path moved its payload. A
matched path that returns does not affect the ownership join.

## Match a value

`match` selects one pattern and can produce a value:

```casa
import "std"

enum Status {
    Ready
    Busy
    Failed(str)
}

Status::Ready = status
status match
    Status::Ready => "ready".to_str
    Status::Busy => "busy".to_str
    Status::Failed(message) => f"failed: {message}"
end
print
```

Each arm has a pattern, `=>`, and a body. Use braces for a multiline body:

```casa
shape match
    Shape::Circle(radius) => {
        "circle: " print
        radius print
    }
    Shape::Rectangle(width height) => width height * print
    Shape::Point => "point" print
end
```

Pattern bindings exist in that arm only. Struct patterns can name all or some
fields:

```casa
person match
    Person { name: name } => name print
end
```

An owned match subject is consumed. Non-Copy fields move into named bindings,
and the compiler destroys omitted fields.

A borrowed match subject stays available. A `$T` subject binds shared field
borrows. A `mut$T` subject binds exclusive field borrows. Omitted fields stay
in the subject and are not destroyed.

Literal patterns work with booleans, integers, characters, and strings. `_`
matches any remaining value.

## Nested patterns and alternatives

Payload and field positions accept patterns recursively:

```casa
import "std"

fn read value:std::Option[std::Result[i64 i64]] -> i64 {
    value match
        std::Option::Some(std::Result::Ok(number)) |
        std::Option::Some(std::Result::Error(number)) => number
        std::Option::None => 0
    end
}
```

Use `|` between complete arm patterns to share a body. Put spaces around `|`.
An unbraced body ending in a literal followed by bitwise `|` can look like the
start of the next arm's alternatives. Put that body in braces, for example
`true => { flags 1 | } false => 0`.

Each alternative must bind the same names with the same types and capabilities.
The names can occur at different payload or field positions. A name cannot be
bound twice in one alternative. Alternatives inside payload or field positions
are not supported.

A nested bare name binds that value. `_` ignores it. Literal patterns test the
value at that position. Omitted enum payloads and omitted struct fields match
any value. All nested bindings follow the subject's ownership or borrow
capability. On success, owned values move into their bindings, and ignored
fields and consumed wrappers are destroyed.

Nested patterns also work in enum-root `is` checks:

```casa
if value std::Option::Some(std::Result::Ok(number)) is then
    number print
fi
```

A failed nested check retains the complete subject. A successful check that
binds only `Copy` payloads also retains an owned subject.

## Exhaustiveness

Every `match` must handle every possible input:

- An enum match must cover every variant and its nested payload patterns, or use `_`.
- A boolean match must cover `true` and `false` or use `_`.
- An integer, character, or string match must use `_`.
- A struct pattern is exhaustive when all named field patterns are exhaustive.
  `_` is also valid.

Coverage combines unguarded alternatives and arms. For example, `Some(true)`
and `Some(false)` cover the boolean payload of `Some`. With multiple fields,
every combination must be covered. An arm or alternative that matches no new
values is a compile-time error. All continuing arms must leave
the same stack effect, just like branches of an `if`.

Ownership also joins across arms. An arm that returns does not affect the join.
An owner consumed by any arm that reaches `end` is unavailable after `end`.

## Guards

Add `if condition` before `=>` to restrict an arm:

```casa
fn classify number:i64 -> str {
    number match
        _ if number 0 > => "positive"
        _ if 0 number == => "zero"
        _ => "negative"
    end
}
```

A guard must produce one `bool`. Pattern bindings are available in the guard.
A guard cannot move an owner because a false guard must leave the subject and
outer bindings available to later arms. A guarded arm does not count toward
exhaustiveness because its condition can be false.

Alternatives are tried from left to right. The first matching alternative
supplies bindings to the shared guard. If that guard fails, execution continues
with the next arm. The guard is not retried with another alternative from the
same arm.

See [examples/enum.casa](../examples/enum.casa) for nested patterns, alternatives,
and guards on enum payloads.
