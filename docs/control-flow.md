# Control Flow and Patterns

Casa conditions consume a `bool`. Branches and loops must keep the value stack
consistent.

## Conditionals

Use `if`, optional `elif` and `else` branches, and `fi`:

```casa
82 = score

if 90 score >= then
    "excellent"
elif 60 score >= then
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

## While loops

`while` evaluates its condition before each iteration:

```casa
0 = index
while 5 index < do
    index print "\n" print
    1 += index
done
```

Use `break` to leave the innermost loop. Use `continue` to start its next
condition check.

The stack at the end of the body, at `break`, and at `continue` must match the
stack before the loop. A loop cannot accumulate values between iterations.

## For loops

`for` consumes an iterator and binds each yielded value:

```casa
import "std"

for number in [1, 2, 3].iter do
    number print "\n" print
done
```

The iterable expression is evaluated once. `break` and `continue` work as they
do in a `while` loop. See [Collections](collections.md) for standard iterators
and [`Iterable`](traits.md#built-in-trait-iterablet) for custom iterators.

## Test and bind an enum variant

`is` consumes an enum value and reports whether it has a given variant:

```casa
enum Shape {
    Circle(i64)
    Rectangle(i64 i64)
    Point
}

10 Shape::Circle = shape
shape Shape::Circle is print    # true
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

## Match a value

`match` selects one pattern and can produce a value:

```casa
enum Status { Ready Busy Failed(str) }

Status::Ready = status
status match
    Status::Ready => "ready"
    Status::Busy => "busy"
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

Literal patterns work with booleans, integers, characters, and strings. `_`
matches any remaining value.

## Exhaustiveness

Every `match` must handle every possible input:

- An enum match must cover every variant or use `_`.
- A boolean match must cover `true` and `false` or use `_`.
- An integer, character, or string match must use `_`.
- One matching struct pattern is exhaustive. `_` is also valid.

Duplicate unguarded arms are compile-time errors. All arms must leave the same
stack effect, just like branches of an `if`.

## Guards

Add `if condition` before `=>` to restrict an arm:

```casa
fn classify number:i64 -> str {
    number match
        _ if 0 number > => "positive"
        _ if 0 number == => "zero"
        _ => "negative"
    end
}
```

A guard must produce one `bool`. Pattern bindings are available in the guard.
A guarded arm does not count toward exhaustiveness because its condition can be
false.

See [`examples/match_guard.casa`](../examples/match_guard.casa) for a runnable
guard example.
