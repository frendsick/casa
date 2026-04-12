# Control Flow

Casa provides conditionals, loops, and pattern matching for controlling program flow. All conditions must leave a `bool` on the stack.

## Conditionals

Casa uses `if`/`elif`/`else`/`fi` for branching. Each condition must be followed by `then`.

### Syntax

```
if <condition> then
    <body>
elif <condition> then
    <body>
else
    <body>
fi
```

The condition must leave a `bool` on top of the stack (it is consumed by `then`).

### Basic Example

```casa
42 = guess
42 = number

if guess number > then
    "Too low" print
elif guess number < then
    "Too high" print
else
    "Correct!" print
fi
```

### Stack Interaction

Conditionals can leave values on the stack, but all branches must leave the stack in the same state. This is enforced by the type checker.

```casa
42 = guess
42 = number

if guess number > then
    "Too low"
elif guess number < then
    "Too high"
else
    "Correct!"
fi

print    # prints whatever the branch pushed
```

### Stack Consistency

All branches of a conditional must produce the same stack effect. The following is a type error:

```casa
# ERROR: branches leave different stack states
if true then
    1
else
    "hello"
fi
```

The first branch pushes an `int`, the second pushes a `str` — the type checker rejects this.

Compatible types are allowed across branches. For example, `Option::None` (bare `Option`) and `Option::Some` (`Option[T]`) can appear in different branches. The type checker unifies them to the more specific type:

```casa
if condition then
    Option::None           # Option
else
    42 Option::Some        # Option[int]
fi
# result type: Option[int]
```

The same applies to bare `array` and `array[T]`, and to `any` with any concrete type.

If there is no `else` branch, the `if`/`elif` branches must not change the stack at all (since the "no match" path leaves the stack unchanged).

### Variant Checking with `is`

The `is` keyword can be used in `if`/`elif` conditions to check and destructure enum variants. See [Enums — Variant Checking with `is`](enums.md#variant-checking-with-is) for details.

```casa
enum Shape {
    Circle(int)
    Rectangle(int int)
    Point
}

10 Shape::Circle = shape

if shape Shape::Circle(radius) is then
    "radius=" print
    radius print
elif shape Shape::Rectangle(w h) is then
    "area=" print
    w h * print
fi
```

## Loops

Casa has `while`/`do`/`done` loops with `break` and `continue`.

### Syntax

```
while <condition> do
    <body>
done
```

The condition must leave a `bool` on top of the stack (consumed by `do`).

### Basic Example

```casa
0 = i
while i 10 > do
    i print
    1 += i
done
```

### Infinite Loops

Use `true` as the condition with `break` to exit:

```casa
0 = i
while true do
    i print
    1 += i
    if i 5 <= then
        break
    fi
done
```

### `break` and `continue`

- `break` — exit the innermost loop immediately
- `continue` — skip the rest of the loop body and jump to the condition

```casa
0 = index
while true do
    index print

    if index 15 <= then
        break
    fi

    if index 2 % 0 == then
        3 += index
        continue
    fi

    1 += index
done
```

### Stack Consistency in Loops

The stack state must be identical:
1. Before the loop starts and after the loop ends
2. At the start of each iteration (before the condition)
3. At every `break` and `continue` point

The type checker enforces that loops do not accumulate or consume stack values across iterations.

## For Loops

Casa has `for`/`in`/`do`/`done` loops that iterate over any value whose type
provides a `next` method returning `Option[T]`.

### Syntax

```
for <ident> in <iterable> do
    <body>
done
```

The iterable expression is evaluated once. On each iteration the loop calls
`<iterable_type>::next`; if it returns `Option::Some(value)`, `value` is
bound to `<ident>` and the body runs. If it returns `Option::None`, the loop
exits. `break` and `continue` work the same way as in `while`.

### Iterating Standard Collections

The standard library provides `.iter` for `array[T]`, `List[T]`, and `str`:

```casa
[1 2 3] = nums
for n in nums.iter do
    n print "\n" print
done

for c in "casa".iter do
    c print "\n" print
done
```

### Custom Iterators

Any struct that provides a `next` method returning `Option[T]` can be used
in a `for` loop. The `Iterable[T]` trait in `lib/std.casa` documents the
contract and provides default methods like `collect`, `map`, `filter`, and
others (see [Traits -- Iterable](traits.md#built-in-trait-iterablet)):

```casa
trait Iterable[T] {
    fn next self:self -> Option[T]

    # default methods: collect, map, filter, fold, count, any, all, find
}
```

A `Counter` that yields integers from `0` up to (but not including) a
limit:

```casa
struct Counter { value:int limit:int }

impl Counter {
    fn next self:Counter -> Option[int] {
        if self Counter::limit self Counter::value >= then
            Option::None (Option[int]) return
        fi
        self Counter::value = current
        self Counter::value 1 + self->value
        current Option::Some
    }
}

Counter { value: 0 limit: 5 } = c
for x in c do
    x print "\n" print
done
```

Note that iterators are stateful: a single `Counter` instance can be
consumed only once. Re-bind the iterable in each outer iteration if you
want to restart it.

## Match

Casa has exhaustive pattern matching using `match`/`end`. Match works with enum types, struct types, and literal types (`bool`, `int`, `char`, `str`).

### Syntax

```
<value> match
    <pattern1> => <body>
    <pattern2> => <body>
    ...
end
```

The `match` keyword pops a value from the stack. Each arm specifies a pattern followed by `=>` and a body. The block is closed with `end`.

### Block Arm Bodies

Multiline arm bodies require `{}`:

```casa
circle match
    Shape::Circle(radius) => {
        "Circle with radius: " print
        radius print
        "\n" print
    }
    Shape::Rectangle(width height) => {
        width height * print
        "\n" print
    }
    Shape::Point => "point\n" print
end
```

Single-line arms do not need braces. Braced and unbraced arms can be mixed freely.

### Enum Matching

```casa
enum Color { Red Green Blue }

Color::Green match
    Color::Red => "red" print
    Color::Green => "green" print
    Color::Blue => "blue" print
end
```

### Literal Matching

Match on `bool`, `int`, `char`, and `str` values using literal patterns:

```casa
fn describe n:int {
    n match
        0 => "zero" print
        1 => "one" print
        _ => "other" print
    end
}

flag match
    true => "yes" print
    false => "no" print
end

ch match
    'a' => "letter a" print
    _ => "something else" print
end

name match
    "Alice" => "Hi Alice" print
    "Bob" => "Hi Bob" print
    _ => "Hi stranger" print
end
```

### Exhaustiveness

All match blocks must be exhaustive:

- **Enums**: All variants must be covered, or a wildcard `_` arm must be present.
- **Bool**: Both `true` and `false` must be covered, or a wildcard `_` arm must be present.
- **Int, char, str**: A wildcard `_` arm is always required (infinite domain).
- **Structs**: A struct pattern or wildcard `_` arm must be present.

Missing arms are a compile-time error. Duplicate arms are also rejected.

```casa
color match
    Color::Red => "red" print
    _ => "other" print
end
```

### Match as Expression

Match arms can leave values on the stack. All arms must produce the same stack effect:

```casa
Color::Blue match
    Color::Red => 0
    Color::Green => 1
    Color::Blue => 2
end
print    # 2
```

See [Enums](enums.md) for full details on enum types and match.

## Nested Control Flow

Conditionals and loops can be freely nested:

```casa
1 = index
30 = last

while index last >= do
    index 3 % 0 == = fizz
    index 5 % 0 == = buzz

    if fizz buzz && then
        "FizzBuzz" print
    elif fizz then
        "Fizz" print
    elif buzz then
        "Buzz" print
    else
        index print
    fi

    1 += index
done
```

See [`examples/fizzbuzz.casa`](../examples/fizzbuzz.casa) for the full program.

## See Also

- [Enums](enums.md) -- enum types and variant checking with `is`
- [Functions and Lambdas](functions-and-lambdas.md) -- function definitions and lambdas
- [Types and Literals](types-and-literals.md) -- `Option` and `Result` type unification in branches
