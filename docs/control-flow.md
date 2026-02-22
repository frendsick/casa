# Control Flow

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

Compatible types are allowed across branches. For example, `none` (bare `option`) and `some` (`option[T]`) can appear in different branches. The type checker unifies them to the more specific type:

```casa
if condition then
    none           # option
else
    42 some        # option[int]
fi
# result type: option[int]
```

The same applies to bare `array` and `array[T]`, and to `any` with any concrete type.

If there is no `else` branch, the `if`/`elif` branches must not change the stack at all (since the "no match" path leaves the stack unchanged).

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
