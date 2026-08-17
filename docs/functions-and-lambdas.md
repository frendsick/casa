# Functions and Lambdas

Casa functions consume stack values and can push results. Functions are
declared at global scope and called by name.

## Declare and call a function

```casa
fn divide dividend:i64 divisor:i64 -> i64 {
    dividend divisor /
}

3 12 divide print    # 4
```

Parameters are listed in consumption order. The first parameter receives the
topmost value, so `dividend` receives `12` and `divisor` receives `3` in this
call.

Use unnamed inputs when a local name adds no clarity:

```casa
fn square i64 -> i64 { dup * }

6 square print    # 36
```

Omit `->` when a function pushes no result:

```casa
fn greet name:str {
    f"Hello, {name}!\n" print
}
```

A function can push multiple results by listing each output type after `->`.
Calls can appear before the declaration. Use `return` to leave a function
early. Every exit path must produce the declared outputs.

See [Generics and Traits](traits.md) for type parameters and trait bounds.

## Function values

`&name` pushes a named function as a value. `exec` calls the function value on
top of the stack:

```casa
fn increment value:i64 -> i64 { value 1 + }

&increment = operation
41 operation exec print    # 42
```

Its type records the stack effect. For example, `&increment` has type
`fn[i64 -> i64]`. A function can accept that type as a parameter:

```casa
fn apply operation:fn[i64 -> i64] value:i64 -> i64 {
    value operation exec
}

40 { 2 + } apply print    # 42
```

The function value must be on top when `exec` runs. Its arguments stay below
it.

## Bindings

`= name` pops the top value and binds it. The first assignment fixes the
binding's type:

```casa
42 = count
1 += count
count print
```

A binding inside a function is local, even when a global has the same name.
Declare a global at the start of a function before you assign it:

```casa
0 = COUNTER

fn increment_counter {
    global COUNTER
    1 += COUNTER
}
```

`global NAME` must appear before other statements. The global must already
exist. Lambdas cannot declare globals.

Use `= name:Type` when the value needs an explicit type:

```casa
Option::None = result:Option[i64]
```

See [Operators](operators.md#assignment) for assignment forms.

## Lambdas and closures

Braces create an anonymous function value:

```casa
{ 1 + } = increment
41 increment exec print    # 42
```

The compiler infers a lambda's stack effect from its body and the context in
which it is used. Here, `increment` has type `fn[i64 -> i64]`.

A lambda can capture bindings from its enclosing scope. Captured values are
copied when the lambda is created:

```casa
10 = offset
{ offset + } = add_offset
32 add_offset exec print    # 42
```

Use a lambda for a short callback. Use a named function when the operation is
shared or needs its own documentation.
