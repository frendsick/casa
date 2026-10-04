# Optional Values and Errors

`Option`, `Result`, and `?` represent absent values and recoverable errors.
Import them from `std`:

```casa
import "std"
```

Compile with a library path that contains `std.casa`, such as
`casac -L lib program.casa`. See [Modules](modules.md) for import resolution.

Reference tables abbreviate library type names and list inputs in consumption
order. Source examples use qualified names. See [reference notation](notation.md)
for signatures, fragments, and commands for running complete examples.

## Option

`Option[T]` represents a value that can be present or absent. It is defined as
`enum Option[T] { None Some(T) }`.

```casa
import "std"

42 std::Option::Some = present:std::Option[i64]
std::Option::None = absent:std::Option[i64]
```

Prefer [pattern matching](control-flow.md#match-a-value) when both cases need behavior:

```casa
import "std"

present match
    std::Option::Some(value) => value print
    std::Option::None => "nothing" print
end
```

Use an optional result for an operation that can fail:

```casa
import "std"

fn divide dividend:i64 divisor:i64 -> std::Option[i64] {
    divisor dividend.try_div
}

0 12 divide match
    std::Option::Some(value) => value print
    std::Option::None => "cannot divide by zero" print
end
```

| Method | Signature | Behavior |
|---|---|---|
| [and_then](#optiontand_then) | `fn and_then [T U] self:Option[T] f:fn[T -> Option[U]] -> Option[U]` | Chain an optional operation |
| [filter](#optiontfilter) | `fn filter [T] self:Option[T] f:fn[$T -> bool] -> Option[T]` | Keep a present value only if it matches |
| [is_none](#optiontis_none) | `fn is_none [T] self:$Option[T] -> bool` | Whether no value is present |
| [is_ok](#optiontis_ok) | `fn is_ok [T] self:$Option[T] -> bool` | Alias used by `?` |
| [is_some](#optiontis_some) | `fn is_some [T] self:$Option[T] -> bool` | Whether a value is present |
| [map](#optiontmap) | `fn map [T U] self:Option[T] f:fn[T -> U] -> Option[U]` | Transform a present value |
| [or_else](#optiontor_else) | `fn or_else [T] self:Option[T] f:fn[-> Option[T]] -> Option[T]` | Compute a fallback for `None` |
| [propagate](#optiontpropagate) | `fn propagate [T U] self:Option[T] -> Option[U]` | Convert `None` for an enclosing `?` return |
| [unwrap](#optiontunwrap) | `fn unwrap [T] self:Option[T] -> T` | Present value, or terminate on `None` |
| [unwrap_or](#optiontunwrap_or) | `fn unwrap_or [T] self:Option[T] default:T -> T` | Present value or `default` |

### Option[T]::and_then

```text
fn and_then [T U] self:Option[T] f:fn[T -> Option[U]] -> Option[U]
```

Chains an optional operation.

### Option[T]::filter

```text
fn filter [T] self:Option[T] f:fn[$T -> bool] -> Option[T]
```

Keeps a present value only if it matches.

### Option[T]::is_none

```text
fn is_none [T] self:$Option[T] -> bool
```

Returns whether no value is present.

### Option[T]::is_ok

```text
fn is_ok [T] self:$Option[T] -> bool
```

Returns whether a value is present. The `?` operator uses this alias of `is_some`.

### Option[T]::is_some

```text
fn is_some [T] self:$Option[T] -> bool
```

Returns whether a value is present.

### Option[T]::map

```text
fn map [T U] self:Option[T] f:fn[T -> U] -> Option[U]
```

Transforms a present value.

### Option[T]::or_else

```text
fn or_else [T] self:Option[T] f:fn[-> Option[T]] -> Option[T]
```

Computes a fallback for `None`.

### Option[T]::propagate

```text
fn propagate [T U] self:Option[T] -> Option[U]
```

Converts `None` for an enclosing `?` return.

### Option[T]::unwrap

```text
fn unwrap [T] self:Option[T] -> T
```

Returns the present value. `None` terminates the program.

### Option[T]::unwrap_or

```text
fn unwrap_or [T] self:Option[T] default:T -> T
```

Returns the present value, or `default` when the option is `None`.

### Option ownership and calls

The observation methods `is_some`, `is_none`, and `is_ok` borrow the option.
They leave its owner available. Every other method in the table consumes the
option. A consuming call [moves](ownership.md#move-a-value) the owner. It returns the payload or a new
option, or destroys a payload that it does not return.

Callbacks are pushed before the option receiver:

```casa
import "std"

{ 2 * } 5 std::Option::Some .map    # std::Option::Some(10)
```

## Result

`Result[T E]` represents a successful value or an error. It is defined as
`enum Result[T E] { Error(E) Ok(T) }`.

```casa
import "std"

42 std::Result::Ok = success:std::Result[i64 str]
"invalid input" std::Result::Error = failure:std::Result[i64 str]
```

Handle both cases with `match`:

```casa
import "std"

failure match
    std::Result::Ok(value) => value print
    std::Result::Error(message) => message print
end
```

| Method | Signature | Behavior |
|---|---|---|
| [and_then](#resultt-eand_then) | `fn and_then [T U E] self:Result[T E] f:fn[T -> Result[U E]] -> Result[U E]` | Chain a fallible operation |
| [is_error](#resultt-eis_error) | `fn is_error [T E] self:$Result[T E] -> bool` | Whether the result is an error |
| [is_ok](#resultt-eis_ok) | `fn is_ok [T E] self:$Result[T E] -> bool` | Whether the result is successful |
| [map](#resultt-emap) | `fn map [T U E] self:Result[T E] f:fn[T -> U] -> Result[U E]` | Transform a success value |
| [map_error](#resultt-emap_error) | `fn map_error [T E F] self:Result[T E] f:fn[E -> F] -> Result[T F]` | Transform an error value |
| [or_else](#resultt-eor_else) | `fn or_else [T E F] self:Result[T E] f:fn[E -> Result[T F]] -> Result[T F]` | Recover from an error |
| [propagate](#resultt-epropagate) | `fn propagate [T U E] self:Result[T E] -> Result[U E]` | Preserve `Error` for an enclosing `?` return |
| [unwrap](#resultt-eunwrap) | `fn unwrap [T E] self:Result[T E] -> T` | Success value, or terminate on `Error` |
| [unwrap_error](#resultt-eunwrap_error) | `fn unwrap_error [T E] self:Result[T E] -> E` | Error value, or terminate on `Ok` |
| [unwrap_or](#resultt-eunwrap_or) | `fn unwrap_or [T E] self:Result[T E] default:T -> T` | Success value or `default` |

### Result[T E]::and_then

```text
fn and_then [T U E] self:Result[T E] f:fn[T -> Result[U E]] -> Result[U E]
```

Chains a fallible operation.

### Result[T E]::is_error

```text
fn is_error [T E] self:$Result[T E] -> bool
```

Returns whether the result is an error.

### Result[T E]::is_ok

```text
fn is_ok [T E] self:$Result[T E] -> bool
```

Returns whether the result is successful.

### Result[T E]::map

```text
fn map [T U E] self:Result[T E] f:fn[T -> U] -> Result[U E]
```

Transforms a success value.

### Result[T E]::map_error

```text
fn map_error [T E F] self:Result[T E] f:fn[E -> F] -> Result[T F]
```

Transforms an error value.

### Result[T E]::or_else

```text
fn or_else [T E F] self:Result[T E] f:fn[E -> Result[T F]] -> Result[T F]
```

Recovers from an error.

### Result[T E]::propagate

```text
fn propagate [T U E] self:Result[T E] -> Result[U E]
```

Preserves `Error` for an enclosing `?` return.

### Result[T E]::unwrap

```text
fn unwrap [T E] self:Result[T E] -> T
```

Returns the success value. An `Error` result terminates the program.

### Result[T E]::unwrap_error

```text
fn unwrap_error [T E] self:Result[T E] -> E
```

Returns the error value. An `Ok` result terminates the program.

### Result[T E]::unwrap_or

```text
fn unwrap_or [T E] self:Result[T E] default:T -> T
```

Returns the success value, or `default` when the result is an error.

### Result ownership

The observation methods `is_ok` and `is_error` borrow the result. They leave
its owner available. Every other method in the table consumes the result. A
consuming call [moves](ownership.md#move-a-value) the owner and transfers or destroys each owned payload
exactly once.

## Propagate with `?`

Inside a function, `?` unwraps `Some` or `Ok`. On `None` or `Error`, it returns
from the function immediately:

```casa
import "std"

fn half_if_even value:i64 -> std::Option[i64] {
    if value 2 % 0 == then
        value 2 / std::Option::Some
    else
        std::Option::None
    fi
}

fn quarter_if_even value:i64 -> std::Option[i64] {
    value half_if_even ? 2 / std::Option::Some
}
```

An `Option[T]` can propagate into another `Option`. A `Result[T E]` can
propagate into another `Result` with the same error type. Use `map_error` first
when the error type must change.

`?` uses three ordinary methods and does not recognize enum names. `is_ok`
borrows the source and returns `bool`. On success, `unwrap` produces the value.
On failure, `propagate` produces the enclosing function's one declared return
value. The source is consumed on either path. Any enum with compatible methods
uses the same behavior.

See [`examples/propagate_result.casa`](../examples/propagate_result.casa) for a
runnable file operation and a custom enum that use `?`.
