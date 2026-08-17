# Optional Values and Errors

`Option`, `Result`, and `?` represent absent values and recoverable errors.
Import them from `std`:

```casa
import "std"
```

Compile with a library path that contains `std.casa`, such as
`casac -L lib program.casa`. See [Modules](modules.md) for import resolution.

## Option

`Option[T]` represents a value that can be present or absent. It is defined as
`enum Option[T] { None Some(T) }`.

```casa
42 Option::Some = present:Option[i64]
Option::None = absent:Option[i64]
```

Prefer pattern matching when both cases need behavior:

```casa
present match
    Option::Some(value) => value print
    Option::None => "nothing" print
end
```

| Method | Result or action |
|---|---|
| `is_some self:Option[T] -> bool` | Whether a value is present |
| `is_none self:Option[T] -> bool` | Whether no value is present |
| `is_ok self:Option[T] -> bool` | Alias used by `?` |
| `unwrap self:Option[T] -> T` | Present value, or terminate on `None` |
| `unwrap_or self:Option[T] default:T -> T` | Present value or `default` |
| `map self:Option[T] transform:fn[T -> U] -> Option[U]` | Transform a present value |
| `and_then self:Option[T] transform:fn[T -> Option[U]] -> Option[U]` | Chain an optional operation |
| `or_else self:Option[T] fallback:fn[-> Option[T]] -> Option[T]` | Compute a fallback for `None` |
| `filter self:Option[T] predicate:fn[T -> bool] -> Option[T]` | Keep a present value only if it matches |

Callbacks are pushed before the option receiver:

```casa
{ 2 * } 5 Option::Some .map    # Option::Some(10)
```

## Result

`Result[T E]` represents a successful value or an error. It is defined as
`enum Result[T E] { Error(E) Ok(T) }`.

```casa
42 Result::Ok = success:Result[i64 str]
"invalid input" Result::Error = failure:Result[i64 str]
```

Handle both cases with `match`:

```casa
failure match
    Result::Ok(value) => value print
    Result::Error(message) => message print
end
```

| Method | Result or action |
|---|---|
| `is_ok self:Result[T E] -> bool` | Whether the result is successful |
| `is_error self:Result[T E] -> bool` | Whether the result is an error |
| `unwrap self:Result[T E] -> T` | Success value, or terminate on `Error` |
| `unwrap_error self:Result[T E] -> E` | Error value, or terminate on `Ok` |
| `unwrap_or self:Result[T E] default:T -> T` | Success value or `default` |
| `map self:Result[T E] transform:fn[T -> U] -> Result[U E]` | Transform a success value |
| `map_error self:Result[T E] transform:fn[E -> F] -> Result[T F]` | Transform an error value |
| `and_then self:Result[T E] transform:fn[T -> Result[U E]] -> Result[U E]` | Chain a fallible operation |
| `or_else self:Result[T E] recover:fn[E -> Result[T F]] -> Result[T F]` | Recover from an error |

## Propagate with `?`

Inside a function, `?` unwraps `Some` or `Ok`. On `None` or `Error`, it returns
from the function immediately:

```casa
fn half_if_even value:i64 -> Option[i64] {
    if value 2 % 0 == then
        value 2 / Option::Some
    else
        Option::None
    fi
}

fn quarter_if_even value:i64 -> Option[i64] {
    value half_if_even ? 2 / Option::Some
}
```

An `Option[T]` can propagate into another `Option`. A `Result[T E]` can
propagate into another `Result` with the same error type. Use `map_error` first
when the error type must change.

See [`examples/propagate_result.casa`](../examples/propagate_result.casa) for a
runnable file operation that uses `?`.
