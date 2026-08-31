# Casa Style Guide

Naming conventions and idiomatic patterns for Casa source code.
Rules are tagged **MUST** or **SHOULD**.

For mechanical formatting rules (indentation, line length, whitespace, etc.)
see [FORMAT.md](./FORMAT.md).

---

## Naming

### Functions

- **MUST** use `snake_case`.
- **MUST** be verb-first and descriptive: `parse_token`, `resolve_variable`, `count_neighbors`.
- **MUST NOT** use function-name abbreviation prefixes. No `tc_pop`, `lexer_skip`. Move
  helpers into the type's `impl` block instead (see [impl blocks](#impl-blocks-over-prefixed-helpers)).
- **MUST NOT** use abbreviated names. `token` not `t`, `function` not `fn`.

### Constructors

- **MUST** use `Type::new` as the canonical constructor for types with an `impl` block:
- **MUST** write qualified calls without whitespace around `::`, including generic
  receivers such as `List[T]::new`.

  ```casa
  impl Timer {
      fn new -> Timer {
          ...
      }
  }
  Timer::new = timer
  ```

- Use `make_type` (free function, `make_` prefix) only for constructors that:
  - compose multiple types, or
  - live outside any impl block because they're too complex to be methods

  ```casa
  fn make_location file:str offset:u64 length:u64 -> Location {
      length offset file Location
  }
  ```

- **MUST NOT** use both `Type::new` and `make_type` for the same type.

### Variables

- **MUST** use `snake_case`.
- **MUST** use descriptive names. `token` not `t`, `index` not `i` (except see loop indices below).
- **MUST NOT** use function-name abbreviation prefixes as variable prefixes.
  No `fnd_pos`, `sw_matched`, `spl_result`. Rename descriptively instead.
- **MUST NOT** use underscore prefix.

### Loop indices

- `i` is acceptable for a simple integer counter in a non-nested single loop where the
  counter has no semantic meaning beyond position:

  ```casa
  0 = i
  while i length > do
      i items.get process
      1 += i
  done
  ```

- **MUST** use a descriptive name for nested loop counters, or when the counter
  has semantic meaning:

  ```casa
  0 = row
  while row height > do
      0 = col
      while col width > do
          ...
          1 += col
      done
      1 += row
  done
  ```

### Structs, enums, and enum variants

- **MUST** use `PascalCase` for struct names, enum names, and enum variant names.
- **MUST** use `SCREAMING_SNAKE_CASE` for constants:

  ```casa
  const ALIVE_THRESHOLD 64
  const FRAME_DELAY_MS 200
  ```

- **MUST** name the `self` parameter `self` in all impl methods.

### Type parameters

- **MUST** use short uppercase names only.
- Canonical set: `T` (single unconstrained), `T1`/`T2` (multiple unconstrained),
  `K` (map key), `V` (map value), `E` (error in Result).

  ```casa
  fn id[T] T -> T { }
  fn swap_t[T1 T2] T1 T2 -> T1 T2 { swap }
  fn get[K: Hashable, V] self:Map[K V] key:K -> Option[V] { ... }
  ```

---

## impl blocks over prefixed helpers

When a function's primary purpose is to operate on a specific struct type,
it **MUST** live in that type's `impl` block rather than as a free function
with a type-name prefix:

```casa
# MUST NOT — type-name prefix on a free function
fn lexer_skip_whitespace lexer:Lexer {
    ...
}

# MUST — impl method
impl Lexer {
    fn skip_whitespace self:Lexer {
        ...
    }
}
```

When a function operates on two or more struct types equally, a free function
without a type-name prefix is acceptable.

---

## Accessor shorthand

- **MUST** use `.field` for getter shorthand and field assignment targets when
  available.
- **MUST NOT** put a space between the receiver and the accessor: `person.age` not
  `person .age`.
- Keep one or two accessor calls on one line. Use continuation lines for three
  or more calls:

  ```casa
  person.age print                  # MUST: shorthand getter, no space
  analysis.result.document print
  analysis
      .result
      .document
      .location print
  42 = person.age                   # MUST: field assignment, no space
  ```

- Use the explicit form (`person Person::age`) only when passing an accessor as a
  function reference (`&Person::age`) or when the shorthand creates an ambiguous
  RPN expression.
- For method pipeline formatting see [FORMAT.md — Getter chaining and method pipelines](./FORMAT.md#getter-chaining-and-method-pipelines).

---

## Type annotations

- **SHOULD** annotate parameter types on public-facing functions, even when
  the type checker can infer them. This documents intent for readers:

  ```casa
  fn greet name:$str -> String {
      f"Hello, {name}!"
  }
  ```

- In function bodies, annotate a variable **only** when inference would fail:

  ```casa
  # Required: bare Option needs narrowing
  Option::None = empty:Option[i64]

  # MUST NOT: inference works fine, annotation is noise
  42 = x:i64
  ```

---

## Function parameters

### Order: primary data first, config flags last

- **MUST** put primary data parameters first and config/flag/mode parameters last.
- Casa's RPN means param 1 is the topmost stack slot — the first argument pushed last.
  That position belongs to the primary data being operated on. A config flag in param 1
  forces every call site to push the flag immediately before the function name, burying
  the important argument under a trailing literal.

  ```casa
  # MUST — primary data first, flag last
  fn find_matching_label
      ops:List[Op]
      op_index:u64
      boundary:i64
      target:OpKind
      backward:bool
  -> i64 { ... }

  # MUST NOT — flag in first position
  fn find_matching_label
      backward:bool
      ops:List[Op]
      ...
  -> i64 { ... }
  ```

### Flag types match the value range

- **MUST** pick the parameter type matching the actual value set. Two states = `bool`.
  Arbitrary integer = `i64`. Don't use `i64` as a stand-in for "one of two values".
- `i64` tells the reader "any integer" and invites misuse. If only `1` and `-1` are
  ever valid, the type lies about the domain. Use `bool` and derive the integer
  internally (`if backward then -1 else 1 fi = step`).

---

## Option and Result

- **MUST** use `Option[T]` for values that may be absent. Never return a sentinel
  value (e.g. `-1` for "not found") when `Option` is available.
- **MUST** use `Result[T E]` for operations that may fail with a meaningful error.

---

## `is` destructuring

- **MUST** use `if … is` for destructuring and variant checking instead of
  `.is_some` / `.is_ok` followed by `.unwrap`:

  ```casa
  # MUST
  if value Option::Some(inner) is then
      inner process
  fi

  # MUST NOT
  if value .is_some then
      value .unwrap process
  fi
  ```

---

## `match` for enum dispatch

- **MUST** use `if … is` when control flow checks one enum variant, with an
  optional `else` for the remaining values.
- **MUST** use `match` when control flow checks multiple variants. Prefer it
  for exhaustive or near-exhaustive enum dispatch. Avoid `if/elif` chains that
  check the same enum value repeatedly:

  ```casa
  # MUST
  direction match
      Direction::North => go_north
      Direction::South => go_south
      Direction::East => go_east
      Direction::West => go_west
  end

  # MUST NOT
  if direction Direction::North == then go_north
  elif direction Direction::South == then go_south
  ...
  fi
  ```

---

## No magic values

- **MUST NOT** hard-code numeric or string literals whose meaning is not
  immediately obvious. Use a named constant instead:

  ```casa
  # MUST
  const ALIVE_THRESHOLD 64
  if unsafe { ALIVE_THRESHOLD cell load8 < } then ...

  # MUST NOT
  if unsafe { 64 cell load8 < } then ...
  ```

---

## Loops

- **SHOULD** prefer `for x in <iter> do … done` over the equivalent `while`
  loop whenever the loop walks every element of a collection or iterator. The
  `for` form is shorter, makes the intent obvious, and removes the off-by-one
  trap of manual index bookkeeping.

  ```casa
  # SHOULD
  for token in tokens.iter do
      token process
  done

  # SHOULD NOT — manual indexing where `for` works
  0 = index
  while index tokens.length > do
      index tokens.get process
      1 += index
  done
  ```

- Reach for `while` only when `for` cannot express the loop: condition-driven
  iteration, parallel iteration over multiple sources, mid-loop mutation of the
  collection being iterated, or early termination that depends on state outside
  the iterator.

---

## Maximum nesting depth

- **MUST NOT** nest `if`, `while`, `match`, or other block constructs more than
  **3 indentation levels** deep. Extract inner logic into a helper function.

---

## Explicit conversions

- **MUST** use a typed binding for literal context and a named operation for a
  numeric conversion:

  ```casa
  42 = x:i64
  x u8::try_from = maybe_byte

  # Raw representation boundary
  unsafe { x u64::wrapping_from buf store64 }
  ```

---

## Mutation

- **MUST** use `+=` or `-=` when adding or subtracting a literal value:

  ```casa
  1 += index       # MUST
  index 1 + = index  # MUST NOT for literal increment
  ```

- Use field assignment for computed updates that are not `+=` or `-=`:

  ```casa
  self.capacity 2 * = self.capacity
  ```

---

## Error handling

- **MUST** return `Result` for meaningful failures and `Option` for absence.
- Compiler phases **MUST** record recoverable diagnostics in their phase-owned
  `Diagnostics`; callers decide whether to continue, report, or exit.
- Application adapters may print an unrecoverable internal error and exit when their
  public interface cannot represent failure.
- **MUST** write fixed error messages as plain string literals, not constructed
  `String` values:

  ```casa
  # MUST
  location "Expected type name" ErrorKind::Syntax diagnostics.record_error

  # MUST NOT
  String::new = msg
  "Expected " msg.append ...
  location msg ErrorKind::Syntax diagnostics.record_error
  ```
