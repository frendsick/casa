# Integer arithmetic is checked consistently

Ordinary integer `+`, `-`, `*`, and negation detect overflow and panic in every build mode. Integer literals outside their contextual or defaulted type are compile errors. Recoverable arithmetic uses `try_add`, `try_sub`, and `try_mul`, returning `Option[T]`; intentional modular arithmetic uses `wrapping_add`, `wrapping_sub`, and `wrapping_mul`. Saturating arithmetic remains deferred until a concrete use requires it.

Division and remainder truncate toward zero, with the remainder carrying the dividend's sign. `/` and `%` panic on division by zero and on signed minimum divided by `-1`; `try_div` and `try_mod` return `Option[T]` for recoverable use. Shift counts use `u64` and must be smaller than the operand width. Left shift discards shifted-out bits, signed right shift preserves the sign, and unsigned right shift inserts zero bits. Recoverable shift operations remain deferred.

## Considered options

- Silent wrapping is fastest, but turns arithmetic mistakes into later data corruption.
- Checking only in development builds makes program behavior depend on optimization mode.
- Requiring recoverable arithmetic everywhere is explicit but makes ordinary arithmetic unnecessarily verbose.
- Checked operators with recoverable and wrapping escape hatches keep the common case safe while preserving deliberate control.

## Consequences

- The x86-64 backend adds an overflow branch after ordinary integer arithmetic. Proven-safe check elimination may be added later, but is not required for the first implementation.
- The overflow path uses Casa's non-unwinding panic behavior.
- The implementation is not complete until its real-program cost is measured through self-compilation:
  1. Preserve a compiler whose own arithmetic uses the legacy unchecked instructions.
  2. Bootstrap the checked compiler far enough that the compiler executable itself uses checked arithmetic; a first-stage compiler built by the legacy compiler is not sufficient.
  3. Warm up both executables, then repeatedly compile the same `casa.casa` source with the same library path, output location class, and command-line options.
  4. Report the median wall-clock time for each compiler, the absolute difference, and the percentage difference. Record the machine and commands so the comparison can be repeated.
- The same checked and legacy code generators must also compile long-running arithmetic loops. Include at least one addition/subtraction-heavy loop and one multiplication-heavy loop, keep every operation in range, consume the final result so later optimization cannot delete the work, and make arithmetic dominate loop setup and output. Report medians and percentage differences separately from self-compilation.
- The benchmark informs whether checked arithmetic remains the default; there is no preset pass/fail threshold before measurements exist.
