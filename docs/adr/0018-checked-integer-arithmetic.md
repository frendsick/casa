# Integer arithmetic is checked consistently

Ordinary integer `+`, `-`, `*`, and negation detect overflow and panic in every build mode. Integer literals outside their contextual or defaulted type are compile errors. Recoverable arithmetic uses `try_add`, `try_sub`, and `try_mul`, returning `Option[T]`; intentional modular arithmetic uses `wrapping_add`, `wrapping_sub`, and `wrapping_mul`. Saturating arithmetic remains deferred until a concrete use requires it.

Division and remainder truncate toward zero, with the remainder carrying the dividend's sign. `/` and `%` panic on division by zero and on signed minimum divided by `-1`; `try_div` and `try_mod` return `Option[T]` for recoverable use. Shift counts use `u64` and must be smaller than the operand width. Left shift discards shifted-out bits, signed right shift preserves the sign, and unsigned right shift inserts zero bits. Recoverable shift operations remain deferred.

Checked operators prevent silent corruption and keep behavior independent of
build mode. Requiring `Option` for ordinary arithmetic would burden composition.
The x86-64 overflow branch uses non-unwinding panic. Proven-safe check elimination
may be added later.

The cost decision has no preset pass/fail threshold. Its
[historical protocol](https://github.com/frendsick/casa/blob/e9a258837d40185376c97e4fd03fccb98d238d82/docs/adr/0018-checked-integer-arithmetic.md#consequences)
and [measured report](https://github.com/frendsick/casa/issues/327#issuecomment-5309863458)
compare checked and legacy executables on self-compilation and in-range
addition/subtraction-heavy and multiplication-heavy loops. The checked
executable must itself use checked arithmetic. Report warmed medians, absolute
and percentage differences, machine, and commands. The measurements inform
whether checked arithmetic remains the default.
