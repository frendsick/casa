# Copy-Clone relation follows the trait declaration

The compiler does not intrinsically make every Copy type satisfy Clone. The relationship follows ordinary declared supertraits:

```casa
trait Copy { }        # Copy does not imply Clone

trait Copy: Clone { } # Copy implementations must also satisfy Clone
```

Clone remains an ordinary explicitly implemented trait. For the standard second declaration, a validated Copy implementation supplies a missing fieldwise Clone implementation so Copy types satisfy the visible supertrait without another annotation. An explicit Clone implementation takes precedence over that fallback under ADR-0088. This narrow generation does not apply to arbitrary supertraits or a freestanding Copy declaration that omits Clone.

## Consequences

- A type may implement either capability, both capabilities, or only Copy when the active Copy declaration has no Clone supertrait.
- Generic code that needs explicit duplication uses `[T: Clone]`; code requiring implicit or stack duplication uses `[T: Copy]`.
- ADR-0084 chooses `trait Copy: Clone { }` for Casa's standard library while retaining this declaration-driven rule for alternative freestanding environments.
