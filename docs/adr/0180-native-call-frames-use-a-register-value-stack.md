# Native call frames use a register value stack

status: amended by [ADR-0181](0181-register-storage-is-planned-within-one-function.md)

Related issues: [native call frames](https://github.com/frendsick/casa/issues/879)
and [value transfers across calls](https://github.com/frendsick/casa/issues/901).

Keep return addresses and function locals on `%rsp`. Use ordinary `call` and
`ret`, with DWARF call frame information for generated functions and called
runtime helpers. Standard unwinders can then follow Casa calls through runtime
helpers and extern C functions. Keep runtime failure calls inside their caller's
unwind range, and mark the root return address undefined so unwinding stops at
`_start`.

Keep the value stack behind the C callee-saved register `%rbx`. Track its
displacement during emission and synchronize the pointer at calls, returns,
extern calls, and control-flow boundaries. This separates values from native
return addresses. Explicit slot loads and stores have larger encodings than
the hardware stack's `pushq` and `popq`, so reducing instructions alone does not
establish a runtime benefit.

For eligible Casa functions, transfer up to four arguments and four results in
`%r8` through `%r11`, from the bottom to the top of the transferred stack suffix.
Treat all four registers as caller-clobbered. Normalize the suffix before the
call. [ADR-0181](0181-register-storage-is-planned-within-one-function.md) defines
preservation of live values below the arguments and register storage for locals.
Seed the callee's incoming cache from the same registers and normalize results at
every return. Parameter locals follow the same allocation rules as other locals.
Unsupported operations materialize cached values before their existing lowering.

Plan eligibility from the concrete checked stack effect. Named calls and typed
function-value calls use the same rule, including address-taken functions and
capturing closures. Every current Casa stack value has one word carrier,
including indirect aggregate addresses. A Copy aggregate result needs hidden
destination storage, so those signatures and signatures with more than four
arguments or results retain the value-stack memory convention. Generated
destruction entry points also retain that convention. Their calls to checked
custom destruction hooks use the checked function's plan. Extern calls retain
the platform C ABI and its native-call adapter.

Keep the original capture record and its mutation state. Environment setup and
restoration preserve argument and result registers. Cleanup after an owned
closure call must preserve its results, materializing them when destruction can
clobber their registers. The physical convention does not change source argument
order, borrow origins, ownership, or destruction obligations.

The native stack uses the existing fixed return-stack allocation and guard
region. Large frame reservations must not skip the guard undetected. Guard
handling uses an alternate signal stack. The guard fault and return-stack
overflow handlers have no CFI and are outside the unwind contract. Extern C
functions and allocator calls now consume this native stack too, rather than
using the initial process stack.
Extern-call alignment and unwind rules use `%r13` while `%rsp` is realigned.

This remains private target planning under
[ADR-0169](0169-backend-plans-and-renders-one-function-at-a-time.md). Keeping a
register suffix across eligible calls reduces transfers through memory without
changing aggregate placement. ADR-0181 extends it with bounded function-wide
register allocation.
