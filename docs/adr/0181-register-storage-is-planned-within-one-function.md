# Register storage is planned within one function

Related issue: [function register allocation](https://github.com/frendsick/casa/issues/838).

Keep a bounded register allocation plan beside each function's machine buffer.
This extends [ADR-0169](0169-backend-plans-and-renders-one-function-at-a-time.md)
and [ADR-0180](0180-native-call-frames-use-a-register-value-stack.md) without
adding a general control-flow graph or a separate optimization framework.

Assign frequently accessed scalar locals to `%r12` through `%r15` for the
whole function. A local whose address is taken stays in its frame slot so
borrows, captures, and indirect writes observe the same storage. Scalar slots
and inline aggregate regions remain disjoint. Rank candidates by access count,
weighting uses inside backward-jump ranges more heavily, with slot order breaking
ties. Keep the original frame slots so physical addresses and fallback storage
remain simple. Revisit slot reuse or liveness
intervals only when measurements justify their additional analysis.

Save each allocated register in the native frame and restore it at every return.
Describe those saves in DWARF CFI. Wrap runtime helpers and native adapters with
saves for the callee-saved registers they use as scratch, including registers not
allocated by their immediate caller. This makes preservation independent of each callee's
allocation choices.

Retain the `%r8` through `%r11` value-stack suffix at branches. Each label has
one canonical register order and depth, selected by its first incoming edge.
Later edges spill or refill to that contract. Back edges use the established
loop-entry contract. Unreachable fallthrough does not contribute a register
state. Jump-table targets and internal array-initialization loops use a
memory-stack contract. The latter consume one element per iteration, so their
stack height changes at the back edge. Semantic checking still owns stack
compatibility at source joins.

Reserve `%rbp` when a straight-line cache estimate identifies a live value below
the arguments of an eligible direct or typed indirect call. Functions without
local frames retain the memory fallback. Preserve `%rbp` in functions that use it.
Spill deeper live values in order. This bounded choice leaves four callee-saved registers available
for locals. Calls with larger signatures and unsupported operations retain their
memory lowering. The argument and result convention from ADR-0180 is unchanged.

When a fifth cached scalar is needed, spill only the oldest cached value.
Normalize narrow integers in registers. Keep floating-point arithmetic operands
and results in registers without reassociation, contraction, or changes to IEEE
behavior. These choices reduce memory traffic within the same allocation plan.
