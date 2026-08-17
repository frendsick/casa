# Recursive destruction is initially call-stack recursive

Compiler-generated destruction of recursive owned values initially follows the ordinary recursive field structure. Each owner runs its compiler-called cleanup method if present, then destroys fields in reverse declaration order under the existing LIFO rule. The compiler does not initially generate a heap worklist or pointer-reversal traversal solely to guarantee constant call-stack usage.

A sufficiently deep structure may exhaust the process call stack; stack exhaustion terminates without unwinding. Programs that intentionally build exceptional depths may consume or drain the structure iteratively before scope exit. This limitation does not permit double destruction or use after move.

## Considered options

- A general iterative worklist avoids call-stack exhaustion but adds allocation or complex in-place traversal to every arbitrary branching recursive layout.
- Specialized tail-destructor loops cover linear structures but introduce type-shape-specific lowering before measurements establish the useful cases.
- Recursive lowering is the smallest correct initial implementation and matches recursive Clone, comparison, and hashing costs.

## Consequences

- Validation must include long linear and branching destruction tests plus a benchmark reporting the practical depth limit.
- Stack depth and destruction time must be measured without conflating allocation or construction time.
- A later iterative lowering must preserve custom cleanup method and reverse-field order.
- Stack exhaustion is a process termination, not a catchable panic or cleanup path.
