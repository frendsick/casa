# Binary symbolic operators use source operand order

Binary symbolic operators interpret `left right operator` as
`left operator right`. This includes arithmetic, shifts, bitwise operations,
comparisons, and boolean operations. Operand expressions evaluate from left to
right, and boolean operators remain eager. Constant evaluation uses the same
rule as runtime evaluation.

Named functions and receiver methods retain topmost-first parameter consumption.
Comparison operators adapt their two evaluated operands to the corresponding
trait method. The left operand becomes `self`, and the right becomes `other`.
For already evaluated values, `left right <` corresponds to `right left.lt`.
The same rule applies to `eq`, `ne`, `le`, `gt`, and `ge`. The method chosen by
each operator remains as specified in ADR-0082 and ADR-0083.

Stack effects continue to list inputs in consumption order. Binary symbolic
operators consume the right operand first. Thus `<<: u64 T -> T` describes
`value count <<`, and comparisons have effect `T T -> bool`. Source evaluation,
operand roles, and stack-effect notation are separate rules.

The former split between arithmetic and comparisons made mixed expressions
easy to misread. Using one operand rule for binary symbolic operators removes
that split while retaining the existing call convention.
