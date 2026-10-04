# Compiler Capsule owns phase state
related issue: [Confirm Compiler Capsule constraints and tradeoffs](https://github.com/frendsick/casa/issues/645)

Compiler Capsule hides mutable phase coordination behind compiler products.
[ADR-0167](0167-compiler-products-own-independent-snapshots.md) defines their
representations and ownership. [ADR-0172](0172-editor-products-retain-verified-source-facts.md)
defines partial tooling results, workspace scope, and freshness.

## Accepted constraints

- Keep Casa self-hosted and dependency-light, with direct parsing and x86-64
  emission. Do not add LLVM, a parser generator, or a backend registry.
- Complete source-level checking, ownership validation, concrete semantic
  specialization, generic cycle checks, and trait dispatch before constructing
  a `CheckedProgram`. Its consumers must not repeat those
  semantic decisions or mutate the product into an invalid state.
- Use one shared x86-64 backend with a closed platform policy. The policy owns
  physical layout, `size_of` values, ABI, runtime, and assembly spelling.
  Target-specific rejection remains possible after semantic checking.
- Linux x86-64 is the implemented target under
  [ADR-0169](0169-backend-plans-and-renders-one-function-at-a-time.md).
- Carry the target with `AssemblySource` so the build layer can select the
  assembler, object format, and linker. Native process execution stays outside
  the compiler module.
- The shared analysis path produces source-oriented editor facts. Only analysis
  retains an `EditorIndex`. Assembly discards it. Editor queries must not expose
  compiler operations or reconstruct semantic decisions from them.
- Preserve accumulated diagnostics and exact source context on rejection and
  internal failure. Partial editor facts must not grant access to codegen
  input. Internal failure and native build failure remain distinct from invalid
  source.

Judge the design by locality, explicit state ownership, caller knowledge, and
fewer representable invalid states. A capsule around the existing mutable
store alone does not meet this decision. Private passes and internal seams are
allowed when they consume established facts and hide their protocols.

## Alternatives and costs

| Design | Useful property | Cost compared with the selected direction |
| --- | --- | --- |
| Compiler Capsule | Callers obtain compiler products without managing phase transitions | A large private module can still hide shared-state coupling. Internal ownership and validation need concrete evidence. |
| Typed Product Ladder | Distinct phase products make transitions inspectable | More products, conversions, and lifetime rules can preserve the coordination burden inside the compiler. |
| Function-at-a-Time Direct Compiler | Short-lived body state can reduce retained work | Streaming, generic recipes, and separate editor and assembly products add coordination before a complete program is known to be valid. |
