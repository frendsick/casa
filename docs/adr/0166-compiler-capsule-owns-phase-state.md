# Compiler Capsule owns phase state
related issue: [Confirm Compiler Capsule constraints and tradeoffs](https://github.com/frendsick/casa/issues/645)

Compiler Capsule hides mutable phase coordination behind compiler products.
[ADR-0167](0167-compiler-products-own-independent-snapshots.md) defines their
representations and ownership. [ADR-0172](0172-editor-products-retain-verified-source-facts.md)
defines partial tooling results, workspace scope, and freshness.

The [blueprint](../benchmarks/compiler-simplification-blueprint.md) is complete.
Production consumer cutover remains open in
[#715](https://github.com/frendsick/casa/issues/715). Selecting the architecture
alone establishes no performance gain or completed production migration.

## Accepted constraints

- Keep Casa self-hosted and dependency-light, with direct parsing and x86-64
  emission. Do not add LLVM, a parser generator, or a backend registry.
- Complete source-level checking, ownership validation, concrete semantic
  specialization, generic cycle checks, and trait dispatch before constructing
  a target-neutral `CheckedProgram`. Its consumers must not repeat those
  semantic decisions or mutate the product into an invalid state.
- Use one shared x86-64 backend with a closed platform policy. The policy owns
  physical layout, `size_of` values, ABI, runtime, and assembly spelling.
  Target-specific rejection remains possible after semantic checking.
- Linux is the current target and the immediate blueprint acceptance target.
  Windows is the only planned additional target and remains later work under
  [ADR-0169](0169-backend-plans-and-renders-one-function-at-a-time.md). Expose its
  production target variant only when its policy works.
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

The [pinned comparison](https://github.com/frendsick/casa/blob/bd3516658e11b4a1544562a552ae47e37329c073/compiler/compiler_architecture_prototype.html)
presents three designs. Capsule was selected as the simplest and most natural
model. The following tradeoffs explain that choice without treating the
prototype's estimates as measurements.

| Design | Useful property | Cost compared with the selected direction |
| --- | --- | --- |
| Compiler Capsule | Callers obtain compiler products without managing phase transitions | A large private module can still hide shared-state coupling. Internal ownership and validation need concrete evidence. |
| Typed Product Ladder | Distinct phase products make transitions inspectable | More products, conversions, and lifetime rules can preserve the coordination burden inside the compiler. |
| Function-at-a-Time Direct Compiler | Short-lived body state can reduce retained work | Streaming, generic recipes, and separate editor and assembly products add coordination before a complete program is known to be valid. |

These are design risks, not measured performance conclusions. Capsule does not
commit to every private type or removal proposed by the prototype. In
particular, removing inspectable machine state must still support backend
validation and diagnostics.

## Interface comparison

ADR-0167 selects typed operations over the prototype's `run -> CompilerProduct`
and `query -> ToolAnswer` protocols. Compare caller knowledge: variants, valid
pairings, ordering, ownership transfer, retained borrows, reclamation, and failure
states. Typed operations remove tag matching without removing lifetime rules.
Call counts and source estimates are not fixed quotas.

## Open work and reconsideration

The accepted front-end, semantic, backend, constant, generic, and tooling
contracts are [ADR-0168](0168-imports-expose-qualified-names-only.md) through
[ADR-0174](0174-front-end-parses-source-before-module-resolution.md).
Derivation, trait defaults, root-owned runtime state, and ownership follow the
accepted decisions linked from the [map](https://github.com/frendsick/casa/issues/638).
Prototype examples do not override them. Older identifiers have destinations in
[the ADR index](README.md#retired-records).

Do not infer syntax removal, diagnostic changes, performance ceilings, or new
interface commitments from architecture selection. Proposals must identify any
open-contract assumption and which result depends on it.

Revisit the direction if the executable slice shows that retained behavior
requires consumers to coordinate mutable phase state, that checked products
cannot prevent invalid backend entry, or that editor and assembly work require
parallel semantic implementations. Also revisit it if measured time or memory
costs miss the subsequently agreed acceptance gates. A more inspectable private
product or a typed operation can be adopted without reopening the whole choice.

The blueprint requires an integrated executable slice, resolved behavior,
agreed measurements, an implementation breakdown, a stable bootstrap route, and
final fixed-point validation. A non-incremental cutover remains permitted without
changing the repository's stable-release and CI rules. Measured acceptance gates
are recorded in [the measurement contract](../benchmarks/compiler-simplification-measurements.md).
