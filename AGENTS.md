# Casa

See [README.md](./README.md) for basic info, language docs, and examples.

## General principles

- Never commit benchmarks, including workload sources, measurement tools, and
  generated results. Keep benchmark files outside the repository.
- Don't web-search Casa specifics; this repo is the only authoritative source.
- When language features or stdlib functions change, update the corresponding
  documentation, examples, and tests.

## Code conventions

- See [FORMAT.md](./docs/FORMAT.md) for formatting rules.
- See [STYLE.md](./docs/STYLE.md) for naming and idiomatic patterns.
- Files should contain the relevant imports to be self-compilable.

### CRITICAL: Reverse polish notation

- Named functions and receiver methods consume their first argument from the
  top of the stack. `z y x foo` corresponds to `foo(x, y, z)`.
- Binary symbolic operators use source operand order. `a b <` means `a < b`,
  and `a b -` means `a - b`.
- Comparison operators use the left operand as the receiver. `a b <`
  corresponds to `b a.lt`.
- When troubleshooting, check symbolic operand order and named-call argument
  order first.

See [ADR-0177](./docs/adr/0177-binary-symbolic-operators-use-source-operand-order.md)
for the operator rules.

### Import paths

`import` accepts two forms:

- **Path-style** (`import "lib/std.casa"`, `import "/abs/path.casa"`): contains `/` or ends with `.casa`. Resolved relative to the importing file (or used as-is when absolute). No search.
- **Module-style** (`import "std"`): bare name. Resolved against the importing file's directory first, then each `-L`/`--library-path` directory in CLI order. First existing match wins.

## Worktree setup

In each new worktree, run `./install.sh` to download `casac` and `casafmt`
from the release in `casa-release.env`.

## Bootstrap releases

- When the newest stable compiler cannot compile valid Casa syntax used by
  repository sources, create the next stable release and update
  `casa-release.env` to use it. This applies even when the stable compiler can
  still bootstrap `casa.casa`.
- Intentional invalid-input fixtures and non-executable documentation snippets
  do not require a release.
- Create a prerelease compiler only when the user explicitly requests one.

## Agent documentation

Always load the relevant doc when the matching workflow comes up:

- **Domain**: use `CONTEXT.md` for the glossary and `docs/adr/` for decisions.
- **Examples**: `docs/agents/testing.md#when-examples-change`
- **Functions**: load `function-design` once per task before adding, changing, or
  reviewing non-trivial functions. Reuse that analysis during review.
- **Memory efficiency**: `docs/agents/memory.md`
- **Releases**: `docs/agents/testing.md#ci-bootstrap-compiler`
- **Review**: `docs/agents/review.md`
- **Testing**: `docs/agents/testing.md`
