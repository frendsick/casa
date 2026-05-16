# Casa

See [README.md](./README.md) for basic info, language docs, and examples.

## General principles

- The codebase is the source of truth — verify against current code before asserting.
- Don't web-search Casa specifics; this repo is the only authoritative source.
- When language features or stdlib functions change, update the corresponding
  documentation, examples, and tests.

## Code conventions

- See [FORMAT.md](./docs/FORMAT.md) for formatting rules.
- See [STYLE.md](./docs/STYLE.md) for naming and idiomatic patterns.
- Files should contain the relevant imports to be self-compilable.

### CRITICAL: Reverse polish notation

- The topmost value in the stack is the first argument
- `a b <` equals to `b < a` in traditional languages
- `z y x foo` equals to `foo(x, y, z)` in traditional language
- When troubleshooting, first check if the related function call sites are using the correct argument order

### Import paths

`import` accepts two forms:

- **Path-style** (`import "lib/std.casa"`, `import "/abs/path.casa"`): contains `/` or ends with `.casa`. Resolved relative to the importing file (or used as-is when absolute). No search.
- **Module-style** (`import "std"`): bare name. Resolved against the importing file's directory first, then each `-L`/`--library-path` directory in CLI order. First existing match wins.

## Agent skills

Always load the relevant doc when the matching workflow comes up:

- **Git** — `docs/agents/git.md`
- **Testing** — `docs/agents/testing.md`
- **Review** — `docs/agents/review.md`
- **Examples** — `docs/agents/examples.md`
- **Pitfalls** — `docs/agents/pitfalls.md` (known Casa gotchas with workarounds)
- **Issue tracker** — `docs/agents/issue-tracker.md`
- **Triage labels** — `docs/agents/triage-labels.md`
- **Domain docs** — `docs/agents/domain.md`
