# Casa

See [README.md](./README.md) for basic information of the language and the links to documentation and examples.

## Planning

Work back and forth with me, starting with your open questions and outline before writing the plan

## Documentation

When any language feature or standard library function is changed or added, the corresponding documentation must be updated to reflect the change.

## Code conventions

- See [FORMAT.md](./docs/FORMAT.md) for formatting rules
- See [STYLE.md](./docs/STYLE.md) for naming conventions and idiomatic patterns
- Files should contain the relevant imports to be self-compilable

### CRITICAL: Reverse polish notation

- The topmost value in the stack is the first argument
- `a b <` equals to `b < a` in traditional languages
- `z y x foo` equals to `foo(x, y, z)` in traditional language
- When troubleshooting, first check if the related function call sites are using the correct argument order

### Import paths

`import` accepts two forms:

- **Path-style** (`import "lib/std.casa"`, `import "/abs/path.casa"`): contains `/` or ends with `.casa`. Resolved relative to the importing file (or used as-is when absolute). No search.
- **Module-style** (`import "std"`): bare name. Resolved against the importing file's directory first, then each `-L`/`--library-path` directory in CLI order. First existing match wins.

## Git conventions

- Never append any Co-Authored-By messages to commit messages
- Never mention Claude, AI usage, or other tool usage in commit messages, pull request messages or anywhere else
- Never commit Claude-related files
- One commit should only contain changes for one functionality
- Always sign commits. If signing does not work, ask the user to ssh-add the SSH key.
- Never make changes to `main` branch
- Always make commits when a planned changes are finished
- Always make pull requests when the reviewer agent does not find any more things to fix
- Keep commit messages brief
- Never commit binary files unless explicitly asked to do so

## Examples

- Always update examples for new and updated functionality
- When examples are changed, regenerate the expected output for `tests/test_examples.sh`

## Testing

- Always autoformat changed `.casa` files with `./casafmt` before running tests or creating a pull request (e.g. `./casafmt < file.casa > tmp && mv tmp file.casa`)
- Always run `tests/test_examples.sh` and `tests/test_compiler.sh` before creating a pull request
- Both scripts default to the `./casac` binary at the repo root

## Review

- Always use `simplify` and review changes with `feature-dev:code-reviewer` before making a pull request

## Agent skills

### Issue tracker

GitHub Issues at `frendsick/casa`, accessed via `gh`. See `docs/agents/issue-tracker.md`.

### Triage labels

Canonical defaults (`needs-triage`, `needs-info`, `ready-for-agent`, `ready-for-human`, `wontfix`). See `docs/agents/triage-labels.md`.

### Domain docs

Single-context layout (`CONTEXT.md` + `docs/adr/` at repo root). See `docs/agents/domain.md`.
