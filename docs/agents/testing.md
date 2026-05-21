# Testing

Rules for running and updating tests.

## During development

Use selective filters to run only relevant tests while iterating:

```
tests/test_compiler.sh array_methods       # just the test you're working on
tests/test_examples.sh game_of_life        # just the example you changed
```

Rebuild the compiler before testing if compiler sources changed:

```
./casac -L lib casa.casa -o casac_new
CASA_COMPILER=./casac_new tests/test_compiler.sh array_methods
```

## Before opening a PR

Run the full suite — no filters:

- **MUST** autoformat every changed `.casa` file with `./casafmt`:

  ```
  ./casafmt < file.casa > tmp && mv tmp file.casa
  ```

- **MUST** run all test scripts:

  ```
  tests/test_compiler.sh
  tests/test_examples.sh
  tests/test_bootstrap.sh
  ```

- All scripts default to `./casac` (or `./casafmt` for formatter tests). Rebuild
  casac before testing if compiler sources changed:

  ```
  ./casac -L lib casa.casa -o casac
  ```

- Override the compiler with `CASA_COMPILER`:

  ```
  CASA_COMPILER=./casac_debug tests/test_compiler.sh
  ```

## Selective test running

All test scripts accept substring filters as arguments. Only tests whose name
contains at least one filter run. No filters = full suite.

```
tests/test_compiler.sh lexer              # only test_lexer + any error fixture matching "lexer"
tests/test_compiler.sh lexer typechecker  # tests matching "lexer" OR "typechecker"
tests/test_examples.sh fibonacci          # only fibonacci example
tests/test_formatter.sh indent            # only golden file tests matching "indent"
```

When `test_formatter.sh` has filters, idempotency and error passthrough tests
are skipped (they only run in the full suite).

`test_bootstrap.sh` has no filters — it always runs both self-compilation and
fixed-point tests.

## Test categories

| Script | What it tests |
|---|---|
| `test_compiler.sh` | Unit tests (`tests/compiler/test_*.casa`) and error fixtures (`tests/compiler/errors/*.casa`) |
| `test_examples.sh` | Example programs (`examples/*.casa`) against expected output |
| `test_formatter.sh` | Golden file formatting, idempotency sweep, error passthrough |
| `test_bootstrap.sh` | Self-compilation (3-stage) and fixed-point verification |

## CI bootstrap compiler

- `casa-release.env` is the single tracked source for the release tag used by CI
  and `install.sh`.
- Consumers must parse and validate `casa-release.env` as data. Do not source it or
  append it directly to `$GITHUB_ENV`.
- PR CI should use a stable released bootstrap compiler by default.
- Temporary or prerelease bootstrap compilers require the `bootstrap-override` PR
  label and must be cleaned up back to a stable release tag.

## When examples change

See [examples.md](./examples.md).
