# Testing

Rules for running and updating tests.

## During development

Trace the affected code paths and add or update one focused regression test for
changed behavior. Run the matching compiler, example, or formatter filters while
iterating:

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

After the implementation is stable:

- **MUST** autoformat each changed `.casa` file once with `./casafmt`:

  ```
  ./casafmt < file.casa > tmp && mv tmp file.casa
  ```

- **MUST** run focused filters for the changed behavior. Run `test_bootstrap.sh`
  only when the affected path requires bootstrap validation.
- Full local suites are not required before a pull request. **MUST NOT** run them
  only to duplicate pull request CI. CI runs:

  ```
  tests/test_compiler.sh
  tests/test_examples.sh
  tests/test_bootstrap.sh
  tests/test_formatter.sh
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

When `test_formatter.sh` has filters, idempotency and safety tests
are skipped (they only run in the full suite).

A filtered compiler, example, or formatter run that selects no tests prints the
supplied filters in an explicit notice and exits successfully.

`test_bootstrap.sh` has no filters — it always runs both self-compilation and
fixed-point tests.

## Test categories

| Script | What it tests |
|---|---|
| `test_compiler.sh` | Unit tests (`tests/compiler/test_*.casa`) and error fixtures (`tests/compiler/errors/*.casa`) |
| `test_examples.sh` | Example programs (`examples/*.casa`) against expected output |
| `test_formatter.sh` | Golden file formatting, idempotency sweep, safety checks |
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
