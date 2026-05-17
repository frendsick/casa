# Testing

Rules for running and updating tests.

## Before running tests or opening a PR

- **MUST** autoformat every changed `.casa` file with `./casafmt`:

  ```
  ./casafmt < file.casa > tmp && mv tmp file.casa
  ```

- **MUST** run both test scripts:

  ```
  tests/test_examples.sh
  tests/test_compiler.sh
  ```

- Both scripts default to the `./casac` binary at the repo root. Rebuild casac before
  testing if compiler sources changed:

  ```
  ./casac -L lib casa.casa -o casac
  ```

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
