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
  tests/test_compiler.sh < /dev/null
  ```

  The stdin redirect on `test_compiler.sh` is required to avoid hanging in the
  `test_lsp` unit when run non-interactively. Tracked in
  [issue #170](https://github.com/frendsick/casa/issues/170).

- Both scripts default to the `./casac` binary at the repo root. Rebuild casac before
  testing if compiler sources changed:

  ```
  ./casac -L lib casa.casa -o casac
  ```

## When examples change

See [examples.md](./examples.md).
