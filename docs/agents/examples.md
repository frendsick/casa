# Examples

Rules for updating examples when language features or standard library functions change.

- **MUST** update the relevant examples for any new or changed functionality.
- **MUST** regenerate the expected-output files used by `tests/test_examples.sh` after
  the example output changes. The test script will tell you which file to refresh.

Then run the tests as described in [testing.md](./testing.md).
