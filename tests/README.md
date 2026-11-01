# Compiler tests

Run `tests/test_compiler.sh` from any directory. Optional arguments select test
names by substring. `CASA_TEST_CATEGORY` restricts selection to one CI category.
Every fixture must have a category in the runner, including unselected fixtures.

Compiler-heavy unit tests expose a zero-argument `pub fn run_tests { ... }` and
call it from their root body. When more than one selected file has this entry
function, the runner imports them into one temporary executable. Their compiler
dependencies are compiled once. Each selected test runs in a separate process
with standard input connected to `/dev/null`. Direct execution of each source
file remains supported.

Keep tests that depend on root-body behavior without this entry function. The
runner builds them separately. Error fixtures, native fixtures, and runtime-error
fixtures also retain their individual builds. If the shared build fails, the
runner retries individual builds to report each compilation failure and run the
remaining tests.

The runner sets `TMPDIR` to a private directory for each run. A test that creates
a real file must get its path from `temp_paths::path` so concurrent runs do not
collide. Paths that exist only in `source_overrides` or `SourceStore` never reach
the file system and can remain fixed.

Run `tests/test_compiler_runner.sh` to check selection, shared builds, process
isolation, and failure reporting. These checks also run in the formatter shard.
