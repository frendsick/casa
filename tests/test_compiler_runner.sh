#!/usr/bin/env sh
set -eu

ROOT_DIR=$(cd "$(dirname "$0")/.." && pwd)
RUN_DIR=$(mktemp -d "${TMPDIR:-/tmp}/casa_compiler_runner.XXXXXX")
trap 'status=$?
    if [ "$status" -ne 0 ] && [ -f "$RUN_DIR/output" ]; then
        cat "$RUN_DIR/output"
    fi
    rm -rf "$RUN_DIR"' EXIT
mkdir -p "$RUN_DIR/fixtures"
CASA_RUNNER_COMPILER=${CASA_COMPILER:-"$ROOT_DIR/casac"}
CASA_RUNNER_LOG="$RUN_DIR/builds"
export CASA_RUNNER_COMPILER CASA_RUNNER_LOG
cat > "$RUN_DIR/compiler" <<'COMPILER'
#!/usr/bin/env sh
printf 'build\n' >> "$CASA_RUNNER_LOG"
exec "$CASA_RUNNER_COMPILER" "$@"
COMPILER
chmod +x "$RUN_DIR/compiler"

cat > "$RUN_DIR/fixtures/test_lexer.casa" <<'CASA'
pub fn run_tests { "lexer passed\n" print }
run_tests
CASA
cat > "$RUN_DIR/fixtures/test_parser.casa" <<'CASA'
pub fn run_tests { "parser passed\n" print }
run_tests
CASA
cat > "$RUN_DIR/fixtures/test_type_ast.casa" <<'CASA'
"standalone passed\n" print
CASA
# An unselected invalid test must never enter the shared compilation.
printf 'not_a_function\n' > "$RUN_DIR/fixtures/test_bytecode.casa"

run_tests() {
    : > "$CASA_RUNNER_LOG"
    status=0
    CASA_COMPILER="$RUN_DIR/compiler" CASA_COMPILER_TESTS_DIR="$RUN_DIR/fixtures" \
        CASA_TEST_CATEGORY=compiler_parsing "$ROOT_DIR/tests/test_compiler.sh" "$@" \
        > "$RUN_DIR/output" 2>&1 || status=$?
}

expect() {
    if ! grep -Fq "$1" "$RUN_DIR/output"; then
        echo "Missing runner output: $1" >&2
        exit 1
    fi
}

expect_builds() {
    actual_builds=$(wc -l < "$CASA_RUNNER_LOG")
    if [ "$actual_builds" -ne "$1" ]; then
        echo "Expected $1 builds, got $actual_builds" >&2
        exit 1
    fi
}

# Category selection shares one build and retains standalone execution.
run_tests
[ "$status" -eq 0 ]
expect 'Summary: 3 passed, 0 failed'
expect 'lexer passed'
expect 'parser passed'
expect 'standalone passed'
expect_builds 2

# Multiple substring filters select only their union, including within a category.
run_tests lexer parser
[ "$status" -eq 0 ]
expect 'Summary: 2 passed, 0 failed'
expect_builds 1
run_tests lexer
[ "$status" -eq 0 ]
expect 'Summary: 1 passed, 0 failed'
expect_builds 1
run_tests no-such-test
[ "$status" -eq 0 ]
expect 'Notice: no tests matched filters: no-such-test'
expect_builds 0

# A process failure must not stop the following test or run it in the same process.
cat > "$RUN_DIR/fixtures/test_lexer.casa" <<'CASA'
import "std"
pub fn run_tests { "intentional runtime failure\n" print 1 std::process::exit }
run_tests
CASA
run_tests lexer parser
[ "$status" -ne 0 ]
expect 'RUNTIME FAIL'
expect 'intentional runtime failure'
expect 'parser passed'
expect 'Summary: 1 passed, 1 failed'
expect_builds 1

# A batch build error falls back to individual builds and identifies the bad test.
printf 'pub fn run_tests { missing_function }\nrun_tests\n' \
    > "$RUN_DIR/fixtures/test_lexer.casa"
run_tests lexer parser
[ "$status" -ne 0 ]
expect 'Retrying individual builds'
expect 'Running: test_lexer'
expect 'COMPILE FAIL'
expect 'parser passed'
expect 'Summary: 1 passed, 1 failed'
expect_builds 3

# Coverage is checked before category and substring selection.
: > "$RUN_DIR/fixtures/test_uncategorized.casa"
run_tests no-such-test
[ "$status" -ne 0 ]
expect 'Uncategorized compiler test: test/test_uncategorized'
expect_builds 0
