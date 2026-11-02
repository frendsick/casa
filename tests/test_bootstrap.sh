#!/usr/bin/env sh
set -eu

. "$(dirname "$0")/test-lib.sh"

ROOT_DIR=$(cd "$(dirname "$0")/.." && pwd)
LIB_DIR="$ROOT_DIR/lib"
TEST_TMP=$(mktemp -d "${TMPDIR:-/tmp}/casa_bootstrap_tests.XXXXXX")
trap 'rm -rf "$TEST_TMP"' EXIT
cd "$ROOT_DIR"

select_tool "${CASA_COMPILER:-}" "$ROOT_DIR/casac" "${1:-}"
COMPILER=$TEST_TOOL
if [ "$TEST_TOOL_ARG" = true ]; then
    shift
fi

RED='\033[0;31m'
GREEN='\033[0;32m'
RESET='\033[0m'

pass=0
fail=0

# Build the shared chain once. The input may be a release or branch compiler.
stage1="$TEST_TMP/stage1"
stage2="$TEST_TMP/stage2"
stage3="$TEST_TMP/stage3"
stage2_test_bin="$TEST_TMP/stage2_test"

printf "Running: self_compilation ... "
if ! $COMPILER -L "$LIB_DIR" "$ROOT_DIR/casa.casa" -o "$stage1" 2>"$TEST_TMP/compile_err"; then
    printf "${RED}STAGE1 COMPILE FAIL${RESET}\n"
    cat "$TEST_TMP/compile_err"
    printf '\nSummary: 0 passed, 1 failed\n'
    exit 1
fi
if ! "$stage1" -L "$LIB_DIR" "$ROOT_DIR/casa.casa" -o "$stage2" --keep-asm 2>"$TEST_TMP/compile_err"; then
    printf "${RED}STAGE2 COMPILE FAIL${RESET}\n"
    cat "$TEST_TMP/compile_err"
    printf '\nSummary: 0 passed, 1 failed\n'
    exit 1
fi

# Verify stage2 can compile and run a program.
if ! "$stage2" -L "$LIB_DIR" "$ROOT_DIR/examples/hello_world.casa" -o "$stage2_test_bin" 2>"$TEST_TMP/compile_err"; then
    printf "${RED}STAGE2 COMPILE OUTPUT FAIL${RESET}\n"
    cat "$TEST_TMP/compile_err"
    fail=$((fail+1))
else
    stage2_status=0
    stage2_output=$("$stage2_test_bin" 2>&1) || stage2_status=$?
    expected="Hello world!"
    if [ "$stage2_status" -eq 0 ] && [ "$stage2_output" = "$expected" ]; then
        printf "${GREEN}OK${RESET}\n"
        pass=$((pass+1))
    else
        printf "${RED}STAGE2 RUNTIME FAIL${RESET}\n"
        echo "  exit:     $stage2_status"
        echo "  expected: $expected"
        echo "  got:      $stage2_output"
        fail=$((fail+1))
    fi
fi

# Reuse stage2 and its assembly for fixed-point verification: stage2.s == stage3.s.
printf "Running: fixed_point ... "
if ! "$stage2" -L "$LIB_DIR" "$ROOT_DIR/casa.casa" -o "$stage3" --keep-asm 2>"$TEST_TMP/compile_err"; then
    printf "${RED}STAGE3 COMPILE FAIL${RESET}\n"
    cat "$TEST_TMP/compile_err"
    fail=$((fail+1))
elif diff "$stage2.s" "$stage3.s" > /dev/null 2>&1; then
    printf "${GREEN}OK${RESET}\n"
    pass=$((pass+1))
else
    printf "${RED}STAGE2/STAGE3 ASSEMBLY DIFFERS${RESET}\n"
    diff "$stage2.s" "$stage3.s" | head -20
    fail=$((fail+1))
fi

echo
echo "Summary: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
