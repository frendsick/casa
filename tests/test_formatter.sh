#!/usr/bin/env bash
set -eu

. "$(dirname "$0")/test-lib.sh"

ROOT_DIR=$(git rev-parse --show-toplevel)

select_tool "${CASA_FORMATTER:-}" "$ROOT_DIR/casafmt" "${1:-}"
FORMATTER=$TEST_TOOL
if [ "$TEST_TOOL_ARG" = true ]; then
    shift
fi

cd "$ROOT_DIR"
TESTS_DIR="tests/formatter"

RED='\033[0;31m'
GREEN='\033[0;32m'
RESET='\033[0m'

pass=0
fail=0
has_filter=false
if [ $# -gt 0 ]; then
    has_filter=true
fi

# ============================================================================
# Golden file tests
# ============================================================================

for input_file in "$TESTS_DIR"/*.input.casa; do
    [ -f "$input_file" ] || continue
    base=$(basename "$input_file" .input.casa)

    if ! matches_filter "$base" "$@"; then
        continue
    fi

    expected_file="$TESTS_DIR/$base.expected.casa"

    if [ ! -f "$expected_file" ]; then
        printf "${RED}[SKIP]${RESET} Missing expected file: %s\n" "$expected_file"
        continue
    fi

    printf "Running test: %s\n" "$base"
    status=0
    actual=$("$FORMATTER" < "$input_file" 2>/dev/null) || status=$?

    if [ "$status" -eq 0 ] && [ "$actual" = "$(cat "$expected_file")" ]; then
        printf "${GREEN}[OK]${RESET} Passed: %s\n" "$base"
        pass=$((pass + 1))
    else
        printf "${RED}[FAIL]${RESET} Failed: %s (status %s)\n" "$base" "$status"
        diff <(echo "$actual") "$expected_file" || true
        fail=$((fail + 1))
    fi
done

if [ "$has_filter" = false ]; then

# ============================================================================
# Idempotency tests (full suite only)
# ============================================================================

printf "\nRunning idempotency tests...\n"
idem_pass=0
idem_fail=0

for f in examples/*.casa compiler/*.casa lib/*.casa; do
    [ -f "$f" ] || continue
    if ! first=$("$FORMATTER" < "$f" 2>/dev/null); then
        printf "${RED}[FAIL]${RESET} Formatter rejected: %s\n" "$f"
        idem_fail=$((idem_fail + 1))
        continue
    fi
    if ! second=$(echo "$first" | "$FORMATTER" 2>/dev/null); then
        printf "${RED}[FAIL]${RESET} Formatter rejected second pass: %s\n" "$f"
        idem_fail=$((idem_fail + 1))
        continue
    fi

    if [ "$first" = "$second" ]; then
        idem_pass=$((idem_pass + 1))
    else
        printf "${RED}[FAIL]${RESET} Not idempotent: %s\n" "$f"
        idem_fail=$((idem_fail + 1))
    fi
done

printf "${GREEN}[OK]${RESET} Idempotency: %d passed, %d failed\n" "$idem_pass" "$idem_fail"
pass=$((pass + idem_pass))
fail=$((fail + idem_fail))

# ============================================================================
# Safety tests (full suite only)
# ============================================================================

printf "\nRunning safety tests...\n"
safety_dir=$(mktemp -d /tmp/casa_formatter_safety.XXXXXX)
trap 'rm -rf "$safety_dir"' EXIT

check_formatter_case() {
    case_name=$1
    expected_status=$2
    input=$3
    expected_output=$4
    expected_error=${5:-}

    printf '%s' "$input" > "$safety_dir/input"
    printf '%s' "$expected_output" > "$safety_dir/expected"
    status=0
    timeout 2 "$FORMATTER" < "$safety_dir/input" > "$safety_dir/actual" 2> "$safety_dir/error" || status=$?

    if
        [ "$status" -eq "$expected_status" ] &&
        cmp -s "$safety_dir/expected" "$safety_dir/actual" &&
        { [ -z "$expected_error" ] && [ ! -s "$safety_dir/error" ] || grep -q "$expected_error" "$safety_dir/error"; }
    then
        printf "${GREEN}[OK]${RESET} Passed: %s\n" "$case_name"
        pass=$((pass + 1))
    else
        printf "${RED}[FAIL]${RESET} Failed: %s (status %s, expected %s)\n" "$case_name" "$status" "$expected_status"
        diff "$safety_dir/expected" "$safety_dir/actual" || true
        cat "$safety_dir/error"
        fail=$((fail + 1))
    fi
}

check_formatter_case syntax_only 0 \
    $'import "missing_formatter_module_410"\nconst alias missing_constant\nfn use[T:MissingTrait] value:T { value missing_call }\n' \
    $'import "missing_formatter_module_410"\n\nconst alias missing_constant\n\nfn use [T:MissingTrait] value:T { value missing_call }\n'
check_formatter_case invalid_lex 1 \
    'unclosed string "broken' \
    'unclosed string "broken' \
    'Unclosed string literal'
check_formatter_case invalid_input 1 \
    $'fn broken {\n' \
    $'fn broken {\n' \
    'UNMATCHED_BLOCK'
check_formatter_case invalid_candidate 1 \
    $'fn foo\n# note\n{\n}\n' \
    $'fn foo\n# note\n{\n}\n' \
    'invalid syntax'
check_formatter_case preservation_failure 1 \
    $'fn foo\n{ # note\n1\n}\n' \
    $'fn foo\n{ # note\n1\n}\n' \
    'changed source meaning'
# Array items must be comma-separated; a space-separated literal is a syntax
# error, so the formatter leaves the source untouched and exits 1.
check_formatter_case delimited_requires_commas 1 \
    $'fn f {\n    [1 2 3] use\n}\n' \
    $'fn f {\n    [1 2 3] use\n}\n' \
    'Expected `,` between array items'
# Dropping the optional trailing comma when compacting must not trip the
# token-equality safety net, since a trailing comma is non-meaningful.
check_formatter_case delimited_trailing_comma 0 \
    $'fn f {\n    [1, 2, 3,] use\n}\n' \
    $'fn f {\n    [1, 2, 3] use\n}\n'
check_formatter_case final_newline 0 $'1\n\n\n' $'1\n'
check_formatter_case crlf 0 $'1\r\n2 +\r\n' $'1\n2 +\n'
check_formatter_case bare_cr 0 $'1\r2 +\r' $'1\n2 +\n'

# ============================================================================
# Paired-input convergence tests (full suite only)
#
# Each directory under tests/formatter/paired holds structurally-equivalent
# inputs (same tokens, different layout). All variants must format to the same
# output and to a stable fixpoint.
# ============================================================================

printf "\nRunning paired-input tests...\n"
paired_pass=0
paired_fail=0

for group in tests/formatter/paired/*/; do
    [ -d "$group" ] || continue
    group_name=$(basename "$group")
    expected=""
    ok=true
    for variant in "$group"*.casa; do
        [ -f "$variant" ] || continue
        if ! out=$("$FORMATTER" < "$variant" 2>/dev/null); then
            printf "${RED}[FAIL]${RESET} Formatter rejected: %s\n" "$variant"
            ok=false
            continue
        fi
        if [ -z "$expected" ]; then
            expected=$out
            # The canonical form must also be a fixpoint.
            if ! stable=$(echo "$out" | "$FORMATTER" 2>/dev/null) || [ "$stable" != "$out" ]; then
                printf "${RED}[FAIL]${RESET} Not idempotent: %s\n" "$variant"
                ok=false
            fi
        elif [ "$out" != "$expected" ]; then
            printf "${RED}[FAIL]${RESET} Divergent output: %s\n" "$variant"
            diff <(echo "$expected") <(echo "$out") || true
            ok=false
        fi
    done
    if [ "$ok" = true ]; then
        printf "${GREEN}[OK]${RESET} Converged: %s\n" "$group_name"
        paired_pass=$((paired_pass + 1))
    else
        paired_fail=$((paired_fail + 1))
    fi
done

printf "${GREEN}[OK]${RESET} Paired-input: %d passed, %d failed\n" "$paired_pass" "$paired_fail"
pass=$((pass + paired_pass))
fail=$((fail + paired_fail))

fi # has_filter

# ============================================================================
# Summary
# ============================================================================

printf "\nSummary: %d passed, %d failed\n" "$pass" "$fail"

if [ "$fail" -ne 0 ]; then
    exit 1
fi
