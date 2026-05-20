#!/usr/bin/env bash
set -eu

ROOT_DIR=$(git rev-parse --show-toplevel)

# Formatter: env var > positional path (backward compat) > default
if [ -n "${CASA_FORMATTER:-}" ]; then
    FORMATTER="$CASA_FORMATTER"
elif [ $# -ge 1 ] && echo "$1" | grep -q '/'; then
    FORMATTER="$1"
    shift
else
    FORMATTER="$ROOT_DIR/casafmt"
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

matches_filter() {
    name="$1"
    shift
    if [ $# -eq 0 ]; then
        return 0
    fi
    for pattern in "$@"; do
        case "$name" in
            *"$pattern"*) return 0 ;;
        esac
    done
    return 1
}

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
    actual=$("$FORMATTER" < "$input_file" 2>/dev/null) || true

    if [ "$actual" = "$(cat "$expected_file")" ]; then
        printf "${GREEN}[OK]${RESET} Passed: %s\n" "$base"
        pass=$((pass + 1))
    else
        printf "${RED}[FAIL]${RESET} Failed: %s\n" "$base"
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
    first=$("$FORMATTER" < "$f" 2>/dev/null) || continue
    second=$(echo "$first" | "$FORMATTER" 2>/dev/null) || continue

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
# Error handling test (full suite only)
# ============================================================================

printf "\nRunning error handling test...\n"
invalid_input='unclosed string "broken'
actual_err=$(echo "$invalid_input" | "$FORMATTER" 2>/dev/null) || true
if [ "$actual_err" = "$invalid_input" ]; then
    printf "${GREEN}[OK]${RESET} Passed: error_passthrough\n"
    pass=$((pass + 1))
else
    printf "${RED}[FAIL]${RESET} Failed: error_passthrough\n"
    fail=$((fail + 1))
fi

fi # has_filter

# ============================================================================
# Summary
# ============================================================================

printf "\nSummary: %d passed, %d failed\n" "$pass" "$fail"

if [ "$fail" -ne 0 ]; then
    exit 1
fi
