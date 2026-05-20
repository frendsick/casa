#!/usr/bin/env sh
set -eu

ROOT_DIR=$(cd "$(dirname "$0")/.." && pwd)
TESTS_DIR="$ROOT_DIR/tests/compiler"
LIB_DIR="$ROOT_DIR/lib"
cd "$ROOT_DIR"

# Compiler: env var > positional path (backward compat) > default
if [ -n "${CASA_COMPILER:-}" ]; then
    COMPILER="$CASA_COMPILER"
elif [ $# -ge 1 ] && echo "$1" | grep -q '/'; then
    COMPILER="$1"
    shift
else
    COMPILER="$ROOT_DIR/casac"
fi

RED='\033[0;31m'
GREEN='\033[0;32m'
RESET='\033[0m'

pass=0
fail=0

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

for f in "$TESTS_DIR"/test_*.casa; do
    base=$(basename "$f" .casa)

    if ! matches_filter "$base" "$@"; then
        continue
    fi

    binary="/tmp/casa_${base}"

    printf "Running: %s ... " "$base"

    if ! $COMPILER -L "$LIB_DIR" "$f" -o "$binary" 2>/tmp/casa_compile_err; then
        printf "${RED}COMPILE FAIL${RESET}\n"
        cat /tmp/casa_compile_err
        fail=$((fail+1))
        rm -f "$binary"
        continue
    fi

    # Keep tests headless even when a compiled test reads stdin.
    output=$("$binary" 2>&1 < /dev/null) || {
        printf "${RED}RUNTIME FAIL${RESET}\n"
        echo "$output"
        fail=$((fail+1))
        rm -f "$binary"
        continue
    }

    printf "${GREEN}OK${RESET} %s\n" "$(echo "$output" | tail -1)"
    pass=$((pass+1))
    rm -f "$binary"
done

# Error-fixture tests: files in errors/ must fail to compile with a specific tag
for f in "$TESTS_DIR"/errors/*.casa; do
    [ -f "$f" ] || continue
    base=$(basename "$f" .casa)

    if ! matches_filter "$base" "$@"; then
        continue
    fi

    expected_tag=$(head -1 "$f" | sed 's/^# expect: //')

    printf "Running: error/%s ... " "$base"

    if $COMPILER -L "$LIB_DIR" "$f" -o /dev/null 2>/tmp/casa_err; then
        printf "${RED}EXPECTED COMPILE FAIL${RESET}\n"
        fail=$((fail+1))
    elif ! grep -q "$expected_tag" /tmp/casa_err; then
        printf "${RED}WRONG ERROR (expected %s)${RESET}\n" "$expected_tag"
        cat /tmp/casa_err
        fail=$((fail+1))
    else
        printf "${GREEN}OK${RESET}\n"
        pass=$((pass+1))
    fi
done

echo
echo "Summary: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
