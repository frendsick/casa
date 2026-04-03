#!/usr/bin/env sh
set -eu

ROOT_DIR=$(git rev-parse --show-toplevel)
TESTS_DIR="$ROOT_DIR/tests/self_hosted"

RED='\033[0;31m'
GREEN='\033[0;32m'
RESET='\033[0m'

pass=0
fail=0

for f in "$TESTS_DIR"/test_*.casa; do
    base=$(basename "$f" .casa)
    binary="/tmp/casa_${base}"

    printf "Running: %s ... " "$base"

    if ! python3 "$ROOT_DIR/casa.py" "$f" -o "$binary" 2>/tmp/casa_compile_err; then
        printf "${RED}COMPILE FAIL${RESET}\n"
        cat /tmp/casa_compile_err
        fail=$((fail+1))
        rm -f "$binary"
        continue
    fi

    output=$("$binary" 2>&1) || {
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

echo
echo "Summary: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
