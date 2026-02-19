#!/usr/bin/env sh
set -eu

ROOT_DIR=$(git rev-parse --show-toplevel)
EXAMPLES_DIR="$ROOT_DIR/examples"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RESET='\033[0m'

pass=0
fail=0

for f in "$EXAMPLES_DIR"/*.casa; do
    base=$(basename "$f" .casa)
    out_file="$EXAMPLES_DIR/outputs/$base.out"
    binary="/tmp/casa_test_$base"

    echo "Running test: $base"

    # Compile
    python3 "$ROOT_DIR/casa.py" "$f" -o "$binary"

    # Run and capture output
    output=$("$binary")

    # Clean up binary
    rm -f "$binary"

    if [ -f "$out_file" ]; then
        if echo "$output" | diff -u - "$out_file"; then
            echo "${GREEN}[OK]${RESET} Passed: $base"
            pass=$((pass+1))
        else
            echo "${RED}[X]${RESET}  Failed: $base"
            fail=$((fail+1))
        fi
    else
        echo "${YELLOW}[!]${RESET}  Missing expected output: $base"
        echo "$output" > "$out_file"
        echo "${YELLOW}[+]${RESET}  Generated $out_file"
    fi
done

echo
echo "Summary: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
