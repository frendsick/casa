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
RED='\033[0;31m'
GREEN='\033[0;32m'
RESET='\033[0m'

# Idempotency sweep over repository sources

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

[ "$idem_fail" -eq 0 ]
