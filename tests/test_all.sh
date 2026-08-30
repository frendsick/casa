#!/usr/bin/env sh
set -eu

ROOT_DIR=$(cd "$(dirname "$0")/.." && pwd)
RUN_DIR=$(mktemp -d "${TMPDIR:-/tmp}/casa_tests.XXXXXX")
trap 'rm -rf "$RUN_DIR"' EXIT

SHARDS='compiler_parsing
compiler_analysis
compiler_integration
types_ownership
types_generics
types_traits
language_memory
language_control_flow
language_values
runtime
cli
examples
bootstrap
formatter'

run_shard() {
    case "$1" in
        compiler_parsing|compiler_analysis|compiler_integration|\
        types_ownership|types_generics|types_traits|language_memory|\
        language_control_flow|language_values|runtime)
            CASA_TEST_CATEGORY=$1 "$ROOT_DIR/tests/test_compiler.sh"
            ;;
        cli) "$ROOT_DIR/tests/test_cli.sh" ;;
        examples) "$ROOT_DIR/tests/test_examples.sh" ;;
        bootstrap) "$ROOT_DIR/tests/test_bootstrap.sh" ;;
        formatter)
            ${CASA_COMPILER:-"$ROOT_DIR/casac"} -L "$ROOT_DIR/lib" \
                "$ROOT_DIR/formatter/format.casa" -o "$RUN_DIR/casafmt"
            CASA_FORMATTER="$RUN_DIR/casafmt" \
                "$ROOT_DIR/tests/test_formatter.sh"
            ;;
        *)
            echo "Unknown test shard: $1" >&2
            exit 2
            ;;
    esac
}

if [ $# -eq 1 ]; then
    run_shard "$1"
    exit
fi
if [ $# -ne 0 ]; then
    echo "Usage: $0 [shard]" >&2
    exit 2
fi

jobs=${CASA_TEST_JOBS:-$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 1)}
case "$jobs" in
    ''|0|*[!0-9]*)
        echo "CASA_TEST_JOBS must be a positive integer" >&2
        exit 2
        ;;
esac

# Match CI: build the branch compiler once, then give it to every test shard.
${CASA_COMPILER:-"$ROOT_DIR/casac"} -L "$ROOT_DIR/lib" \
    "$ROOT_DIR/casa.casa" -o "$RUN_DIR/casac-stage1"
CASA_COMPILER="$RUN_DIR/casac-stage1"
export CASA_COMPILER

printf 'Running CI test shards with %s parallel jobs\n' "$jobs"
printf '%s\n' "$SHARDS" | xargs -n 1 -P "$jobs" sh -c '
    script=$1
    run_dir=$2
    shard=$3
    {
        if "$script" "$shard"; then
            : >"$run_dir/$shard.pass"
        else
            : >"$run_dir/$shard.fail"
        fi
    } 2>&1 | awk -v shard="$shard" '\''
        { print "[" shard "] " $0; fflush() }
    '\''
    [ -f "$run_dir/$shard.pass" ]
' sh "$ROOT_DIR/tests/test_all.sh" "$RUN_DIR" || :

pass=0
fail=0
for shard in $SHARDS; do
    if [ -f "$RUN_DIR/$shard.pass" ]; then
        printf 'Shard passed: %s\n' "$shard"
        pass=$((pass + 1))
    else
        printf 'Shard failed: %s\n' "$shard"
        fail=$((fail + 1))
    fi
done

printf '\nShard summary: %s passed, %s failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
