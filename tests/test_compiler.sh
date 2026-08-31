#!/usr/bin/env sh
set -eu

. "$(dirname "$0")/test-lib.sh"

ROOT_DIR=$(cd "$(dirname "$0")/.." && pwd)
TESTS_DIR=${CASA_COMPILER_TESTS_DIR:-"$ROOT_DIR/tests/compiler"}
LIB_DIR="$ROOT_DIR/lib"
TEST_TMP=$(mktemp -d "${TMPDIR:-/tmp}/casa_compiler_tests.XXXXXX")
trap 'rm -rf "$TEST_TMP"' EXIT
cd "$ROOT_DIR"

select_tool "${CASA_COMPILER:-}" "$ROOT_DIR/casac" "${1:-}"
COMPILER=$TEST_TOOL
if [ "$TEST_TOOL_ARG" = true ]; then
    shift
fi

TEST_CATEGORY=${CASA_TEST_CATEGORY:-all}
case "$TEST_CATEGORY" in
    all|compiler_parsing|compiler_analysis|compiler_integration|\
    types_ownership|types_generics|types_traits|language_memory|\
    language_control_flow|language_values|runtime) ;;
    *)
        echo "Unknown compiler test category: $TEST_CATEGORY" >&2
        exit 2
        ;;
esac

# Assign every fixture to one CI category. Unknown fixtures fail instead of
# being silently omitted from category-based CI runs.
test_category() {
    case "$1" in
        test/test_lexer|test/test_parser|test/test_pattern|test/test_type_ast)
            echo compiler_parsing
            ;;
        test/test_analysis|test/test_block_scope|test/test_common|\
        test/test_const|test/test_semantics|test/test_type_annotations|\
        test/test_typechecker|test/test_underflow_messages|error/*)
            echo compiler_analysis
            ;;
        test/test_bytecode|test/test_document|test/test_emitter|test/test_error|\
        test/test_extern|\
        test/test_lsp|test/test_modules|test/test_selective_import|\
        test/test_selective_import_closure)
            echo compiler_integration
            ;;
        test/test_closure_ownership|test/test_copy_clone|\
        test/test_for_owned_item|test/test_owned_contexts|test/test_scope)
            echo types_ownership
            ;;
        test/test_const_param|test/test_enum_variant_hint|\
        test/test_generic_structs|test/test_one_letter_nominal_types|\
        test/test_selective_import_type_deps|test/test_typed_struct_fields)
            echo types_generics
            ;;
        test/test_derived_conformances|test/test_display|test/test_traits)
            echo types_traits
            ;;
        test/test_array_length|test/test_array_methods|test/test_size_of|\
        test/test_typed_raw|test/test_unsafe)
            echo language_memory
            ;;
        test/test_global_keyword|test/test_root_state|\
        test/test_match_underflow|test/test_question)
            echo language_control_flow
            ;;
        test/test_argv|test/test_compare|test/test_enum|\
        test/test_numeric_types|test/test_struct_literal|test/test_typeof)
            echo language_values
            ;;
        test/test_argparse|test/test_bytes|test/test_collection_is_empty|\
        test/test_collection_reclamation|test/test_destruction|test/test_file|\
        test/test_iterator_combinators|test/test_list_contains|\
        test/test_map_iter|test/test_parser_borrows|test/test_set_iter|\
        test/test_slice|test/test_sorting|test/test_string_iteration|\
        test/test_string_utilities|runtime_error/*)
            echo runtime
            ;;
        *)
            echo "Uncategorized compiler test: $1" >&2
            return 1
            ;;
    esac
}

RED='\033[0;31m'
GREEN='\033[0;32m'
RESET='\033[0m'

pass=0
fail=0
matched=false

for f in "$TESTS_DIR"/test_*.casa; do
    base=$(basename "$f" .casa)

    category=$(test_category "test/$base")
    if [ "$TEST_CATEGORY" != all ] && [ "$TEST_CATEGORY" != "$category" ]; then
        continue
    fi

    if ! matches_filter "$base" "$@"; then
        continue
    fi
    matched=true

    binary="$TEST_TMP/$base"

    printf "Running: %s ... " "$base"

    if ! $COMPILER -L "$LIB_DIR" "$f" -o "$binary" 2>"$TEST_TMP/compile_err"; then
        printf "${RED}COMPILE FAIL${RESET}\n"
        cat "$TEST_TMP/compile_err"
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

    category=$(test_category "error/$base")
    if [ "$TEST_CATEGORY" != all ] && [ "$TEST_CATEGORY" != "$category" ]; then
        continue
    fi

    if ! matches_filter "$base" "$@"; then
        continue
    fi
    matched=true

    expected_tag=$(head -1 "$f" | sed 's/^# expect: //')

    printf "Running: error/%s ... " "$base"

    if $COMPILER -L "$LIB_DIR" "$f" -o /dev/null 2>"$TEST_TMP/error"; then
        printf "${RED}EXPECTED COMPILE FAIL${RESET}\n"
        fail=$((fail+1))
    elif ! grep -q "$expected_tag" "$TEST_TMP/error"; then
        printf "${RED}WRONG ERROR (expected %s)${RESET}\n" "$expected_tag"
        cat "$TEST_TMP/error"
        fail=$((fail+1))
    else
        printf "${GREEN}OK${RESET}\n"
        pass=$((pass+1))
    fi
done

# Runtime-error fixtures must compile, terminate unsuccessfully, and emit the
# expected message from their first line.
for f in "$TESTS_DIR"/runtime_errors/*.casa; do
    [ -f "$f" ] || continue
    base=$(basename "$f" .casa)

    category=$(test_category "runtime_error/$base")
    if [ "$TEST_CATEGORY" != all ] && [ "$TEST_CATEGORY" != "$category" ]; then
        continue
    fi

    if ! matches_filter "$base" "$@"; then
        continue
    fi
    matched=true

    expected_message=$(head -1 "$f" | sed 's/^# expect: //')
    binary="$TEST_TMP/runtime_${base}"

    printf "Running: runtime_error/%s ... " "$base"

    if ! $COMPILER -L "$LIB_DIR" "$f" -o "$binary" 2>"$TEST_TMP/compile_err"; then
        printf "${RED}COMPILE FAIL${RESET}\n"
        cat "$TEST_TMP/compile_err"
        fail=$((fail+1))
    elif "$binary" >"$TEST_TMP/runtime_out" 2>"$TEST_TMP/runtime_err"; then
        printf "${RED}EXPECTED RUNTIME FAIL${RESET}\n"
        fail=$((fail+1))
    elif ! grep -q "$expected_message" "$TEST_TMP/runtime_err"; then
        printf "${RED}WRONG ERROR (expected %s)${RESET}\n" "$expected_message"
        cat "$TEST_TMP/runtime_err"
        fail=$((fail+1))
    else
        printf "${GREEN}OK${RESET}\n"
        pass=$((pass+1))
    fi
    rm -f "$binary"
done

report_no_matches "$matched" "$@"
echo
echo "Summary: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
