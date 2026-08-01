select_tool() {
    test_tool_override=$1
    test_tool_default=$2
    test_tool_candidate=${3:-}
    TEST_TOOL_ARG=false

    if [ -n "$test_tool_override" ]; then
        TEST_TOOL=$test_tool_override
    else
        case "$test_tool_candidate" in
            */*)
                TEST_TOOL=$test_tool_candidate
                TEST_TOOL_ARG=true
                ;;
            *) TEST_TOOL=$test_tool_default ;;
        esac
    fi
}

matches_filter() {
    test_name=$1
    shift
    if [ $# -eq 0 ]; then
        return 0
    fi
    for test_pattern in "$@"; do
        case "$test_name" in
            *"$test_pattern"*) return 0 ;;
        esac
    done
    return 1
}
