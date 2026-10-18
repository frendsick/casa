import "test" as test
import "source_builder" as source_builder

fn assert_invalid_reference instruction:InstValue {
    lexer::SourceStore::new = sources
    sources Emitter::new = selector
    std::List[InstValue]::new = actions
    instruction actions.push
    0 actions FunctionPlan::new = plan
    "invalid reference rejected" plan selector.select_root.is_error test::assert_true
    "failed selector cannot retry" 0 std::List[InstValue]::new FunctionPlan::new selector.select_root.is_error test::assert_true
    "invariant failure prevents publication" selector.finish.is_error test::assert_true
}

fn test_invalid_machine_references {
    "invalid machine references" test::test_start
    7 InstValue::Jump assert_invalid_reference
    0 InstValue::PushStr assert_invalid_reference
    0 InstValue::LocalGet assert_invalid_reference
    0 InstValue::CaptureLoad assert_invalid_reference
    "missing".to_str InstValue::FnCall assert_invalid_reference
    "native".to_str InstValue::ExternCall assert_invalid_reference

    lexer::SourceStore::new = sources
    sources Emitter::new = selector
    "missing" selector.register_function
    std::List[InstValue]::new = actions
    "missing".to_str InstValue::FnCall actions.push
    "forward call accepted" 0 actions FunctionPlan::new selector.select_root.is_ok test::assert_true
    "uncompleted forward call prevents publication" selector.finish.is_error test::assert_true
}

fn test_invalid_static_references {
    "invalid static references" test::test_start
    lexer::SourceStore::new = sources
    sources Emitter::new = selector
    std::List[StaticArrayElement]::new = elements
    0 StaticArrayElement::String elements.push
    elements StaticArray selector.static_arrays.push
    "root selects" 0 std::List[InstValue]::new FunctionPlan::new selector.select_root.is_ok test::assert_true
    "missing pool reference prevents publication" selector.finish.is_error test::assert_true
}

pub fn test_sanitize_name {
    "sanitize_name" test::test_start
    "simple name" "foo" "foo" sanitize_name.as_str test::assert_str_eq
    "double colon" "Foo__bar" "Foo::bar" sanitize_name.as_str test::assert_str_eq
    "multiple" "A__B__C" "A::B::C" sanitize_name.as_str test::assert_str_eq
    "no colons" "hello_world" "hello_world" sanitize_name.as_str test::assert_str_eq
    "single colon preserved" "a:b" "a:b" sanitize_name.as_str test::assert_str_eq
    lexer::SourceStore::new = sources
    sources Emitter::new = selector
    "space" selector.register_function
    "space " selector.register_function
    "generic :_" selector.register_function
    "generic .58._" selector.register_function
    "space" selector.function_label = source
    "space " selector.function_label = generated
    "generated namespace is distinct" source generated != test::assert_true
    "generic :_" selector.function_label.to_str = escaped
    "readable reversible label" ".specialized_generic.32..58._" escaped.as_str test::assert_str_eq
    "generic .58._" selector.function_label = literal
    "generated identities stay distinct" escaped literal != test::assert_true
    "generic :_" selector.register_function
    "registration is idempotent" escaped.as_str "generic :_" selector.function_label test::assert_str_eq
}

pub fn test_escape_string {
    "escape_string" test::test_start
    "plain" "hello" "hello" escape_string.as_str test::assert_str_eq
    "newline" "a\\nb" "a\nb" escape_string.as_str test::assert_str_eq
    "tab" "a\\tb" "a\tb" escape_string.as_str test::assert_str_eq
    "backslash" "\\\\" "\\" escape_string.as_str test::assert_str_eq
    "quote" "\\\"" "\"" escape_string.as_str test::assert_str_eq
    std::String::new = nul_then_digit
    '\0' nul_then_digit.push
    '1' nul_then_digit.push
    "NUL before octal digit" "\\0001" nul_then_digit.as_str
    escape_string.as_str test::assert_str_eq
}

fn check_inline_source source:$str -> typechecker::CheckedProgram {
    "inline-cache.casa" source lexer::lex_source.into_parts = diagnostics = tokens
    lexer::SourceStore::new = sources
    source "inline-cache.casa" sources.add
    sources diagnostics std::List[std::String]::new tokens source_builder::parse_and_resolve.into_parts
    = parse_diagnostics = parsed
    "cache fixture parses" parse_diagnostics.has_errors ! test::assert_true
    parsed.unwrap typechecker::type_check.into_checked_program.unwrap
}

fn test_reusable_inline_plans {
    "reusable inline plans retain one concrete candidate" test::test_start
    "inline fn candidate value:i64 -> i64 { if value 0 < then value 1 - return fi value 1 + } 1 candidate drop"
    check_inline_source = checked
    lexer::SourceStore::new = sources
    sources checked BackendPlanner::new = state
    0 "candidate" "candidate" state.find_inline_plan.unwrap = first
    state.label_counter = after_first
    "branch plan allocates local labels" after_first 0 > test::assert_true
    0 "candidate" "candidate" state.find_inline_plan.unwrap = second
    "reused candidate allocates no new labels" after_first state.label_counter test::assert_eq
    0 "candidate" state
        .closure_targets
        .defaults
        .get_copy_str
        .unwrap_or = context
    context "candidate" "candidate" state.find_inline_plan.unwrap drop
    "default context shares its plan" 1 state.inline_plans.length test::assert_eq
    "reuse retains the same local extent" first.get.locals_count second.get.locals_count test::assert_eq
    "reuse retains the same action count" first
        .get
        .instructions
        .length second
        .get
        .instructions
        .length test::assert_eq
    "retained action accounting counts the plan once" first
        .get
        .instructions
        .length state.inline_retained test::assert_eq
    "inline fn recursive value:i64 -> i64 { if value 0 == then 0 return fi value 1 - recursive } 1 recursive drop"
    check_inline_source = recursive
    sources recursive BackendPlanner::new = rejected
    "recursive candidate rejected" 0 "recursive" "recursive" rejected.find_inline_plan.is_none test::assert_true
    rejected.label_counter = after_rejection
    "negative plan reused" 0 "recursive" "recursive" rejected.find_inline_plan.is_none test::assert_true
    "negative reuse does not plan labels" after_rejection rejected.label_counter test::assert_eq
    "one bounded negative entry" 1 rejected.inline_plans.length test::assert_eq
}
test_sanitize_name
test_escape_string
test_invalid_machine_references
test_invalid_static_references
test_reusable_inline_plans
test::test_summary
