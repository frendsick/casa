import "test" as test

fn assert_invalid_reference instruction:InstValue {
    Emitter::new = selector
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

    Emitter::new = selector
    "missing" selector.register_function
    std::List[InstValue]::new = actions
    "missing".to_str InstValue::FnCall actions.push
    "forward call accepted" 0 actions FunctionPlan::new selector.select_root.is_ok test::assert_true
    "uncompleted forward call prevents publication" selector.finish.is_error test::assert_true
}

fn test_invalid_static_references {
    "invalid static references" test::test_start
    Emitter::new = selector
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
    Emitter::new = selector
    "space" selector.register_function
    "space " selector.register_function
    "generic :_" selector.register_function
    "generic _58__" selector.register_function
    "space" selector.function_label = source
    "space " selector.function_label = generated
    "generated namespace is distinct" source generated != test::assert_true
    "generic :_" selector.function_label.to_str = escaped
    "generic _58__" selector.function_label = literal
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
test_sanitize_name
test_escape_string
test_invalid_machine_references
test_invalid_static_references
test::test_summary
