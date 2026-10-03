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
    "missing" selector.functions.add_str
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

test_invalid_machine_references
test_invalid_static_references
test::test_summary
