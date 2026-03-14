"""Tests for the Casa Language Server (casa_ls.py)."""

import sys
from pathlib import Path
from unittest.mock import MagicMock

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

import pytest

pygls = pytest.importorskip("pygls", reason="pygls not installed")
from lsprotocol import types

from casa.bytecode import _op_label_map
from casa.common import (
    GLOBAL_ENUMS,
    GLOBAL_FUNCTIONS,
    GLOBAL_STRUCTS,
    GLOBAL_TRAITS,
    GLOBAL_VARIABLES,
    INCLUDED_FILES,
    CasaEnum,
    EnumVariant,
    Function,
    Intrinsic,
    Keyword,
    Location,
    Member,
    Op,
    OpKind,
    Parameter,
    Signature,
    Span,
    Struct,
    Variable,
)
from casa.error import (
    SOURCE_CACHE,
    WARNINGS,
    CasaError,
    CasaWarning,
    ErrorKind,
    WarningKind,
)
from casa_ls import (
    DocumentState,
    casa_error_to_diagnostic,
    casa_warning_to_diagnostic,
    clear_compilation_state,
    document_states,
    find_all_references,
    find_containing_function,
    find_op_at_position,
    find_variable_definition,
    format_hover,
    offset_to_range,
    position_to_offset,
    run_diagnostics,
    run_pipeline,
    text_document_completion,
    text_document_definition,
    text_document_hover,
    text_document_references,
    text_document_rename,
    text_document_semantic_tokens_full,
)

TEST_FILE = Path("test.casa")


class TestClearCompilationState:
    """Tests for clear_compilation_state()."""

    def test_clears_all_global_state(self):
        """All module-level mutable collections are emptied."""
        GLOBAL_ENUMS["dummy"] = "value"
        GLOBAL_FUNCTIONS["dummy"] = "value"
        GLOBAL_STRUCTS["dummy"] = "value"
        GLOBAL_TRAITS["dummy"] = "value"
        GLOBAL_VARIABLES["dummy"] = "value"
        INCLUDED_FILES.add("dummy")
        SOURCE_CACHE[Path("dummy")] = "code"
        WARNINGS.append("dummy")
        _op_label_map["dummy"] = "value"

        clear_compilation_state()

        assert len(GLOBAL_ENUMS) == 0
        assert len(GLOBAL_FUNCTIONS) == 0
        assert len(GLOBAL_STRUCTS) == 0
        assert len(GLOBAL_TRAITS) == 0
        assert len(GLOBAL_VARIABLES) == 0
        assert len(INCLUDED_FILES) == 0
        assert len(SOURCE_CACHE) == 0
        assert len(WARNINGS) == 0
        assert len(_op_label_map) == 0


class TestOffsetToRange:
    """Tests for offset_to_range()."""

    def test_single_line_start(self):
        """Offset at start of single-line source gives line 0, char 0."""
        source = "hello world"
        result = offset_to_range(source, 0, 5)
        assert result.start.line == 0
        assert result.start.character == 0
        assert result.end.line == 0
        assert result.end.character == 5

    def test_multiline_second_line(self):
        """Offset on the second line maps to line 1 (0-based)."""
        source = "first\nsecond"
        # "second" starts at offset 6
        result = offset_to_range(source, 6, 6)
        assert result.start.line == 1
        assert result.start.character == 0
        assert result.end.line == 1
        assert result.end.character == 6

    def test_zero_length_uses_minimum_of_one(self):
        """Length of 0 is treated as 1 so the range spans at least one char."""
        source = "abc"
        result = offset_to_range(source, 1, 0)
        assert result.start.line == 0
        assert result.start.character == 1
        assert result.end.line == 0
        assert result.end.character == 2

    def test_mid_line_offset(self):
        """Offset in the middle of a line gives correct character position."""
        source = "hello world"
        result = offset_to_range(source, 6, 5)
        assert result.start.line == 0
        assert result.start.character == 6
        assert result.end.line == 0
        assert result.end.character == 11


class TestCasaErrorToDiagnostic:
    """Tests for casa_error_to_diagnostic()."""

    def test_with_location(self):
        """Error with location produces correct range and Error severity."""
        source = "42 print"
        file = TEST_FILE
        SOURCE_CACHE[file] = source
        error = CasaError(
            kind=ErrorKind.TYPE_MISMATCH,
            message="type mismatch",
            location=Location(file=file, span=Span(offset=0, length=2)),
        )

        diag = casa_error_to_diagnostic(error)

        assert diag.severity == types.DiagnosticSeverity.Error
        assert diag.source == "casa"
        assert "type mismatch" in diag.message
        assert diag.range.start.line == 0
        assert diag.range.start.character == 0

    def test_without_location_falls_back_to_zero(self):
        """Error without location defaults to (0,0) range."""
        error = CasaError(
            kind=ErrorKind.SYNTAX,
            message="unexpected EOF",
            location=None,
        )

        diag = casa_error_to_diagnostic(error)

        assert diag.range.start.line == 0
        assert diag.range.start.character == 0
        assert diag.range.end.line == 0
        assert diag.range.end.character == 0
        assert diag.severity == types.DiagnosticSeverity.Error

    def test_includes_expected_and_got(self):
        """Expected and got fields are appended to the message."""
        error = CasaError(
            kind=ErrorKind.TYPE_MISMATCH,
            message="type mismatch",
            location=None,
            expected="int",
            got="str",
        )

        diag = casa_error_to_diagnostic(error)

        assert "Expected: int" in diag.message
        assert "Got: str" in diag.message


class TestCasaWarningToDiagnostic:
    """Tests for casa_warning_to_diagnostic()."""

    def test_with_location(self):
        """Warning with location produces correct range and Warning severity."""
        source = "fn foo x:int {}"
        file = TEST_FILE
        SOURCE_CACHE[file] = source
        warning = CasaWarning(
            kind=WarningKind.UNUSED_PARAMETER,
            message="unused parameter 'x'",
            location=Location(file=file, span=Span(offset=7, length=1)),
        )

        diag = casa_warning_to_diagnostic(warning)

        assert diag.severity == types.DiagnosticSeverity.Warning
        assert diag.source == "casa"
        assert "unused parameter" in diag.message
        assert diag.range.start.line == 0
        assert diag.range.start.character == 7

    def test_without_location_falls_back_to_zero(self):
        """Warning without location defaults to (0,0) range."""
        warning = CasaWarning(
            kind=WarningKind.UNUSED_PARAMETER,
            message="unused parameter",
            location=None,
        )

        diag = casa_warning_to_diagnostic(warning)

        assert diag.range.start.line == 0
        assert diag.range.start.character == 0
        assert diag.range.end.line == 0
        assert diag.range.end.character == 0
        assert diag.severity == types.DiagnosticSeverity.Warning


class TestRunDiagnostics:
    """Tests for run_diagnostics()."""

    def test_valid_code_produces_no_diagnostics(self, tmp_path):
        """Valid Casa code should yield an empty diagnostics list."""
        source_file = tmp_path / "valid.casa"
        source_file.write_text("42 print")
        uri = f"file://{source_file}"

        mock_server = MagicMock()
        run_diagnostics(mock_server, uri)

        mock_server.text_document_publish_diagnostics.assert_called_once()
        params = mock_server.text_document_publish_diagnostics.call_args[0][0]
        assert params.uri == uri
        assert len(params.diagnostics) == 0

    def test_invalid_code_produces_error_diagnostics(self, tmp_path):
        """Invalid Casa code should yield at least one error diagnostic."""
        source_file = tmp_path / "invalid.casa"
        source_file.write_text('42 "hello" +')
        uri = f"file://{source_file}"

        mock_server = MagicMock()
        run_diagnostics(mock_server, uri)

        mock_server.text_document_publish_diagnostics.assert_called_once()
        params = mock_server.text_document_publish_diagnostics.call_args[0][0]
        assert len(params.diagnostics) > 0
        assert params.diagnostics[0].severity == types.DiagnosticSeverity.Error


class TestPositionToOffset:
    """Tests for position_to_offset()."""

    def test_first_line_first_col(self):
        """Line 0, col 0 maps to byte offset 0."""
        source = "hello world"
        assert position_to_offset(source, 0, 0) == 0

    def test_second_line(self):
        """Line 1, col 0 maps to byte offset after first newline."""
        source = "first\nsecond"
        assert position_to_offset(source, 1, 0) == 6

    def test_mid_line(self):
        """Middle of a line gives correct byte offset."""
        source = "hello world"
        assert position_to_offset(source, 0, 6) == 6

    def test_third_line_mid_col(self):
        """Line 2, col 3 on a multi-line source."""
        source = "aaa\nbbb\ncccdef"
        assert position_to_offset(source, 2, 3) == 11


class TestRunPipeline:
    """Tests for run_pipeline()."""

    def test_returns_document_state(self, tmp_path):
        """Valid code returns a DocumentState with ops, functions, etc."""
        source_file = tmp_path / "pipeline.casa"
        source_file.write_text("fn greet { 42 print }\ngreet")

        state, errors = run_pipeline(source_file)

        assert isinstance(state, DocumentState)
        assert len(state.ops) > 0
        assert "greet" in state.functions
        assert state.source == "fn greet { 42 print }\ngreet"
        assert state.file_path == source_file
        assert len(errors) == 0

    def test_partial_failure_still_returns_ops(self, tmp_path):
        """Code with a type error still returns resolved ops."""
        source_file = tmp_path / "partial.casa"
        source_file.write_text('42 "hello" +')

        state, errors = run_pipeline(source_file)

        assert isinstance(state, DocumentState)
        assert len(state.ops) > 0
        assert state.source == '42 "hello" +'
        assert len(errors) > 0

    def test_populates_structs(self, tmp_path):
        """Structs defined in the source appear in the DocumentState."""
        source_file = tmp_path / "structs.casa"
        source_file.write_text("struct Point {\n    x: int\n    y: int\n}")

        state, _ = run_pipeline(source_file)

        assert "Point" in state.structs

    def test_populates_enums(self, tmp_path):
        """Enums defined in the source appear in the DocumentState."""
        source_file = tmp_path / "enums.casa"
        source_file.write_text("enum Color { Red Green Blue }")

        state, _ = run_pipeline(source_file)

        assert "Color" in state.enums


class TestFindOpAtPosition:
    """Tests for find_op_at_position()."""

    def test_finds_op(self, tmp_path):
        """An op at a given position is returned."""
        source_file = tmp_path / "findop.casa"
        source_file.write_text("42 print")

        state, _ = run_pipeline(source_file)
        result = find_op_at_position(state, 0, 0)

        assert result is not None
        op, func = result
        assert op.kind == OpKind.PUSH_INT
        assert op.value == 42
        assert func is None

    def test_finds_op_in_function(self, tmp_path):
        """An op inside a function returns the containing function."""
        source_file = tmp_path / "findop_fn.casa"
        source = "fn greet {\n    42 print\n}\ngreet"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        result = find_op_at_position(state, 1, 4)

        assert result is not None
        op, func = result
        assert op.kind == OpKind.PUSH_INT
        assert op.value == 42
        assert func is not None
        assert func.name == "greet"

    def test_returns_none_for_empty_position(self, tmp_path):
        """Position with no op returns None."""
        source_file = tmp_path / "empty.casa"
        source_file.write_text("42 print")

        state, _ = run_pipeline(source_file)
        # Column 50 is well past the end of the source
        result = find_op_at_position(state, 0, 50)

        assert result is None


class TestFindContainingFunction:
    """Tests for find_containing_function()."""

    def test_offset_inside_function(self, tmp_path):
        """Offset inside a function body returns that function."""
        source_file = tmp_path / "contain.casa"
        source = "fn greet {\n    42 print\n}\ngreet"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        offset = position_to_offset(source, 1, 4)
        func = find_containing_function(state, offset)

        assert func is not None
        assert func.name == "greet"

    def test_offset_outside_function(self, tmp_path):
        """Offset at top level returns None."""
        source_file = tmp_path / "contain2.casa"
        source = "42 print"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        func = find_containing_function(state, 0)

        assert func is None


class TestFindVariableDefinition:
    """Tests for find_variable_definition()."""

    def test_finds_global_variable(self, tmp_path):
        """A global variable assignment is found."""
        source_file = tmp_path / "vardef.casa"
        source = "42 = count\ncount print"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        location = find_variable_definition("count", None, state)

        assert location is not None

    def test_finds_local_variable(self, tmp_path):
        """A local variable inside a function is found."""
        source_file = tmp_path / "localvar.casa"
        source = "fn foo {\n    42 = count\n    count print\n}\nfoo"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        func = state.functions.get("foo")
        location = find_variable_definition("count", func, state)

        assert location is not None

    def test_returns_none_for_missing_variable(self, tmp_path):
        """Non-existent variable returns None."""
        source_file = tmp_path / "novar.casa"
        source = "42 print"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        location = find_variable_definition("nonexistent", None, state)

        assert location is None


def _make_definition_params(uri, line, col):
    """Create DefinitionParams for testing."""
    return types.DefinitionParams(
        text_document=types.TextDocumentIdentifier(uri=uri),
        position=types.Position(line=line, character=col),
    )


def _make_hover_params(uri, line, col):
    """Create HoverParams for testing."""
    return types.HoverParams(
        text_document=types.TextDocumentIdentifier(uri=uri),
        position=types.Position(line=line, character=col),
    )


def _make_completion_params(uri, line, col):
    """Create CompletionParams for testing."""
    return types.CompletionParams(
        text_document=types.TextDocumentIdentifier(uri=uri),
        position=types.Position(line=line, character=col),
    )


class TestGoToDefinition:
    """Integration tests for go-to-definition functionality."""

    def test_function_definition(self, tmp_path):
        """Go to definition on a function call returns the function's location."""
        source_file = tmp_path / "gotodef_fn.casa"
        source = "fn greet {\n    42 print\n}\ngreet"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_definition(_make_definition_params(uri, 3, 0))
        assert result is not None
        assert result.range.start.line == 0

    def test_variable_definition(self, tmp_path):
        """Go to definition on a variable usage finds the assignment."""
        source_file = tmp_path / "gotodef_var.casa"
        source = "42 = count\ncount print"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_definition(_make_definition_params(uri, 1, 0))
        assert result is not None

    def test_struct_definition(self, tmp_path):
        """Go to definition on a struct constructor returns the struct's location."""
        source_file = tmp_path / "gotodef_struct.casa"
        source = "struct Point {\n    x: int\n    y: int\n}\n1 2 Point"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_definition(_make_definition_params(uri, 4, 4))
        assert result is not None
        assert result.range.start.line == 0

    def test_enum_definition(self, tmp_path):
        """Go to definition on an enum variant returns the enum's location."""
        source_file = tmp_path / "gotodef_enum.casa"
        source = "enum Color { Red Green Blue }\nColor::Red print"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_definition(_make_definition_params(uri, 1, 0))
        assert result is not None
        assert result.range.start.line == 0

    def test_returns_none_for_literal(self, tmp_path):
        """Go to definition on an integer literal returns None."""
        source_file = tmp_path / "gotodef_lit.casa"
        source = "42 print"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_definition(_make_definition_params(uri, 0, 0))
        assert result is None


class TestHover:
    """Integration tests for hover functionality."""

    def test_function_hover(self, tmp_path):
        """Hover over a function call shows its signature."""
        source_file = tmp_path / "hover_fn.casa"
        source = "fn add x:int y:int -> int {\n    x y +\n}\n1 2 add"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_hover(_make_hover_params(uri, 3, 4))
        assert result is not None
        assert "fn add" in result.contents.value
        assert "x:int" in result.contents.value
        assert "y:int" in result.contents.value

    def test_variable_hover(self, tmp_path):
        """Hover over a variable shows its type."""
        source_file = tmp_path / "hover_var.casa"
        source = "42 = count\ncount print"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_hover(_make_hover_params(uri, 1, 0))
        assert result is not None
        assert "count" in result.contents.value

    def test_literal_hover(self, tmp_path):
        """Hover over an integer literal shows its type."""
        source_file = tmp_path / "hover_lit.casa"
        source = "42 print"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_hover(_make_hover_params(uri, 0, 0))
        assert result is not None
        assert "(int) 42" in result.contents.value

    def test_operator_hover(self, tmp_path):
        """Hover over an operator shows its stack effect."""
        source_file = tmp_path / "hover_op.casa"
        source = "1 2 +"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_hover(_make_hover_params(uri, 0, 4))
        assert result is not None
        assert "+: int int -> int" in result.contents.value

    def test_struct_hover(self, tmp_path):
        """Hover over struct constructor shows members."""
        source_file = tmp_path / "hover_struct.casa"
        source = "struct Point {\n    x: int\n    y: int\n}\n1 2 Point"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_hover(_make_hover_params(uri, 4, 4))
        assert result is not None
        assert "struct Point" in result.contents.value
        assert "x: int" in result.contents.value

    def test_string_literal_hover(self, tmp_path):
        """Hover over a string literal shows str type."""
        source_file = tmp_path / "hover_str.casa"
        source = '"hello" print'
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_hover(_make_hover_params(uri, 0, 0))
        assert result is not None
        assert '(str) "hello"' in result.contents.value

    def test_assign_variable_hover(self, tmp_path):
        """Hover over a variable assignment shows variable name and type."""
        source_file = tmp_path / "hover_assign.casa"
        source = "42 = count"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_hover(_make_hover_params(uri, 0, 5))
        assert result is not None
        assert "= count: int" in result.contents.value


class TestCompletion:
    """Integration tests for completion functionality."""

    def test_includes_functions(self, tmp_path):
        """Completion list includes defined functions."""
        source_file = tmp_path / "comp_fn.casa"
        source = "fn greet { 42 print }\ngreet"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_completion(_make_completion_params(uri, 1, 5))
        labels = [item.label for item in result.items]
        assert "greet" in labels

    def test_includes_variables(self, tmp_path):
        """Completion list includes global variables."""
        source_file = tmp_path / "comp_var.casa"
        source = "42 = count\ncount print"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_completion(_make_completion_params(uri, 1, 0))
        labels = [item.label for item in result.items]
        assert "count" in labels

    def test_includes_keywords(self, tmp_path):
        """Completion list includes language keywords."""
        source_file = tmp_path / "comp_kw.casa"
        source_file.write_text("42 print")

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_completion(_make_completion_params(uri, 0, 0))
        labels = [item.label for item in result.items]
        assert "fn" in labels
        assert "if" in labels
        assert "while" in labels

    def test_includes_intrinsics(self, tmp_path):
        """Completion list includes built-in intrinsics."""
        source_file = tmp_path / "comp_intr.casa"
        source_file.write_text("42 print")

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_completion(_make_completion_params(uri, 0, 0))
        labels = [item.label for item in result.items]
        assert "print" in labels
        assert "drop" in labels
        assert "dup" in labels

    def test_excludes_lambda_functions(self, tmp_path):
        """Functions starting with lambda__ are excluded from completions."""
        source_file = tmp_path / "comp_lambda.casa"
        source = "fn apply func:fn[int -> int] x:int -> int {\n    x func exec\n}\n{ 2 * } 5 apply print"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_completion(_make_completion_params(uri, 3, 0))
        labels = [item.label for item in result.items]
        assert "apply" in labels
        lambda_labels = [l for l in labels if l.startswith("lambda__")]
        assert len(lambda_labels) == 0

    def test_includes_structs(self, tmp_path):
        """Completion list includes defined structs."""
        source_file = tmp_path / "comp_struct.casa"
        source = "struct Point {\n    x: int\n    y: int\n}\n1 2 Point"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_completion(_make_completion_params(uri, 4, 0))
        labels = [item.label for item in result.items]
        assert "Point" in labels

    def test_includes_enums(self, tmp_path):
        """Completion list includes defined enum variants."""
        source_file = tmp_path / "comp_enum.casa"
        source = "enum Color { Red Green Blue }\nColor::Red print"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_completion(_make_completion_params(uri, 1, 0))
        labels = [item.label for item in result.items]
        assert "Color::Red" in labels
        assert "Color::Green" in labels
        assert "Color::Blue" in labels

    def test_keywords_without_document_state(self):
        """Keywords and intrinsics are available even without document state."""
        uri = "file:///nonexistent.casa"
        document_states.pop(uri, None)

        result = text_document_completion(_make_completion_params(uri, 0, 0))
        labels = [item.label for item in result.items]
        assert "fn" in labels
        assert "print" in labels

    def test_intrinsics_use_keyword_kind(self, tmp_path):
        """Intrinsics in the completion list use CompletionItemKind.Keyword."""
        source_file = tmp_path / "comp_intr_kind.casa"
        source_file.write_text("42 print")

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_completion(_make_completion_params(uri, 0, 0))
        intrinsic_items = {
            item.label: item
            for item in result.items
            if item.label in ("print", "drop", "dup", "swap", "alloc")
        }
        for label, item in intrinsic_items.items():
            assert (
                item.kind == types.CompletionItemKind.Keyword
            ), f"Intrinsic '{label}' should use Keyword kind, got {item.kind}"

    def test_dot_triggered_method_completion(self, tmp_path):
        """Dot-triggered completion after a struct variable shows methods."""
        source_file = tmp_path / "comp_dot.casa"
        source = (
            "struct Point {\n"
            "    x: int\n"
            "    y: int\n"
            "}\n"
            "impl Point {\n"
            "    fn get_x self:Point -> int { self Point::x }\n"
            "}\n"
            "1 2 Point = p\n"
            "p."
        )
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        params = types.CompletionParams(
            text_document=types.TextDocumentIdentifier(uri=uri),
            position=types.Position(line=8, character=2),
            context=types.CompletionContext(
                trigger_kind=types.CompletionTriggerKind.TriggerCharacter,
                trigger_character=".",
            ),
        )
        result = text_document_completion(params)
        labels = [item.label for item in result.items]
        assert "get_x" in labels

        method_items = [item for item in result.items if item.label == "get_x"]
        assert len(method_items) > 0
        assert method_items[0].kind == types.CompletionItemKind.Method


def _make_references_params(uri, line, col, include_declaration=True):
    """Create ReferenceParams for testing."""
    return types.ReferenceParams(
        text_document=types.TextDocumentIdentifier(uri=uri),
        position=types.Position(line=line, character=col),
        context=types.ReferenceContext(include_declaration=include_declaration),
    )


def _make_rename_params(uri, line, col, new_name):
    """Create RenameParams for testing."""
    return types.RenameParams(
        text_document=types.TextDocumentIdentifier(uri=uri),
        position=types.Position(line=line, character=col),
        new_name=new_name,
    )


def _make_semantic_tokens_params(uri):
    """Create SemanticTokensParams for testing."""
    return types.SemanticTokensParams(
        text_document=types.TextDocumentIdentifier(uri=uri),
    )


class TestHoverStackEffects:
    """Tests for hover on intrinsics/operators with stack effects."""

    def test_drop_hover(self, tmp_path):
        """Hover over drop shows its stack effect."""
        source_file = tmp_path / "hover_drop.casa"
        source = "42 drop"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_hover(_make_hover_params(uri, 0, 3))
        assert result is not None
        assert "drop: any -> None" in result.contents.value

    def test_dup_hover(self, tmp_path):
        """Hover over dup shows its stack effect."""
        source_file = tmp_path / "hover_dup.casa"
        source = "42 dup drop drop"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_hover(_make_hover_params(uri, 0, 3))
        assert result is not None
        assert "dup: any -> any any" in result.contents.value

    def test_swap_hover(self, tmp_path):
        """Hover over swap shows its stack effect."""
        source_file = tmp_path / "hover_swap.casa"
        source = "1 2 swap drop drop"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_hover(_make_hover_params(uri, 0, 4))
        assert result is not None
        assert "swap: any any -> any any" in result.contents.value

    def test_not_hover(self, tmp_path):
        """Hover over ! shows its stack effect."""
        source_file = tmp_path / "hover_not.casa"
        source = "true ! drop"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_hover(_make_hover_params(uri, 0, 5))
        assert result is not None
        assert "!: bool -> bool" in result.contents.value

    def test_eq_hover(self, tmp_path):
        """Hover over == shows its stack effect."""
        source_file = tmp_path / "hover_eq.casa"
        source = "1 2 == drop"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_hover(_make_hover_params(uri, 0, 4))
        assert result is not None
        assert "==: any any -> bool" in result.contents.value

    def test_print_hover(self, tmp_path):
        """Hover over print shows its stack effect."""
        source_file = tmp_path / "hover_print.casa"
        source = "42 print"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_hover(_make_hover_params(uri, 0, 3))
        assert result is not None
        assert "print: any -> None" in result.contents.value

    def test_some_hover(self, tmp_path):
        """Hover over some shows its stack effect."""
        source_file = tmp_path / "hover_some.casa"
        source = "42 some drop"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_hover(_make_hover_params(uri, 0, 3))
        assert result is not None
        assert "some: any -> option[any]" in result.contents.value

    def test_exec_hover(self, tmp_path):
        """Hover over exec shows its stack effect."""
        source_file = tmp_path / "hover_exec.casa"
        source = "{ 42 print } exec"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_hover(_make_hover_params(uri, 0, 13))
        assert result is not None
        assert "exec: fn[sig] -> ..." in result.contents.value


class TestGoToDefinitionQualifiedNames:
    """Tests for go-to-definition on split qualified names."""

    def test_qualified_name_cursor_on_type(self, tmp_path):
        """Cursor on the type part of a qualified name goes to type definition."""
        source_file = tmp_path / "gotodef_qual_type.casa"
        source = (
            "struct Point {\n"
            "    x: int\n"
            "    y: int\n"
            "}\n"
            "impl Point {\n"
            "    fn get_x self:Point -> int { self Point::x }\n"
            "}\n"
            "1 2 Point = p\n"
            "p Point::get_x print"
        )
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        # Cursor on "Point" part of "Point::get_x" (line 8, col 2)
        result = text_document_definition(_make_definition_params(uri, 8, 2))
        assert result is not None
        # Should go to struct definition on line 0
        assert result.range.start.line == 0

    def test_qualified_name_cursor_on_method(self, tmp_path):
        """Cursor on the method part of a qualified name goes to method definition."""
        source_file = tmp_path / "gotodef_qual_method.casa"
        source = (
            "struct Point {\n"
            "    x: int\n"
            "    y: int\n"
            "}\n"
            "impl Point {\n"
            "    fn get_x self:Point -> int { self Point::x }\n"
            "}\n"
            "1 2 Point = p\n"
            "p Point::get_x print"
        )
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        # Cursor on "get_x" part of "Point::get_x" (line 8, col 9)
        result = text_document_definition(_make_definition_params(uri, 8, 9))
        assert result is not None
        # Should go to method definition on line 5
        assert result.range.start.line == 5

    def test_qualified_name_cursor_on_separator_returns_none(self, tmp_path):
        """Cursor on :: separator returns None."""
        source_file = tmp_path / "gotodef_qual_sep.casa"
        source = (
            "struct Point {\n"
            "    x: int\n"
            "    y: int\n"
            "}\n"
            "impl Point {\n"
            "    fn get_x self:Point -> int { self Point::x }\n"
            "}\n"
            "1 2 Point = p\n"
            "p Point::get_x print"
        )
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        # Cursor on "::" of "Point::get_x" (line 8, col 7)
        result = text_document_definition(_make_definition_params(uri, 8, 7))
        assert result is None

    def test_enum_variant_cursor_on_separator_returns_none(self, tmp_path):
        """Cursor on :: in an enum variant returns None."""
        source_file = tmp_path / "gotodef_enum_sep.casa"
        source = "enum Color { Red Green Blue }\nColor::Red print"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        # Cursor on "::" of "Color::Red" (line 1, col 5)
        result = text_document_definition(_make_definition_params(uri, 1, 5))
        assert result is None


class TestHoverQualifiedNames:
    """Tests for hover on split qualified names."""

    def test_hover_qualified_fn_type_part(self, tmp_path):
        """Hovering the type part of Type::method shows struct info."""
        source_file = tmp_path / "hover_qual_type.casa"
        source = (
            "struct Point {\n"
            "    x: int\n"
            "    y: int\n"
            "}\n"
            "impl Point {\n"
            "    fn get_x self:Point -> int { self Point::x }\n"
            "}\n"
            "1 2 Point = p\n"
            "p Point::get_x print"
        )
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        # Hover on "Point" part of "Point::get_x" (line 8, col 2)
        result = text_document_hover(_make_hover_params(uri, 8, 2))
        assert result is not None
        assert "struct Point" in result.contents.value
        assert "x: int" in result.contents.value

    def test_hover_qualified_fn_method_part(self, tmp_path):
        """Hovering the method part of Type::method shows function signature."""
        source_file = tmp_path / "hover_qual_method.casa"
        source = (
            "struct Point {\n"
            "    x: int\n"
            "    y: int\n"
            "}\n"
            "impl Point {\n"
            "    fn get_x self:Point -> int { self Point::x }\n"
            "}\n"
            "1 2 Point = p\n"
            "p Point::get_x print"
        )
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        # Hover on "get_x" part of "Point::get_x" (line 8, col 9)
        result = text_document_hover(_make_hover_params(uri, 8, 9))
        assert result is not None
        assert "fn Point::get_x" in result.contents.value

    def test_hover_qualified_separator_returns_none(self, tmp_path):
        """Hovering on :: separator returns None."""
        source_file = tmp_path / "hover_qual_sep.casa"
        source = (
            "struct Point {\n"
            "    x: int\n"
            "    y: int\n"
            "}\n"
            "impl Point {\n"
            "    fn get_x self:Point -> int { self Point::x }\n"
            "}\n"
            "1 2 Point = p\n"
            "p Point::get_x print"
        )
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        # Hover on "::" of "Point::get_x" (line 8, col 7)
        result = text_document_hover(_make_hover_params(uri, 8, 7))
        assert result is None

    def test_hover_enum_variant_type_part(self, tmp_path):
        """Hovering the enum name part of Enum::Variant shows enum info."""
        source_file = tmp_path / "hover_enum_type.casa"
        source = "enum Color { Red Green Blue }\nColor::Red print"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        # Hover on "Color" part of "Color::Red" (line 1, col 0)
        result = text_document_hover(_make_hover_params(uri, 1, 0))
        assert result is not None
        assert "enum Color" in result.contents.value

    def test_hover_enum_variant_member_part(self, tmp_path):
        """Hovering the variant part of Enum::Variant shows variant info."""
        source_file = tmp_path / "hover_enum_member.casa"
        source = "enum Color { Red Green Blue }\nColor::Red print"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        # Hover on "Red" part of "Color::Red" (line 1, col 7)
        result = text_document_hover(_make_hover_params(uri, 1, 7))
        assert result is not None
        assert "Color::Red" in result.contents.value

    def test_hover_enum_variant_separator_returns_none(self, tmp_path):
        """Hovering on :: in an enum variant returns None."""
        source_file = tmp_path / "hover_enum_sep.casa"
        source = "enum Color { Red Green Blue }\nColor::Red print"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        # Hover on "::" of "Color::Red" (line 1, col 5)
        result = text_document_hover(_make_hover_params(uri, 1, 5))
        assert result is None


class TestFindReferences:
    """Tests for find references functionality."""

    def test_function_references(self, tmp_path):
        """Find all references to a function includes call sites and definition."""
        source_file = tmp_path / "refs_fn.casa"
        source = "fn greet { 42 print }\ngreet\ngreet"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        # Click on the function call on line 1
        result = text_document_references(
            _make_references_params(uri, 1, 0, include_declaration=True)
        )
        assert result is not None
        assert len(result) >= 3  # definition + 2 call sites

    def test_function_references_exclude_declaration(self, tmp_path):
        """Find references without declaration excludes the definition site."""
        source_file = tmp_path / "refs_fn_excl.casa"
        source = "fn greet { 42 print }\ngreet\ngreet"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result_with = text_document_references(
            _make_references_params(uri, 1, 0, include_declaration=True)
        )
        result_without = text_document_references(
            _make_references_params(uri, 1, 0, include_declaration=False)
        )
        assert result_with is not None
        assert result_without is not None
        assert len(result_without) < len(result_with)

    def test_variable_references(self, tmp_path):
        """Find all references to a variable includes assignment and usages."""
        source_file = tmp_path / "refs_var.casa"
        source = "42 = count\ncount print\ncount drop"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        # Click on variable usage on line 1
        result = text_document_references(
            _make_references_params(uri, 1, 0, include_declaration=True)
        )
        assert result is not None
        assert len(result) >= 3  # assignment + 2 usages

    def test_struct_references(self, tmp_path):
        """Find all references to a struct includes definition and constructor usage."""
        source_file = tmp_path / "refs_struct.casa"
        source = (
            "struct Point {\n"
            "    x: int\n"
            "    y: int\n"
            "}\n"
            "1 2 Point\n"
            "3 4 Point"
        )
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        # Click on struct constructor on line 4
        result = text_document_references(
            _make_references_params(uri, 4, 4, include_declaration=True)
        )
        assert result is not None
        assert len(result) >= 3  # definition + 2 constructor usages


class TestRename:
    """Tests for rename functionality."""

    def test_rename_function(self, tmp_path):
        """Renaming a function updates definition and all call sites."""
        source_file = tmp_path / "rename_fn.casa"
        source = "fn greet { 42 print }\ngreet\ngreet"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_rename(_make_rename_params(uri, 1, 0, "hello"))
        assert result is not None
        assert isinstance(result, types.WorkspaceEdit)
        assert uri in result.changes
        edits = result.changes[uri]
        # Should have edits for definition and both call sites
        assert len(edits) >= 3

    def test_rename_variable(self, tmp_path):
        """Renaming a variable updates assignment and all usage sites."""
        source_file = tmp_path / "rename_var.casa"
        source = "42 = count\ncount print\ncount drop"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_rename(_make_rename_params(uri, 1, 0, "total"))
        assert result is not None
        assert isinstance(result, types.WorkspaceEdit)
        assert uri in result.changes
        edits = result.changes[uri]
        # Should have edits for assignment + 2 usages
        assert len(edits) >= 3


class TestSemanticTokens:
    """Tests for semantic tokens functionality."""

    def test_produces_tokens(self, tmp_path):
        """Simple source produces semantic tokens."""
        source_file = tmp_path / "semtok.casa"
        source = "42 print"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_semantic_tokens_full(_make_semantic_tokens_params(uri))
        assert result is not None
        assert len(result.data) > 0

    def test_int_literal_token_type(self, tmp_path):
        """Integer literal gets token type 3 (number)."""
        source_file = tmp_path / "semtok_int.casa"
        source = "42 print"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_semantic_tokens_full(_make_semantic_tokens_params(uri))
        assert result is not None
        # Semantic tokens are encoded as groups of 5 integers:
        # [deltaLine, deltaStart, length, tokenType, tokenModifiers]
        # The first token should be the integer literal 42
        data = result.data
        assert len(data) >= 5
        # Token type for number is 3
        first_token_type = data[3]
        assert first_token_type == 3

    def test_print_intrinsic_token_type(self, tmp_path):
        """Print intrinsic gets token type 9 (macro/intrinsic)."""
        source_file = tmp_path / "semtok_print.casa"
        source = "42 print"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_semantic_tokens_full(_make_semantic_tokens_params(uri))
        assert result is not None
        data = result.data
        # Second token (print) is at index 5-9
        assert len(data) >= 10
        print_token_type = data[8]
        assert print_token_type == 9

    def test_function_call_token_type(self, tmp_path):
        """Function call gets token type 0 (function)."""
        source_file = tmp_path / "semtok_fncall.casa"
        source = "fn greet { 42 print }\ngreet"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_semantic_tokens_full(_make_semantic_tokens_params(uri))
        assert result is not None
        data = result.data
        # Find a token with type 0 (function)
        token_types = [data[i + 3] for i in range(0, len(data), 5)]
        assert 0 in token_types

    def test_keyword_token_type(self, tmp_path):
        """Keyword gets token type 4 (keyword)."""
        source_file = tmp_path / "semtok_kw.casa"
        source = "fn greet { 42 print }\ngreet"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_semantic_tokens_full(_make_semantic_tokens_params(uri))
        assert result is not None
        data = result.data
        # Find a token with type 4 (keyword) for "fn"
        token_types = [data[i + 3] for i in range(0, len(data), 5)]
        assert 4 in token_types

    def test_enum_keyword_token(self, tmp_path):
        """Enum keyword gets token type 4 (keyword)."""
        source_file = tmp_path / "semtok_enum.casa"
        source = "enum Color { Red Green Blue }"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_semantic_tokens_full(_make_semantic_tokens_params(uri))
        assert result is not None
        data = result.data
        # First token should be "enum" keyword (type 4) at col 0, length 4
        assert len(data) >= 5
        assert data[0] == 0  # line 0
        assert data[1] == 0  # col 0
        assert data[2] == 4  # length of "enum"
        assert data[3] == 4  # keyword type

    def test_enum_name_token(self, tmp_path):
        """Enum name gets token type 6 (type)."""
        source_file = tmp_path / "semtok_enum_name.casa"
        source = "enum Color { Red Green Blue }"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_semantic_tokens_full(_make_semantic_tokens_params(uri))
        assert result is not None
        data = result.data
        # Second token should be "Color" name (type 6) at col 5, length 5
        assert len(data) >= 10
        assert data[5] == 0  # same line
        assert data[6] == 5  # col 5
        assert data[7] == 5  # length of "Color"
        assert data[8] == 6  # type token type

    def test_struct_keyword_token(self, tmp_path):
        """Struct keyword gets token type 4 (keyword)."""
        source_file = tmp_path / "semtok_struct.casa"
        source = "struct Point {\n    x: int\n    y: int\n}"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_semantic_tokens_full(_make_semantic_tokens_params(uri))
        assert result is not None
        data = result.data
        # Should have "struct" keyword (type 4) at col 0, length 6
        assert len(data) >= 5
        assert data[0] == 0  # line 0
        assert data[1] == 0  # col 0
        assert data[2] == 6  # length of "struct"
        assert data[3] == 4  # keyword type

    def test_if_then_fi_tokens(self, tmp_path):
        """Conditional keywords (if, then, fi) get token type 4 (keyword)."""
        source_file = tmp_path / "semtok_if.casa"
        source = "true if 42 then 0 fi print"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_semantic_tokens_full(_make_semantic_tokens_params(uri))
        assert result is not None
        data = result.data
        token_types = [data[i + 3] for i in range(0, len(data), 5)]
        # Count keyword tokens (type 4): true, if, then, fi = 4 keyword tokens
        keyword_count = token_types.count(4)
        assert keyword_count >= 3  # if, then, fi at minimum

    def test_while_do_done_tokens(self, tmp_path):
        """Loop keywords (while, do, done) get token type 4 (keyword)."""
        source_file = tmp_path / "semtok_while.casa"
        source = "0 = i\ni 10 < while i 1 + = i do done"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        result = text_document_semantic_tokens_full(_make_semantic_tokens_params(uri))
        assert result is not None
        data = result.data
        token_types = [data[i + 3] for i in range(0, len(data), 5)]
        # while, do, done = 3 keyword tokens (type 4)
        keyword_count = token_types.count(4)
        assert keyword_count >= 3


class TestGoToDefinitionEnumVariant:
    """Tests for go-to-definition navigating to enum variant location."""

    def test_goto_variant_definition(self, tmp_path):
        """Clicking on variant name in Color::Blue goes to Blue in the enum definition."""
        source_file = tmp_path / "goto_variant.casa"
        source = "enum Color { Red Green Blue }\nColor::Blue print"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        # Click on "Blue" in "Color::Blue" (line 1, col 7)
        result = text_document_definition(_make_definition_params(uri, 1, 7))
        assert result is not None
        # Should go to "Blue" in the enum definition (line 0, col 23)
        assert result.range.start.line == 0
        assert result.range.start.character == 23

    def test_goto_enum_name_from_variant(self, tmp_path):
        """Clicking on enum name in Color::Blue goes to Color enum definition."""
        source_file = tmp_path / "goto_enum_name.casa"
        source = "enum Color { Red Green Blue }\nColor::Blue print"
        source_file.write_text(source)

        state, _ = run_pipeline(source_file)
        uri = f"file://{source_file}"
        document_states[uri] = state

        # Click on "Color" in "Color::Blue" (line 1, col 0)
        result = text_document_definition(_make_definition_params(uri, 1, 0))
        assert result is not None
        # Should go to "Color" in the enum definition (line 0, col 5)
        assert result.range.start.line == 0
        assert result.range.start.character == 5
