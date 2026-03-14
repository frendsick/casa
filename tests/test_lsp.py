"""Tests for the Casa Language Server (casa_ls.py)."""

import sys
from pathlib import Path
from unittest.mock import MagicMock

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

import pytest
from lsprotocol import types

from casa.bytecode import _op_label_map
from casa.common import (
    GLOBAL_ENUMS,
    GLOBAL_FUNCTIONS,
    GLOBAL_STRUCTS,
    GLOBAL_TRAITS,
    GLOBAL_VARIABLES,
    INCLUDED_FILES,
    Location,
    Span,
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
    casa_error_to_diagnostic,
    casa_warning_to_diagnostic,
    clear_compilation_state,
    offset_to_range,
    run_diagnostics,
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
