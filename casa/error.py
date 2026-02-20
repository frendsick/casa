"""Custom error reporting for the Casa compiler.

Provides Rust-style diagnostics with source context and multi-error collection.
"""

import sys
from dataclasses import dataclass
from enum import Enum, auto
from pathlib import Path

from casa.common import Location


class ErrorKind(Enum):
    """Categories of compiler errors."""

    SYNTAX = auto()
    UNEXPECTED_TOKEN = auto()
    UNDEFINED_NAME = auto()
    DUPLICATE_NAME = auto()
    INVALID_SCOPE = auto()
    TYPE_MISMATCH = auto()
    STACK_MISMATCH = auto()
    SIGNATURE_MISMATCH = auto()
    INVALID_VARIABLE = auto()
    UNMATCHED_BLOCK = auto()


SOURCE_CACHE: dict[Path, str] = {}


def _display_path(path: Path) -> str:
    """Return a relative path for display if under cwd, otherwise absolute."""
    try:
        return str(path.relative_to(Path.cwd()))
    except ValueError:
        return str(path)


def offset_to_line_col(source: str, offset: int) -> tuple[int, int, str]:
    """Convert a byte offset to 1-based (line, col, source_line)."""
    offset = max(0, min(offset, len(source)))
    line = source[:offset].count("\n") + 1
    line_start = source.rfind("\n", 0, offset) + 1
    col = offset - line_start + 1
    line_end = source.find("\n", offset)
    if line_end == -1:
        line_end = len(source)
    source_line = source[line_start:line_end]
    return line, col, source_line


@dataclass
class CasaError:
    """A single compiler error with optional source location."""

    kind: ErrorKind
    message: str
    location: Location | None = None
    expected: str | None = None
    got: tuple[str, str] | None = None

    def format(self, source_cache: dict[Path, str] | None = None) -> str:
        """Format this error as a Rust-style diagnostic string."""
        if source_cache is None:
            source_cache = SOURCE_CACHE

        header = f"error[{self.kind.name}]: {self.message}"

        if not self.location:
            lines = [header]
            lines.extend(self._format_expected_got())
            return "\n".join(lines)

        source = source_cache.get(self.location.file)
        if not source:
            lines = [header, f"  --> {_display_path(self.location.file)}"]
            lines.extend(self._format_expected_got())
            return "\n".join(lines)

        span_offset = self.location.span.offset
        span_length = self.location.span.length or 1
        start_line, start_col, _ = offset_to_line_col(source, span_offset)
        end_line, end_col, _ = offset_to_line_col(source, span_offset + span_length - 1)

        lines = [header]

        if start_line == end_line:
            source_line = source.split("\n")[start_line - 1]
            line_num_width = len(str(start_line))
            padding = " " * line_num_width
            lines.extend(
                [
                    f"  --> {_display_path(self.location.file)}:{start_line}:{start_col}",
                    f"{padding} |",
                    f"{start_line} | {source_line}",
                    f"{padding} | {' ' * (start_col - 1)}{'^' * span_length}",
                ]
            )
        else:
            lines.append(
                f"  --> {_display_path(self.location.file)}:{start_line}:{start_col}"
            )
            self._format_multiline_span(
                lines, source, start_line, start_col, end_line, end_col
            )
        lines.extend(self._format_expected_got())
        return "\n".join(lines)

    def _format_multiline_span(
        self,
        lines: list[str],
        source: str,
        start_line: int,
        start_col: int,
        end_line: int,
        end_col: int,
    ) -> None:
        """Append multiline source context with per-line carets."""
        source_lines = source.split("\n")
        line_num_width = len(str(end_line))
        padding = " " * line_num_width
        lines.append(f"{padding} |")

        for line_num in range(start_line, end_line + 1):
            src = source_lines[line_num - 1]
            padded = str(line_num).rjust(line_num_width)
            lines.append(f"{padded} | {src}")

            if line_num == start_line:
                caret_start = start_col - 1
                caret_len = len(src) - caret_start
            elif line_num == end_line:
                caret_start = 0
                caret_len = end_col
            else:
                caret_start = 0
                caret_len = len(src)

            if caret_len > 0:
                lines.append(f"{padding} | {' ' * caret_start}{'^' * caret_len}")

    def _format_expected_got(self) -> list[str]:
        """Format expected/got fields as trailing lines."""
        lines: list[str] = []
        if self.expected:
            lines.append(f"  Expected: {self.expected}")
        if self.got:
            label, value = self.got
            lines.append(f"  {label}: {value}")
        return lines


class WarningKind(Enum):
    """Categories of compiler warnings."""

    UNUSED_PARAMETER = auto()


@dataclass
class CasaWarning:
    """A non-fatal compiler warning with optional source location."""

    kind: WarningKind
    message: str
    location: Location | None = None

    def format(self, source_cache: dict[Path, str] | None = None) -> str:
        """Format this warning as a diagnostic string."""
        if source_cache is None:
            source_cache = SOURCE_CACHE

        header = f"warning[{self.kind.name}]: {self.message}"

        if not self.location:
            return header

        source = source_cache.get(self.location.file)
        if not source:
            return f"{header}\n  --> {_display_path(self.location.file)}"

        span_offset = self.location.span.offset
        span_length = self.location.span.length or 1
        line, col, source_line = offset_to_line_col(source, span_offset)
        line_num_width = len(str(line))
        padding = " " * line_num_width

        return "\n".join(
            [
                header,
                f"  --> {_display_path(self.location.file)}:{line}:{col}",
                f"{padding} |",
                f"{line} | {source_line}",
                f"{padding} | {' ' * (col - 1)}{'^' * span_length}",
            ]
        )


WARNINGS: list[CasaWarning] = []


def report_warnings(
    warnings: list[CasaWarning] | None = None,
    source_cache: dict[Path, str] | None = None,
):
    """Print all warnings to stderr."""
    if warnings is None:
        warnings = WARNINGS
    if source_cache is None:
        source_cache = SOURCE_CACHE

    for warning in warnings:
        print(warning.format(source_cache), file=sys.stderr)
        print(file=sys.stderr)


class CasaErrorCollection(Exception):
    """Carries one or more CasaError instances to halt a compilation phase."""

    def __init__(self, errors: list[CasaError] | CasaError):
        if isinstance(errors, CasaError):
            errors = [errors]
        self.errors: list[CasaError] = errors
        super().__init__(self._summary())

    def _summary(self) -> str:
        return f"Found {len(self.errors)} error(s)."


def report_errors(
    errors: list[CasaError],
    source_cache: dict[Path, str] | None = None,
):
    """Print all errors to stderr, then print the count."""
    if source_cache is None:
        source_cache = SOURCE_CACHE

    for error in errors:
        print(error.format(source_cache), file=sys.stderr)
        print(file=sys.stderr)

    print(f"Found {len(errors)} error(s).", file=sys.stderr)
