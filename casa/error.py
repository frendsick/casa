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

    def format(self, source_cache: dict[Path, str] | None = None) -> str:
        """Format this error as a Rust-style diagnostic string."""
        if source_cache is None:
            source_cache = SOURCE_CACHE

        header = f"error[{self.kind.name}]: {self.message}"

        if not self.location:
            return header

        source = source_cache.get(self.location.file)
        if not source:
            return f"{header}\n  --> {_display_path(self.location.file)}"

        line, col, source_line = offset_to_line_col(source, self.location.span.offset)
        span_length = self.location.span.length or 1
        line_num_width = len(str(line))
        padding = " " * line_num_width

        lines = [
            header,
            f"  --> {_display_path(self.location.file)}:{line}:{col}",
            f"{padding} |",
            f"{line} | {source_line}",
            f"{padding} | {' ' * (col - 1)}{'^' * span_length}",
        ]
        return "\n".join(lines)


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
