#!/usr/bin/env python3
"""Casa Language Server - provides diagnostics via LSP."""

import logging
from pathlib import Path
from urllib.parse import unquote, urlparse

from lsprotocol import types
from pygls.lsp.server import LanguageServer

from casa.bytecode import _op_label_map, reset_labels
from casa.common import (
    GLOBAL_ENUMS,
    GLOBAL_FUNCTIONS,
    GLOBAL_STRUCTS,
    GLOBAL_TRAITS,
    GLOBAL_VARIABLES,
    INCLUDED_FILES,
)
from casa.error import (
    SOURCE_CACHE,
    WARNINGS,
    CasaError,
    CasaErrorCollection,
    CasaWarning,
    offset_to_line_col,
)
from casa.lexer import lex_file
from casa.parser import parse_ops, resolve_identifiers
from casa.typechecker import type_check_functions, type_check_ops

logger = logging.getLogger(__name__)

ZERO_RANGE = types.Range(
    start=types.Position(line=0, character=0),
    end=types.Position(line=0, character=0),
)


def clear_compilation_state():
    """Reset all module-level mutable state to a clean slate."""
    GLOBAL_ENUMS.clear()
    GLOBAL_FUNCTIONS.clear()
    GLOBAL_STRUCTS.clear()
    GLOBAL_TRAITS.clear()
    GLOBAL_VARIABLES.clear()
    INCLUDED_FILES.clear()
    SOURCE_CACHE.clear()
    WARNINGS.clear()
    reset_labels()
    _op_label_map.clear()


def offset_to_range(source: str, offset: int, length: int) -> types.Range:
    """Convert Casa byte offset+length to an LSP Range (0-based)."""
    start_line, start_col, _ = offset_to_line_col(source, offset)
    end_offset = offset + max(length, 1)
    end_line, end_col, _ = offset_to_line_col(source, end_offset)
    return types.Range(
        start=types.Position(line=start_line - 1, character=start_col - 1),
        end=types.Position(line=end_line - 1, character=end_col - 1),
    )


def casa_error_to_diagnostic(error: CasaError) -> types.Diagnostic:
    """Convert a CasaError to an LSP Diagnostic."""
    if error.location:
        source = SOURCE_CACHE.get(error.location.file, "")
        diag_range = offset_to_range(
            source, error.location.span.offset, error.location.span.length
        )
    else:
        diag_range = ZERO_RANGE
    message = error.message
    if error.expected:
        message += f"\nExpected: {error.expected}"
    if error.got:
        message += f"\n{error.got_label}: {error.got}"
    return types.Diagnostic(
        range=diag_range,
        message=message,
        severity=types.DiagnosticSeverity.Error,
        source="casa",
    )


def casa_warning_to_diagnostic(warning: CasaWarning) -> types.Diagnostic:
    """Convert a CasaWarning to an LSP Diagnostic."""
    if warning.location:
        source = SOURCE_CACHE.get(warning.location.file, "")
        diag_range = offset_to_range(
            source, warning.location.span.offset, warning.location.span.length
        )
    else:
        diag_range = ZERO_RANGE
    return types.Diagnostic(
        range=diag_range,
        message=warning.message,
        severity=types.DiagnosticSeverity.Warning,
        source="casa",
    )


def run_diagnostics(server: LanguageServer, uri: str):
    """Run the Casa compiler pipeline and publish diagnostics."""
    clear_compilation_state()
    file_path = Path(unquote(urlparse(uri).path)).resolve()
    diagnostics: list[types.Diagnostic] = []
    try:
        tokens = lex_file(file_path)
        ops = resolve_identifiers(parse_ops(tokens))
        type_check_ops(ops)
        type_check_functions(GLOBAL_FUNCTIONS.values())
    except CasaErrorCollection as exc:
        for error in exc.errors:
            if error.location and error.location.file.resolve() != file_path:
                continue
            diagnostics.append(casa_error_to_diagnostic(error))
    except Exception:
        logger.exception("Unexpected error during diagnostics for %s", uri)

    for warning in WARNINGS:
        if warning.location and warning.location.file.resolve() != file_path:
            continue
        diagnostics.append(casa_warning_to_diagnostic(warning))

    server.text_document_publish_diagnostics(
        types.PublishDiagnosticsParams(uri=uri, diagnostics=diagnostics)
    )


server = LanguageServer("casa-language-server", "v0.1.0")


@server.feature(types.TEXT_DOCUMENT_DID_OPEN)
def did_open(params: types.DidOpenTextDocumentParams):
    """Publish diagnostics when a file is opened."""
    run_diagnostics(server, params.text_document.uri)


@server.feature(types.TEXT_DOCUMENT_DID_SAVE)
def did_save(params: types.DidSaveTextDocumentParams):
    """Publish diagnostics when a file is saved."""
    run_diagnostics(server, params.text_document.uri)


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO, format="[%(levelname)s] %(message)s")
    server.start_io()
