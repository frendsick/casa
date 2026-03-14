#!/usr/bin/env python3
"""Casa Language Server - provides diagnostics, go-to-definition, hover, and completion via LSP."""

import logging
from dataclasses import dataclass
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
    CasaEnum,
    Function,
    Intrinsic,
    Keyword,
    Location,
    Op,
    OpKind,
    Struct,
    Variable,
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
from casa.typechecker import OP_STACK_EFFECTS, type_check_functions, type_check_ops

logger = logging.getLogger(__name__)

ZERO_RANGE = types.Range(
    start=types.Position(line=0, character=0),
    end=types.Position(line=0, character=0),
)


@dataclass
class DocumentState:
    """Cached compilation state for a document."""

    ops: list[Op]
    functions: dict[str, Function]
    structs: dict[str, Struct]
    enums: dict[str, CasaEnum]
    variables: dict[str, Variable]
    source: str
    file_path: Path


document_states: dict[str, DocumentState] = {}


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


def run_pipeline(file_path: Path) -> tuple[DocumentState, list[CasaError]]:
    """Run the Casa compiler pipeline and return document state with any errors."""
    clear_compilation_state()
    ops: list[Op] = []
    errors: list[CasaError] = []
    try:
        tokens = lex_file(file_path)
        ops = resolve_identifiers(parse_ops(tokens))
        type_check_ops(ops)
        type_check_functions(GLOBAL_FUNCTIONS.values())
    except CasaErrorCollection as exc:
        errors = exc.errors
    except Exception:
        logger.exception("Unexpected error during pipeline for %s", file_path)

    source = SOURCE_CACHE.get(file_path, "")
    state = DocumentState(
        ops=ops,
        functions=dict(GLOBAL_FUNCTIONS),
        structs=dict(GLOBAL_STRUCTS),
        enums=dict(GLOBAL_ENUMS),
        variables=dict(GLOBAL_VARIABLES),
        source=source,
        file_path=file_path,
    )
    return state, errors


def run_diagnostics(server: LanguageServer, uri: str):
    """Run the Casa compiler pipeline and publish diagnostics."""
    file_path = Path(unquote(urlparse(uri).path)).resolve()
    state, errors = run_pipeline(file_path)
    document_states[uri] = state

    diagnostics: list[types.Diagnostic] = []
    for error in errors:
        if error.location and error.location.file.resolve() != file_path:
            continue
        diagnostics.append(casa_error_to_diagnostic(error))

    for warning in WARNINGS:
        if warning.location and warning.location.file.resolve() != file_path:
            continue
        diagnostics.append(casa_warning_to_diagnostic(warning))

    server.text_document_publish_diagnostics(
        types.PublishDiagnosticsParams(uri=uri, diagnostics=diagnostics)
    )


def position_to_offset(source: str, line: int, col: int) -> int:
    """Convert LSP 0-based line/col to byte offset in source."""
    offset = 0
    for _ in range(line):
        newline = source.find("\n", offset)
        if newline == -1:
            break
        offset = newline + 1
    return offset + col


def _best_op_match(
    ops: list[Op],
    offset: int,
    file_path: Path,
    context: Function | None,
    best: tuple[Op, Function | None] | None,
    best_length: float,
) -> tuple[tuple[Op, Function | None] | None, float]:
    """Find the narrowest op spanning offset, updating the current best."""
    for op in ops:
        if op.location.file.resolve() != file_path:
            continue
        span = op.location.span
        if (
            span.offset <= offset < span.offset + max(span.length, 1)
            and span.length < best_length
        ):
            best = (op, context)
            best_length = span.length
    return best, best_length


def find_op_at_position(
    state: DocumentState, line: int, col: int
) -> tuple[Op, Function | None] | None:
    """Find the op whose span contains the given LSP position."""
    offset = position_to_offset(state.source, line, col)
    best: tuple[Op, Function | None] | None = None
    best_length = float("inf")

    best, best_length = _best_op_match(
        state.ops, offset, state.file_path, None, best, best_length
    )

    for func in state.functions.values():
        if not func.location or func.location.file.resolve() != state.file_path:
            continue
        best, best_length = _best_op_match(
            func.ops, offset, state.file_path, func, best, best_length
        )

    return best


def find_containing_function(state: DocumentState, offset: int) -> Function | None:
    """Find which function the cursor is inside."""
    for func in state.functions.values():
        if not func.location or func.location.file.resolve() != state.file_path:
            continue
        if not func.ops:
            continue
        first_op = func.ops[0]
        last_op = func.ops[-1]
        func_start = first_op.location.span.offset
        func_end = last_op.location.span.offset + last_op.location.span.length
        if func_start <= offset <= func_end:
            return func
    return None


def find_variable_definition(
    name: str, function: Function | None, state: DocumentState
) -> types.Location | None:
    """Find the first ASSIGN_VARIABLE op with the given name."""
    if function:
        for op in function.ops:
            if op.kind == OpKind.ASSIGN_VARIABLE and op.value == name:
                return casa_location_to_lsp(op.location)

    for op in state.ops:
        if op.kind == OpKind.ASSIGN_VARIABLE and op.value == name:
            return casa_location_to_lsp(op.location)

    return None


def casa_location_to_lsp(location: Location) -> types.Location:
    """Convert a Casa Location to an LSP Location."""
    source = SOURCE_CACHE.get(location.file, "")
    lsp_range = offset_to_range(source, location.span.offset, location.span.length)
    uri = f"file://{location.file.resolve()}"
    return types.Location(uri=uri, range=lsp_range)


def format_hover(op: Op, state: DocumentState, func: Function | None) -> str | None:
    """Format hover content for an op."""
    if op.kind in (OpKind.FN_CALL, OpKind.FN_PUSH):
        fn_def = state.functions.get(op.value)
        if fn_def and fn_def.signature:
            params = " ".join(f"{p.name}:{p.typ}" for p in fn_def.signature.parameters)
            returns = " ".join(fn_def.signature.return_types) or "None"
            if params:
                return f"fn {fn_def.name} {params} -> {returns}"
            return f"fn {fn_def.name} -> {returns}"
        if fn_def:
            return f"fn {fn_def.name}"
        return None

    if op.kind in (OpKind.PUSH_VARIABLE, OpKind.PUSH_CAPTURE):
        var_name = op.value
        if func:
            for var in func.variables:
                if var.name == var_name and var.typ:
                    return f"{var_name}: {var.typ}"
            for var in func.captures:
                if var.name == var_name and var.typ:
                    return f"{var_name}: {var.typ}"
        global_var = state.variables.get(var_name)
        if global_var and global_var.typ:
            return f"{var_name}: {global_var.typ}"
        return var_name

    if op.kind == OpKind.STRUCT_NEW:
        struct = op.value
        members = ", ".join(f"{m.name}: {m.typ}" for m in struct.members)
        return f"struct {struct.name} {{ {members} }}"

    if op.kind == OpKind.PUSH_ENUM_VARIANT:
        variant = op.value
        return f"{variant.enum_name}::{variant.variant_name}"

    literal_formats = {
        OpKind.PUSH_INT: "(int) {}",
        OpKind.PUSH_STR: '(str) "{}"',
        OpKind.PUSH_BOOL: "(bool) {}",
        OpKind.PUSH_CHAR: "(char) '{}'",
    }
    if op.kind in literal_formats:
        return literal_formats[op.kind].format(op.value)

    if op.kind == OpKind.ASSIGN_VARIABLE:
        return f"= {op.value}"

    if op.kind in OP_STACK_EFFECTS:
        name, sig = OP_STACK_EFFECTS[op.kind]
        return f"{name}: {sig}"

    return None


server = LanguageServer("casa-language-server", "v0.1.0")


@server.feature(types.TEXT_DOCUMENT_DID_OPEN)
def did_open(params: types.DidOpenTextDocumentParams):
    """Publish diagnostics when a file is opened."""
    run_diagnostics(server, params.text_document.uri)


@server.feature(types.TEXT_DOCUMENT_DID_SAVE)
def did_save(params: types.DidSaveTextDocumentParams):
    """Publish diagnostics when a file is saved."""
    run_diagnostics(server, params.text_document.uri)


@server.feature(types.TEXT_DOCUMENT_DEFINITION)
def text_document_definition(
    params: types.DefinitionParams,
) -> types.Location | None:
    """Go to definition for the symbol at cursor."""
    uri = params.text_document.uri
    state = document_states.get(uri)
    if not state:
        return None

    line = params.position.line
    col = params.position.character
    result = find_op_at_position(state, line, col)
    if not result:
        return None

    op, func = result

    if op.kind in (OpKind.FN_CALL, OpKind.FN_PUSH):
        fn_def = state.functions.get(op.value)
        if fn_def and fn_def.location:
            return casa_location_to_lsp(fn_def.location)

    elif op.kind in (OpKind.PUSH_VARIABLE, OpKind.PUSH_CAPTURE):
        return find_variable_definition(op.value, func, state)

    elif op.kind == OpKind.STRUCT_NEW:
        struct = op.value
        return casa_location_to_lsp(struct.location)

    elif op.kind == OpKind.PUSH_ENUM_VARIANT:
        variant = op.value
        if variant.enum_name and variant.enum_name in state.enums:
            return casa_location_to_lsp(state.enums[variant.enum_name].location)

    elif op.kind == OpKind.TYPE_CAST:
        cast_type = op.value
        if cast_type in state.structs:
            return casa_location_to_lsp(state.structs[cast_type].location)
        if cast_type in state.enums:
            return casa_location_to_lsp(state.enums[cast_type].location)

    return None


@server.feature(types.TEXT_DOCUMENT_HOVER)
def text_document_hover(params: types.HoverParams) -> types.Hover | None:
    """Show type/signature info on hover."""
    uri = params.text_document.uri
    state = document_states.get(uri)
    if not state:
        return None

    line = params.position.line
    col = params.position.character
    result = find_op_at_position(state, line, col)
    if not result:
        return None

    op, func = result
    content = format_hover(op, state, func)
    if not content:
        return None

    hover_range = offset_to_range(
        state.source, op.location.span.offset, op.location.span.length
    )
    return types.Hover(
        contents=types.MarkupContent(
            kind=types.MarkupKind.Markdown,
            value=f"```casa\n{content}\n```",
        ),
        range=hover_range,
    )


@server.feature(types.TEXT_DOCUMENT_COMPLETION)
def text_document_completion(
    params: types.CompletionParams,
) -> types.CompletionList:
    """Provide completion items."""
    uri = params.text_document.uri
    state = document_states.get(uri)
    items: list[types.CompletionItem] = []

    if state:
        # Functions (skip internal names)
        for name, fn_def in state.functions.items():
            if name.startswith("lambda__") or name.startswith("__"):
                continue
            detail = repr(fn_def.signature) if fn_def.signature else None
            items.append(
                types.CompletionItem(
                    label=name,
                    kind=types.CompletionItemKind.Function,
                    detail=detail,
                )
            )

        # Structs
        for name, struct in state.structs.items():
            members = ", ".join(f"{m.name}: {m.typ}" for m in struct.members)
            items.append(
                types.CompletionItem(
                    label=name,
                    kind=types.CompletionItemKind.Struct,
                    detail=members,
                )
            )

        # Enum variants
        for name, enum in state.enums.items():
            for variant in enum.variants:
                items.append(
                    types.CompletionItem(
                        label=f"{name}::{variant}",
                        kind=types.CompletionItemKind.EnumMember,
                    )
                )

        # Local variables from containing function
        offset = position_to_offset(
            state.source, params.position.line, params.position.character
        )
        containing_fn = find_containing_function(state, offset)
        if containing_fn:
            for var in containing_fn.variables:
                items.append(
                    types.CompletionItem(
                        label=var.name,
                        kind=types.CompletionItemKind.Variable,
                        detail=str(var.typ) if var.typ else None,
                    )
                )

        # Global variables
        for name, var in state.variables.items():
            items.append(
                types.CompletionItem(
                    label=name,
                    kind=types.CompletionItemKind.Variable,
                    detail=str(var.typ) if var.typ else None,
                )
            )

    # Keywords (always available)
    for kw in Keyword:
        items.append(
            types.CompletionItem(
                label=kw.name.lower(),
                kind=types.CompletionItemKind.Keyword,
            )
        )

    # Intrinsics (always available)
    for intr in Intrinsic:
        items.append(
            types.CompletionItem(
                label=intr.name.lower(),
                kind=types.CompletionItemKind.Function,
            )
        )

    return types.CompletionList(is_incomplete=False, items=items)


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO, format="[%(levelname)s] %(message)s")
    server.start_io()
