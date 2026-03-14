#!/usr/bin/env python3
"""Casa Language Server - LSP server with diagnostics, go-to-definition, hover, completion, find references, rename, and semantic tokens."""

import logging
from dataclasses import dataclass
from enum import Enum, auto
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
    extract_generic_base,
)
from casa.error import (
    SOURCE_CACHE,
    WARNINGS,
    CasaError,
    CasaErrorCollection,
    CasaWarning,
    offset_to_line_col,
)
from casa.lexer import lex_file, lex_source
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


def run_pipeline(
    file_path: Path, source: str | None = None
) -> tuple[DocumentState, list[CasaError]]:
    """Run the Casa compiler pipeline and return document state with any errors."""
    clear_compilation_state()
    ops: list[Op] = []
    errors: list[CasaError] = []

    # Run each pipeline stage independently to preserve partial state on errors
    try:
        if source is not None:
            tokens = lex_source(source, file_path)
        else:
            tokens = lex_file(file_path)
    except CasaErrorCollection as exc:
        errors = exc.errors
        tokens = None
    except Exception:
        logger.exception("Unexpected error during lex for %s", file_path)
        tokens = None

    if tokens is not None:
        try:
            parse_errors: list[CasaError] = []
            parsed = parse_ops(tokens, resilient=True, errors_out=parse_errors)
            errors.extend(parse_errors)
            ops = resolve_identifiers(parsed)
        except CasaErrorCollection as exc:
            errors.extend(exc.errors)
        except Exception:
            logger.exception("Unexpected error during parse for %s", file_path)

    if ops:
        try:
            type_check_ops(ops)
            type_check_functions(GLOBAL_FUNCTIONS.values())
        except CasaErrorCollection as exc:
            errors.extend(exc.errors)
        except Exception:
            logger.exception("Unexpected error during type check for %s", file_path)

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


def run_diagnostics(
    server: LanguageServer,
    uri: str,
    source: str | None = None,
):
    """Run the Casa compiler pipeline and publish diagnostics."""
    file_path = Path(unquote(urlparse(uri).path)).resolve()
    state, errors = run_pipeline(file_path, source=source)
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


def _extract_word_before(source: str, offset: int) -> str:
    """Extract the identifier immediately before the given offset."""
    end = offset
    start = end - 1
    while start >= 0 and (source[start].isalnum() or source[start] == "_"):
        start -= 1
    return source[start + 1 : end]


def _extract_chain_before(source: str, offset: int) -> list[str]:
    """Extract a dotted chain before offset, e.g. 'test.hash' -> ['test', 'hash']."""
    parts: list[str] = []
    pos = offset
    while pos > 0:
        word = _extract_word_before(source, pos)
        if not word:
            break
        parts.append(word)
        pos -= len(word)
        if pos > 0 and source[pos - 1] == ".":
            pos -= 1
        else:
            break
    parts.reverse()
    return parts


def _resolve_chain_type(
    parts: list[str], state: DocumentState, offset: int
) -> str | None:
    """Resolve the type at the end of a method chain like ['test', 'hash']."""
    if not parts:
        return None
    containing_fn = find_containing_function(state, offset)
    current_type = resolve_variable_type(parts[0], containing_fn, state)
    if not current_type:
        current_type = _infer_literal_type(parts[0])
    if not current_type:
        return None
    for method_name in parts[1:]:
        base_type = extract_generic_base(current_type) or current_type
        fn_def = state.functions.get(f"{base_type}::{method_name}")
        if not fn_def or not fn_def.signature or not fn_def.signature.return_types:
            return None
        current_type = fn_def.signature.return_types[0]
    return current_type


def _handle_triggered_completion(
    state: DocumentState, offset: int
) -> list[types.CompletionItem]:
    """Handle dot and :: triggered completion."""
    source = state.source
    pos = offset - 1
    if pos < 0:
        return []

    # :: triggered: suggest enum variants and methods for the type
    if pos >= 1 and source[pos - 1 : pos + 1] == "::":
        type_name = _extract_word_before(source, pos - 1)
        if type_name:
            return _members_for_qualified(type_name, state)
        return []

    # . triggered: suggest methods for the receiver type
    if source[pos] == ".":
        chain = _extract_chain_before(source, pos)
        receiver_type = _resolve_chain_type(chain, state, offset)
        if receiver_type:
            return _methods_for_type(receiver_type, state)

    return []


def _infer_literal_type(name: str) -> str | None:
    """Infer the type of a literal value from its textual representation."""
    if name and name.isdigit():
        return "int"
    if name in ("true", "false"):
        return "bool"
    return None


def resolve_variable_type(
    name: str, func: Function | None, state: DocumentState
) -> str | None:
    """Look up a variable's type from the containing function or global scope."""
    if func:
        for var in func.variables:
            if var.name == name and var.typ:
                return var.typ
        for var in func.captures:
            if var.name == name and var.typ:
                return var.typ
    global_var = state.variables.get(name)
    if global_var and global_var.typ:
        return global_var.typ
    return None


def _methods_for_type(
    receiver_type: str, state: DocumentState
) -> list[types.CompletionItem]:
    """Return completion items for methods available on the given type."""
    base_type = extract_generic_base(receiver_type) or receiver_type
    prefix = f"{base_type}::"
    items: list[types.CompletionItem] = []
    for name, fn_def in state.functions.items():
        if not name.startswith(prefix):
            continue
        method_name = name[len(prefix) :]
        detail = repr(fn_def.signature) if fn_def.signature else None
        items.append(
            types.CompletionItem(
                label=method_name,
                kind=types.CompletionItemKind.Method,
                detail=detail,
            )
        )
    return items


def _members_for_qualified(
    type_name: str, state: DocumentState
) -> list[types.CompletionItem]:
    """Return completion items for Type:: (methods and enum variants)."""
    items: list[types.CompletionItem] = []
    enum = state.enums.get(type_name)
    if enum:
        for variant in enum.variants:
            items.append(
                types.CompletionItem(
                    label=variant,
                    kind=types.CompletionItemKind.EnumMember,
                )
            )
    items.extend(_methods_for_type(type_name, state))
    return items


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
    name: str,
    function: Function | None,
    state: DocumentState,
    usage_offset: int = 0,
) -> types.Location | None:
    """Find the most recent ASSIGN_VARIABLE op before usage_offset."""
    best: Op | None = None

    if function:
        for op in function.ops:
            if op.kind == OpKind.ASSIGN_VARIABLE and op.value == name:
                if op.location.span.offset < usage_offset:
                    best = op
                elif best is None:
                    best = op

    if best is None:
        for op in state.ops:
            if op.kind == OpKind.ASSIGN_VARIABLE and op.value == name:
                if op.location.span.offset < usage_offset:
                    best = op
                elif best is None:
                    best = op

    if best is not None:
        return casa_location_to_lsp(best.location)
    return None


def casa_location_to_lsp(location: Location) -> types.Location:
    """Convert a Casa Location to an LSP Location."""
    source = SOURCE_CACHE.get(location.file, "")
    lsp_range = offset_to_range(source, location.span.offset, location.span.length)
    uri = f"file://{location.file.resolve()}"
    return types.Location(uri=uri, range=lsp_range)


SEPARATOR = "::"
SEPARATOR_LEN = len(SEPARATOR)


class QualifiedPart(Enum):
    """Which part of a qualified name (Type::member) the cursor is on."""

    TYPE = auto()
    SEPARATOR = auto()
    MEMBER = auto()


def _qualified_name_part(
    source: str, op: Op, cursor_offset: int
) -> QualifiedPart | None:
    """Determine which part of a qualified name (Type::member) the cursor is on.

    Returns None if the op source text does not contain "::".
    """
    op_start = op.location.span.offset
    op_text = source[op_start : op_start + op.location.span.length]
    sep_pos = op_text.find(SEPARATOR)
    if sep_pos < 0:
        return None
    relative = cursor_offset - op_start
    if relative < sep_pos:
        return QualifiedPart.TYPE
    if relative < sep_pos + SEPARATOR_LEN:
        return QualifiedPart.SEPARATOR
    return QualifiedPart.MEMBER


def _qualified_type_range(source: str, op: Op) -> types.Range:
    """Return the LSP range covering just the type part before ::."""
    op_start = op.location.span.offset
    op_text = source[op_start : op_start + op.location.span.length]
    sep_pos = op_text.find(SEPARATOR)
    return offset_to_range(source, op_start, sep_pos)


def _qualified_member_range(source: str, op: Op) -> types.Range:
    """Return the LSP range covering just the member part after ::."""
    op_start = op.location.span.offset
    op_text = source[op_start : op_start + op.location.span.length]
    sep_pos = op_text.find(SEPARATOR)
    member_offset = op_start + sep_pos + SEPARATOR_LEN
    member_len = op.location.span.length - sep_pos - SEPARATOR_LEN
    return offset_to_range(source, member_offset, member_len)


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
        var_type = resolve_variable_type(var_name, func, state)
        if var_type:
            return f"{var_name}: {var_type}"
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
        var_name = op.value
        var_type = resolve_variable_type(var_name, func, state)
        if var_type:
            return f"= {var_name}: {var_type}"
        return f"= {var_name}"

    if op.kind in OP_STACK_EFFECTS:
        name, sig = OP_STACK_EFFECTS[op.kind]
        return f"{name}: {sig}"

    return None


def _iter_all_ops(state: DocumentState):
    """Yield all ops from global scope and all functions."""
    yield from state.ops
    for search_func in state.functions.values():
        yield from search_func.ops


def find_all_references(
    state: DocumentState, op: Op, func: Function | None, include_declaration: bool
) -> list[types.Location]:
    """Find all references to the symbol at the given op."""
    results: list[types.Location] = []

    if op.kind in (OpKind.FN_CALL, OpKind.FN_PUSH):
        name = op.value
        fn_def = state.functions.get(name)
        if fn_def and fn_def.location and include_declaration:
            results.append(casa_location_to_lsp(fn_def.location))
        for search_op in _iter_all_ops(state):
            if (
                search_op.kind in (OpKind.FN_CALL, OpKind.FN_PUSH)
                and search_op.value == name
            ):
                results.append(casa_location_to_lsp(search_op.location))

    elif op.kind in (
        OpKind.PUSH_VARIABLE,
        OpKind.PUSH_CAPTURE,
        OpKind.ASSIGN_VARIABLE,
        OpKind.ASSIGN_INCREMENT,
        OpKind.ASSIGN_DECREMENT,
    ):
        name = op.value
        var_kinds = {
            OpKind.PUSH_VARIABLE,
            OpKind.PUSH_CAPTURE,
            OpKind.ASSIGN_VARIABLE,
            OpKind.ASSIGN_INCREMENT,
            OpKind.ASSIGN_DECREMENT,
        }
        is_local = func is not None and any(v.name == name for v in func.variables)
        if is_local and func:
            for search_op in func.ops:
                if search_op.kind in var_kinds and search_op.value == name:
                    results.append(casa_location_to_lsp(search_op.location))
        else:
            for search_op in _iter_all_ops(state):
                if search_op.kind in var_kinds and search_op.value == name:
                    results.append(casa_location_to_lsp(search_op.location))

    elif op.kind == OpKind.STRUCT_NEW:
        struct = op.value
        name = struct.name
        if include_declaration:
            results.append(casa_location_to_lsp(struct.location))
        for search_op in _iter_all_ops(state):
            if search_op.kind == OpKind.STRUCT_NEW and search_op.value.name == name:
                results.append(casa_location_to_lsp(search_op.location))
            elif search_op.kind == OpKind.TYPE_CAST and search_op.value == name:
                results.append(casa_location_to_lsp(search_op.location))

    elif op.kind == OpKind.PUSH_ENUM_VARIANT:
        variant = op.value
        name = variant.enum_name
        if include_declaration and name in state.enums:
            results.append(casa_location_to_lsp(state.enums[name].location))
        for search_op in _iter_all_ops(state):
            if (
                search_op.kind == OpKind.PUSH_ENUM_VARIANT
                and search_op.value.enum_name == name
            ):
                results.append(casa_location_to_lsp(search_op.location))

    return results


SEMANTIC_TOKEN_TYPES = [
    "function",
    "variable",
    "string",
    "number",
    "keyword",
    "operator",
    "type",
    "enumMember",
    "struct",
    "macro",
]

SEMANTIC_TOKEN_LEGEND = types.SemanticTokensLegend(
    token_types=SEMANTIC_TOKEN_TYPES,
    token_modifiers=[],
)

OP_TOKEN_TYPE: dict[OpKind, int] = {
    # function (0)
    OpKind.FN_CALL: 0,
    OpKind.FN_PUSH: 0,
    # variable (1)
    OpKind.PUSH_VARIABLE: 1,
    OpKind.PUSH_CAPTURE: 1,
    OpKind.ASSIGN_VARIABLE: 1,
    OpKind.ASSIGN_INCREMENT: 1,
    OpKind.ASSIGN_DECREMENT: 1,
    # string (2)
    OpKind.PUSH_STR: 2,
    # number (3)
    OpKind.PUSH_INT: 3,
    OpKind.PUSH_CHAR: 3,
    # keyword (4)
    OpKind.PUSH_BOOL: 4,
    OpKind.PUSH_NONE: 4,
    OpKind.SOME: 4,
    OpKind.FN_EXEC: 4,
    OpKind.IF_START: 4,
    OpKind.IF_CONDITION: 4,
    OpKind.IF_ELIF: 4,
    OpKind.IF_ELSE: 4,
    OpKind.IF_END: 4,
    OpKind.WHILE_START: 4,
    OpKind.WHILE_CONDITION: 4,
    OpKind.WHILE_END: 4,
    OpKind.WHILE_BREAK: 4,
    OpKind.WHILE_CONTINUE: 4,
    OpKind.MATCH_START: 4,
    OpKind.MATCH_ARM: 4,
    OpKind.MATCH_END: 4,
    OpKind.FN_RETURN: 4,
    # operator (5)
    OpKind.ADD: 5,
    OpKind.SUB: 5,
    OpKind.MUL: 5,
    OpKind.DIV: 5,
    OpKind.MOD: 5,
    OpKind.SHL: 5,
    OpKind.SHR: 5,
    OpKind.BIT_AND: 5,
    OpKind.BIT_OR: 5,
    OpKind.BIT_XOR: 5,
    OpKind.BIT_NOT: 5,
    OpKind.AND: 5,
    OpKind.OR: 5,
    OpKind.NOT: 5,
    OpKind.EQ: 5,
    OpKind.NE: 5,
    OpKind.LT: 5,
    OpKind.LE: 5,
    OpKind.GT: 5,
    OpKind.GE: 5,
    # type (6)
    OpKind.TYPE_CAST: 6,
    # enumMember (7)
    OpKind.PUSH_ENUM_VARIANT: 7,
    # struct (8)
    OpKind.STRUCT_NEW: 8,
    # macro/intrinsic (9)
    OpKind.DROP: 9,
    OpKind.DUP: 9,
    OpKind.SWAP: 9,
    OpKind.OVER: 9,
    OpKind.ROT: 9,
    OpKind.PRINT: 9,
    OpKind.PRINT_INT: 9,
    OpKind.PRINT_STR: 9,
    OpKind.PRINT_BOOL: 9,
    OpKind.PRINT_CHAR: 9,
    OpKind.PRINT_CSTR: 9,
    OpKind.HEAP_ALLOC: 9,
    OpKind.LOAD8: 9,
    OpKind.LOAD16: 9,
    OpKind.LOAD32: 9,
    OpKind.LOAD64: 9,
    OpKind.STORE8: 9,
    OpKind.STORE16: 9,
    OpKind.STORE32: 9,
    OpKind.STORE64: 9,
    OpKind.SYSCALL0: 9,
    OpKind.SYSCALL1: 9,
    OpKind.SYSCALL2: 9,
    OpKind.SYSCALL3: 9,
    OpKind.SYSCALL4: 9,
    OpKind.SYSCALL5: 9,
    OpKind.SYSCALL6: 9,
    OpKind.TYPEOF: 9,
}


server = LanguageServer("casa-language-server", "v0.1.0")


@server.feature(types.TEXT_DOCUMENT_DID_OPEN)
def did_open(params: types.DidOpenTextDocumentParams):
    """Publish diagnostics when a file is opened."""
    run_diagnostics(server, params.text_document.uri)


@server.feature(types.TEXT_DOCUMENT_DID_CHANGE)
def did_change(params: types.DidChangeTextDocumentParams):
    """Publish diagnostics when file content changes."""
    uri = params.text_document.uri
    document = server.workspace.get_text_document(uri)
    run_diagnostics(server, uri, source=document.source)


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
    cursor_offset = position_to_offset(state.source, line, col)

    # Qualified names: cursor on :: is a dead zone
    part = _qualified_name_part(state.source, op, cursor_offset)
    if part == QualifiedPart.SEPARATOR:
        return None

    if op.kind in (OpKind.FN_CALL, OpKind.FN_PUSH):
        if part == QualifiedPart.TYPE:
            type_name = op.value.split(SEPARATOR)[0]
            if type_name in state.structs:
                return casa_location_to_lsp(state.structs[type_name].location)
            if type_name in state.enums:
                return casa_location_to_lsp(state.enums[type_name].location)
            return None
        fn_def = state.functions.get(op.value)
        if fn_def and fn_def.location:
            return casa_location_to_lsp(fn_def.location)

    elif op.kind in (OpKind.PUSH_VARIABLE, OpKind.PUSH_CAPTURE):
        return find_variable_definition(op.value, func, state, op.location.span.offset)

    elif op.kind == OpKind.STRUCT_NEW:
        struct = op.value
        return casa_location_to_lsp(struct.location)

    elif op.kind == OpKind.PUSH_ENUM_VARIANT:
        variant = op.value
        if variant.enum_name and variant.enum_name in state.enums:
            enum = state.enums[variant.enum_name]
            if part == QualifiedPart.MEMBER and enum.variant_locations:
                variant_loc = enum.variant_locations.get(variant.variant_name)
                if variant_loc:
                    return casa_location_to_lsp(variant_loc)
            return casa_location_to_lsp(enum.location)

    elif op.kind == OpKind.TYPE_CAST:
        cast_type = op.value
        if cast_type in state.structs:
            return casa_location_to_lsp(state.structs[cast_type].location)
        if cast_type in state.enums:
            return casa_location_to_lsp(state.enums[cast_type].location)

    return None


def _hover_qualified_fn(
    op: Op, state: DocumentState, part: QualifiedPart
) -> tuple[str, types.Range] | None:
    """Return hover content and range for a qualified function call (Type::method)."""
    type_name = op.value.split(SEPARATOR)[0]
    if part == QualifiedPart.TYPE:
        hover_range = _qualified_type_range(state.source, op)
        struct = state.structs.get(type_name)
        if struct:
            members = ", ".join(f"{m.name}: {m.typ}" for m in struct.members)
            return f"struct {struct.name} {{ {members} }}", hover_range
        enum = state.enums.get(type_name)
        if enum:
            variants = ", ".join(enum.variants)
            return f"enum {enum.name} {{ {variants} }}", hover_range
        return None
    hover_range = _qualified_member_range(state.source, op)
    fn_def = state.functions.get(op.value)
    if fn_def and fn_def.signature:
        params = " ".join(f"{p.name}:{p.typ}" for p in fn_def.signature.parameters)
        returns = " ".join(fn_def.signature.return_types) or "None"
        if params:
            return f"fn {fn_def.name} {params} -> {returns}", hover_range
        return f"fn {fn_def.name} -> {returns}", hover_range
    if fn_def:
        return f"fn {fn_def.name}", hover_range
    return None


def _hover_qualified_enum(
    op: Op, state: DocumentState, part: QualifiedPart
) -> tuple[str, types.Range] | None:
    """Return hover content and range for a qualified enum variant (Enum::Variant)."""
    variant = op.value
    if part == QualifiedPart.TYPE:
        hover_range = _qualified_type_range(state.source, op)
        enum = state.enums.get(variant.enum_name)
        if enum:
            variants = ", ".join(enum.variants)
            return f"enum {enum.name} {{ {variants} }}", hover_range
        return None
    hover_range = _qualified_member_range(state.source, op)
    return f"{variant.enum_name}::{variant.variant_name}", hover_range


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
    cursor_offset = position_to_offset(state.source, line, col)

    # Handle qualified names: cursor on :: is a dead zone
    part = _qualified_name_part(state.source, op, cursor_offset)
    if part == QualifiedPart.SEPARATOR:
        return None

    if part and op.kind in (OpKind.FN_CALL, OpKind.FN_PUSH):
        hover_result = _hover_qualified_fn(op, state, part)
        if not hover_result:
            return None
        content, hover_range = hover_result
        return types.Hover(
            contents=types.MarkupContent(
                kind=types.MarkupKind.Markdown,
                value=f"```casa\n{content}\n```",
            ),
            range=hover_range,
        )

    if part and op.kind == OpKind.PUSH_ENUM_VARIANT:
        hover_result = _hover_qualified_enum(op, state, part)
        if not hover_result:
            return None
        content, hover_range = hover_result
        return types.Hover(
            contents=types.MarkupContent(
                kind=types.MarkupKind.Markdown,
                value=f"```casa\n{content}\n```",
            ),
            range=hover_range,
        )

    hover_content = format_hover(op, state, func)
    if not hover_content:
        return None

    hover_range = offset_to_range(
        state.source, op.location.span.offset, op.location.span.length
    )
    return types.Hover(
        contents=types.MarkupContent(
            kind=types.MarkupKind.Markdown,
            value=f"```casa\n{hover_content}\n```",
        ),
        range=hover_range,
    )


@server.feature(
    types.TEXT_DOCUMENT_COMPLETION,
    types.CompletionOptions(trigger_characters=[".", ":"]),
)
def text_document_completion(
    params: types.CompletionParams,
) -> types.CompletionList:
    """Provide completion items."""
    uri = params.text_document.uri
    state = document_states.get(uri)
    items: list[types.CompletionItem] = []

    if state and params.context and params.context.trigger_character in (".", ":"):
        offset = position_to_offset(
            state.source, params.position.line, params.position.character
        )
        triggered_items = _handle_triggered_completion(state, offset)
        if triggered_items:
            return types.CompletionList(is_incomplete=False, items=triggered_items)
        # Don't fall through to general list on trigger characters
        return types.CompletionList(is_incomplete=False, items=[])

    if state:
        # Functions (skip internal names and qualified methods)
        for name, fn_def in state.functions.items():
            if name.startswith("lambda__") or name.startswith("__"):
                continue
            if "::" in name:
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

        # Enums (type names only, variants available via ::)
        for name in state.enums:
            items.append(
                types.CompletionItem(
                    label=name,
                    kind=types.CompletionItemKind.Enum,
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
                kind=types.CompletionItemKind.Keyword,
            )
        )

    return types.CompletionList(is_incomplete=False, items=items)


@server.feature(types.TEXT_DOCUMENT_REFERENCES)
def text_document_references(
    params: types.ReferenceParams,
) -> list[types.Location] | None:
    """Find all references to the symbol at cursor."""
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
    include_declaration = params.context.include_declaration
    refs = find_all_references(state, op, func, include_declaration)
    return refs if refs else None


@server.feature(types.TEXT_DOCUMENT_RENAME)
def text_document_rename(params: types.RenameParams) -> types.WorkspaceEdit | None:
    """Rename the symbol at cursor."""
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
    refs = find_all_references(state, op, func, include_declaration=True)
    if not refs:
        return None

    new_name = params.new_name
    old_name = op.value
    changes: dict[str, list[types.TextEdit]] = {}

    for ref in refs:
        ref_uri = ref.uri
        if ref_uri not in changes:
            changes[ref_uri] = []
        edit_range = ref.range
        # For assignment ops, the span includes the operator prefix (= / += / -=).
        # Adjust the range to cover only the variable name at the end.
        name_len = len(old_name)
        span_len = ref.range.end.character - ref.range.start.character
        if span_len > name_len:
            edit_range = types.Range(
                start=types.Position(
                    line=ref.range.end.line,
                    character=ref.range.end.character - name_len,
                ),
                end=ref.range.end,
            )
        changes[ref_uri].append(types.TextEdit(range=edit_range, new_text=new_name))

    return types.WorkspaceEdit(changes=changes)


@server.feature(
    types.TEXT_DOCUMENT_SEMANTIC_TOKENS_FULL,
    types.SemanticTokensRegistrationOptions(legend=SEMANTIC_TOKEN_LEGEND, full=True),
)
def text_document_semantic_tokens_full(
    params: types.SemanticTokensParams,
) -> types.SemanticTokens:
    """Provide semantic tokens for the document."""
    uri = params.text_document.uri
    state = document_states.get(uri)
    if not state:
        return types.SemanticTokens(data=[])

    tokens: list[tuple[int, int, int, int, int]] = []

    # Token types for the two parts of a qualified name
    qualified_type_tokens = {
        OpKind.FN_CALL: 6,  # type
        OpKind.FN_PUSH: 6,  # type
        OpKind.PUSH_ENUM_VARIANT: 6,  # type
    }
    qualified_member_tokens = {
        OpKind.FN_CALL: 0,  # function
        OpKind.FN_PUSH: 0,  # function
        OpKind.PUSH_ENUM_VARIANT: 7,  # enumMember
    }

    def _append_token(offset: int, length: int, token_type: int):
        if length <= 0:
            return
        line, col, _ = offset_to_line_col(state.source, offset)
        tokens.append((line - 1, col - 1, length, token_type, 0))

    def collect_tokens(ops: list[Op]):
        for op in ops:
            if op.location.file.resolve() != state.file_path:
                continue
            token_type = OP_TOKEN_TYPE.get(op.kind)
            if token_type is None:
                continue
            op_start = op.location.span.offset
            op_len = op.location.span.length
            if op_len <= 0:
                continue
            # Split qualified names into type and member tokens
            if op.kind in qualified_type_tokens:
                op_text = state.source[op_start : op_start + op_len]
                sep_pos = op_text.find(SEPARATOR)
                if sep_pos >= 0:
                    _append_token(op_start, sep_pos, qualified_type_tokens[op.kind])
                    member_offset = op_start + sep_pos + SEPARATOR_LEN
                    member_len = op_len - sep_pos - SEPARATOR_LEN
                    _append_token(
                        member_offset, member_len, qualified_member_tokens[op.kind]
                    )
                    continue
            _append_token(op_start, op_len, token_type)

    collect_tokens(state.ops)

    for func in state.functions.values():
        collect_tokens(func.ops)

    def _scan_keyword_before(name_offset: int, keyword: str):
        """Scan backwards past whitespace to find a keyword token before name_offset."""
        scan_pos = name_offset - 1
        while scan_pos >= 0 and state.source[scan_pos] in (" ", "\t", "\n"):
            scan_pos -= 1
        kw_start = scan_pos - len(keyword) + 1
        if kw_start >= 0 and state.source[kw_start : scan_pos + 1] == keyword:
            kw_line, kw_col, _ = offset_to_line_col(state.source, kw_start)
            tokens.append((kw_line - 1, kw_col - 1, len(keyword), 4, 0))

    # Add function definition names and fn keyword
    for func in state.functions.values():
        if not func.location or func.location.file.resolve() != state.file_path:
            continue
        if func.name.startswith("lambda__") or func.name.startswith("__"):
            continue
        name_offset = func.location.span.offset
        length = func.location.span.length
        if length > 0:
            start_line, start_col, _ = offset_to_line_col(state.source, name_offset)
            tokens.append((start_line - 1, start_col - 1, length, 0, 0))
        _scan_keyword_before(name_offset, "fn")

    # Add enum definition names and enum keyword
    for enum in state.enums.values():
        if enum.location.file.resolve() != state.file_path:
            continue
        name_offset = enum.location.span.offset
        length = enum.location.span.length
        if length > 0:
            _append_token(name_offset, length, 6)  # type
        _scan_keyword_before(name_offset, "enum")

    # Add struct definition names and struct keyword
    for struct in state.structs.values():
        if struct.location.file.resolve() != state.file_path:
            continue
        name_offset = struct.location.span.offset
        length = struct.location.span.length
        if length > 0:
            _append_token(name_offset, length, 8)  # struct
        _scan_keyword_before(name_offset, "struct")

    tokens.sort(key=lambda t: (t[0], t[1]))

    data: list[int] = []
    prev_line = 0
    prev_col = 0
    for line, col, length, token_type, modifiers in tokens:
        delta_line = line - prev_line
        delta_col = col - prev_col if delta_line == 0 else col
        data.extend([delta_line, delta_col, length, token_type, modifiers])
        prev_line = line
        prev_col = col

    return types.SemanticTokens(data=data)


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO, format="[%(levelname)s] %(message)s")
    server.start_io()
