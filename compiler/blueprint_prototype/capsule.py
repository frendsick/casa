#!/usr/bin/env python3
"""Throwaway executable Compiler Capsule slice. This is not a Casa compiler.

Only the documented subset is supported. No production compiler is called by
syntax, analyze, or assembly. The native build adapter is a separate operation.
"""
from __future__ import annotations

from dataclasses import dataclass, fields, is_dataclass, replace
from pathlib import Path
import argparse
import re
import subprocess
import sys
import tempfile

from runtime import RUNTIME


@dataclass(frozen=True)
class Span:
    start: int
    end: int


@dataclass(frozen=True)
class Diagnostic:
    span: Span
    message: str


@dataclass(frozen=True)
class Report:
    source: str
    diagnostics: tuple[Diagnostic, ...]


@dataclass(frozen=True)
class Token:
    text: str
    span: Span


@dataclass(frozen=True)
class Group:
    kind: str
    parts: tuple
    token: Token


@dataclass(frozen=True)
class Declaration:
    name: str
    parameters: tuple[tuple[str, str], ...]
    results: tuple[str, ...]
    generics: tuple[str, ...]
    body: tuple
    token: Token
    external: bool = False


@dataclass(frozen=True)
class Shape:
    name: str
    members: tuple[tuple[str, str], ...]
    copy: bool
    external: bool = False


@dataclass(frozen=True)
class SyntaxResult:
    report: Report
    tokens: tuple[Token, ...]
    declarations: tuple[Declaration, ...]
    shapes: tuple[Shape, ...]
    root: tuple


@dataclass(frozen=True)
class Fact:
    span: Span
    description: str
    definition: Span | None


@dataclass(frozen=True)
class AnalysisSnapshot:
    report: Report
    index: tuple[Fact, ...]
    missing: tuple[Span, ...]


@dataclass(frozen=True)
class Answer:
    availability: str
    payload: str | Span | None


@dataclass(frozen=True)
class AssemblySource:
    text: str
    target: str = "linux-x86_64"


@dataclass(frozen=True)
class AssemblyResult:
    report: Report
    source: AssemblySource | None


@dataclass(frozen=True)
class CompilerFailure:
    report: Report
    phase: str
    message: str


class Rejection(Exception):
    def __init__(self, token, message):
        self.diagnostic = Diagnostic(token.span, message)


class InvariantError(Exception):
    pass


TOKEN = re.compile(r"\s+|\#[^\n]*|->|=>|::|mut\$|[A-Za-z_][A-Za-z_0-9]*|-?[0-9]+|==|!=|<=|>=|\+=|[-+*<>=.:,$\[\]{}]|\S")


class Parser:
    """One grammar builds source facts without consulting declaration meanings."""
    def __init__(self, source):
        self.source = source
        tokens, offset = [], 0
        for match in TOKEN.finditer(source):
            end = offset + len(match[0].encode())
            tokens.append(Token(match[0], Span(offset, end)))
            offset = end
        self.all_tokens = tuple(tokens)
        self.tokens = [t for t in self.all_tokens if not t.text.isspace() and not t.text.startswith("#")]
        self.tokens.append(Token("<eof>", Span(len(source.encode()), len(source.encode()))))
        self.position = 0

    def peek(self):
        return self.tokens[self.position].text

    def take(self, text=None):
        token = self.tokens[self.position]
        if token.text == "<eof>" or (text is not None and token.text != text):
            raise Rejection(token, f"expected {text or 'token'}, got {token.text}")
        self.position += 1
        return token

    def type(self):
        prefix = self.take().text if self.peek() in ("$", "mut$") else ""
        name = self.take().text
        if name == "array":
            self.take("[")
            element = self.type()
            length = self.take().text
            self.take("]")
            name = f"array[{element} {length}]"
        return prefix + name

    def body(self, stops):
        result = []
        while self.peek() not in stops:
            result.append(self.node())
        return tuple(result)

    def node(self):
        token = self.take()
        text = token.text
        if text == "if":
            condition = self.body({"then"})
            self.take("then")
            yes = self.body({"else", "fi"})
            no = ()
            if self.peek() == "else":
                self.take()
                no = self.body({"fi"})
            self.take("fi")
            return Group("if", (condition, yes, no), token)
        if text == "while":
            condition = self.body({"do"})
            self.take("do")
            body = self.body({"done"})
            self.take("done")
            return Group("loop", (condition, body), token)
        if text in ("unsafe", "{"):
            if text == "unsafe":
                self.take("{")
            body = self.body({"}"})
            self.take("}")
            return Group(text, (body,), token)
        if text == "[":
            elements = []
            while self.peek() != "]":
                elements.append(self.body({",", "]"}))
                if self.peek() != "]":
                    self.take(",")
            self.take("]")
            return Group("array", tuple(elements), token)
        if text == "match":
            arms = []
            while self.peek() != "end":
                pattern = self.take()
                self.take("=>")
                self.take("{")
                body = self.body({"}"})
                self.take("}")
                arms.append((pattern.text, body))
            self.take("end")
            return Group("match", tuple(arms), token)
        if text in ("=", "+="):
            name = self.take().text
            annotation = None
            if self.peek() == ":":
                self.take()
                annotation = self.type()
            return Group(text, (name, annotation), token)
        if text == ".":
            return Group("field", (self.take().text,), token)
        if self.peek() == "::":
            self.take()
            final = self.take()
            return Token(text + "::" + final.text, Span(token.span.start, final.span.end))
        return token

    def declaration(self, prefix="", external=False):
        self.take("fn")
        name = self.take()
        generics = []
        if self.peek() == "[":
            self.take()
            while self.peek() != "]":
                parameter = self.take()
                if not re.fullmatch(r"[A-Za-z_][A-Za-z_0-9]*", parameter.text):
                    raise Rejection(parameter, "slice limitation: unconstrained type parameters only")
                generics.append(parameter.text)
            self.take("]")
        parameters = []
        while self.position + 1 < len(self.tokens) and self.tokens[self.position + 1].text == ":":
            parameter = self.take().text
            self.take(":")
            parameters.append((parameter, self.type()))
        results = []
        if self.peek() == "->":
            self.take()
            # The slice permits one return type. General tuple returns are not implemented.
            results.append(self.type())
        body = ()
        if not external:
            self.take("{")
            body = self.body({"}"})
            self.take("}")
        return Declaration(prefix + name.text, tuple(parameters), tuple(results),
                           tuple(generics), body, name, external)

    def shape(self, external=False):
        self.take("struct")
        name = self.take().text
        copy = False
        if self.peek() == "derives":
            self.take()
            self.take("__casa_std__Copy")
            copy = True
        self.take("{")
        members = []
        while self.peek() != "}":
            member = self.take().text
            self.take(":")
            members.append((member, self.type()))
        self.take("}")
        return Shape(name, tuple(members), copy, external)


def syntax(source: str) -> SyntaxResult:
    parser = Parser(source)
    declarations, shapes, root, diagnostics = [], [], [], []
    try:
        while parser.peek() != "<eof>":
            if parser.peek() == "fn":
                declarations.append(parser.declaration())
            elif parser.peek() == "extern":
                parser.take()
                if parser.peek() == "struct":
                    shapes.append(parser.shape(external=True))
                else:
                    declarations.append(parser.declaration(external=True))
            elif parser.peek() == "struct":
                shapes.append(parser.shape())
            elif parser.peek() == "trait":
                # The real compiler's own fixture prelude supplies the two reserved
                # standard marker traits. Arbitrary trait dispatch is not in this slice.
                parser.take()
                name = parser.take()
                if name.text == "__casa_std__Clone":
                    parser.take("{")
                    signature = parser.declaration(external=True)
                    if (signature.name, signature.parameters, signature.results) != ("clone", (("self", "$self"),), ("self",)):
                        raise Rejection(name, "unsupported Clone prelude")
                    parser.take("}")
                elif name.text == "__casa_std__Copy":
                    for text in (":", "__casa_std__Clone", "{", "}"):
                        parser.take(text)
                else:
                    raise Rejection(name, "slice limitation: general traits")
            elif parser.peek() == "impl":
                parser.take()
                name = parser.take().text
                parser.take("{")
                declarations.append(parser.declaration(name + "::"))
                parser.take("}")
            else:
                root.append(parser.node())
    except Rejection as error:
        diagnostics.append(error.diagnostic)
    return SyntaxResult(Report(source, tuple(diagnostics)), parser.all_tokens,
                        tuple(declarations), tuple(shapes), tuple(root))


@dataclass(frozen=True)
class Value:
    identity: int
    type: str
    origins: frozenset[int] = frozenset()
    place: int | None = None


@dataclass(frozen=True)
class Literal:
    result: Value
    number: int


@dataclass(frozen=True)
class Transfer:
    kind: str  # Established copy, address, dereference, or scalar load.
    result: Value
    source: Value


@dataclass(frozen=True)
class Arithmetic:
    operation: str
    result: Value
    left: Value
    right: Value


@dataclass(frozen=True)
class Construct:
    result: Value
    members: tuple[Value, ...]


@dataclass(frozen=True)
class Projection:
    result: Value
    source: Value
    member: int


@dataclass(frozen=True)
class RawMemory:
    operation: str
    address: Value
    value: Value


@dataclass(frozen=True)
class Call:
    declaration: int
    bindings: tuple[str, ...]
    arguments: tuple[Value, ...]
    results: tuple[Value, ...]


@dataclass(frozen=True)
class Active:
    binding: Value
    active: bool


@dataclass(frozen=True)
class Cleanup:
    binding: Value
    drop: int | None


@dataclass(frozen=True)
class Return:
    values: tuple[Value, ...]
    cleanup: tuple[Cleanup, ...]


@dataclass(frozen=True)
class Branch:
    condition: Value
    yes: tuple
    no: tuple


@dataclass(frozen=True)
class Loop:
    condition_body: tuple
    condition: Value
    body: tuple


@dataclass(frozen=True)
class Match:
    subject: Value
    arms: tuple[tuple[bool, tuple], ...]


@dataclass(frozen=True)
class Print:
    value: Value


@dataclass(frozen=True)
class Recipe:
    declaration: int
    parameters: tuple[Value, ...]
    body: tuple
    return_origins: tuple[frozenset[int], ...]


def base_type(type):
    return type.removeprefix("mut$").removeprefix("$")


def borrowed(type):
    return type.startswith(("$", "mut$"))


def substitute(type, parameters, bindings):
    for parameter, binding in zip(parameters, bindings):
        type = re.sub(r"\b" + re.escape(parameter) + r"\b", lambda _: binding, type)
    return type


def walk(value):
    yield value
    if is_dataclass(value):
        for field in fields(value):
            yield from walk(getattr(value, field.name))
    elif isinstance(value, tuple):
        for child in value:
            yield from walk(child)


class Checker:
    """Request-owned proof state. No checker or recipe survives in editor results."""
    def __init__(self, parsed):
        self.parsed = parsed
        self.declarations = (Declaration("<root>", (), (), (), parsed.root,
                                        Token("<root>", Span(0, 0))),) + parsed.declarations
        self.names = {declaration.name: i for i, declaration in enumerate(self.declarations)}
        self.shapes = {shape.name: shape for shape in parsed.shapes}
        self.recipes = {}
        self.facts = [Fact(d.token.span, d.name + " " + repr(d.parameters) + " -> " + repr(d.results),
                           d.token.span) for d in parsed.declarations]
        self.missing = []
        self.diagnostics = list(parsed.report.diagnostics)
        self.next_identity = 0
        self.active_checks = set()

    def value(self, type, origins=frozenset(), place=None):
        self.next_identity += 1
        return Value(self.next_identity, type, origins, place)

    def is_copy(self, type):
        if borrowed(type) or type in ("i64", "u64", "bool", "ptr"):
            return True
        if type.startswith("array["):
            return self.is_copy(type[6:-1].split()[0])
        return type in self.shapes and self.shapes[type].copy

    def cleanup(self, bindings, available):
        return tuple(Cleanup(value, self.names.get(value.type + "::drop"))
                     for value in reversed(tuple(bindings.values()))
                     if not self.is_copy(value.type) and available.get(value.identity) is not False)

    def check(self, identity):
        if identity in self.recipes:
            return self.recipes[identity]
        declaration = self.declarations[identity]
        if declaration.external:
            return None
        if identity in self.active_checks:
            if not declaration.generics and not any(borrowed(t) for t in declaration.results):
                # Scalar recursion needs no provisional returned-origin summary.
                return None
            raise Rejection(declaration.token, "slice limitation: recursive return summaries")
        self.active_checks.add(identity)
        bindings = {name: self.value(type, frozenset({-index - 1}) if borrowed(type) else frozenset())
                    for index, (name, type) in enumerate(declaration.parameters)}
        parameters = tuple(bindings.values())
        available = {value.identity: True for value in parameters}
        prefix = tuple(Active(value, True) for value in parameters if not self.is_copy(value.type))
        return_origins = []
        try:
            body, stack, terminated = self.body(declaration.body, bindings, available, [], False,
                                               declaration, return_origins)
            if not terminated:
                body += (self.finish(stack, bindings, available, declaration, return_origins),)
            summary = tuple(frozenset().union(*(origins[i] for origins in return_origins))
                            for i in range(len(declaration.results)))
            recipe = Recipe(identity, parameters, prefix + body, summary)
            self.recipes[identity] = recipe
            return recipe
        finally:
            self.active_checks.remove(identity)

    def pop(self, stack, token):
        if not stack:
            raise Rejection(token, "stack underflow")
        return stack.pop()

    def consume(self, value, type, available, bindings, live, output, token):
        if borrowed(type):
            if not borrowed(value.type) and not self.is_copy(value.type):
                raise Rejection(token, "slice limitation: bind an owned temporary before borrowing it")
            if base_type(value.type) != base_type(type):
                raise Rejection(token, f"expected {type}, got {value.type}")
            if value.type.startswith("$") and type.startswith("mut$"):
                place = next((v for v in bindings.values() if v.identity == value.place), None)
                bound_loans = [v for v in bindings.values() if available.get(v.identity)]
                if place is None or borrowed(place.type) or any(
                    borrowed(v.type) and value.place in v.origins and v.identity != value.identity
                    for v in bound_loans + list(live)
                ):
                    raise Rejection(token, "shared borrow cannot become exclusive")
                value = replace(value, type=type)
            if not borrowed(value.type):
                owner = next((v for v in bindings.values() if v.identity == value.place), value)
                result = self.value(type, frozenset({owner.identity}), owner.identity)
                output.append(Transfer("address", result, owner))
                return result
            return value
        if base_type(value.type) != type:
            raise Rejection(token, f"expected {type}, got {value.type}")
        if borrowed(value.type):
            if not self.is_copy(type):
                owner = next((v for v in bindings.values() if v.identity == value.place), None)
                if owner is None or available.get(value.place) is not True:
                    raise Rejection(token, "owner is moved or only conditionally available")
                if borrowed(owner.type):
                    raise Rejection(token, "cannot move an owner through an input borrow")
                bound_loans = [v for v in bindings.values() if available.get(v.identity)]
                for binding in bound_loans + list(live):
                    if borrowed(binding.type) and value.place in binding.origins and binding.identity != value.identity:
                        raise Rejection(token, "owner still has a live borrowed result")
                available[value.place] = False
                output.append(Active(owner, False))
            result = self.value(type)
            output.append(Transfer("dereference", result, value))
            return result
        return value

    def finish(self, stack, bindings, available, declaration, summaries):
        if len(stack) != len(declaration.results):
            raise Rejection(declaration.token, f"result stack differs from {declaration.results}")
        # Fixture results are Copy values or input borrows. Owned returns are outside this slice.
        for value, type in zip(stack, declaration.results):
            if value.type != type:
                raise Rejection(declaration.token, f"return expects {type}, got {value.type}")
            if borrowed(type) and any(origin >= 0 for origin in value.origins):
                raise Rejection(declaration.token, "borrow of a local owner escapes")
            if not self.is_copy(type) and type not in declaration.generics:
                raise Rejection(declaration.token, "slice limitation: owned return")
        summaries.append(tuple(value.origins for value in stack))
        return Return(tuple(stack), self.cleanup(bindings, available))

    def body(self, nodes, bindings, available, stack, unsafe, declaration, summaries):
        output = []
        for node in nodes:
            token = node.token if isinstance(node, Group) else node
            text = node.kind if isinstance(node, Group) else node.text
            if text == "{":
                raise Rejection(token, "slice limitation: closures")
            if text == "unsafe":
                before = set(bindings)
                nested, stack, terminated = self.body(node.parts[0], bindings, available, stack,
                                                      unsafe or text == "unsafe", declaration, summaries)
                output.extend(nested)
                locals = {name: value for name, value in bindings.items() if name not in before}
                if not terminated:
                    if any(value.origins & {v.identity for v in locals.values()} for value in stack):
                        raise Rejection(token, "scope-local borrow escapes")
                    output.extend(self.cleanup(locals, available))
                for name in locals:
                    del bindings[name]
                if terminated:
                    return tuple(output), stack, True
            elif text in ("if", "match"):
                if text == "if":
                    condition_nodes, yes, no = node.parts
                    condition_body, stack, terminal = self.body(condition_nodes, bindings, available, stack,
                                                                unsafe, declaration, summaries)
                    if terminal:
                        raise Rejection(token, "slice limitation: terminating condition")
                    output.extend(condition_body)
                    condition = self.pop(stack, token)
                    branches = [(True, yes), (False, no)]
                else:
                    condition = self.pop(stack, token)
                    if {pattern for pattern, _ in node.parts} != {"true", "false"} or len(node.parts) != 2:
                        raise Rejection(token, "slice requires an exhaustive bool match")
                    branches = [(pattern == "true", body) for pattern, body in node.parts]
                if condition.type != "bool":
                    raise Rejection(token, "condition must be bool")
                outcomes = []
                for pattern, branch_nodes in branches:
                    branch_bindings, branch_available = dict(bindings), dict(available)
                    branch, branch_stack, terminal = self.body(branch_nodes, branch_bindings, branch_available,
                                                               list(stack), unsafe, declaration, summaries)
                    locals = {name: value for name, value in branch_bindings.items() if name not in bindings}
                    if not terminal:
                        branch += self.cleanup(locals, branch_available)
                        if any(value.origins & {v.identity for v in locals.values()} for value in branch_stack):
                            raise Rejection(token, "branch-local borrow escapes")
                    outcomes.append([pattern, branch, branch_stack, branch_available, terminal])
                continuing = [outcome for outcome in outcomes if not outcome[4]]
                joined = []
                if continuing:
                    types = tuple(value.type for value in continuing[0][2])
                    if any(tuple(value.type for value in outcome[2]) != types for outcome in continuing):
                        raise Rejection(token, "branch stack mismatch")
                    joined = [self.value(type, frozenset().union(*(outcome[2][i].origins for outcome in continuing)))
                              for i, type in enumerate(types)]
                    for outcome in continuing:
                        outcome[1] += tuple(Transfer("copy", target, source) for target, source in zip(joined, outcome[2]))
                    for identity in available:
                        states = {outcome[3].get(identity) for outcome in continuing}
                        available[identity] = states.pop() if len(states) == 1 else None
                if text == "if":
                    output.append(Branch(condition, outcomes[0][1], outcomes[1][1]))
                else:
                    output.append(Match(condition, tuple((o[0], o[1]) for o in outcomes)))
                stack = joined
                if not continuing:
                    return tuple(output), stack, True
            elif text == "loop":
                if stack:
                    raise Rejection(token, "slice limitation: loop-carried operand stack")
                initial = dict(available)
                condition_body, condition_stack, terminal = self.body(node.parts[0], bindings, available, [],
                                                                       unsafe, declaration, summaries)
                if terminal or len(condition_stack) != 1 or condition_stack[0].type != "bool":
                    raise Rejection(token, "loop condition must produce bool")
                nested_bindings = dict(bindings)
                body, end_stack, terminal = self.body(node.parts[1], nested_bindings, available, [],
                                                      unsafe, declaration, summaries)
                if terminal or end_stack or any(available.get(k) != v for k, v in initial.items()):
                    raise Rejection(token, "loop back-edge changes stack or ownership")
                locals = {name: value for name, value in nested_bindings.items() if name not in bindings}
                output.append(Loop(condition_body, condition_stack[0], body + self.cleanup(locals, available)))
            elif text in ("=", "+="):
                name, annotation = node.parts
                value = self.pop(stack, token)
                if annotation is not None and annotation != value.type:
                    raise Rejection(token, "slice limitation: contextual binding conversion")
                if name in bindings:
                    binding = bindings[name]
                    if borrowed(binding.type):
                        raise Rejection(token, "slice limitation: borrow-binding reassignment")
                    bound_loans = [v for v in bindings.values() if available.get(v.identity)]
                    if not self.is_copy(binding.type) and any(borrowed(v.type) and binding.identity in v.origins
                                                              for v in bound_loans + stack):
                        raise Rejection(token, "cannot replace a borrowed owner")
                    value = self.consume(value, binding.type, available, bindings, stack, output, token)
                    if text == "+=":
                        output.append(Arithmetic("+", binding, binding, value))
                    else:
                        output.extend(self.cleanup({name: binding}, available))
                        output.append(Transfer("copy", binding, value))
                else:
                    if text == "+=":
                        raise Rejection(token, "unknown binding")
                    binding = self.value(value.type, value.origins)
                    bindings[name] = binding
                    output.append(Transfer("copy", binding, value))
                available[binding.identity] = True
                if not self.is_copy(binding.type):
                    output.append(Active(binding, True))
            elif text == "array":
                members = []
                for element in node.parts:
                    body, element_stack, terminal = self.body(element, bindings, available, [], unsafe,
                                                              declaration, summaries)
                    output.extend(body)
                    if terminal or len(element_stack) != 1 or element_stack[0].type != "i64":
                        raise Rejection(token, "slice arrays contain i64 values")
                    members.append(element_stack[0])
                result = self.value(f"array[i64 {len(members)}]")
                output.append(Construct(result, tuple(members)))
                stack.append(result)
            elif text == "field":
                source = self.pop(stack, token)
                if not borrowed(source.type) and not self.is_copy(source.type):
                    raise Rejection(token, "slice limitation: bind an owned temporary before projecting it")
                shape = self.shapes.get(base_type(source.type))
                if not shape or node.parts[0] not in dict(shape.members):
                    raise Rejection(token, "unknown field")
                member = list(dict(shape.members)).index(node.parts[0])
                type = shape.members[member][1]
                if not self.is_copy(type):
                    raise Rejection(token, "slice limitation: owned field projection")
                result = self.value(type)
                output.append(Projection(result, source, member))
                stack.append(result)
            elif re.fullmatch(r"-?[0-9]+", text) or text in ("true", "false"):
                number = {"true": 1, "false": 0}.get(text)
                number = int(text) if number is None else number
                if not -(2**63) <= number < 2**63:
                    raise Rejection(token, "i64 literal out of range")
                result = self.value("bool" if text in ("true", "false") else "i64")
                output.append(Literal(result, number))
                stack.append(result)
                self.facts.append(Fact(token.span, result.type, None))
            elif text in ("+", "-", "*", "<", ">", "==", "!="):
                top, below = self.pop(stack, token), self.pop(stack, token)
                if top.type != "i64" or below.type != "i64":
                    raise Rejection(token, "arithmetic requires two i64 values")
                comparison = text in ("<", ">", "==", "!=")
                result = self.value("bool" if comparison else "i64")
                left, right = (top, below) if comparison else (below, top)
                output.append(Arithmetic(text, result, left, right))
                stack.append(result)
            elif text == "ptr::from_ref":
                value = self.pop(stack, token)
                if not borrowed(value.type):
                    value = self.consume(value, "$" + value.type, available, bindings, stack, output, token)
                result = self.value("ptr")
                output.append(Transfer("copy", result, value))
                stack.append(result)
            elif text in ("load64", "store64"):
                if not unsafe:
                    raise Rejection(token, "raw memory operation requires unsafe")
                address = self.pop(stack, token)
                if address.type != "ptr":
                    raise Rejection(token, "raw memory operation requires ptr")
                if text == "load64":
                    result = self.value("u64")
                    output.append(RawMemory(text, address, result))
                    stack.append(result)
                else:
                    value = self.pop(stack, token)
                    if value.type not in ("i64", "u64"):
                        raise Rejection(token, "store64 requires an integer")
                    output.append(RawMemory(text, address, value))
            elif text == "return":
                output.append(self.finish(stack, bindings, available, declaration, summaries))
                return tuple(output), stack, True
            elif text == "print":
                value = self.pop(stack, token)
                if value.type not in ("i64", "u64"):
                    raise Rejection(token, "slice print requires i64 or u64")
                output.append(Print(value))
            elif text in ("drop", "copy"):
                value = self.pop(stack, token)
                if text == "copy":
                    if not self.is_copy(base_type(value.type)):
                        raise Rejection(token, "type does not implement Copy")
                    result = self.consume(value, base_type(value.type), available, bindings, stack, output, token)
                    stack.append(result)
                elif value.place is not None and not self.is_copy(base_type(value.type)):
                    self.consume(value, base_type(value.type), available, bindings, stack, output, token)
                    owner = next(v for v in bindings.values() if v.identity == value.place)
                    # Transfer cleared the flag. Explicit drop acts on the moved value.
                    output.append(Active(owner, True))
                    output.append(Cleanup(owner, self.names.get(owner.type + "::drop")))
                elif not self.is_copy(value.type):
                    output.append(Active(value, True))
                    output.append(Cleanup(value, self.names.get(value.type + "::drop")))
            elif text in bindings:
                binding = bindings[text]
                if available.get(binding.identity) is not True:
                    raise Rejection(token, "owner is moved or only conditionally available")
                if self.is_copy(binding.type) and not binding.type.startswith("array[") and binding.type not in self.shapes:
                    result = self.value(binding.type, binding.origins, binding.identity)
                    output.append(Transfer("copy", result, binding))
                else:
                    result = self.value("$" + binding.type, frozenset({binding.identity}), binding.identity)
                    output.append(Transfer("address", result, binding))
                stack.append(result)
                self.facts.append(Fact(token.span, binding.type, None))
            elif text in self.shapes:
                shape = self.shapes[text]
                members = tuple(self.consume(self.pop(stack, token), type, available, bindings, stack, output, token)
                                for _, type in shape.members)
                result = self.value(text)
                output.append(Construct(result, members))
                stack.append(result)
            elif text in self.names:
                target = self.names[text]
                called = self.declarations[target]
                self.facts.append(Fact(token.span, called.name, called.token.span))
                if called.external and not unsafe:
                    raise Rejection(token, "extern call requires unsafe")
                if called.external and any(not borrowed(t) and not self.is_copy(t) for _, t in called.parameters):
                    raise Rejection(token, "extern by-value parameter must implement Copy")
                recipe = self.check(target)
                arguments = []
                inferred = {}
                raw = [self.pop(stack, token) for _ in called.parameters]
                for argument, (_, type) in zip(raw, called.parameters):
                    parameter = base_type(type)
                    if parameter in called.generics:
                        actual = base_type(argument.type)
                        if parameter in inferred and inferred[parameter] != actual:
                            raise Rejection(token, "inconsistent generic inputs")
                        inferred[parameter] = actual
                if set(inferred) != set(called.generics):
                    raise Rejection(token, "cannot infer generic inputs")
                concrete = tuple(inferred[p] for p in called.generics)
                for argument, (_, type) in zip(raw, called.parameters):
                    expected = substitute(type, called.generics, concrete)
                    arguments.append(self.consume(argument, expected, available, bindings, stack + raw, output, token))
                results = []
                for index, type in enumerate(called.results):
                    origins = frozenset()
                    if recipe and borrowed(type):
                        origins = frozenset().union(*(arguments[-origin - 1].origins
                                                      for origin in recipe.return_origins[index]))
                    results.append(self.value(substitute(type, called.generics, concrete), origins))
                output.append(Call(target, concrete, tuple(arguments), tuple(results)))
                stack.extend(results)
            else:
                raise Rejection(token, f"unknown name or unsupported slice syntax: {text}")
        return tuple(output), stack, False

    def run(self):
        if len(self.names) != len(self.declarations):
            self.diagnostics.append(Diagnostic(Span(0, 0), "duplicate function declaration"))
        if len(self.shapes) != len(self.parsed.shapes) or set(self.shapes) & set(self.names):
            self.diagnostics.append(Diagnostic(Span(0, 0), "duplicate declaration name"))
        for shape in self.shapes.values():
            if len({name for name, _ in shape.members}) != len(shape.members):
                self.diagnostics.append(Diagnostic(Span(0, 0), "duplicate struct field"))
            if any(type not in ("i64", "u64", "ptr") for _, type in shape.members):
                self.diagnostics.append(Diagnostic(Span(0, 0), "slice limitation: flat word-field structs only"))
            drop = self.names.get(shape.name + "::drop")
            if shape.copy and (not shape.external or drop is not None):
                self.diagnostics.append(Diagnostic(Span(0, 0), "struct representation cannot implement Copy"))
            if drop is not None:
                hook = self.declarations[drop]
                if hook.generics or len(hook.parameters) != 1 or hook.parameters[0][1] != "mut$" + shape.name or hook.results:
                    self.diagnostics.append(Diagnostic(hook.token.span, "drop requires one exclusive self parameter and no results"))
        for identity, declaration in enumerate(self.declarations):
            if len({name for name, _ in declaration.parameters}) != len(declaration.parameters) or len(set(declaration.generics)) != len(declaration.generics):
                self.diagnostics.append(Diagnostic(declaration.token.span, "duplicate parameter name"))
            known = {"i64", "u64", "bool", "ptr"} | set(self.shapes) | set(declaration.generics)
            for type in tuple(t for _, t in declaration.parameters) + declaration.results:
                base = base_type(type)
                if base not in known and not re.fullmatch(r"array\[i64 [1-9][0-9]*\]", base):
                    self.diagnostics.append(Diagnostic(declaration.token.span, "unknown or unsupported slice type: " + type))
            try:
                self.check(identity)
            except Rejection as error:
                if error.diagnostic not in self.diagnostics:
                    self.diagnostics.append(error.diagnostic)
                end = max((value.span.end for value in walk(declaration.body) if isinstance(value, Token)),
                          default=error.diagnostic.span.end)
                self.missing.append(Span(error.diagnostic.span.start, max(end, error.diagnostic.span.end)))
        return Report(self.parsed.report.source, tuple(self.diagnostics))


def analyze(source: str) -> AnalysisSnapshot | CompilerFailure:
    checker = Checker(syntax(source))
    try:
        report = checker.run()
        return AnalysisSnapshot(report, tuple(checker.facts), tuple(checker.missing))
    except InvariantError as error:
        return CompilerFailure(Report(source, tuple(checker.diagnostics)), "checking", str(error))


def hover(snapshot: AnalysisSnapshot, offset: int) -> Answer:
    if not 0 <= offset < len(snapshot.report.source.encode()):
        return Answer("unavailable", "invalid position")
    for fact in snapshot.index:
        if fact.span.start <= offset < fact.span.end:
            return Answer("known", fact.description)
    if snapshot.report.diagnostics and (not snapshot.missing or any(s.start <= offset <= s.end for s in snapshot.missing)):
        return Answer("unavailable", "missing semantic facts")
    return Answer("absent", None)


def definition(snapshot: AnalysisSnapshot, offset: int) -> Answer:
    answer = hover(snapshot, offset)
    if answer.availability != "known":
        return answer
    fact = next(f for f in snapshot.index if f.span.start <= offset < f.span.end)
    return Answer("known", fact.definition) if fact.definition else Answer("absent", None)


SEAL = object()


@dataclass(frozen=True)
class CheckedProgram:
    seal: object
    declarations: tuple[Declaration, ...]
    shapes: tuple[Shape, ...]
    instances: tuple[tuple[tuple[int, tuple[str, ...]], Recipe], ...]

    def __post_init__(self):
        if self.seal is not SEAL:
            raise InvariantError("private checked-program constructor")


def commit(checker):
    """Substitute checked recipes, reserve identities, then validate all references."""
    instances = {}
    pending = [(0, ())]
    while pending:
        key = pending.pop()
        if key in instances:
            continue
        identity, bindings = key
        if not 0 <= identity < len(checker.declarations):
            raise InvariantError("invalid declaration reference")
        declaration = checker.declarations[identity]
        if declaration.external:
            continue
        if identity not in checker.recipes or len(bindings) != len(declaration.generics):
            raise InvariantError("unfinished instance")

        def specialize(value):
            if isinstance(value, Value):
                concrete_type = substitute(value.type, declaration.generics, bindings)
                if any(re.search(r"\b" + re.escape(p) + r"\b", concrete_type) for p in declaration.generics):
                    raise InvariantError("unresolved symbolic type")
                return replace(value, type=concrete_type)
            if isinstance(value, Call):
                return replace(value, bindings=tuple(substitute(t, declaration.generics, bindings) for t in value.bindings),
                               arguments=tuple(map(specialize, value.arguments)), results=tuple(map(specialize, value.results)))
            if is_dataclass(value):
                return type(value)(**{field.name: specialize(getattr(value, field.name)) for field in fields(value)})
            if isinstance(value, tuple):
                return tuple(map(specialize, value))
            return value

        recipe = specialize(checker.recipes[identity])
        instances[key] = recipe
        for node in walk(recipe):
            if isinstance(node, Call):
                pending.append((node.declaration, node.bindings))
            if isinstance(node, Cleanup) and node.drop is not None:
                pending.append((node.drop, ()))
    for _, recipe in instances.items():
        for node in walk(recipe):
            if isinstance(node, Call):
                if not 0 <= node.declaration < len(checker.declarations):
                    raise InvariantError("invalid call reference")
                called = checker.declarations[node.declaration]
                if not called.external and (node.declaration, node.bindings) not in instances:
                    raise InvariantError("missing call instance")
                expected_arguments = tuple(substitute(t, called.generics, node.bindings) for _, t in called.parameters)
                expected_results = tuple(substitute(t, called.generics, node.bindings) for t in called.results)
                actual_arguments = tuple(v.type for v in node.arguments)
                actual_results = tuple(v.type for v in node.results)
                if len(expected_arguments) != len(actual_arguments) or any(
                    actual != expected and not (actual.startswith("mut$") and expected == "$" + base_type(actual))
                    for actual, expected in zip(actual_arguments, expected_arguments)
                ) or expected_results != actual_results:
                    raise InvariantError("call does not match its concrete stack effect")
    # Declaration products retain signatures, never source bodies.
    declarations = tuple(replace(d, body=()) for d in checker.declarations)
    return CheckedProgram(SEAL, declarations, tuple(checker.shapes.values()), tuple(instances.items()))


@dataclass(frozen=True)
class StoragePlan:
    size: int
    alignment: int
    offsets: tuple[int, ...]
    carrier: str
    body_size: int = 0


@dataclass(frozen=True)
class NativePlan:
    arguments: tuple[tuple[int, str], ...]
    result: tuple[str, ...]
    stack_bytes: int
    clobbers: tuple[str, ...]


class TargetPlanner:
    def __init__(self, program):
        self.shapes = {shape.name: shape for shape in program.shapes}
        self.storage = {}
        self.native = {}

    def layout(self, type):
        if type in self.storage:
            return self.storage[type]
        if borrowed(type) or type in ("i64", "u64", "bool", "ptr"):
            plan = StoragePlan(8, 8, (), "word")
        else:
            if type.startswith("array["):
                element, count = type[6:-1].split()
                members = [element] * int(count)
            elif type in self.shapes:
                members = [t for _, t in self.shapes[type].members]
            else:
                raise InvariantError("unresolved target type: " + type)
            sizes = [self.layout(t).size for t in members]
            offsets, total = [], 0
            for size in sizes:
                offsets.append(total)
                total += size
            indirect = type in self.shapes and not self.shapes[type].external
            plan = StoragePlan(8 if indirect else total, 8, tuple(offsets),
                               "owning_pointer" if indirect else "inline", total)
        self.storage[type] = plan
        return plan

    def native_call(self, declaration):
        key = (declaration.parameters, declaration.results)
        if key in self.native:
            return self.native[key]
        for _, type in declaration.parameters + tuple(("result", t) for t in declaration.results):
            shape = self.shapes.get(type)
            if type not in ("i64", "u64", "ptr") and not borrowed(type) and (
                shape is None or any(member_type not in ("i64", "u64", "ptr") for _, member_type in shape.members)
            ):
                raise Rejection(declaration.token, "slice target restriction: native word fields only")
        registers = ("%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9")
        arguments, offset, used, spilled = [], 0, 0, 0
        for _, type in declaration.parameters:
            size = self.layout(type).size
            if size > 16:
                destinations = tuple(f"{spilled + i}(%rsp)" for i in range(0, size, 8))
                spilled += size
            elif used + size // 8 <= len(registers):
                destinations = registers[used:used + size // 8]
                used += size // 8
            else:
                destinations = tuple(f"{spilled + i}(%rsp)" for i in range(0, size, 8))
                spilled += size
            arguments.extend((offset + i * 8, destination) for i, destination in enumerate(destinations))
            offset += size
        result_size = sum(self.layout(type).size for type in declaration.results)
        if result_size > 16:
            raise Rejection(declaration.token, "slice target restriction: memory-class extern return")
        result = ("%rax", "%rdx")[:result_size // 8]
        plan = NativePlan(tuple(arguments), result, (spilled + 15) // 16 * 16,
                          ("rax", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11"))
        self.native[key] = plan
        return plan


@dataclass(frozen=True)
class Instruction:
    opcode: str
    operands: tuple[str, ...] = ()


@dataclass(frozen=True)
class Label:
    name: str


def render(buffer):
    """Only concrete instructions reach rendering. No declarations or types are accepted."""
    return "\n".join(node.name + ":" if isinstance(node, Label)
                     else "    " + node.opcode + (" " + ", ".join(node.operands) if node.operands else "")
                     for node in buffer) + "\n"


class FunctionBuilder:
    def __init__(self, program, planner, symbols, recipe, name):
        self.program, self.planner, self.symbols = program, planner, symbols
        self.recipe, self.name = recipe, name
        self.buffer = []
        self.slots = {}
        self.flags = {}
        self.size = 8  # Saved caller frame pointer. Locals live on the return stack.
        self.next_label = 0

    def reserve(self, size):
        self.size += max(size, 8)
        return -self.size

    def slot(self, value):
        if value.identity not in self.slots:
            self.slots[value.identity] = self.reserve(self.planner.layout(value.type).size)
        return self.slots[value.identity]

    def flag(self, value):
        if value.identity not in self.flags:
            self.flags[value.identity] = self.reserve(8)
        return self.flags[value.identity]

    def label(self):
        self.next_label += 1
        return f".L{self.name}_{self.next_label}"

    def emit(self, opcode, *operands):
        self.buffer.append(Instruction(opcode, tuple(operands)))

    def mark(self, label):
        self.buffer.append(Label(label))

    def move(self, source, destination, size):
        for offset in range(0, size, 8):
            self.emit("movq", f"{source[0] + offset}({source[1]})", "%rax")
            self.emit("movq", "%rax", f"{destination[0] + offset}({destination[1]})")

    def runtime_call(self, name):
        returned = self.label()
        self.emit("leaq", "return_stack+1048576(%rip)", "%rax")
        self.emit("cmpq", "%rax", "%r14")
        self.emit("jae", "return_stack_overflow")
        self.emit("leaq", returned + "(%rip)", "%rax")
        self.emit("movq", "%rax", "(%r14)")
        self.emit("addq", "$8", "%r14")
        self.emit("jmp", name)
        self.mark(returned)

    def cleanup(self, node):
        done = self.label()
        self.emit("cmpq", "$0", f"{self.flag(node.binding)}(%rbp)")
        self.emit("je", done)
        self.emit("movq", "$0", f"{self.flag(node.binding)}(%rbp)")
        storage = self.planner.layout(node.binding.type)
        if node.drop is not None:
            self.emit("movq" if storage.carrier == "owning_pointer" else "leaq",
                      f"{self.slot(node.binding)}(%rbp)", "%rax")
            self.emit("pushq", "%rax")
            self.runtime_call(self.symbols[(node.drop, ())])
        if storage.carrier == "owning_pointer":
            self.emit("movq", f"{self.slot(node.binding)}(%rbp)", "%rdi")
            self.runtime_call("heap_free")
        self.mark(done)

    def lower(self, body):
        for node in body:
            if isinstance(node, Literal):
                self.emit("movabsq", "$" + str(node.number), "%rax")
                self.emit("movq", "%rax", f"{self.slot(node.result)}(%rbp)")
            elif isinstance(node, Transfer):
                destination, source = self.slot(node.result), self.slot(node.source)
                source_plan = self.planner.layout(node.source.type)
                result_plan = self.planner.layout(node.result.type)
                if node.kind == "address":
                    self.emit("movq" if source_plan.carrier == "owning_pointer" else "leaq",
                              f"{source}(%rbp)", "%rax")
                    self.emit("movq", "%rax", f"{destination}(%rbp)")
                elif node.kind == "dereference":
                    if result_plan.carrier == "owning_pointer":
                        self.move((source, "%rbp"), (destination, "%rbp"), result_plan.size)
                    else:
                        self.emit("movq", f"{source}(%rbp)", "%r10")
                        self.move((0, "%r10"), (destination, "%rbp"), result_plan.size)
                else:
                    self.move((source, "%rbp"), (destination, "%rbp"), self.planner.layout(node.result.type).size)
            elif isinstance(node, Arithmetic):
                self.emit("movq", f"{self.slot(node.left)}(%rbp)", "%rax")
                self.emit("movq", f"{self.slot(node.right)}(%rbp)", "%rcx")
                if node.operation in ("+", "-", "*"):
                    self.emit({"+": "addq", "-": "subq", "*": "imulq"}[node.operation], "%rcx", "%rax")
                    self.emit("jo", "arithmetic_error")
                else:
                    self.emit("cmpq", "%rcx", "%rax")
                    self.emit({"<": "setl", ">": "setg", "==": "sete", "!=": "setne"}[node.operation], "%al")
                    self.emit("movzbq", "%al", "%rax")
                self.emit("movq", "%rax", f"{self.slot(node.result)}(%rbp)")
            elif isinstance(node, Construct):
                plan = self.planner.layout(node.result.type)
                destination = self.slot(node.result)
                register = "%rbp"
                if plan.carrier == "owning_pointer":
                    self.emit("movq", "$" + str(plan.body_size), "%rdi")
                    self.runtime_call("heap_alloc")
                    self.emit("popq", "%r10")
                    self.emit("movq", "%r10", f"{destination}(%rbp)")
                    destination, register = 0, "%r10"
                for member, offset in zip(node.members, plan.offsets):
                    self.move((self.slot(member), "%rbp"), (destination + offset, register),
                              self.planner.layout(member.type).size)
            elif isinstance(node, Projection):
                plan = self.planner.layout(base_type(node.source.type))
                source = self.slot(node.source)
                register = "%rbp"
                if borrowed(node.source.type) or plan.carrier == "owning_pointer":
                    self.emit("movq", f"{source}(%rbp)", "%r10")
                    source, register = 0, "%r10"
                offset = plan.offsets[node.member]
                self.move((source + offset, register), (self.slot(node.result), "%rbp"),
                          self.planner.layout(node.result.type).size)
            elif isinstance(node, RawMemory):
                self.emit("movq", f"{self.slot(node.address)}(%rbp)", "%r10")
                if node.operation == "load64":
                    self.emit("movq", "(%r10)", "%rax")
                    self.emit("movq", "%rax", f"{self.slot(node.value)}(%rbp)")
                else:
                    self.emit("movq", f"{self.slot(node.value)}(%rbp)", "%rax")
                    self.emit("movq", "%rax", "(%r10)")
            elif isinstance(node, Call):
                declaration = self.program.declarations[node.declaration]
                if declaration.external:
                    sizes = [self.planner.layout(value.type).size for value in node.arguments]
                    arguments = self.reserve(sum(sizes))
                    offset = 0
                    for value, size in zip(node.arguments, sizes):
                        self.move((self.slot(value), "%rbp"), (arguments + offset, "%rbp"), size)
                        offset += size
                    plan = self.planner.native_call(declaration)
                    if plan.stack_bytes:
                        self.emit("subq", "$" + str(plan.stack_bytes), "%rsp")
                    for offset, destination in plan.arguments:
                        self.emit("movq", f"{arguments + offset}(%rbp)", "%rax")
                        self.emit("movq", "%rax", destination)
                    self.emit("call", declaration.name)
                    if plan.stack_bytes:
                        self.emit("addq", "$" + str(plan.stack_bytes), "%rsp")
                    if node.results:
                        for offset, register in enumerate(plan.result):
                            self.emit("movq", register, f"{self.slot(node.results[0]) + offset * 8}(%rbp)")
                else:
                    for value in reversed(node.arguments):
                        plan = self.planner.layout(value.type)
                        self.emit("leaq" if plan.carrier == "inline" else "movq",
                                  f"{self.slot(value)}(%rbp)", "%rax")
                        self.emit("pushq", "%rax")
                    self.runtime_call(self.symbols[(node.declaration, node.bindings)])
                    for value in reversed(node.results):
                        self.emit("popq", "%r10")
                        plan = self.planner.layout(value.type)
                        if plan.carrier == "inline":
                            self.move((0, "%r10"), (self.slot(value), "%rbp"), plan.size)
                        else:
                            self.emit("movq", "%r10", f"{self.slot(value)}(%rbp)")
            elif isinstance(node, Active):
                self.emit("movq", "$1" if node.active else "$0", f"{self.flag(node.binding)}(%rbp)")
            elif isinstance(node, Cleanup):
                self.cleanup(node)
            elif isinstance(node, Return):
                for cleanup in node.cleanup:
                    self.cleanup(cleanup)
                for value in node.values:
                    plan = self.planner.layout(value.type)
                    self.emit("leaq" if plan.carrier == "inline" else "movq",
                              f"{self.slot(value)}(%rbp)", "%rax")
                    self.emit("pushq", "%rax")
                self.emit("restore_frame")
            elif isinstance(node, Branch):
                other, end = self.label(), self.label()
                self.emit("cmpq", "$0", f"{self.slot(node.condition)}(%rbp)")
                self.emit("je", other)
                self.lower(node.yes)
                self.emit("jmp", end)
                self.mark(other)
                self.lower(node.no)
                self.mark(end)
            elif isinstance(node, Loop):
                start, end = self.label(), self.label()
                self.mark(start)
                self.lower(node.condition_body)
                self.emit("cmpq", "$0", f"{self.slot(node.condition)}(%rbp)")
                self.emit("je", end)
                self.lower(node.body)
                self.emit("jmp", start)
                self.mark(end)
            elif isinstance(node, Match):
                yes = next(body for pattern, body in node.arms if pattern)
                no = next(body for pattern, body in node.arms if not pattern)
                self.lower((Branch(node.subject, yes, no),))
            elif isinstance(node, Print):
                self.emit("movq", f"{self.slot(node.value)}(%rbp)", "%rdi")
                self.runtime_call("print_uint" if node.value.type == "u64" else "print_int")
            else:
                raise InvariantError("unsupported semantic node")

    def finish(self):
        for parameter in self.recipe.parameters:
            plan = self.planner.layout(parameter.type)
            self.emit("popq", "%r10")
            if plan.carrier == "inline":
                self.move((0, "%r10"), (self.slot(parameter), "%rbp"), plan.size)
            else:
                self.emit("movq", "%r10", f"{self.slot(parameter)}(%rbp)")
        self.lower(self.recipe.body)
        frame = (self.size + 15) // 16 * 16
        if frame > 1048576:
            raise InvariantError("function frame exceeds return-stack capacity")
        prologue = [Label(self.name),
                    Instruction("leaq", (f"return_stack+{1048576 - frame}(%rip)", "%rax")),
                    Instruction("cmpq", ("%rax", "%r14")),
                    Instruction("ja", ("return_stack_overflow",)),
                    Instruction("addq", ("$" + str(frame), "%r14")),
                    Instruction("movq", ("%rbp", "%r10")),
                    Instruction("movq", ("%r14", "%rbp")),
                    Instruction("movq", ("%r10", "-8(%rbp)"))]
        prologue.extend(Instruction("movq", ("$0", f"{offset}(%rbp)")) for offset in self.flags.values())
        finalized = []
        for node in self.buffer:
            if isinstance(node, Instruction) and node.opcode == "restore_frame":
                finalized.extend((Instruction("movq", ("-8(%rbp)", "%rbp")),
                                  Instruction("subq", ("$" + str(frame + 8), "%r14")),
                                  Instruction("jmpq", ("*(%r14)",))))
            else:
                finalized.append(node)
        buffer = tuple(prologue + finalized)
        labels = [node.name for node in buffer if isinstance(node, Label)]
        if len(labels) != len(set(labels)):
            raise InvariantError("duplicate function label")
        for node in buffer:
            if isinstance(node, Instruction) and node.opcode.startswith("j"):
                target = node.operands[0]
                if target.startswith(".L") and target not in labels:
                    raise InvariantError("undefined function label")
        return render(buffer)


def emit(program):
    if program.seal is not SEAL:
        raise InvariantError("backend requires checked program")
    planner = TargetPlanner(program)
    symbols = {key: f"capsule_fn_{index}" for index, (key, _) in enumerate(program.instances)}
    fragments = [RUNTIME, ".section .text\n.globl _start\n_start:\n"
                 "    leaq return_stack(%rip), %r14\n"
                 "    xorq %rbp, %rbp\n"
                 "    leaq .Lcapsule_exit(%rip), %rax\n"
                 "    movq %rax, (%r14)\n    addq $8, %r14\n"
                 f"    jmp {symbols[(0, ())]}\n.Lcapsule_exit:\n"
                 "    movq $60, %rax\n    xorq %rdi, %rdi\n    syscall\n"]
    for key, recipe in program.instances:
        fragments.append(FunctionBuilder(program, planner, symbols, recipe, symbols[key]).finish())
    return AssemblySource("\n".join(fragments))


def assembly(source: str) -> AssemblyResult | CompilerFailure:
    checker = Checker(syntax(source))
    phase = "checking"
    report = Report(source, ())
    try:
        report = checker.run()
        if report.diagnostics:
            return AssemblyResult(report, None)
        phase = "commit"
        program = commit(checker)
        phase = "target"
        del checker
        return AssemblyResult(report, emit(program))
    except Rejection as error:
        diagnostics = tuple(checker.diagnostics) if phase == "checking" else report.diagnostics
        return AssemblyResult(Report(source, diagnostics + (error.diagnostic,)), None)
    except InvariantError as error:
        diagnostics = tuple(checker.diagnostics) if phase == "checking" else report.diagnostics
        return CompilerFailure(Report(source, diagnostics), phase, str(error))


@dataclass(frozen=True)
class NativeFailure:
    stage: str
    message: str


def build(source: AssemblySource, output: Path, libraries=(), keep_asm=False, driver="/usr/bin/cc"):
    """Own temporary files and native effects, preserving driver diagnostics."""
    if source.target != "linux-x86_64":
        return NativeFailure("target", source.target)
    try:
        with tempfile.TemporaryDirectory(prefix="capsule-") as temporary:
            assembly_path = Path(str(output) + ".s") if keep_asm else Path(temporary) / "program.s"
            assembly_path.write_text(source.text)
            command = [driver, "-nostdlib", "-no-pie", "-Wl,-e,_start", "-Wl,-z,noexecstack",
                       "-o", str(output), str(assembly_path)]
            command.extend("-l" + library for library in libraries)
            try:
                completed = subprocess.run(command, capture_output=True, text=True)
            except OSError as error:
                return NativeFailure("launch", str(error))
            if completed.returncode:
                return NativeFailure("build", completed.stderr)
    except OSError as error:
        return NativeFailure("write", str(error))
    return None


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("source", type=Path)
    parser.add_argument("-o", "--output", type=Path, required=True)
    parser.add_argument("--keep-asm", action="store_true")
    parser.add_argument("-l", dest="libraries", action="append", default=[])
    args = parser.parse_args()
    result = assembly(args.source.read_text())
    if isinstance(result, CompilerFailure) or result.source is None:
        print(result, file=sys.stderr)
        return 1
    failure = build(result.source, args.output, args.libraries, args.keep_asm)
    if failure:
        print(failure, file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
