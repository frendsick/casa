"""Tests for the trait system: parsing, type checking, and auto-injection."""

import os
import subprocess

import pytest

from casa.common import (
    GLOBAL_FUNCTIONS,
    GLOBAL_TRAITS,
    Function,
    OpKind,
    Parameter,
    Signature,
    Trait,
    TraitMethod,
    extract_generic_params,
    split_type_tokens,
)
from casa.error import CasaErrorCollection, ErrorKind
from tests.conftest import parse_string, resolve_string, typecheck_string

CASA_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
STD_LIB_PATH = os.path.join(CASA_ROOT, "lib", "std.casa")
STD_INCLUDE = f'include "{STD_LIB_PATH}"\n'

# Inline Hashable trait definition for tests that go through typecheck_string
# (include is only processed during resolve_identifiers, not during parse_ops,
# so trait bounds on functions would fail if the trait comes from an include)
HASHABLE_TRAIT = (
    "trait Hashable { fn hash self:self -> int  fn eq self:self other:self -> bool }\n"
)

HASHABLE_IMPL_STR = (
    "impl str {\n"
    "    fn hash self:str -> int { 0 }\n"
    "    fn eq b:str a:str -> bool { true }\n"
    "}\n"
)

HASHABLE_IMPL_INT = (
    "impl int {\n"
    "    fn hash self:int -> int { self }\n"
    "    fn eq self:int other:int -> bool { self other == }\n"
    "}\n"
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
def find_ops(ops: list, kind: OpKind) -> list:
    return [op for op in ops if op.kind == kind]


# ---------------------------------------------------------------------------
# extract_generic_params
# ---------------------------------------------------------------------------
class TestExtractGenericParams:
    def test_single_param(self):
        assert extract_generic_params("array[int]") == ["int"]

    def test_single_param_list(self):
        assert extract_generic_params("List[int]") == ["int"]

    def test_two_params(self):
        assert extract_generic_params("Map[str int]") == ["str", "int"]

    def test_nested_param(self):
        assert extract_generic_params("Map[str List[int]]") == ["str", "List[int]"]

    def test_two_nested_params(self):
        assert extract_generic_params("Map[List[str] List[int]]") == [
            "List[str]",
            "List[int]",
        ]

    def test_option_param(self):
        assert extract_generic_params("option[int]") == ["int"]

    def test_fn_type_returns_none(self):
        assert extract_generic_params("fn[int -> int]") is None

    def test_bare_type_returns_none(self):
        assert extract_generic_params("int") is None

    def test_bare_generic_returns_none(self):
        assert extract_generic_params("fn") is None

    def test_set_single_param(self):
        assert extract_generic_params("Set[str]") == ["str"]

    def test_three_levels_deep(self):
        assert extract_generic_params("Map[str Map[int List[str]]]") == [
            "str",
            "Map[int List[str]]",
        ]


# ---------------------------------------------------------------------------
# split_type_tokens
# ---------------------------------------------------------------------------
class TestSplitTypeTokens:
    def test_simple_types(self):
        assert split_type_tokens("str int") == ["str", "int"]

    def test_single_type(self):
        assert split_type_tokens("int") == ["int"]

    def test_generic_type(self):
        assert split_type_tokens("str List[int]") == ["str", "List[int]"]

    def test_arrow(self):
        assert split_type_tokens("int -> int") == ["int", "->", "int"]

    def test_nested_brackets(self):
        assert split_type_tokens("Map[str List[int]]") == ["Map[str List[int]]"]

    def test_empty_string(self):
        assert split_type_tokens("") == []


# ---------------------------------------------------------------------------
# Trait parsing
# ---------------------------------------------------------------------------
class TestTraitParsing:
    def test_parse_trait_registers_globally(self):
        parse_string(HASHABLE_TRAIT)
        assert "Hashable" in GLOBAL_TRAITS

    def test_parse_trait_name(self):
        parse_string("trait Hashable { fn hash self:self -> int }")
        trait = GLOBAL_TRAITS["Hashable"]
        assert trait.name == "Hashable"

    def test_parse_trait_is_trait_type(self):
        parse_string("trait Hashable { fn hash self:self -> int }")
        trait = GLOBAL_TRAITS["Hashable"]
        assert isinstance(trait, Trait)

    def test_parse_trait_method_count(self):
        parse_string(HASHABLE_TRAIT)
        trait = GLOBAL_TRAITS["Hashable"]
        assert len(trait.methods) == 2

    def test_parse_trait_method_names(self):
        parse_string(HASHABLE_TRAIT)
        trait = GLOBAL_TRAITS["Hashable"]
        names = [m.name for m in trait.methods]
        assert names == ["hash", "eq"]

    def test_parse_trait_method_is_trait_method(self):
        parse_string("trait Hashable { fn hash self:self -> int }")
        trait = GLOBAL_TRAITS["Hashable"]
        assert isinstance(trait.methods[0], TraitMethod)

    def test_parse_trait_method_signature_params(self):
        parse_string(HASHABLE_TRAIT)
        trait = GLOBAL_TRAITS["Hashable"]
        hash_method = trait.methods[0]
        assert len(hash_method.signature.parameters) == 1
        assert hash_method.signature.parameters[0].typ == "self"
        assert hash_method.signature.parameters[0].name == "self"

    def test_parse_trait_method_signature_return(self):
        parse_string(HASHABLE_TRAIT)
        trait = GLOBAL_TRAITS["Hashable"]
        hash_method = trait.methods[0]
        assert hash_method.signature.return_types == ["int"]

    def test_parse_trait_eq_method_params(self):
        parse_string(HASHABLE_TRAIT)
        trait = GLOBAL_TRAITS["Hashable"]
        eq_method = trait.methods[1]
        assert len(eq_method.signature.parameters) == 2
        assert eq_method.signature.parameters[0].typ == "self"
        assert eq_method.signature.parameters[1].typ == "self"

    def test_parse_trait_eq_method_return(self):
        parse_string(HASHABLE_TRAIT)
        trait = GLOBAL_TRAITS["Hashable"]
        eq_method = trait.methods[1]
        assert eq_method.signature.return_types == ["bool"]

    def test_parse_empty_trait(self):
        parse_string("trait Empty { }")
        assert "Empty" in GLOBAL_TRAITS
        assert len(GLOBAL_TRAITS["Empty"].methods) == 0

    def test_parse_trait_single_method(self):
        parse_string("trait Printable { fn to_str self:self -> str }")
        trait = GLOBAL_TRAITS["Printable"]
        assert len(trait.methods) == 1
        assert trait.methods[0].name == "to_str"

    def test_parse_trait_unclosed_block_raises(self):
        with pytest.raises(CasaErrorCollection):
            parse_string("trait Foo { fn bar self:self -> int")

    def test_parse_trait_unexpected_token_raises(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string("trait Foo { 42 }")
        assert exc_info.value.errors[0].kind == ErrorKind.UNEXPECTED_TOKEN

    def test_parse_duplicate_trait_raises(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string(
                "trait Foo { fn bar self:self -> int }\n"
                "trait Foo { fn baz self:self -> int }\n"
            )
        assert exc_info.value.errors[0].kind == ErrorKind.DUPLICATE_NAME

    def test_parse_trait_has_location(self):
        parse_string("trait MyTrait { fn do_thing self:self -> int }")
        trait = GLOBAL_TRAITS["MyTrait"]
        assert trait.location is not None


# ---------------------------------------------------------------------------
# Trait bounds parsing (parse_type_vars)
# ---------------------------------------------------------------------------
class TestTraitBoundsParsing:
    def test_trait_bound_basic(self):
        parse_string(
            "trait Hashable { fn hash self:self -> int }\n"
            "fn test_fn[K: Hashable] K -> int { .hash }"
        )
        fn = GLOBAL_FUNCTIONS["test_fn"]
        assert fn.signature is not None
        assert "K" in fn.signature.type_vars
        assert fn.signature.trait_bounds == {"K": "Hashable"}

    def test_trait_bound_with_unbounded_var(self):
        parse_string(
            "trait Hashable { fn hash self:self -> int }\n"
            "fn test_fn[K: Hashable, V] K V -> int { drop .hash }"
        )
        fn = GLOBAL_FUNCTIONS["test_fn"]
        assert fn.signature.type_vars == {"K", "V"}
        assert fn.signature.trait_bounds == {"K": "Hashable"}

    def test_trait_bound_multiple_bounded(self):
        parse_string(
            "trait Hashable { fn hash self:self -> int }\n"
            "trait Printable { fn to_str self:self -> str }\n"
            "fn test_fn[K: Hashable, V: Printable] K V -> int { .to_str drop .hash }"
        )
        fn = GLOBAL_FUNCTIONS["test_fn"]
        assert fn.signature.trait_bounds == {"K": "Hashable", "V": "Printable"}

    def test_trait_bound_undefined_trait_raises(self):
        with pytest.raises(CasaErrorCollection) as exc_info:
            parse_string("fn test_fn[K: Nonexistent] K -> int { 0 }")
        assert exc_info.value.errors[0].kind == ErrorKind.UNDEFINED_NAME

    def test_trait_bound_hidden_params_prepended(self):
        parse_string(
            "trait Hashable { fn hash self:self -> int }\n"
            "fn test_fn[K: Hashable] K -> int { .hash }"
        )
        fn = GLOBAL_FUNCTIONS["test_fn"]
        sig = fn.signature
        hidden = [p for p in sig.parameters if p.name and p.name.startswith("__trait_")]
        assert len(hidden) == 1
        assert hidden[0].name == "__trait_K_hash"
        assert hidden[0].typ == "fn[K -> int]"

    def test_trait_bound_two_methods_hidden_params(self):
        parse_string(HASHABLE_TRAIT + "fn test_fn[K: Hashable] K -> int { .hash }")
        fn = GLOBAL_FUNCTIONS["test_fn"]
        sig = fn.signature
        hidden = [p for p in sig.parameters if p.name and p.name.startswith("__trait_")]
        assert len(hidden) == 2
        names = [p.name for p in hidden]
        assert "__trait_K_hash" in names
        assert "__trait_K_eq" in names

    def test_trait_bound_hidden_params_have_correct_types(self):
        parse_string(HASHABLE_TRAIT + "fn test_fn[K: Hashable] K -> int { .hash }")
        fn = GLOBAL_FUNCTIONS["test_fn"]
        hidden = {
            p.name: p.typ
            for p in fn.signature.parameters
            if p.name and p.name.startswith("__trait_")
        }
        assert hidden["__trait_K_hash"] == "fn[K -> int]"
        assert hidden["__trait_K_eq"] == "fn[K K -> bool]"

    def test_trait_bound_creates_variables_for_hidden_params(self):
        parse_string(
            "trait Hashable { fn hash self:self -> int }\n"
            "fn test_fn[K: Hashable] K -> int { .hash }"
        )
        fn = GLOBAL_FUNCTIONS["test_fn"]
        var_names = [v.name for v in fn.variables]
        assert "__trait_K_hash" in var_names

    def test_trait_bound_creates_assign_ops_for_hidden_params(self):
        parse_string(
            "trait Hashable { fn hash self:self -> int }\n"
            "fn test_fn[K: Hashable] K -> int { .hash }"
        )
        fn = GLOBAL_FUNCTIONS["test_fn"]
        assign_ops = find_ops(fn.ops, OpKind.ASSIGN_VARIABLE)
        assign_values = [op.value for op in assign_ops]
        assert "__trait_K_hash" in assign_values


# ---------------------------------------------------------------------------
# Structural trait satisfaction (type_satisfies_trait)
# ---------------------------------------------------------------------------
class TestTypeSatisfiesTrait:
    def _setup_hashable_with_foo(self):
        """Set up the Hashable trait and a Foo struct with impl."""
        parse_string(
            HASHABLE_TRAIT
            + "struct Foo { val: int }\n"
            + "impl Foo { fn hash self:Foo -> int { self Foo::val } fn eq self:Foo other:Foo -> bool { self Foo::val other Foo::val == } }\n"
        )

    def test_struct_with_impl_satisfies(self):
        from casa.typechecker import type_satisfies_trait

        self._setup_hashable_with_foo()
        assert type_satisfies_trait("Foo", "Hashable") is True

    def test_struct_without_impl_does_not_satisfy(self):
        from casa.typechecker import type_satisfies_trait

        parse_string(HASHABLE_TRAIT + "struct Bar { val: int }\n")
        assert type_satisfies_trait("Bar", "Hashable") is False

    def test_partial_impl_does_not_satisfy(self):
        from casa.typechecker import type_satisfies_trait

        parse_string(
            HASHABLE_TRAIT
            + "struct Baz { val: int }\n"
            + "impl Baz { fn hash self:Baz -> int { self Baz::val } }\n"
        )
        assert type_satisfies_trait("Baz", "Hashable") is False

    def test_undefined_trait_returns_false(self):
        from casa.typechecker import type_satisfies_trait

        assert type_satisfies_trait("int", "Nonexistent") is False

    def test_type_var_with_matching_bound_satisfies(self):
        from casa.typechecker import type_satisfies_trait

        parse_string(HASHABLE_TRAIT)
        fn = Function(
            name="test_fn",
            ops=[],
            location=None,  # type: ignore[arg-type]
            signature=Signature(
                parameters=[],
                return_types=[],
                type_vars={"K"},
                trait_bounds={"K": "Hashable"},
            ),
        )
        assert type_satisfies_trait("K", "Hashable", fn) is True

    def test_type_var_without_bound_does_not_satisfy(self):
        from casa.typechecker import type_satisfies_trait

        parse_string(HASHABLE_TRAIT)
        fn = Function(
            name="test_fn",
            ops=[],
            location=None,  # type: ignore[arg-type]
            signature=Signature(
                parameters=[],
                return_types=[],
                type_vars={"K"},
                trait_bounds={},
            ),
        )
        assert type_satisfies_trait("K", "Hashable", fn) is False

    def test_type_var_with_wrong_bound_does_not_satisfy(self):
        from casa.typechecker import type_satisfies_trait

        parse_string(
            HASHABLE_TRAIT + "trait Printable { fn to_str self:self -> str }\n"
        )
        fn = Function(
            name="test_fn",
            ops=[],
            location=None,  # type: ignore[arg-type]
            signature=Signature(
                parameters=[],
                return_types=[],
                type_vars={"K"},
                trait_bounds={"K": "Printable"},
            ),
        )
        assert type_satisfies_trait("K", "Hashable", fn) is False


# ---------------------------------------------------------------------------
# Trait method resolution via dot syntax on type variables
# ---------------------------------------------------------------------------
class TestTraitMethodCallDotSyntax:
    def test_dot_hash_on_bounded_type_var(self):
        """Calling .hash on a trait-bounded type variable should typecheck."""
        typecheck_string(
            HASHABLE_TRAIT + "fn get_hash[K: Hashable] k:K -> int { k .hash }\n"
        )
        fn = GLOBAL_FUNCTIONS["get_hash"]
        assert fn.signature is not None

    def test_dot_eq_on_bounded_type_var(self):
        """Calling .eq on a trait-bounded type variable should typecheck."""
        typecheck_string(
            HASHABLE_TRAIT + "fn are_eq[K: Hashable] a:K b:K -> bool { b a .eq }\n"
        )
        fn = GLOBAL_FUNCTIONS["are_eq"]
        assert fn.signature is not None


# ---------------------------------------------------------------------------
# Trait ref resolution (K::hash, &K::hash)
# ---------------------------------------------------------------------------
class TestTraitRefResolution:
    def test_k_coloncolon_method_resolved_as_push_variable_exec(self):
        """K::hash inside a trait-bounded fn resolves to PUSH_VARIABLE + FN_EXEC."""
        code = (
            HASHABLE_TRAIT
            + "struct MyType { val: int }\n"
            + "impl MyType {\n"
            + "    fn hash self:MyType -> int { self MyType::val }\n"
            + "    fn eq self:MyType other:MyType -> bool { self MyType::val other MyType::val == }\n"
            + "}\n"
            + "fn get_hash[K: Hashable] k:K -> int { k K::hash }\n"
            + "42 MyType = m\n"
            + "m get_hash\n"
        )
        resolve_string(code)
        fn = GLOBAL_FUNCTIONS["get_hash"]
        push_var_ops = find_ops(fn.ops, OpKind.PUSH_VARIABLE)
        push_var_values = [op.value for op in push_var_ops]
        assert "__trait_K_hash" in push_var_values
        exec_ops = find_ops(fn.ops, OpKind.FN_EXEC)
        assert len(exec_ops) >= 1

    def test_ampersand_k_coloncolon_method_resolved_as_push_variable(self):
        """&K::hash inside a trait-bounded fn resolves to PUSH_VARIABLE (fn ref)."""
        code = (
            HASHABLE_TRAIT
            + "struct MyType { val: int }\n"
            + "impl MyType {\n"
            + "    fn hash self:MyType -> int { self MyType::val }\n"
            + "    fn eq self:MyType other:MyType -> bool { self MyType::val other MyType::val == }\n"
            + "}\n"
            + "fn get_hash_fn[K: Hashable] -> fn[K -> int] { &K::hash }\n"
            + "42 MyType = m\n"
            + "m get_hash_fn\n"
        )
        resolve_string(code)
        fn = GLOBAL_FUNCTIONS["get_hash_fn"]
        push_var_ops = find_ops(fn.ops, OpKind.PUSH_VARIABLE)
        push_var_values = [op.value for op in push_var_ops]
        assert "__trait_K_hash" in push_var_values


# ---------------------------------------------------------------------------
# Auto-injection of FN_PUSH ops for trait-bounded calls
# ---------------------------------------------------------------------------
class TestTraitAutoInjection:
    def test_calling_trait_bounded_fn_injects_fn_push(self):
        """Calling a trait-bounded function with a concrete type should auto-inject FN_PUSH."""
        code = (
            HASHABLE_TRAIT
            + "struct MyType { val: int }\n"
            + "impl MyType {\n"
            + "    fn hash self:MyType -> int { self MyType::val }\n"
            + "    fn eq self:MyType other:MyType -> bool { self MyType::val other MyType::val == }\n"
            + "}\n"
            + "fn get_hash[K: Hashable] k:K -> int { k .hash }\n"
            + "42 MyType = m\n"
            + "m get_hash\n"
        )
        typecheck_string(code)

    def test_trait_bounded_fn_with_str(self):
        """str satisfies Hashable, so calling a bounded fn with str should work."""
        code = (
            HASHABLE_TRAIT
            + HASHABLE_IMPL_STR
            + "fn get_hash[K: Hashable] k:K -> int { k .hash }\n"
            + '"hello" get_hash\n'
        )
        typecheck_string(code)

    def test_trait_bounded_fn_with_int(self):
        """int satisfies Hashable, so calling a bounded fn with int should work."""
        code = (
            HASHABLE_TRAIT
            + HASHABLE_IMPL_INT
            + "fn get_hash[K: Hashable] k:K -> int { k .hash }\n"
            + "42 get_hash\n"
        )
        typecheck_string(code)

    def test_trait_bounded_fn_with_non_satisfying_type_raises(self):
        """Calling a trait-bounded fn with a type that does not satisfy the trait should error."""
        code = (
            HASHABLE_TRAIT
            + "struct NoHash { val: int }\n"
            + "fn get_hash[K: Hashable] k:K -> int { k .hash }\n"
            + "42 NoHash = m\n"
            + "m get_hash\n"
        )
        with pytest.raises(CasaErrorCollection) as exc_info:
            typecheck_string(code)
        assert exc_info.value.errors[0].kind == ErrorKind.MISSING_TRAIT_METHOD


# ---------------------------------------------------------------------------
# Signature.trait_bounds field
# ---------------------------------------------------------------------------
class TestSignatureTraitBounds:
    def test_trait_bounds_default_empty(self):
        sig = Signature([], [])
        assert sig.trait_bounds == {}

    def test_trait_bounds_preserved_in_signature(self):
        sig = Signature([], [], type_vars={"K"}, trait_bounds={"K": "Hashable"})
        assert sig.trait_bounds == {"K": "Hashable"}


# ---------------------------------------------------------------------------
# Multi-parameter generic types in function signatures
# ---------------------------------------------------------------------------
class TestMultiParamGenerics:
    def test_parse_map_type_in_signature(self):
        parse_string(
            "trait Hashable { fn hash self:self -> int }\n"
            "struct MapStruct { data: ptr }\n"
            "fn test_fn m:MapStruct -> int { 0 }"
        )

    def test_map_kv_type_params(self):
        params = extract_generic_params("Map[str int]")
        assert params == ["str", "int"]

    def test_set_k_type_params(self):
        params = extract_generic_params("Set[str]")
        assert params == ["str"]

    def test_nested_generic_params(self):
        params = extract_generic_params("Map[str List[int]]")
        assert params == ["str", "List[int]"]


# ---------------------------------------------------------------------------
# Integration: compile and run Map/Set programs
# ---------------------------------------------------------------------------
class TestMapSetIntegration:
    """Integration tests that compile and run Casa programs with Map and Set."""

    @pytest.fixture
    def run_casa(self, tmp_path):
        """Fixture to compile and run a Casa program, returning stdout."""

        def _run(code: str) -> str:
            src = tmp_path / "test.casa"
            src.write_text(code)
            binary = tmp_path / "test"
            result = subprocess.run(
                ["python3", "casa.py", str(src), "-o", str(binary)],
                capture_output=True,
                text=True,
                cwd=CASA_ROOT,
                timeout=30,
            )
            if result.returncode != 0:
                pytest.fail(
                    f"Compilation failed:\nstdout: {result.stdout}\nstderr: {result.stderr}"
                )
            run_result = subprocess.run(
                [str(binary)],
                capture_output=True,
                text=True,
                timeout=10,
            )
            return run_result.stdout

        return _run

    def test_map_str_int_set_and_get(self, run_casa):
        output = run_casa(
            STD_INCLUDE
            + "Map::new (Map[str int]) = m\n"
            + '"hello" 42 m.set = m\n'
            + '"hello" m.get .unwrap print\n'
        )
        assert output == "42"

    def test_map_int_str_set_and_get(self, run_casa):
        output = run_casa(
            STD_INCLUDE
            + "Map::new (Map[int str]) = m\n"
            + '1 "hello" m.set = m\n'
            + "1 m.get .unwrap print\n"
        )
        assert output == "hello"

    def test_map_length(self, run_casa):
        output = run_casa(
            STD_INCLUDE
            + "Map::new (Map[str int]) = m\n"
            + '"a" 1 m.set = m\n'
            + '"b" 2 m.set = m\n'
            + "m.length print\n"
        )
        assert output == "2"

    def test_map_has(self, run_casa):
        output = run_casa(
            STD_INCLUDE
            + "Map::new (Map[str int]) = m\n"
            + '"a" 1 m.set = m\n'
            + '"a" m.has print\n'
            + '"b" m.has print\n'
        )
        assert output == "truefalse"

    def test_map_delete(self, run_casa):
        output = run_casa(
            STD_INCLUDE
            + "Map::new (Map[str int]) = m\n"
            + '"a" 1 m.set = m\n'
            + '"a" m.delete = m\n'
            + "m.length print\n"
            + '"a" m.has print\n'
        )
        assert output == "0false"

    def test_set_add_and_has(self, run_casa):
        output = run_casa(
            STD_INCLUDE
            + "Set::new (Set[str]) = s\n"
            + '"apple" s.add = s\n'
            + '"apple" s.has print\n'
            + '"banana" s.has print\n'
        )
        assert output == "truefalse"

    def test_set_length(self, run_casa):
        output = run_casa(
            STD_INCLUDE
            + "Set::new (Set[str]) = s\n"
            + '"a" s.add = s\n'
            + '"b" s.add = s\n'
            + '"c" s.add = s\n'
            + "s.length print\n"
        )
        assert output == "3"

    def test_set_remove(self, run_casa):
        output = run_casa(
            STD_INCLUDE
            + "Set::new (Set[str]) = s\n"
            + '"a" s.add = s\n'
            + '"a" s.remove = s\n'
            + "s.length print\n"
            + '"a" s.has print\n'
        )
        assert output == "0false"

    def test_map_update_existing_key(self, run_casa):
        output = run_casa(
            STD_INCLUDE
            + "Map::new (Map[str int]) = m\n"
            + '"a" 1 m.set = m\n'
            + '"a" 99 m.set = m\n'
            + '"a" m.get .unwrap print\n'
            + "m.length print\n"
        )
        assert output == "991"

    def test_set_add_duplicate(self, run_casa):
        output = run_casa(
            STD_INCLUDE
            + "Set::new (Set[str]) = s\n"
            + '"a" s.add = s\n'
            + '"a" s.add = s\n'
            + "s.length print\n"
        )
        assert output == "1"
