"""End-to-end tests for array map, filter, reduce methods.

These tests verify that programs using map/filter/reduce compile through
the full pipeline (lex -> parse -> resolve -> typecheck -> bytecode -> emit).
"""

from tests.conftest import compile_string, emit_string, typecheck_string

STD_INCLUDE = 'include "lib/std.casa"\n'


# ---------------------------------------------------------------------------
# map
# ---------------------------------------------------------------------------
def test_e2e_map_double():
    """map that doubles each element compiles through the full pipeline."""
    code = STD_INCLUDE + """
    { 2 * } [1, 2, 3] .map = result
    0 result array::nth print
    """
    emit_string(code)


def test_e2e_map_negate():
    """map that negates each element compiles through the full pipeline."""
    code = STD_INCLUDE + """
    { 0 swap - } [1, 2, 3] .map = result
    0 result array::nth print
    """
    emit_string(code)


def test_e2e_map_to_bool():
    """map that transforms int to bool compiles correctly."""
    code = STD_INCLUDE + """
    { 0 > } [1, -2, 3] .map = result
    result array::length print
    """
    emit_string(code)


# ---------------------------------------------------------------------------
# filter
# ---------------------------------------------------------------------------
def test_e2e_filter_positive():
    """filter that keeps positive numbers compiles through the full pipeline."""
    code = STD_INCLUDE + """
    { 0 > } [1, -2, 3, -4, 5] .filter = result
    result array::length print
    """
    emit_string(code)


def test_e2e_filter_even():
    """filter that keeps even numbers compiles through the full pipeline."""
    code = STD_INCLUDE + """
    { 2 % 0 == } [1, 2, 3, 4, 5, 6] .filter = result
    result array::length print
    """
    emit_string(code)


# ---------------------------------------------------------------------------
# reduce
# ---------------------------------------------------------------------------
def test_e2e_reduce_sum():
    """reduce that sums an array compiles through the full pipeline."""
    code = STD_INCLUDE + """
    { + } 0 [1, 2, 3, 4, 5] .reduce print
    """
    emit_string(code)


def test_e2e_reduce_product():
    """reduce that computes product compiles through the full pipeline."""
    code = STD_INCLUDE + """
    { * } 1 [1, 2, 3, 4, 5] .reduce print
    """
    emit_string(code)


# ---------------------------------------------------------------------------
# Chaining
# ---------------------------------------------------------------------------
def test_e2e_map_then_filter():
    """map followed by filter compiles through the full pipeline."""
    code = STD_INCLUDE + """
    { 2 * } [1, 2, 3, 4, 5] .map = doubled
    { 6 > } doubled .filter = big
    big array::length print
    """
    emit_string(code)


def test_e2e_map_then_reduce():
    """map followed by reduce compiles through the full pipeline."""
    code = STD_INCLUDE + """
    { 2 * } [1, 2, 3] .map = doubled
    { + } 0 doubled .reduce print
    """
    emit_string(code)


def test_e2e_filter_then_reduce():
    """filter followed by reduce compiles through the full pipeline."""
    code = STD_INCLUDE + """
    { 0 > } [1, -2, 3, -4, 5] .filter = positive
    { + } 0 positive .reduce print
    """
    emit_string(code)
