"""Core function definitions for mal."""

import operator
from functools import reduce
from typing import Any, Callable, Dict, Iterator, List

import mal_errors

from mal_types import (
    MalAny,
    MalBool,
    MalBuiltin,
    MalList,
    MalNil,
    MalNum,
    MalSeq,
    MalStr,
)

import printer

import reader


def create_ns() -> Dict[str, MalAny]:
    """Return the core namespace."""  # Within a function so can precede the definitions
    return {
        # Arithmetic and numeric comparisons
        "+": MalBuiltin(addition),
        "*": MalBuiltin(multiplication),
        "-": MalBuiltin(subtraction),
        "/": MalBuiltin(division),
        "=": MalBuiltin(equality),
        ">": make_num_logical(operator.gt),
        "<": make_num_logical(operator.lt),
        ">=": make_num_logical(operator.ge),
        "<=": make_num_logical(operator.le),
        # Collection functions
        "list": MalBuiltin(mal_list),
        "list?": MalBuiltin(list_test),
        "count": MalBuiltin(count),
        "empty?": MalBuiltin(empty_test),
        # IO and string functions
        "pr-str": MalBuiltin(mal_pr_str),
        "str": MalBuiltin(mal_str),
        "prn": MalBuiltin(mal_prn),
        "println": MalBuiltin(mal_println),
        "read-string": MalBuiltin(mal_read_string),
    }


#
# Arithmetic and numeric comparisons
#


def addition(args: List[MalAny]) -> MalNum:
    """Python definition of mal + function."""
    if all_isinstance(args, MalNum):
        num_values: Iterator[int] = map(extract_num, args)
        return MalNum(sum(num_values))
    raise mal_errors.EvalError("Bad arguments to +")


def multiplication(args: List[MalAny]) -> MalNum:
    """Python definition of mal * function."""
    if all_isinstance(args, MalNum):
        num_values: Iterator[int] = map(extract_num, args)
        return MalNum(reduce(lambda x, y: x * y, num_values, 1))
    raise mal_errors.EvalError("Bad arguments to *")


def subtraction(args: List[MalAny]) -> MalNum:
    """Python definition of mal - function."""
    if all_isinstance(args, MalNum) and len(args) == 2:
        [x, y] = map(extract_num, args)
        return MalNum(x - y)
    raise mal_errors.EvalError("Bad arguments to -")


def division(args: List[MalAny]) -> MalNum:
    """Python definition of mal - function."""
    if all_isinstance(args, MalNum) and len(args) == 2:
        [x, y] = map(extract_num, args)
        return MalNum(x // y)
    raise mal_errors.EvalError("Bad arguments to /")


def equality(args: List[MalAny]) -> MalBool:
    """Python definition of mal = function."""
    if len(args) == 2:
        return MalBool(args[0] == args[1])
    raise mal_errors.EvalError("Bad arguments to /")


def make_num_logical(op: Callable[[int, int], bool]) -> MalBuiltin:
    """Return the python function for a mal logical comparison on MalNums."""

    def f(args: List[MalAny]) -> MalBool:
        if all_isinstance(args, MalNum) and len(args) == 2:
            [x, y] = map(extract_num, args)
            return MalBool(op(x, y))
        raise mal_errors.EvalError("Bad arguments to logical comparison")

    return MalBuiltin(f)


#
# Collection functions
#


def mal_list(args: List[MalAny]) -> MalList:
    """Python definition of mal list function."""
    return MalList(args)


def list_test(args: List[MalAny]) -> MalBool:
    """Python definition of mal list? function."""
    if len(args) == 1:
        return MalBool(isinstance(args[0], MalList))
    raise mal_errors.EvalError("Bad arguments to list?")


def count(args: List[MalAny]) -> MalNum:
    """Python definition of mal count function."""
    if len(args) == 1:
        head = args[0]
        if isinstance(head, MalSeq):
            return MalNum(len(head.value))
        if isinstance(head, MalNil):
            return MalNum(0)
    raise mal_errors.EvalError("Bad arguments to count")


def empty_test(args: List[MalAny]) -> MalBool:
    """Python definition of mal empty? function."""
    if len(args) == 1:
        head = args[0]
        if isinstance(head, MalSeq):
            return MalBool(len(head.value) == 0)
    raise mal_errors.EvalError("Bad arguments to empty?")


#
# IO and string functions
#


def mal_pr_str(args: List[MalAny]) -> MalStr:
    """Python definition of mal pr-str function."""
    str_args = map(lambda x: printer.pr_str(x, print_readably=True), args)
    return MalStr(" ".join(str_args))


def mal_str(args: List[MalAny]) -> MalStr:
    """Python definition of mal str function."""
    str_args = map(lambda x: printer.pr_str(x, print_readably=False), args)
    return MalStr("".join(str_args))


def mal_prn(args: List[MalAny]) -> MalNil:
    """Python definition of mal prn function."""
    str_args = map(lambda x: printer.pr_str(x, print_readably=True), args)
    print(" ".join(str_args))
    return MalNil()


def mal_println(args: List[MalAny]) -> MalNil:
    """Python definition of mal println function."""
    str_args = map(lambda x: printer.pr_str(x, print_readably=False), args)
    print(" ".join(str_args))
    return MalNil()


def mal_read_string(args: List[MalAny]) -> MalAny:
    """Python definition of mal read-string function."""
    if len(args) == 1:
        head = args[0]
        if isinstance(head, MalStr):
            return reader.read_str(head.value)
    raise mal_errors.EvalError("Bad arguments to read-string")


#
# Helper functions
#


def all_isinstance(xs: List[Any], t: type) -> bool:
    """Test if all list items are instances of the given type."""
    return all(map(lambda x: isinstance(x, t), xs))


def extract_num(x: MalAny) -> int:
    """Extract value from a MalNum as an int."""
    if isinstance(x, MalNum):
        return x.value
    raise mal_errors.EvalError("Expected a number", str(x))
