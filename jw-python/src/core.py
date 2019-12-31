"""Core function definitions for mal."""

import operator
from functools import reduce
from typing import Any, Callable, Dict, Iterator, List

from mal_errors import EvalError

from mal_types import MalAny, MalBool, MalFunc, MalList, MalNil, MalNum, MalSeq, MalStr

from printer import pr_str


def create_ns() -> Dict[str, MalAny]:
    """Return the core namespace."""  # Within a function so can precede the definitions
    return {
        "+": MalFunc(addition),
        "*": MalFunc(multiplication),
        "-": MalFunc(subtraction),
        "/": MalFunc(division),
        "=": MalFunc(equality),
        ">": make_num_logical(operator.gt),
        "<": make_num_logical(operator.lt),
        ">=": make_num_logical(operator.ge),
        "<=": make_num_logical(operator.le),
        "list": MalFunc(mal_list),
        "list?": MalFunc(list_test),
        "count": MalFunc(count),
        "empty?": MalFunc(empty_test),
        "pr-str": MalFunc(mal_pr_str),
        "str": MalFunc(mal_str),
        "prn": MalFunc(mal_prn),
        "println": MalFunc(mal_println),
    }


def addition(args: List[MalAny]) -> MalNum:
    """Python definition of mal + function."""
    if all_isinstance(args, MalNum):
        num_values: Iterator[int] = map(extract_num, args)
        return MalNum(sum(num_values))
    raise EvalError("Bad arguments to +")


def multiplication(args: List[MalAny]) -> MalNum:
    """Python definition of mal * function."""
    if all_isinstance(args, MalNum):
        num_values: Iterator[int] = map(extract_num, args)
        return MalNum(reduce(lambda x, y: x * y, num_values, 1))
    raise EvalError("Bad arguments to *")


def subtraction(args: List[MalAny]) -> MalNum:
    """Python definition of mal - function."""
    if all_isinstance(args, MalNum) and len(args) == 2:
        [x, y] = map(extract_num, args)
        return MalNum(x - y)
    raise EvalError("Bad arguments to -")


def division(args: List[MalAny]) -> MalNum:
    """Python definition of mal - function."""
    if all_isinstance(args, MalNum) and len(args) == 2:
        [x, y] = map(extract_num, args)
        return MalNum(x // y)
    raise EvalError("Bad arguments to /")


def equality(args: List[MalAny]) -> MalBool:
    """Python definition of mal = function."""
    if len(args) == 2:
        return MalBool(args[0] == args[1])
    raise EvalError("Bad arguments to /")


def make_num_logical(op: Callable[[int, int], bool]) -> MalFunc:
    """Return the python function for a mal logical comparison on MalNums."""

    def f(args: List[MalAny]) -> MalBool:
        if all_isinstance(args, MalNum) and len(args) == 2:
            [x, y] = map(extract_num, args)
            return MalBool(op(x, y))
        raise EvalError("Bad arguments to logical comparison")

    return MalFunc(f)


def mal_list(args: List[MalAny]) -> MalList:
    """Python definition of mal list function."""
    return MalList(args)


def list_test(args: List[MalAny]) -> MalBool:
    """Python definition of mal list? function."""
    if len(args) == 1:
        return MalBool(isinstance(args[0], MalList))
    raise EvalError("Bad arguments to list?")


def count(args: List[MalAny]) -> MalNum:
    """Python definition of mal count function."""
    if len(args) == 1:
        head = args[0]
        if isinstance(head, MalSeq):
            return MalNum(len(head.value))
        if isinstance(head, MalNil):
            return MalNum(0)
    raise EvalError("Bad arguments to count")


def empty_test(args: List[MalAny]) -> MalBool:
    """Python definition of mal empty? function."""
    if len(args) == 1:
        head = args[0]
        if isinstance(head, MalSeq):
            return MalBool(len(head.value) == 0)
    raise EvalError("Bad arguments to empty?")


def mal_pr_str(args: List[MalAny]) -> MalStr:
    """Python definition of mal pr-str function."""
    str_args = map(lambda x: pr_str(x, print_readably=True), args)
    return MalStr(" ".join(str_args))


def mal_str(args: List[MalAny]) -> MalStr:
    """Python definition of mal str function."""
    str_args = map(lambda x: pr_str(x, print_readably=False), args)
    return MalStr("".join(str_args))


def mal_prn(args: List[MalAny]) -> MalNil:
    """Python definition of mal prn function."""
    str_args = map(lambda x: pr_str(x, print_readably=True), args)
    print(" ".join(str_args))
    return MalNil()


def mal_println(args: List[MalAny]) -> MalNil:
    """Python definition of mal println function."""
    str_args = map(lambda x: pr_str(x, print_readably=False), args)
    print(" ".join(str_args))
    return MalNil()


def all_isinstance(xs: List[Any], t: type) -> bool:
    """Test if all list items are instances of the given type."""
    return all(map(lambda x: isinstance(x, t), xs))


def extract_num(x: MalAny) -> int:
    """Extract value from a MalNum as an int."""
    if isinstance(x, MalNum):
        return x.value
    raise EvalError("Expected a number", str(x))
