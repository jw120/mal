"""Core function definitions"""

from functools import reduce
import operator
from typing import Any, Callable, Dict, Iterator, List

from mal_errors import EvalError
from mal_types import MalAny, MalBool, MalFunc, MalNum


def addition(args: List[MalAny]) -> MalNum:
    """Python definition of mal + operator"""
    if all_isinstance(args, MalNum):
        num_values: Iterator[int] = map(extract_num, args)
        return MalNum(sum(num_values))
    raise EvalError("Bad arguments to +")


def multiplication(args: List[MalAny]) -> MalNum:
    """Python definition of mal * operator"""
    if all_isinstance(args, MalNum):
        num_values: Iterator[int] = map(extract_num, args)
        return MalNum(reduce(lambda x, y: x * y, num_values, 1))
    raise EvalError("Bad arguments to *")


def subtraction(args: List[MalAny]) -> MalNum:
    """Python definition of mal - operator"""
    if all_isinstance(args, MalNum) and len(args) == 2:
        [x, y] = map(extract_num, args)
        return MalNum(x - y)
    raise EvalError("Bad arguments to -")


def division(args: List[MalAny]) -> MalNum:
    """Python definition of mal - operator"""
    if all_isinstance(args, MalNum) and len(args) == 2:
        [x, y] = map(extract_num, args)
        return MalNum(x // y)
    raise EvalError("Bad arguments to /")


def equality(args: List[MalAny]) -> MalBool:
    """Python definition of mal = operator"""
    if len(args) == 2:
        return MalBool(args[0] == args[1])
    raise EvalError("Bad arguments to /")


def make_num_logical(op: Callable[[int, int], bool]) -> MalFunc:
    """Returns the python function for a mal logical comparison on MalNums"""

    def f(args: List[MalAny]) -> MalBool:
        if all_isinstance(args, MalNum) and len(args) == 2:
            [x, y] = map(extract_num, args)
            return MalBool(op(x, y))
        raise EvalError("Bad arguments to logical comparison")

    return MalFunc(f)


ns: Dict[str, MalAny] = {
    "+": MalFunc(addition),
    "*": MalFunc(multiplication),
    "-": MalFunc(subtraction),
    "/": MalFunc(division),
    "=": MalFunc(equality),
    ">": make_num_logical(operator.gt),
    "<": make_num_logical(operator.lt),
    ">=": make_num_logical(operator.ge),
    "<=": make_num_logical(operator.le),
}


def all_isinstance(xs: List[Any], t: type) -> bool:
    """Helper function to abbreviate checking the class of a list"""
    return all(map(lambda x: isinstance(x, t), xs))


def extract_num(x: MalAny) -> int:
    """Helper function to extract the value from a MalNum"""
    if isinstance(x, MalNum):
        return x.value
    raise EvalError("Expected a number", str(x))
