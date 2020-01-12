"""Core function definitions for mal.

Functions defined here are in pure python and have no access to the
environment.
"""

import operator
from functools import reduce
from typing import Callable, Dict, List, NoReturn, cast

import mal_errors

from mal_types import MalAny, MalAtom, MalBuiltin, MalList, MalNil, MalSeq

import printer

import reader


def create_ns() -> Dict[str, MalBuiltin]:
    """Create and return the core namespace.

    Provided as a function so it can precede the definitions of the functions
    """
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
        "cons": MalBuiltin(cons),
        "concat": MalBuiltin(mal_concat),
        "nth": MalBuiltin(nth),
        "first": MalBuiltin(first),
        "rest": MalBuiltin(rest),
        # IO and string functions
        "pr-str": MalBuiltin(mal_pr_str),
        "str": MalBuiltin(mal_str),
        "prn": MalBuiltin(mal_prn),
        "println": MalBuiltin(mal_println),
        "read-string": MalBuiltin(mal_read_string),
        "slurp": MalBuiltin(slurp),
        # Atom functions
        "atom": MalBuiltin(atom),
        "atom?": MalBuiltin(atom_test),
        "deref": MalBuiltin(deref),
        "reset!": MalBuiltin(reset),
        # Other functions
        "throw": MalBuiltin(mal_throw),
    }


#
# Arithmetic and numeric comparisons
#


def addition(args: List[MalAny]) -> int:
    """Python definition of mal + function."""
    if all(map(lambda x: isinstance(x, int), args)):
        return sum(cast(List[int], args))
    raise mal_errors.EvalError("Bad arguments to +")


def multiplication(args: List[MalAny]) -> int:
    """Python definition of mal * function."""
    if all(map(lambda x: isinstance(x, int), args)):
        return reduce(lambda x, y: x * y, cast(List[int], args), 1)
    raise mal_errors.EvalError("Bad arguments to *")


def subtraction(args: List[MalAny]) -> int:
    """Python definition of mal - function."""
    if len(args) == 2 and isinstance(args[0], int) and isinstance(args[1], int):
        return args[0] - args[1]
    raise mal_errors.EvalError("Bad arguments to -")


def division(args: List[MalAny]) -> int:
    """Python definition of mal / function."""
    if len(args) == 2 and isinstance(args[0], int) and isinstance(args[1], int):
        return args[0] // args[1]
    raise mal_errors.EvalError("Bad arguments to /")


def equality(args: List[MalAny]) -> bool:
    """Python definition of mal = function."""
    if len(args) == 2:
        return args[0] == args[1]
    raise mal_errors.EvalError("Bad arguments to ==")


def make_num_logical(op: Callable[[int, int], bool]) -> MalBuiltin:
    """Return the python function for a mal logical comparison on ints."""

    def f(args: List[MalAny]) -> bool:
        if len(args) == 2 and isinstance(args[0], int) and isinstance(args[1], int):
            return op(args[0], args[1])
        raise mal_errors.EvalError("Bad arguments to logical comparison")

    return MalBuiltin(f)


#
# Collection functions
#


def mal_list(args: List[MalAny]) -> MalList:
    """Python definition of mal list function."""
    return MalList(args)


def list_test(args: List[MalAny]) -> bool:
    """Python definition of mal list? function."""
    if len(args) == 1:
        return isinstance(args[0], MalList)
    raise mal_errors.EvalError("Bad arguments to list?")


def count(args: List[MalAny]) -> int:
    """Python definition of mal count function."""
    if len(args) == 1:
        head = args[0]
        if isinstance(head, MalSeq):
            return len(head.value)
        if isinstance(head, MalNil):
            return 0
    raise mal_errors.EvalError("Bad arguments to count")


def empty_test(args: List[MalAny]) -> bool:
    """Python definition of mal empty? function."""
    if len(args) == 1:
        head = args[0]
        if isinstance(head, MalSeq):
            return len(head.value) == 0
    raise mal_errors.EvalError("Bad arguments to empty?")


def cons(args: List[MalAny]) -> MalList:
    """Python definition of mal cons function."""
    if len(args) == 2 and isinstance(args[1], MalSeq):
        return MalList([args[0]] + args[1].value)
    raise mal_errors.EvalError("Bad arguments to cons")


def mal_concat(args: List[MalAny]) -> MalList:
    """Python definition of mal concat function."""

    def contents(m: MalAny) -> List[MalAny]:
        if isinstance(m, MalSeq):
            return m.value
        raise mal_errors.EvalError("Non-list argument to concat")

    return MalList([y for x in map(contents, args) for y in x])


def nth(args: List[MalAny]) -> MalAny:
    """Python definition of mal nth function."""
    if len(args) == 2 and isinstance(args[0], MalSeq) and isinstance(args[1], int):
        if args[1] >= 0 and args[1] < len(args[0].value):
            return args[0].value[args[1]]
        raise mal_errors.EvalError("Bad index for nth")
    raise mal_errors.EvalError("Bad arguments to nth")


def first(args: List[MalAny]) -> MalAny:
    """Python definition of mal first function."""
    if len(args) == 1:
        if isinstance(args[0], MalSeq):
            if len(args[0].value) > 0:
                return args[0].value[0]
            return MalNil()
        if isinstance(args[0], MalNil):
            return MalNil()
    raise mal_errors.EvalError("Bad arguments to first")


def rest(args: List[MalAny]) -> MalAny:
    """Python definition of mal first function."""
    if len(args) == 1:
        if isinstance(args[0], MalSeq):
            return MalList(args[0].value[1:])
        if isinstance(args[0], MalNil):
            return MalList([])
    raise mal_errors.EvalError("Bad arguments to rest")


#
# IO and string functions
#


def mal_pr_str(args: List[MalAny]) -> str:
    """Python definition of mal pr-str function."""
    str_args = map(lambda x: printer.pr_str(x, print_readably=True), args)
    return " ".join(str_args)


def mal_str(args: List[MalAny]) -> str:
    """Python definition of mal str function."""
    str_args = map(lambda x: printer.pr_str(x, print_readably=False), args)
    return "".join(str_args)


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
        if isinstance(head, str):
            read_form = reader.read_str(head)
            if read_form is None:
                return MalNil()
            return read_form
    raise mal_errors.EvalError("Bad arguments to read-string")


def slurp(args: List[MalAny]) -> MalAny:
    """Python definition of function to return contents of give filename as a string."""
    if len(args) == 1:
        head = args[0]
        if isinstance(head, str):
            try:
                with open(head, "r") as f:
                    return f.read()
            except (OSError) as err:
                raise mal_errors.EvalError(err.strerror + " (OS error)")
    raise mal_errors.EvalError("Bad arguments to slurp")


#
# Atom functions
#


def atom(args: List[MalAny]) -> MalAny:
    """Python definition of atom function to create an atom."""
    if len(args) == 1:
        return MalAtom(args[0])
    raise mal_errors.EvalError("Bad arguments to atom")


def atom_test(args: List[MalAny]) -> MalAny:
    """Python definition of atom? function to test if value is an atom."""
    if len(args) == 1:
        return isinstance(args[0], MalAtom)
    raise mal_errors.EvalError("Bad arguments to atom?")


def deref(args: List[MalAny]) -> MalAny:
    """Python definition of deref function to return vaue from an atom."""
    if len(args) == 1 and isinstance(args[0], MalAtom):
        return args[0].value
    raise mal_errors.EvalError("Bad arguments to deref")


def reset(args: List[MalAny]) -> MalAny:
    """Python definition of reset! function to reset an atom's value."""
    if len(args) == 2 and isinstance(args[0], MalAtom):
        args[0].value = args[1]
        return args[1]
    raise mal_errors.EvalError("Bad arguments to reset!")


#
# Other functions
#


def mal_throw(args: List[MalAny]) -> NoReturn:
    """Python definition of mal throw function."""
    if len(args) == 1:
        raise mal_errors.UserException(args[0])
    raise mal_errors.EvalError("Bad argument to throw")
