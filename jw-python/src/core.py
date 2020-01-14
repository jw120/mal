"""Core function definitions for mal.

Functions defined here are in pure python and have no access to the
environment.
"""

import operator
from functools import reduce
from typing import Callable, Dict, List, NoReturn, Tuple, cast

from mal_types import (
    MalAny,
    MalAtom,
    MalBuiltin,
    MalException,
    MalKeyword,
    MalList,
    MalNil,
    MalSeq,
    MalSym,
    MalVec,
)

import printer

import reader


def create_ns() -> Dict[str, MalBuiltin]:
    """Create and return the core namespace.

    Provided as a function so it can precede the definitions of the functions
    """
    return dict(
        [
            # Arithmetic and numeric comparisons
            make_any("+", addition),
            make_any("*", multiplication),
            make_2int("-", operator.sub),
            make_2int("/", operator.floordiv),
            make_2arg("=", operator.eq),
            make_2int(">", operator.gt),
            make_2int("<", operator.lt),
            make_2int(">=", operator.ge),
            make_2int("<=", operator.le),
            # Collection functions
            make_any("list", MalList),
            make_1arg("list?", lambda x: isinstance(x, MalList)),
            make_1arg("count", count),
            make_1arg("empty?", empty_test),
            make_2arg("cons", cons),
            make_any("concat", mal_concat),
            make_2arg("nth", nth),
            make_1arg("first", first),
            make_1arg("rest", rest),
            # Hash-map functions
            # IO and string functions
            make_any("pr-str", mal_pr_str),
            make_any("str", mal_str),
            make_any("prn", mal_prn),
            make_any("println", mal_println),
            make_1str("read-string", mal_read_string),
            make_1str("slurp", slurp),
            # Atom functions
            make_1arg("atom", MalAtom),
            make_1arg("atom?", lambda x: isinstance(x, MalAtom)),
            make_1arg("deref", deref),
            make_2arg("reset!", reset),
            # Other type functions
            make_1arg("nil?", lambda x: isinstance(x, MalNil)),
            make_1arg("false?", lambda x: x is False),
            make_1arg("true?", lambda x: x is True),
            make_1arg("symbol?", lambda x: isinstance(x, MalSym)),
            make_1str("symbol", MalSym),
            make_1arg("keyword", mal_keyword),
            make_1arg("keyword?", lambda x: isinstance(x, MalKeyword)),
            make_any("vector", MalVec),
            make_1arg("vector?", lambda x: isinstance(x, MalVec)),
            make_1arg("sequential?", lambda x: isinstance(x, MalSeq)),
            # Misc functions
            make_1arg("throw", mal_throw),
        ]
    )


#
# Helper functions that return (string, MalBuiltin pairs)
#


def make_any(name: str, f: Callable[[List[MalAny]], MalAny]) -> Tuple[str, MalBuiltin]:
    """Make a (name, builtin) tuple for a function of any mal values."""
    return (name, MalBuiltin(f))


def make_1arg(name: str, f: Callable[[MalAny], MalAny]) -> Tuple[str, MalBuiltin]:
    """Make a (name, builtin) tuple for a function of a single mal value."""

    def g(args: List[MalAny]) -> MalAny:
        if len(args) != 1:
            raise MalException("Bad arguments to " + name, str(args))
        return f(args[0])

    return (name, MalBuiltin(g))


def make_1str(name: str, f: Callable[[str], MalAny]) -> Tuple[str, MalBuiltin]:
    """Make a (name, builtin) tuple for a function of a single string."""

    def g(args: List[MalAny]) -> MalAny:
        if len(args) == 1 and isinstance(args[0], str):
            return f(args[0])
        raise MalException("Bad arguments to " + name, str(args))

    return (name, MalBuiltin(g))


def make_2arg(
    name: str, f: Callable[[MalAny, MalAny], MalAny]
) -> Tuple[str, MalBuiltin]:
    """Make a (name, builtin) tuple for a function acting of two mal values."""

    def g(args: List[MalAny]) -> MalAny:
        if len(args) != 2:
            raise MalException("Bad arguments to " + name, str(args))
        return f(args[0], args[1])

    return (name, MalBuiltin(g))


def make_2int(name: str, f: Callable[[int, int], MalAny]) -> Tuple[str, MalBuiltin]:
    """Make a (name, builtin) tuple for a function acting of two ints."""

    def g(args: List[MalAny]) -> MalAny:
        if len(args) == 2:
            if isinstance(args[0], int) and isinstance(args[1], int):
                return f(args[0], args[1])
        raise MalException("Bad arguments to " + name, str(args))

    return (name, MalBuiltin(g))


#
# Arithmetic and numeric comparisons
#


def addition(args: List[MalAny]) -> int:
    """Python definition of mal + function."""
    if all(map(lambda x: isinstance(x, int), args)):
        return sum(cast(List[int], args))
    raise MalException("Bad arguments to +")


def multiplication(args: List[MalAny]) -> int:
    """Python definition of mal * function."""
    if all(map(lambda x: isinstance(x, int), args)):
        return reduce(lambda x, y: x * y, cast(List[int], args), 1)
    raise MalException("Bad arguments to *")


#
# Collection functions
#


def count(x: MalAny) -> int:
    """Python definition of mal count function."""
    if isinstance(x, MalSeq):
        return len(x.value)
    if isinstance(x, MalNil):
        return 0
    raise MalException("Bad arguments to count")


def empty_test(x: MalAny) -> bool:
    """Python definition of mal empty? function."""
    if isinstance(x, MalSeq):
        return len(x.value) == 0
    raise MalException("Bad arguments to empty?")


def cons(x: MalAny, y: MalAny) -> MalList:
    """Python definition of mal cons function."""
    if isinstance(y, MalSeq):
        return MalList([x] + y.value)
    raise MalException("Bad arguments to cons")


def mal_concat(args: List[MalAny]) -> MalList:
    """Python definition of mal concat function."""

    def contents(m: MalAny) -> List[MalAny]:
        if isinstance(m, MalSeq):
            return m.value
        raise MalException("Non-list argument to concat")

    return MalList([y for x in map(contents, args) for y in x])


def nth(x: MalAny, y: MalAny) -> MalAny:
    """Python definition of mal nth function."""
    if isinstance(x, MalSeq) and isinstance(y, int):
        if y >= 0 and y < len(x.value):
            return x.value[y]
        raise MalException("Index for nth out of bounds")
    raise MalException("Bad arguments to nth")


def first(x: MalAny) -> MalAny:
    """Python definition of mal first function."""
    if isinstance(x, MalSeq):
        if len(x.value) > 0:
            return x.value[0]
        return MalNil()
    if isinstance(x, MalNil):
        return MalNil()
    raise MalException("Bad arguments to first")


def rest(x: MalAny) -> MalAny:
    """Python definition of mal first function."""
    if isinstance(x, MalSeq):
        return MalList(x.value[1:])
    if isinstance(x, MalNil):
        return MalList([])
    raise MalException("Bad arguments to rest")


#
# Hash map functions
#


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


def mal_read_string(s: str) -> MalAny:
    """Python definition of mal read-string function."""
    read_form = reader.read_str(s)
    if read_form is None:
        return MalNil()
    return read_form


def slurp(fn: str) -> MalAny:
    """Python definition of function to return contents of give filename as a string."""
    try:
        with open(fn, "r") as f:
            return f.read()
    except (OSError) as err:
        raise MalException(err.strerror, "OS error")


#
# Atom functions
#


def deref(x: MalAny) -> MalAny:
    """Python definition of deref function to return vaue from an atom."""
    if isinstance(x, MalAtom):
        return x.value
    raise MalException("Bad arguments to deref")


def reset(x: MalAny, y: MalAny) -> MalAny:
    """Python definition of reset! function to reset an atom's value."""
    if isinstance(x, MalAtom):
        x.value = y
        return y
    raise MalException("Bad arguments to reset!")


#
# Other type functions
#

def mal_keyword(x: MalAny) -> MalAny:
    """Python definition of keyword."""
    if isinstance(x, str):
        return MalKeyword(x)
    if isinstance(x, MalKeyword):
        return x
    raise MalException("Bad arguments to keyword")


#
# Misc functions
#


def mal_throw(x: MalAny) -> NoReturn:
    """Python definition of mal throw function."""
    raise MalException(x)
