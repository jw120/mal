"""Core function definitions for mal.

Functions defined here are in pure python and have no access to the
environment.
"""

import operator
from functools import reduce
from time import time
from typing import Any, Callable, Dict, List, NoReturn, Optional, Tuple, cast

from mal_types import (
    MalAny,
    MalAtom,
    MalBuiltin,
    MalException,
    MalFunc,
    MalKey,
    MalKeyword,
    MalList,
    MalMap,
    MalNil,
    MalSeq,
    MalSym,
    MalVec,
    isMalKey,
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
            make_1arg("seq", mal_seq),
            make_any("conj", mal_conj),
            # Hash-map functions
            make_1arg("map?", lambda x: isinstance(x, MalMap)),
            make_any("hash-map", lambda xs: MalMap(xs)),
            make_any("assoc", hash_map_assoc),
            make_any("dissoc", hash_map_dissoc),
            make_2arg("get", hash_map_get),
            make_2arg("contains?", hash_map_contains_test),
            make_1arg("keys", hash_map_keys),
            make_1arg("vals", hash_map_vals),
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
            make_1arg(
                "fn?",
                lambda x: isinstance(x, MalBuiltin)
                or (isinstance(x, MalFunc) and not x.is_macro),
            ),
            make_1arg("macro?", lambda x: isinstance(x, MalFunc) and x.is_macro),
            make_1arg("string?", lambda x: isinstance(x, str)),
            make_1arg(
                "number?", lambda x: isinstance(x, int) and not isinstance(x, bool)
            ),
            # Misc functions
            make_1arg("throw", mal_throw),
            make_1str("readline", mal_readline),
            make_0arg("time-ms", lambda: round(time() * 1000)),
            make_any("meta", nyi),
            make_any("with-meta", nyi),
        ]
    )


#
# Helper functions that return (string, MalBuiltin pairs)
#


def make_any(name: str, f: Callable[[List[MalAny]], MalAny]) -> Tuple[str, MalBuiltin]:
    """Make a (name, builtin) tuple for a function of any mal values."""
    return (name, MalBuiltin(f))


def make_0arg(name: str, f: Callable[[], MalAny]) -> Tuple[str, MalBuiltin]:
    """Make a (name, builtin) tuple for a function that takes no arguments."""

    def g(args: List[MalAny]) -> MalAny:
        if len(args) != 0:
            raise MalException("Bad arguments to " + name, args)
        return f()

    return (name, MalBuiltin(g))


def make_1arg(name: str, f: Callable[[MalAny], MalAny]) -> Tuple[str, MalBuiltin]:
    """Make a (name, builtin) tuple for a function of a single mal value."""

    def g(args: List[MalAny]) -> MalAny:
        if len(args) != 1:
            raise MalException("Bad arguments to " + name, args)
        return f(args[0])

    return (name, MalBuiltin(g))


def make_1str(name: str, f: Callable[[str], MalAny]) -> Tuple[str, MalBuiltin]:
    """Make a (name, builtin) tuple for a function of a single string."""

    def g(args: List[MalAny]) -> MalAny:
        if len(args) == 1 and isinstance(args[0], str):
            return f(args[0])
        raise MalException("Bad arguments to " + name, args)

    return (name, MalBuiltin(g))


def make_2arg(
    name: str, f: Callable[[MalAny, MalAny], MalAny]
) -> Tuple[str, MalBuiltin]:
    """Make a (name, builtin) tuple for a function acting of two mal values."""

    def g(args: List[MalAny]) -> MalAny:
        if len(args) != 2:
            raise MalException("Bad arguments to " + name, args)
        return f(args[0], args[1])

    return (name, MalBuiltin(g))


def make_2int(name: str, f: Callable[[int, int], MalAny]) -> Tuple[str, MalBuiltin]:
    """Make a (name, builtin) tuple for a function acting of two ints."""

    def g(args: List[MalAny]) -> MalAny:
        if len(args) == 2:
            if isinstance(args[0], int) and isinstance(args[1], int):
                return f(args[0], args[1])
        raise MalException("Bad arguments to " + name, args)

    return (name, MalBuiltin(g))


def nyi(_: Any) -> NoReturn:
    """Raise an exception."""
    raise MalException("NYI")


#
# Arithmetic and numeric comparisons
#


def addition(args: List[MalAny]) -> int:
    """Python definition of mal + function."""
    if all(map(lambda x: isinstance(x, int), args)):
        return sum(cast(List[int], args))
    raise MalException("Bad arguments to +", args)


def multiplication(args: List[MalAny]) -> int:
    """Python definition of mal * function."""
    if all(map(lambda x: isinstance(x, int), args)):
        return reduce(lambda x, y: x * y, cast(List[int], args), 1)
    raise MalException("Bad arguments to *", args)


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


def mal_seq(x: MalAny) -> MalAny:
    """Python definition of mal seq function."""
    if x == MalList([]) or x == MalVec([]) or x == "" or x == MalNil():
        return MalNil()
    if isinstance(x, MalList):
        return x
    if isinstance(x, MalVec):
        return MalList(x.value)
    if isinstance(x, str):
        return MalList(list(x))
    raise MalException("Bad arguments to seq")


def mal_conj(args: List[MalAny]) -> MalAny:
    """Python definition of mal conj function."""
    if len(args) >= 2:
        new_elems: List[MalAny] = args[1:]
        if isinstance(args[0], MalList):
            rev_new_elems: List[MalAny] = list(new_elems)  # copy
            rev_new_elems.reverse()  # in-place reverse
            return MalList(rev_new_elems + args[0].value)
        if isinstance(args[0], MalVec):
            return MalVec(args[0].value + new_elems)
    raise MalException("Bad arguments to conj")


#
# Hash map functions
#


def hash_map_assoc(args: List[MalAny]) -> MalMap:
    """Python definition of mal assoc function."""
    if len(args) >= 3:
        if isinstance(args[0], MalMap):
            return args[0].assoc(MalMap(args[1:]))
    raise MalException("Bad arguments to assoc")


def hash_map_dissoc(args: List[MalAny]) -> MalMap:
    """Python definition of mal dissoc function."""
    if len(args) >= 1:
        if isinstance(args[0], MalMap):
            for k in args[1:]:
                if not isMalKey(k):
                    raise MalException("Non-key passed to dissoc")
            key_list = cast(List[MalKey], args[1:])
            return args[0].dissoc(key_list)
    raise MalException("Bad arguments to assoc")


def hash_map_get(x: MalAny, y: MalAny) -> MalAny:
    """Python definition of get function."""
    if isinstance(x, MalMap) and isMalKey(y):
        val: Optional[MalAny] = x.get(cast(MalKey, y))
        if val is None:
            return MalNil()
        return val
    if isinstance(x, MalNil):
        return MalNil()
    raise MalException("Bad arguments to get")


def hash_map_contains_test(x: MalAny, y: MalAny) -> bool:
    """Python definition of contains? function."""
    if isinstance(x, MalMap) and isMalKey(y):
        val: Optional[MalAny] = x.get(cast(MalKey, y))
        return val is not None
    raise MalException("Bad arguments to contains?")


def hash_map_keys(x: MalAny) -> MalAny:
    """Python definition of keys function."""
    if isinstance(x, MalMap):
        return MalList(list(x.value))
    raise MalException("Bad arguments to keys")


def hash_map_vals(x: MalAny) -> MalAny:
    """Python definition of vals function."""
    if isinstance(x, MalMap):
        return MalList(list(x.value.values()))
    raise MalException("Bad arguments to keys")


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


def mal_readline(s: str) -> MalAny:
    """Python definition of mal readline function."""
    try:
        return input(s)
    except EOFError:
        return MalNil()
