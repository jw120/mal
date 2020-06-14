"""Utility functions for mal."""

from typing import Iterable, Tuple, TypeVar

from mal_types import MalException


def add_escapes(s: str) -> str:
    """Add backlash escape sequences to a string."""
    new_string = ""
    for c in s:
        if c == "\\":
            new_string += r"\\"
        elif c == '"':
            new_string += r"\""
        elif c == "\n":
            new_string += r"\n"
        else:
            new_string += c
    return new_string


def remove_escapes(s: str) -> str:
    """Remove backslash escape sequences from a string."""
    backslash_active = False
    new_string = ""
    for c in s:
        if backslash_active:
            if c in ["\\", '"']:
                new_string += c
            elif c == "n":
                new_string += "\n"
            else:
                raise MalException("Unknown escape in string", s)
            backslash_active = False
        else:
            if c == "\\":
                backslash_active = True
            elif c == '"':
                raise MalException("Unexpected double quote in string", s)
            else:
                new_string += c
    if backslash_active:
        raise MalException("Backslash at end of input", s)

    return new_string


T = TypeVar("T")


def pairs(xs: Iterable[T]) -> Iterable[Tuple[T, T]]:
    """Extract an iterator over pairs from a list."""
    xs_iterator = iter(xs)
    try:
        for x in xs_iterator:
            yield x, next(xs_iterator)
    except StopIteration:
        raise MalException("Unmatched item in pair list", str(list(xs))) from None
