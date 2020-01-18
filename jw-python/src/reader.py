"""Reader functionality for mal."""

# pylint: disable=too-many-return-statements,too-many-branches

import re
from typing import Generic, List, Optional, TypeVar

from mal_types import (
    MalAny,
    MalException,
    MalKeyword,
    MalList,
    MalMap,
    MalNil,
    MalSym,
    MalVec,
)

from utils import remove_escapes

T = TypeVar("T")


class Reader(Generic[T]):
    """Stateful reader object that supports peek and next."""

    def __init__(self, source: List[T]) -> None:
        """Initialize reader with a list."""
        self.source = source
        self.current = 0

    def peek(self) -> Optional[T]:
        """Return current element without advancing. None if no more elements."""
        return self.source[self.current] if self.current < len(self.source) else None

    def pull(self) -> Optional[T]:
        """Return and advance current element. None if no more elements."""
        val = self.peek()
        self.current += 1
        return val


TOKEN_REGEX = re.compile(
    r"[\s,]*"  # Any number of whitespaces or commas (not captured)
    "(~@|"  # Captures the special two-characters ~@
    r"[\[\]{}()'`~^@]|"  # Captures any special single character, one of []{}()'`~^@
    r'"(?:\\.|[^\\"])*"?|'  # Starts at a " and stops at next " unless escaped
    r";.*|"  # Captures any sequence of characters starting with ;
    r"[^\s\[\]{}('"  # Captures a sequence of zero or more non special characters
    '"`,;)]*)'
)


def read_str(source: str) -> Optional[MalAny]:
    """Read the tokens in the given string and return the MAL AST.

    Returns None in the absence of tokens (just whitespace or comments)
    """
    tokens = TOKEN_REGEX.findall(source)
    return read_form(Reader(tokens))


def read_form(reader: Reader[str]) -> Optional[MalAny]:
    """Read a general form from the given Reader."""
    if reader.peek() == "(":
        return MalList(read_seq(reader, "(", ")"))
    if reader.peek() == "[":
        return MalVec(read_seq(reader, "[", "]"))
    if reader.peek() == "{":
        return MalMap(read_seq(reader, "{", "}"))
    return read_atom(reader)


def read_seq(reader: Reader[str], opener: str, closer: str) -> List[MalAny]:
    """Read a sequence of values between given delimiters."""
    if reader.pull() != opener:
        raise MalException("no opener in read_seq")

    elements: List[MalAny] = []
    while True:
        next_value = reader.peek()
        if next_value == closer:
            reader.pull()
            break
        if next_value is None:
            raise MalException("EOF before closing paren in read_list")
        form = read_form(reader)
        if form is not None:
            elements.append(form)

    return elements


QUOTED_STRING_REGEX = re.compile('"(.*)"', re.DOTALL)
NUMBER_REGEX = re.compile(r"-?\d+")


def read_atom(reader: Reader[str]) -> Optional[MalAny]:
    """Read an atom from the given Reader."""
    token = reader.pull()
    if token is None:
        raise MalException("EOF in read_atom")

    if token == "nil":
        return MalNil()
    if token == "true":
        return True
    if token == "false":
        return False
    if token == "'":
        form = read_form(reader)
        if form is None:
            raise MalException("Missing form in quote")
        return MalList([MalSym("quote"), form])
    if token == "`":
        form = read_form(reader)
        if form is None:
            raise MalException("Missing form in quasiquote")
        return MalList([MalSym("quasiquote"), form])
    if token == "~":
        form = read_form(reader)
        if form is None:
            raise MalException("Missing form in unquote")
        return MalList([MalSym("unquote"), form])
    if token == "~@":
        form = read_form(reader)
        if form is None:
            raise MalException("Missing form in splice-quote")
        return MalList([MalSym("splice-unquote"), form])
    if token == "@":
        form = read_form(reader)
        if form is None:
            raise MalException("Missing form in deref")
        return MalList([MalSym("deref"), form])
    if token == "^":
        target = read_form(reader)
        if target is None:
            raise MalException("Missing target in with-meta")
        meta = read_form(reader)
        if meta is None:
            raise MalException("Missing meta in with-meta")
        return MalList([MalSym("with-meta"), meta, target])

    match = QUOTED_STRING_REGEX.fullmatch(token)
    if match:
        return remove_escapes(match.group(1))
    if token.startswith('"'):
        raise MalException("unbalanced quotes", token)
    if token.startswith(":"):
        return MalKeyword(token[1:])
    if token.startswith(";"):
        return None
    if NUMBER_REGEX.fullmatch(token):
        return int(token)

    return MalSym(token)
