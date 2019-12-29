"""Reader"""

#pylint: disable=too-many-return-statements,too-many-branches

import re
from typing import Generic, List, Optional, TypeVar

from mal_errors import InternalError, ReaderError
from mal_types import MalAny, MalList, MalVec, MalMap, MalNum
from mal_types import MalKeyword, MalSym, MalStr, MalBool, MalNil
from utils import remove_escapes

T = TypeVar('T')


class Reader(Generic[T]):
    """Creates a stateful reader object that supports peek and next

    >>> r = Reader([1,2,3])
    >>> print(r.peek(), r.next(), r.next())
    1 1 2
    """

    def __init__(self, source: List[T]) -> None:
        self.source = source
        self.current = 0

    def peek(self) -> Optional[T]:
        """Return the current element without advancing. None if no more elements."""

        return self.source[self.current] if self.current < len(self.source) else None

    def next(self) -> Optional[T]:
        """Return the current element and advance the current element. None if no more elements"""

        val = self.peek()
        self.current += 1
        return val


TOKEN_REGEX = re.compile(
    r"[\s,]*"                 # Any number of whitespaces or commas (not captured)
    "(~@|"                    # Captures the special two-characters ~@
    r"[\[\]{}()'`~^@]|"       # Captures any special single character, one of []{}()'`~^@
    r'"(?:\\.|[^\\"])*"?|'    # Starts at a " and stops at next " unless escaped
    r";.*|"                   # Captures any sequence of characters starting with ;
    r"[^\s\[\]{}('"           # Captures a sequence of zero or more non special characters
    '"`,;)]*)'
    )


def read_str(source: str) -> MalAny:
    """Read the tokens in the given string and return the MAL AST

    >>> print(read_str("42"), read_str("abc"), read_str('"s123"'))
    42 abc "s123"
    >>> print(read_str("nil"), read_str("true"), read_str("false"))
    nil true false
    >>> print(read_str("(+ 2 3 (- 4 5))"))
    (+ 2 3 (- 4 5))
    """

    tokens = TOKEN_REGEX.findall(source)
    return read_form(Reader(tokens))


def read_form(reader: Reader[str]) -> MalAny:
    """Read a general form from the given Reader"""

    if reader.peek() == "(":
        return MalList(read_seq(reader, "(", ")"))
    if reader.peek() == "[":
        return MalVec(read_seq(reader, "[", "]"))
    if reader.peek() == "{":
        return MalMap(read_seq(reader, "{", "}"))
    return read_atom(reader)


def read_seq(reader: Reader[str], opener: str, closer: str) -> List[MalAny]:
    """Read a sequence of values between given delimiters"""

    if reader.next() != opener:
        raise InternalError("no opener in read_seq")

    elements: List[MalAny] = []
    while True:
        next_value = reader.peek()
        if next_value == closer:
            reader.next()
            break
        if next_value is None:
            raise ReaderError("EOF before closing paren in read_list")
        elements.append(read_form(reader))

    return elements


QUOTED_STRING_REGEX = re.compile('"(.*)"')
NUMBER_REGEX = re.compile(r"-?\d+")

def read_atom(reader: Reader[str]) -> MalAny:
    """Read an atom from the given Reader"""

    token = reader.next()
    if token is None:
        raise InternalError("EOF in read_atom")

    if token == "nil":
        return MalNil()
    if token == "true":
        return MalBool(True)
    if token == "false":
        return MalBool(False)
    if token == "'":
        return MalList([MalSym("quote"), read_form(reader)])
    if token == "`":
        return MalList([MalSym("quasiquote"), read_form(reader)])
    if token == "~":
        return MalList([MalSym("unquote"), read_form(reader)])
    if token == "~@":
        return MalList([MalSym("splice-unquote"), read_form(reader)])
    if token == "@":
        return MalList([MalSym("deref"), read_form(reader)])
    if token == "^":
        target = read_form(reader)
        meta = read_form(reader)
        return MalList([MalSym("with-meta"), meta, target])

    match = QUOTED_STRING_REGEX.fullmatch(token)
    if match:
        return MalStr(remove_escapes(match.group(1)))
    if token.startswith('"'):
        raise ReaderError("unbalanced quotes", token)
    if token.startswith(":"):
        return MalKeyword(token[1:])
    if token.startswith(";"):
        return MalNil()
    if NUMBER_REGEX.fullmatch(token):
        return MalNum(int(token))

    return MalSym(token)
