"""Reader"""

import re
from typing import Generic, List, Optional, TypeVar

from mal import MalType, MalList, MalVec, MalNum, MalSym, MalStr, MalBool, MalNil
from mal import InternalError, ReaderError

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


def read_str(source: str) -> MalType:
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


def read_form(reader: Reader[str]) -> MalType:
    """Read a general form from the given Reader"""

    if reader.peek() == "(":
        return read_list(reader)
    if reader.peek() == "[":
        return read_list(reader, read_vector=True)
    return read_atom(reader)


def read_list(reader: Reader[str], read_vector: bool = False) -> MalType:
    """Read a list or vector from the given Reader"""

    opener = "[" if read_vector else "("
    closer = "]" if read_vector else ")"
    if reader.next() != opener:
        raise InternalError("no opener in read_list")

    elements: List[MalType] = []
    while True:
        next_value = reader.peek()
        if next_value == closer:
            reader.next()
            break
        if next_value is None:
            raise ReaderError("EOF before closing paren in read_list")
        elements.append(read_form(reader))

    return MalVec(elements) if read_vector else MalList(elements)


STRING_REGEX = re.compile('"(.*)"')
NUMBER_REGEX = re.compile(r"\d+")

def read_atom(reader: Reader[str]) -> MalType:
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
    match = STRING_REGEX.fullmatch(token)
    if match:
        return MalStr(match.group(1))
    if token.startswith('"'):
        raise ReaderError("unbalanced quotes", token)
    if token.startswith(";"):
        return MalNil()
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
    if NUMBER_REGEX.fullmatch(token):
        return MalNum(int(token))

    return MalSym(token)
