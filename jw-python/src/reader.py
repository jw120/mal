"""Reader"""

import re
from typing import Generic, List, TypeVar

from mal import MalType, MalList, MalNum, MalSym, MalStr, MalBool, MalNil


T = TypeVar('T') #pylint: disable=invalid-name
class Reader(Generic[T]):
    """Creates a stateful reader object that supports peek and next

    >>> r = Reader([1,2,3])
    >>> print(r.peek(), r.next(), r.next())
    1 1 2
    """

    def __init__(self, source: List[T]) -> None:
        self.source = source
        self.current = 0

    def peek(self) -> T:
        """Return the current element without advancing the current element"""

        return self.source[self.current]

    def next(self) -> T:
        """Return the current element and advance the current element"""

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

def read_form(reader: Reader) -> MalType:
    """Read a general form from the given Reader"""
    if reader.peek() == "(":
        return read_list(reader)
    return read_atom(reader)

def read_list(reader: Reader) -> MalType:
    """Read a list from the given Reader"""

    if reader.next() != "(":
        raise RuntimeError
    elements: List[MalType] = []
    while reader.peek() != ")":
        elements.append(read_form(reader))
    return MalList(elements)

STRING_REGEX = re.compile('"(.*)"')
NUMBER_REGEX = re.compile(r"\d+")

def read_atom(reader: Reader) -> MalType:
    """Read an atom from the given Reader"""

    token = reader.next()
    if token == "nil":
        return MalNil()
    if token == "true":
        return MalBool(True)
    if token == "false":
        return MalBool(False)
    match = STRING_REGEX.fullmatch(token)
    if match:
        return MalStr(match.group(1))
    if NUMBER_REGEX.fullmatch(token):
        return MalNum(int(token))
    return MalSym(token)
