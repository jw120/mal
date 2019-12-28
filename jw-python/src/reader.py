"""Reader"""

import re
from typing import Generic, List, TypeVar

from types import MalType, MalList, MalNil

T = TypeVar('T') #pylint: disable=invalid-name
class Reader(Generic[T]):
    """Stateful reader object"""

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
    """Read the tokens in the given string"""
    tokens = TOKEN_REGEX.findall(source)
    return read_form(Reader(tokens))

def read_form(reader: Reader) -> MalType:
    """Read a general form from the given Reader"""
    if reader.peek() == "()":
        return read_list(reader)
    return read_atom(reader)

def read_list(_reader: Reader) -> MalType:
    """Read a list from the given Reader"""
    return MalList([])

def read_atom(_reader: Reader) -> MalType:
    """Read an atom from the given Reader"""
    return MalNil()

#if __name__ == "__main__":
