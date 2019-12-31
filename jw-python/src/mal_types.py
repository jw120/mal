"""Provide type defintions for mal's AST.

Defines a value equality and a to_string method which allows string
conversion with and without prettifying strings

## Class hierarchy:

* MalAny
    + MalSeq (provides shared implementation and supports functions which work on both)
        - MalList
        - MalVec
    + MalMap
    + MalFunc
    + MalKey (valid keys for a map)
        - MalKeyword
        - MalStr
    + MalSym
    + MalNum
    + MalBool
    + MalNil

"""

# pylint: disable=too-few-public-methods

from abc import ABC, abstractmethod
from typing import Any, Callable, Dict, List, Tuple, Union

import mal_errors as err

from utils import add_escapes


class MalAny(ABC):
    """Abstract type for any Mal element."""

    def __str__(self) -> str:
        """Convert to string using to_string(False). Inherited by all subclasses."""
        return self.to_string(False)

    @abstractmethod
    def to_string(self, _print_readably: bool) -> str:
        """Provide a string version with or without strings made readable."""
        pass


class MalKey(MalAny):
    """Type for mal that can be used as a hash-map key."""

    def __init__(self, value: str) -> None:
        """Create a MalKey."""
        self.value: str = value
        super().__init__()

    def __eq__(self, other: Any) -> bool:
        """Equality based on value."""
        return isinstance(other, self.__class__) and self.value == other.value

    def __hash__(self) -> int:
        """Hash based on value. (needed as defining __eq__ disables default)."""
        return hash(str(self))


class MalKeyword(MalKey):
    """Keyword type for mal."""

    def to_string(self, _print_readably: bool) -> str:
        """Convert to a string."""
        return ":" + self.value


class MalStr(MalKey):
    """String type for mal."""

    def to_string(self, print_readably: bool) -> str:
        """Convert to a string, optionally adding quotes and escapes."""
        if print_readably:
            return '"' + add_escapes(self.value) + '"'
        return self.value


Mal_Environment = Dict[str, MalAny]
Mal_Function = Callable[[List[MalAny]], MalAny]
Mal_Map = Dict[MalKey, MalAny]


class MalSeq(MalAny):
    """Sequence type for Mal - lists or vectors."""

    def __init__(self, value: List[MalAny]) -> None:
        """Create a MalSeq. Called via super() from subclases."""
        self.value: List[MalAny] = value
        super().__init__()

    def __eq__(self, other: Any) -> bool:
        """Value equality considering all MalSeq sub-classses to be equal."""
        return isinstance(other, MalSeq) and self.value == other.value


class MalList(MalSeq):
    """List type for mal."""

    def to_string(self, print_readably: bool) -> str:
        """Convert to a string, optionally adding quotes and escapes."""
        str_values = map(lambda x: x.to_string(print_readably), self.value)
        return "(" + " ".join(str_values) + ")"


class MalVec(MalSeq):
    """Vector type for mal."""

    def to_string(self, print_readably: bool) -> str:
        """Convert to a string, optionally adding quotes and escapes."""
        str_values = map(lambda x: x.to_string(print_readably), self.value)
        return "[" + " ".join(str_values) + "]"


class MalMap(MalAny):
    """Mal type for mal."""

    def __init__(
        self, elements: Union[List[MalAny], Tuple[List[MalKey], List[MalAny]]]
    ) -> None:
        """Create a MalMap.

        Argument:
            element - either an alternating list of symbols and values or a tuple
            of a list of symbls and a list of values
        """
        # Construct from an alternating key-value list
        if isinstance(elements, list):
            if len(elements) % 2 == 1:
                raise err.EvalError("Unmatched key for map", str(elements[-1]))
            self.value: Mal_Map = {}
            for k, v in zip(elements[0::2], elements[1::2]):
                if not isinstance(k, MalKey):
                    raise err.EvalError("Bad key type for map", str(k))
                self.value[k] = v

        # Construct from a tuple of keys and values
        else:
            keys, values = elements
            self.value = dict(zip(keys, values))

        super().__init__()

    def to_string(self, print_readably: bool) -> str:
        """Convert to a string, optionally adding quotes and escapes."""
        accumulated: List[str] = []
        for k in self.value:
            accumulated.append(k.to_string(print_readably))
            accumulated.append(self.value[k].to_string(print_readably))
        return "{" + " ".join(map(str, accumulated)) + "}"

    def __eq__(self, other: Any) -> bool:
        """Value equality."""
        return isinstance(other, self.__class__) and self.value == other.value


class MalFunc(MalAny):
    """Type for mal functions."""

    def __init__(self, value: Mal_Function) -> None:
        """Create a MalFunc."""
        self.value: Mal_Function = value

    def to_string(self, _print_readably: bool) -> str:
        """Convert to a string."""
        return "#<function>"

    def __eq__(self, _other: Any) -> bool:
        """Equality for functions always false."""
        return False


class MalSym(MalAny):
    """Symbol type for mal."""

    def __init__(self, value: str):
        """Create a MalSym."""
        self.value: str = value
        super().__init__()

    def to_string(self, _print_readably: bool) -> str:
        """Convert to a string, optionally adding quotes and escapes."""
        return self.value

    def __eq__(self, other: Any) -> bool:
        """Value equality."""
        return isinstance(other, self.__class__) and self.value == other.value


class MalNum(MalAny):
    """Number type for mal."""

    def __init__(self, value: int) -> None:
        """Create a MalNum."""
        self.value: int = value
        super().__init__()

    def to_string(self, _print_readably: bool) -> str:
        """Convert to a string."""
        return str(self.value)

    def __eq__(self, other: Any) -> bool:
        """Value equality."""
        return isinstance(other, self.__class__) and self.value == other.value


class MalBool(MalAny):
    """Boolean type for mal."""

    def __init__(self, value: bool) -> None:
        """Create a MalBool."""
        self.value: bool = value
        super().__init__()

    def to_string(self, _print_readably: bool) -> str:
        """Convert to a string."""
        return "true" if self.value else "false"

    def __eq__(self, other: Any) -> bool:
        """Value equality."""
        return isinstance(other, self.__class__) and self.value == other.value


class MalNil(MalAny):
    """Nil type for mal."""

    def to_string(self, _print_readably: bool) -> str:
        """Convert to a string."""
        return "nil"

    def __eq__(self, other: Any) -> bool:
        """Value equality."""
        return isinstance(other, self.__class__)
