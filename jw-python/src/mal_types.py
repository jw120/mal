"""Provides type defintions for our AST and for our exceptions

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

#pylint: disable=too-few-public-methods

from typing import Callable, Dict, List, Tuple, Union

import mal_errors as err
from utils import add_escapes


class MalAny:
    """Abstract type for any Mal element"""

    def str_readable(self) -> str:
        """Default implementation of readable string, overridedn by string types"""
        return str(self)


class MalKey(MalAny):
    """Type for mal that can be used as a map key"""

    def __init__(self, value: str) -> None:
        self.value = value
        super().__init__()

    def __str__(self) -> str:
        return '"' + self.value + '"'


Mal_Environment = Dict[str, MalAny]
Mal_Function = Callable[[List[MalAny]], MalAny]
Mal_Map = Dict[MalKey, MalAny]


class MalSeq(MalAny):
    """Sequence type for Mal - lists or vectors"""

    def __init__(self, value: List[MalAny]) -> None:
        self.value: List[MalAny] = value
        super().__init__()


class MalList(MalSeq):
    """List type for mal"""

    def __str__(self) -> str:
        return "(" + " ".join(map(str, self.value)) + ")"


class MalVec(MalSeq):
    """Vector type for mal"""

    def __str__(self) -> str:
        return "[" + " ".join(map(str, self.value)) + "]"





class MalMap(MalAny):
    """Mal type for mal"""

    value: Mal_Map

    def __init__(self, elements: Union[List[MalAny], Tuple[List[MalKey], List[MalAny]]]) -> None:

        # Construct from an alternating key-value list
        if isinstance(elements, list):
            if len(elements) % 2 == 1:
                raise err.EvalError("Unmatched key for map", str(elements[-1]))
            self.value = {}
            for k, v in zip(elements[0::2], elements[1::2]):
                if not isinstance(k, MalKey):
                    raise err.EvalError("Bad key type for map", str(k))
                self.value[k] = v

        # Construct from a tuple of keys and values
        else:
            keys, values = elements
            self.value = dict(zip(keys, values))

        super().__init__()

    def __str__(self) -> str:
        accumulated: List[str] = []
        for k in self.value:
            accumulated.append(str(k))
            accumulated.append(str(self.value[k]))
        return  "{" + " ".join(map(str, accumulated)) + "}"




class MalFunc(MalAny):
    """Type for mal functions"""

    def __init__(self, value: Mal_Function) -> None:
        self.value: Mal_Function = value

    def __str__(self) -> str:
        return "#<function>"


class MalKeyword(MalKey):
    """Keyword type for mal"""

    def __str__(self):
        return ":" + self.value


class MalStr(MalKey):
    """String type for mal"""

    def __str__(self) -> str:
        return '"' + self.value + '"'

    def str_readable(self) -> str:
        return '"' + add_escapes(self.value) + '"'


class MalSym(MalAny):
    """Symbol type for mal"""

    def __init__(self, value: str):
        self.value: str = value
        super().__init__()

    def __str__(self) -> str:
        return self.value


class MalNum(MalAny):
    """Number type for mal"""

    def __init__(self, value: int) -> None:
        self.value: int = value
        super().__init__()

    def __str__(self) -> str:
        return str(self.value)


class MalBool(MalAny):
    """Boolean type for mal"""

    def __init__(self, value: bool) -> None:
        self.value: bool = value
        super().__init__()

    def __str__(self) -> str:
        return "true" if self.value else "false"


class MalNil(MalAny):
    """Nil type for mal"""

    def __str__(self) -> str:
        return "nil"
