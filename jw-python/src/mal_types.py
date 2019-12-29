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

    def str_readable(self):
        """Default implementation of readable string, overridedn by string types"""
        return str(self)


Mal_Environment = Dict[str, MalAny]


class MalSeq(MalAny):
    """Sequence type for Mal - lists or vectors"""

    value: List[MalAny]

    def __init__(self, value: List[MalAny]):
        self.value = value
        super().__init__()


class MalList(MalSeq):
    """List type for mal"""

    def __str__(self):
        return "(" + " ".join(map(str, self.value)) + ")"


class MalVec(MalSeq):
    """Vector type for mal"""

    def __str__(self):
        return "[" + " ".join(map(str, self.value)) + "]"


class MalKey(MalAny):
    """Type for mal that can be used as a map key"""

    def __init__(self, value: str):
        self.value = value
        super().__init__()

    def __str__(self):
        return '"' + self.value + '"'


class MalMap(MalAny):
    """Mal type for mal"""

    value: Dict[MalKey, MalAny]

    def __init__(self, elements: Union[List[MalAny], Tuple[List[MalKey], List[MalAny]]]):

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

    def __str__(self):
        accumulated: List[str] = []
        for k in self.value:
            accumulated.append(k)
            accumulated.append(self.value[k])
        return  "{" + " ".join(map(str, accumulated)) + "}"


Mal_Function = Callable[[List[MalAny]], MalAny]


class MalFunc(MalAny):
    """Type for mal functions"""

#    value: Mal_Function

    def __init__(self, value: Mal_Function):
        self.value = value

    def __str__(self):
        return "#<function>"


class MalKeyword(MalKey):
    """Keyword type for mal"""

    def __str__(self):
        return ":" + self.value


class MalStr(MalKey):
    """String type for mal"""

    def __str__(self):
        return '"' + self.value + '"'

    def str_readable(self):
        return '"' + add_escapes(self.value) + '"'


class MalSym(MalAny):
    """Symbol type for mal"""

    value: str

    def __init__(self, value: str):
        self.value = value
        super().__init__()

    def __str__(self):
        return self.value


class MalNum(MalAny):
    """Number type for mal"""

    value: int

    def __init__(self, value: int):
        self.value = value
        super().__init__()

    def __str__(self):
        return str(self.value)


class MalBool(MalAny):
    """Boolean type for mal"""

    value: bool

    def __init__(self, value: bool):
        self.value = value
        super().__init__()

    def __str__(self):
        return "true" if self.value else "false"


class MalNil(MalAny):
    """Nil type for mal"""

    def __str__(self):
        return "nil"
