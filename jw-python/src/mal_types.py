"""Provides type defintions for our AST and for our exceptions

## Class hierarchy:

* MalAny
    + MalSeq (provides shared implementation and supports functions which work on both)
        - MalList
        - MalVec
    + MalMap
    + MalKey (valid keys for a map)
        - MalKeyword
        - MalStr
    + MalSym
    + MalNum
    + MalBool
    + MalNil

"""

#pylint: disable=too-few-public-methods

from typing import Dict, List

import mal_errors as err
from utils import add_escapes


class MalAny:
    """Abstract type for any Mal element"""

    def str_readable(self):
        """Default implementation of readable string, overridedn by string types"""
        return str(self)


class MalSeq(MalAny):
    """Sequence type for Mal - lists or vectors"""

    def __init__(self, elements: List[MalAny]):
        self.elements = elements
        super().__init__()


class MalList(MalSeq):
    """List type for mal"""

    def __str__(self):
        return "(" + " ".join(map(str, self.elements)) + ")"


class MalVec(MalSeq):
    """Vector type for mal"""

    def __str__(self):
        return "[" + " ".join(map(str, self.elements)) + "]"



class MalMap(MalAny):
    """Mal type for mal"""

    def __init__(self, elements: List[MalAny]):

        if len(elements) % 2 == 1:
            raise err.EvalError("Unmatched key for map", str(elements[-1]))
        self.value: Dict[MalKey, MalAny] = {}
        for k, v in zip(elements[0::2], elements[1::2]):
            if not isinstance(k, MalKey):
                raise err.EvalError("Bad key type for map", str(k))
            self.value[k] = v
        super().__init__()

    def __str__(self):
        accumulated: List[str] = []
        for k in self.value:
            accumulated.append(k)
            accumulated.append(self.value[k])
        return  "{" + " ".join(map(str, accumulated)) + "}"


class MalKey(MalAny):
    """Type for mal that can be used as a map key"""

    def __init__(self, value: str):
        self.value = value
        super().__init__()

    def __str__(self):
        return '"' + self.value + '"'


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

    def __init__(self, value: str):
        self.value = value
        super().__init__()

    def __str__(self):
        return self.value


class MalNum(MalAny):
    """Number type for mal"""

    def __init__(self, value: int):
        self.value = value
        super().__init__()

    def __str__(self):
        return str(self.value)


class MalBool(MalAny):
    """Boolean type for mal"""

    def __init__(self, value: bool):
        self.value = value
        super().__init__()

    def __str__(self):
        return "true" if self.value else "false"


class MalNil(MalAny):
    """Nil type for mal"""

    def __str__(self):
        return "nil"
