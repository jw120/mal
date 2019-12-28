"""Type definitions - mainly the AST"""

#pylint: disable=too-few-public-methods

from typing import List

class MalType:
    """Abstract type for any Mal element"""


class MalList(MalType):
    """List type for MAL"""

    def __init__(self, elements: List[MalType]):
        self.elements = elements
        super().__init__()

    def __str__(self):
        return "(" + " ".join(map(str, self.elements)) + ")"


class MalNum(MalType):
    """Number type for MAL"""

    def __init__(self, value: int):
        self.value = value
        super().__init__()

    def __str__(self):
        return str(self.value)


class MalSym(MalType):
    """Symbol type for MAL"""

    def __init__(self, value: str):
        self.value = value
        super().__init__()

    def __str__(self):
        return self.value

class MalStr(MalType):
    """String type for MAL"""

    def __init__(self, value: str):
        self.value = value
        super().__init__()

    def __str__(self):
        return '"' + self.value + '"'


class MalNil(MalType):
    """Nil type for MAL"""

    def __str__(self):
        return "nil"


class MalBool(MalType):
    """Boolean type for MAL"""

    def __init__(self, value: bool):
        self.value = value
        super().__init__()


    def __str__(self):
        return "true" if self.value else "false"
