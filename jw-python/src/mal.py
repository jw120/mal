"""Type definitions - mainly the AST"""

#pylint: disable=too-few-public-methods

from typing import List, Optional


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


class MalError(Exception):
    """Base class for mal exceptions"""


class ReaderError(MalError):
    """Exception raised for errors in the reader

    Attributes:
        expression -- input expression in which the error occurred
        message -- explanation of the error
    """

    def __init__(self, message: str, expression: Optional[str] = None):
        self.expression = expression
        self.message = message
        super().__init__()

    def __str__(self):
        expression_msg = "" if self.expression is None else " at " + self.expression
        return "Reader Error: " + self.message + expression_msg


class InternalError(MalError):
    """Exception raised for internal errors (should only be caused by bugs in the python code)

    Attribute:
        message -- explanation of the error
    """

    def __init__(self, message: str):
        self.message = message
        super().__init__()

    def __str__(self):
        return "Internal Error: " + self.message
