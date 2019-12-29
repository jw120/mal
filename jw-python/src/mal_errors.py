"""Provides exception type definitions

## Error hierarchy

* MalError
    + EvalError - runtime error during evaluation
    + InternalError - unexpected failure
    + ReaderError - failure due to malformed input

"""

from typing import Optional


class MalError(Exception):
    """Base class for mal exceptions"""


class EvalError(MalError):
    """Exception raised for errors while usercode is being evaluated

    Attribute:
        message -- explanation of the error
        expression -- location of the error
    """

    def __init__(self, message: str, expression: Optional[str] = None):
        self.message = message
        self.expression = expression
        super().__init__()

    def __str__(self):
        expression_msg = "" if self.expression is None else " in " + self.expression
        return "Evaluation error: " + self.message + expression_msg


class InternalError(MalError):
    """Exception raised for internal errors (should only be caused by bugs in the python code)

    Attribute:
        message -- explanation of the error
    """

    def __init__(self, message: str):
        self.message = message
        super().__init__()

    def __str__(self):
        return "Internal error: " + self.message


class ReaderError(MalError):
    """Exception raised for errors in the reader

    Attributes:
        message -- explanation of the error
        input_expression -- input expression in which the error occurred
    """

    def __init__(self, message: str, expression: Optional[str] = None):
        self.message = message
        self.expression = expression
        super().__init__()

    def __str__(self):
        expression_msg = "" if self.expression is None else " at " + self.expression
        return "Reader error: " + self.message + expression_msg
