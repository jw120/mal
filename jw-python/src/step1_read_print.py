"""Implements step0 of https://github.com/kanaka/mal - read and print"""


from mal import MalType, ReaderError
from printer import pr_str
from reader import read_str


def READ(input_string: str) -> MalType:
    """Read a mal element from the given string"""
    return read_str(input_string)


def EVAL(ast: MalType) -> MalType:
    """Dummy eval function - just passes through its argument"""
    return ast


def PRINT(ast: MalType) -> None:
    """Prints the string form of its argument to stdout"""
    print(pr_str(ast))


def rep(input_string: str) -> None:
    """Calls read-eval-print on its argument"""

    try:
        PRINT(EVAL(READ(input_string)))
    except ReaderError as err:
        print(err)


def rep_loop() -> None:
    """Repeatedly provides user prompt and passes the input to read-eval-print"""
    while True:
        try:
            rep(input("user> "))
        except EOFError:
            break


if __name__ == "__main__":
    rep_loop()
