"""Implements step 1 of https://github.com/kanaka/mal - read and print."""


from typing import Optional

import mal_errors

from mal_types import MalAny

import printer

import reader


def READ(input_string: str) -> Optional[MalAny]:
    """Read a mal element from the given string."""
    return reader.read_str(input_string)


def EVAL(ast: MalAny) -> MalAny:
    """Just passes through its argument (dummy eval function)."""
    return ast


def PRINT(ast: MalAny) -> None:
    """Print the string form of its argument to stdout."""
    print(printer.pr_str(ast, True))


def rep(input_string: str) -> None:
    """Call read-eval-print on its argument."""
    try:
        input_form = READ(input_string)
        if input_form is not None:
            PRINT(EVAL(input_form))
    except (mal_errors.EvalError, mal_errors.ReaderError) as err:
        print(err)


def rep_loop() -> None:
    """Repeatedly provides user prompt and passes the input to read-eval-print."""
    while True:
        try:
            rep(input("user> "))
        except EOFError:
            break


if __name__ == "__main__":
    rep_loop()
