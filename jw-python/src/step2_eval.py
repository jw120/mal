"""Implements step 2 of https://github.com/kanaka/mal - eval."""


from typing import Callable, List, Optional, cast

import mal_errors

from mal_types import MalAny, MalBuiltin, MalList, MalMap, MalSeq, MalSym, MalVec
from mal_types import Mal_Environment

import printer

import reader

# Simple environment for step 2
repl_env: Mal_Environment = {
    "+": MalBuiltin(lambda xs: xs[0] + xs[1]),
    "-": MalBuiltin(lambda xs: xs[0] - xs[1]),
    "*": MalBuiltin(lambda xs: xs[0] * xs[1]),
    "/": MalBuiltin(lambda xs: xs[0] // xs[1]),
}


def EVAL(ast: MalAny, env: Mal_Environment) -> MalAny:
    """Top-level eval function that handles apply."""
    # apply for a non-empty list
    if isinstance(ast, MalList) and len(ast.value) > 0:
        evaluated = eval_ast(ast, env)
        if not isinstance(
            evaluated, MalList
        ):  # For type checker - should always be a MalSeq
            raise mal_errors.InternalError("Expected a MalList")
        head = evaluated.value[0]
        if isinstance(head, MalBuiltin):
            f = cast(Callable[[List[MalAny]], MalAny], head.value)  # Workaround
            return f(evaluated.value[1:])
        raise mal_errors.EvalError("Cannot apply a non-function", str(ast))

    # Use eval_ast for all other values
    return eval_ast(ast, env)


def eval_ast(ast: MalAny, env: Mal_Environment) -> MalAny:
    """Eval function."""
    # A symbol evaluates to its value in the environment
    if isinstance(ast, MalSym):
        if ast.value in env:
            return env[ast.value]
        raise mal_errors.EvalError("Unknown symbol", str(ast))

    # A list or vector has all of its contents evaluated
    if isinstance(ast, MalSeq):
        evaluated = list(map(lambda x: EVAL(x, env), ast.value))
        return MalVec(evaluated) if isinstance(ast, MalVec) else MalList(evaluated)

    # A map has its values evaluated
    if isinstance(ast, MalMap):
        evaluated = list(map(lambda x: EVAL(x, env), ast.value.values()))
        return MalMap((list(ast.value.keys()), evaluated))

    # Any other type is just returned
    return ast


def READ(input_string: str) -> Optional[MalAny]:
    """Read a mal element from the given string."""
    return reader.read_str(input_string)


def PRINT(ast: MalAny) -> None:
    """Print the string form of its argument to stdout."""
    print(printer.pr_str(ast, True))


def rep(input_string: str) -> None:
    """Call read-eval-print on its argument."""
    try:
        input_form = READ(input_string)
        if input_form is not None:
            PRINT(EVAL(input_form, repl_env))
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
