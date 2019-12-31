"""Implements step 2 of https://github.com/kanaka/mal - eval."""


from typing import cast

from mal_errors import EvalError, InternalError, ReaderError

from mal_types import MalAny, MalFunc, MalList, MalMap, MalNum, MalSeq, MalSym, MalVec
from mal_types import Mal_Environment

from printer import pr_str

from reader import read_str


# Simple environment for step 2
repl_env: Mal_Environment = {
    "+": MalFunc(
        lambda xs: MalNum(cast(MalNum, xs[0]).value + cast(MalNum, xs[1]).value)
    ),
    "-": MalFunc(
        lambda xs: MalNum(cast(MalNum, xs[0]).value - cast(MalNum, xs[1]).value)
    ),
    "*": MalFunc(
        lambda xs: MalNum(cast(MalNum, xs[0]).value * cast(MalNum, xs[1]).value)
    ),
    "/": MalFunc(
        lambda xs: MalNum(cast(MalNum, xs[0]).value // cast(MalNum, xs[1]).value)
    ),
}


def EVAL(ast: MalAny, env: Mal_Environment) -> MalAny:
    """Top-level eval function that handles apply."""
    # apply for a non-empty list
    if isinstance(ast, MalList) and len(ast.value) > 0:
        evaluated = eval_ast(ast, env)
        if not isinstance(
            evaluated, MalList
        ):  # For type checker - should always be a MalSeq
            raise InternalError("Expected a MalList")
        head = evaluated.value[0]
        if isinstance(head, MalFunc):
            return head.value(evaluated.value[1:])
        raise EvalError("Cannot apply a non-function", str(ast))

    # Use eval_ast for all other values
    return eval_ast(ast, env)


def eval_ast(ast: MalAny, env: Mal_Environment) -> MalAny:
    """Eval function."""
    # A symbol evaluates to its value in the environment
    if isinstance(ast, MalSym):
        if ast.value in env:
            return env[ast.value]
        raise EvalError("Unknown symbol", str(ast))

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


def READ(input_string: str) -> MalAny:
    """Read a mal element from the given string."""
    return read_str(input_string)


def PRINT(ast: MalAny) -> None:
    """Print the string form of its argument to stdout."""
    print(pr_str(ast, True))


def rep(input_string: str) -> None:
    """Call read-eval-print on its argument."""
    try:
        PRINT(EVAL(READ(input_string), repl_env))
    except (EvalError, ReaderError) as err:
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
