"""Implements step 2 of https://github.com/kanaka/mal - eval"""

import operator
from typing import Callable, cast, List

from mal_errors import EvalError, InternalError, ReaderError
from mal_types import MalAny, MalFunc, MalList, MalVec, MalMap, MalSeq, MalSym, MalNum
from env import Environment
from printer import pr_str
from reader import read_str


def EVAL(ast: MalAny, env: Environment) -> MalAny:
    """Top-level eval function that handles apply"""

    # apply for a non-empty list
    if isinstance(ast, MalList) and len(ast.value) > 0:
        evaluated = eval_ast(ast, env)
        if not isinstance(evaluated, MalList): # For type checker - should always be a MalSeq
            raise InternalError("Expected a MalList")
        head = evaluated.value[0]
        if isinstance(head, MalFunc):
            return head.value(evaluated.value[1:])
        raise EvalError("Cannot apply a non-function", str(ast))

    # Use eval_ast for all other values
    return eval_ast(ast, env)


def eval_ast(ast: MalAny, env: Environment) -> MalAny:
    """Eval function"""

    # A symbol evaluates to its value in the environment
    if isinstance(ast, MalSym):
        return env.get(ast.value)

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
    """Read a mal element from the given string"""
    return read_str(input_string)


def PRINT(ast: MalAny) -> None:
    """Prints the string form of its argument to stdout"""
    print(pr_str(ast, True))


def rep(input_string: str, env: Environment) -> None:
    """Calls read-eval-print on its argument"""

    try:
        PRINT(EVAL(READ(input_string), env))
    except (EvalError, ReaderError) as err:
        print(err)


def int_fn(op: Callable[[int, int], int]) -> MalFunc:
    """Helper function to make a quick-and-dirty mal function from an integer function"""

    def f(xs: List[MalAny]) -> MalAny:
        x1 = cast(MalNum, xs[0]).value
        x2 = cast(MalNum, xs[1]).value
        return MalNum(op(x1, x2))

    return MalFunc(f)


def rep_loop() -> None:
    """Repeatedly provides user prompt and passes the input to read-eval-print"""

    repl_env = Environment()
    repl_env.set("+", int_fn(operator.add))
    repl_env.set("-", int_fn(operator.sub))
    repl_env.set("*", int_fn(operator.mul))
    repl_env.set("/", int_fn(operator.floordiv))

    while True:
        try:
            rep(input("user> "), repl_env)
        except EOFError:
            break


if __name__ == "__main__":
    rep_loop()
