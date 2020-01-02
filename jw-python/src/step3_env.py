"""Implements step 3 of https://github.com/kanaka/mal - env."""

import operator
from typing import Callable, List, cast

import mal_errors

from mal_types import (
    Environment,
    MalAny,
    MalBuiltin,
    MalList,
    MalMap,
    MalNum,
    MalSeq,
    MalSym,
    MalVec,
)

import printer

import reader

import utils


def EVAL(ast: MalAny, env: Environment) -> MalAny:
    """Top-level eval function that handles apply."""
    # apply for a non-empty list
    if isinstance(ast, MalList) and len(ast.value) > 0:

        list_contents = ast.value
        head = list_contents[0]
        args = list_contents[1:]
        num_args = len(args)

        # Special form def!
        if head == MalSym("def!"):
            if num_args == 2 and isinstance(args[0], MalSym):
                val = EVAL(args[1], env)
                env.set(args[0], val)
                return val
            raise mal_errors.EvalError("Bad arguments for def!", str(ast))

        # Special form let!
        if head == MalSym("let*"):
            if num_args == 2 and isinstance(args[0], MalSeq):
                local_env = Environment(outer=env)
                for sym, binding in utils.pairs(args[0].value):
                    if isinstance(sym, MalSym):
                        local_env.set(sym, EVAL(binding, local_env))
                return EVAL(args[1], local_env)
            raise mal_errors.EvalError("Bad arguments for let*", str(ast))

        # Apply normal list
        evaluated = eval_ast(ast, env)
        if not isinstance(evaluated, MalList):
            raise mal_errors.InternalError("Expected a MalList")  # For type checker
        eval_head = evaluated.value[0]
        if isinstance(eval_head, MalBuiltin):
            return eval_head.value(evaluated.value[1:])
        raise mal_errors.EvalError("Cannot apply a non-function", str(ast))

    # Use eval_ast for all other values
    return eval_ast(ast, env)


def eval_ast(ast: MalAny, env: Environment) -> MalAny:
    """Eval function."""
    # A symbol evaluates to its value in the environment
    if isinstance(ast, MalSym):
        return env.get(ast)

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
    return reader.read_str(input_string)


def PRINT(ast: MalAny) -> None:
    """Print the string form of its argument to stdout."""
    print(printer.pr_str(ast, True))


def rep(input_string: str, env: Environment) -> None:
    """Call read-eval-print on its argument."""
    try:
        PRINT(EVAL(READ(input_string), env))
    except (mal_errors.EvalError, mal_errors.ReaderError) as err:
        print(err)


def int_fn(op: Callable[[int, int], int]) -> MalBuiltin:
    """Make a quick-and-dirty mal function (helper function)."""

    def f(xs: List[MalAny]) -> MalAny:
        x1 = cast(MalNum, xs[0]).value
        x2 = cast(MalNum, xs[1]).value
        return MalNum(op(x1, x2))

    return MalBuiltin(f)


def rep_loop() -> None:
    """Repeatedly provide user prompt and passes the input to read-eval-print."""
    repl_env = Environment()
    repl_env.set(MalSym("+"), int_fn(operator.add))
    repl_env.set(MalSym("-"), int_fn(operator.sub))
    repl_env.set(MalSym("*"), int_fn(operator.mul))
    repl_env.set(MalSym("/"), int_fn(operator.floordiv))

    while True:
        try:
            rep(input("user> "), repl_env)
        except EOFError:
            break


if __name__ == "__main__":
    rep_loop()