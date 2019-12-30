"""Implements step 4 of https://github.com/kanaka/mal - if, fn, do"""

from typing import Callable, cast, List

from mal_errors import EvalError, InternalError, ReaderError
from mal_types import (
    MalAny,
    MalFunc,
    MalList,
    MalVec,
    MalMap,
    MalSeq,
    MalSym,
    MalNum,
    MalNil,
    MalBool,
)
import core
from env import Environment
from printer import pr_str
from reader import read_str
from utils import pairs


def EVAL(ast: MalAny, env: Environment) -> MalAny:
    """Top-level eval function that handles apply"""

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
            raise EvalError("Bad arguments for def!", str(ast))

        # Special form do
        if head == MalSym("do"):
            if num_args >= 1:
                evaluated_args = list(map(lambda e: EVAL(e, env), args))
                return evaluated_args[-1]
            raise EvalError("Bad arguments for do!", str(ast))

        # Special form fn*
        if head == MalSym("fn*"):
            if num_args == 2:
                if isinstance(args[0], MalList):
                    if all(map(lambda x: isinstance(x, MalSym), args[0].value)):
                        bind_syms = cast(List[MalSym], args[0].value)  # For type check

                        def closure(call_args: List[MalAny]):
                            closure_env = Environment(bind_syms, call_args, outer=env)
                            return EVAL(args[1], closure_env)

                        return MalFunc(closure)
            raise EvalError("Bad arguments for fn*", str(ast))

        # Special form if
        if head == MalSym("if"):
            if num_args in [2, 3]:
                evaluated_first = EVAL(args[0], env)
                if evaluated_first in [MalNil(), MalBool(False)]:
                    return EVAL(args[2], env) if num_args == 3 else MalNil()
                return EVAL(args[1], env)
            raise EvalError("Bad arguments for if", str(ast))

        # Special form let!
        if head == MalSym("let*"):
            if num_args == 2 and isinstance(args[0], MalSeq):
                local_env = Environment(outer=env)
                for sym, binding in pairs(args[0].value):
                    if isinstance(sym, MalSym):
                        local_env.set(sym, EVAL(binding, local_env))
                return EVAL(args[1], local_env)
            raise EvalError("Bad arguments for let*", str(ast))

        # Apply normal list
        evaluated_ast = eval_ast(ast, env)
        if not isinstance(evaluated_ast, MalList):
            raise InternalError("Expected a MalList")  # For type checker
        evaluated_head = evaluated_ast.value[0]
        if isinstance(evaluated_head, MalFunc):
            return evaluated_head.value(evaluated_ast.value[1:])
        raise EvalError("Cannot apply a non-function", str(ast))

    # Use eval_ast for all other values
    return eval_ast(ast, env)


def eval_ast(ast: MalAny, env: Environment) -> MalAny:
    """Eval function"""

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
    """Helper function to make a quick-and-dirty mal function"""

    def f(xs: List[MalAny]) -> MalAny:
        x1 = cast(MalNum, xs[0]).value
        x2 = cast(MalNum, xs[1]).value
        return MalNum(op(x1, x2))

    return MalFunc(f)


def rep_loop() -> None:
    """Repeatedly provides user prompt and passes the input to read-eval-print"""

    repl_env = Environment()
    for sym_name in core.ns:
        repl_env.set(MalSym(sym_name), core.ns[sym_name])

    while True:
        try:
            rep(input("user> "), repl_env)
        except EOFError:
            break


if __name__ == "__main__":
    rep_loop()
