"""Implements step 4 of https://github.com/kanaka/mal - if, fn, do."""

from typing import Callable, List, Optional, cast

import core

from mal_types import (
    Environment,
    MalAny,
    MalBuiltin,
    MalException,
    MalList,
    MalMap,
    MalNil,
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
            raise MalException("Bad arguments for def! in ", ast)

        # Special form do
        if head == MalSym("do"):
            if num_args >= 1:
                evaluated_args = list(map(lambda e: EVAL(e, env), args))
                return evaluated_args[-1]
            raise MalException("Bad arguments for do! in ", ast)

        # Special form fn*
        if head == MalSym("fn*"):
            if num_args == 2:
                if isinstance(args[0], MalSeq):
                    if all(map(lambda x: isinstance(x, MalSym), args[0].value)):
                        bind_syms = cast(List[MalSym], args[0].value)  # For type check

                        def closure(call_args: List[MalAny]) -> MalAny:
                            closure_env = Environment(bind_syms, call_args, outer=env)
                            return EVAL(args[1], closure_env)

                        return MalBuiltin(closure)
            raise MalException("Bad arguments for fn* in ", ast)

        # Special form if
        if head == MalSym("if"):
            if num_args in [2, 3]:
                evaluated_first = EVAL(args[0], env)
                if isinstance(evaluated_first, MalNil) or (
                    isinstance(evaluated_first, bool) and (not evaluated_first)
                ):
                    return EVAL(args[2], env) if num_args == 3 else MalNil()
                return EVAL(args[1], env)
            raise MalException("Bad arguments for if in ", ast)

        # Special form let!
        if head == MalSym("let*"):
            if num_args == 2 and isinstance(args[0], MalSeq):
                local_env = Environment(outer=env)
                for sym, binding in utils.pairs(args[0].value):
                    if isinstance(sym, MalSym):
                        local_env.set(sym, EVAL(binding, local_env))
                return EVAL(args[1], local_env)
            raise MalException("Bad arguments for let* in ", ast)

        # Apply normal list
        evaluated_ast = eval_ast(ast, env)
        if not isinstance(evaluated_ast, MalList):
            raise MalException("Expected a MalList")  # For type checker
        eval_head = evaluated_ast.value[0]
        if isinstance(eval_head, MalBuiltin):
            f = cast(Callable[[List[MalAny]], MalAny], eval_head.value)  # Workaround
            return f(evaluated_ast.value[1:])
        raise MalException("Cannot apply a non-function in ", ast)

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


def READ(input_string: str) -> Optional[MalAny]:
    """Read a mal element from the given string."""
    return reader.read_str(input_string)


def PRINT(ast: MalAny) -> None:
    """Print the string form of its argument to stdout."""
    print(printer.pr_str(ast, True))


def rep(input_string: str, env: Environment) -> None:
    """Call read-eval-print on its argument."""
    try:
        input_form = READ(input_string)
        if input_form is not None:
            PRINT(EVAL(input_form, env))
    except MalException as err:
        print(printer.pr_str(err.value, False))


def rep_loop() -> None:
    """Repeatedly provide user prompt and passes the input to read-eval-print."""
    repl_env = Environment()
    core_ns = core.create_ns()
    for sym_name in core_ns:
        repl_env.set(MalSym(sym_name), core_ns[sym_name])

    prelude_form = READ("(def! not (fn* (a) (if a false true)))")
    assert prelude_form is not None
    EVAL(prelude_form, repl_env)

    while True:
        try:
            rep(input("user> "), repl_env)
        except EOFError:
            break


if __name__ == "__main__":
    rep_loop()
