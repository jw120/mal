"""Implements step 5 of https://github.com/kanaka/mal - tco."""

from enum import Enum, auto
from typing import Callable, Dict, List, NamedTuple, Optional, cast

import core

import mal_errors

from mal_types import (
    Environment,
    MalAny,
    MalBuiltin,
    MalFunc,
    MalList,
    MalMap,
    MalNil,
    MalSeq,
    MalSym,
    MalVec,
    to_symlist,
)

import printer

import reader

import utils


class EvalMode(Enum):
    """Whether EVAL should continue. Part of EvalState."""

    CONTINUING = auto()
    FINISHED = auto()


class EvalState(NamedTuple):
    """State of the EVAL function when iterating.

    Used for TCO to avoid EVAL calling EVAL.
    """

    ast: MalAny
    env: Environment
    mode: EvalMode


def def_handler(args: List[MalAny], env: Environment) -> EvalState:
    """Handle the special form def!."""
    if len(args) == 2 and isinstance(args[0], MalSym):
        val = EVAL(args[1], env)
        env.set(args[0], val)
        return EvalState(val, env, EvalMode.FINISHED)
    raise mal_errors.EvalError("Bad arguments for def!", str(args))


def do_handler(args: List[MalAny], env: Environment) -> EvalState:
    """Handle the special form do."""
    if len(args) >= 1:
        for arg in args[:-1]:  # Evaluate all args except the last one
            eval_ast(arg, env)
        return EvalState(args[-1], env, EvalMode.CONTINUING)
    raise mal_errors.EvalError("Bad arguments for do!", str(args))


def if_handler(args: List[MalAny], env: Environment) -> EvalState:
    """Handle the special form if."""
    if len(args) in [2, 3]:
        evaluated_first = EVAL(args[0], env)
        if isinstance(evaluated_first, MalNil) or (
            isinstance(evaluated_first, bool) and (not evaluated_first)
        ):
            if len(args) == 3:
                return EvalState(args[2], env, EvalMode.CONTINUING)
            else:
                return EvalState(MalNil(), env, EvalMode.FINISHED)
        return EvalState(args[1], env, EvalMode.CONTINUING)
    raise mal_errors.EvalError("Bad arguments for if", str(args))


def fn_handler(args: List[MalAny], env: Environment) -> EvalState:
    """Handle the special form fn*."""
    if len(args) == 2:
        return EvalState(
            MalFunc(args[1], to_symlist(args[0]), env), env, EvalMode.FINISHED,
        )
    raise mal_errors.EvalError("Bad arguments for fn*", str(args))


def let_handler(args: List[MalAny], env: Environment) -> EvalState:
    """Handle the special form let*."""
    if len(args) == 2 and isinstance(args[0], MalSeq):
        new_env = Environment(outer=env)
        for sym, binding in utils.pairs(args[0].value):
            if isinstance(sym, MalSym):
                new_env.set(sym, EVAL(binding, env))
            else:
                raise mal_errors.EvalError("Non-symbol in let*", str(sym))
        return EvalState(args[1], new_env, EvalMode.CONTINUING)
    raise mal_errors.EvalError("Bad arguments for let*", str(args))


special_form_handlers: Dict[str, Callable[[List[MalAny], Environment], EvalState]] = {
    "def!": def_handler,
    "do": do_handler,
    "fn*": fn_handler,
    "if": if_handler,
    "let*": let_handler,
}


def EVAL(entry_ast: MalAny, entry_env: Environment) -> MalAny:
    """Top-level eval function that handles apply."""
    current = EvalState(entry_ast, entry_env, EvalMode.CONTINUING)

    while current.mode == EvalMode.CONTINUING:

        # Hand-off non-list (and empty list)
        if not isinstance(current.ast, MalList) or len(current.ast.value) == 0:
            return eval_ast(current.ast, current.env)
        elements: List[MalAny] = current.ast.value
        head: MalAny = elements[0]

        # Handle special forms
        if isinstance(head, MalSym) and head.value in list(special_form_handlers):
            current = special_form_handlers[head.value](
                current.ast.value[1:], current.env
            )
            continue

        # Evaluate all the list elements
        evaluated_elements: List[MalAny] = list(
            map(lambda x: EVAL(x, current.env), elements)
        )
        evaluated_head: MalAny = evaluated_elements[0]

        # Apply a mal-defined function
        if isinstance(evaluated_head, MalFunc):
            current = EvalState(
                evaluated_head.ast,
                Environment(
                    evaluated_head.params,
                    evaluated_elements[1:],
                    outer=evaluated_head.env,
                ),
                EvalMode.CONTINUING,
            )
            continue

        # Apply a python-defined function
        if isinstance(evaluated_head, MalBuiltin):
            f = cast(
                Callable[[List[MalAny]], MalAny], evaluated_head.value
            )  # Workaround
            return f(evaluated_elements[1:])

        # Fail if applying a non-function
        raise mal_errors.EvalError("Cannot apply a non-function", str(current.ast))

    return current.ast


def eval_ast(ast: MalAny, env: Environment) -> MalAny:
    """Second-level eval function that does not apply."""
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
    except (mal_errors.EvalError, mal_errors.ReaderError) as err:
        print(err)


def rep_loop() -> None:
    """Repeatedly provide user prompt and passes the input to read-eval-print."""
    repl_env = Environment()
    core_ns = core.create_ns()
    for sym_name in core_ns:
        repl_env.set(MalSym(sym_name), core_ns[sym_name])

    prelude_form = READ("(def! not (fn* (a) (if a false true)))")
    if prelude_form is None:
        raise mal_errors.InternalError("Unexpected None reading prelude")

    EVAL(prelude_form, repl_env)
    while True:
        try:
            rep(input("user> "), repl_env)
        except EOFError:
            break


if __name__ == "__main__":
    rep_loop()
