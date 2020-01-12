"""Implements step 6 of https://github.com/kanaka/mal - file."""

from enum import Enum, auto
from sys import argv
from typing import Callable, Dict, List, NamedTuple, Optional, Sequence, cast

import core

import mal_errors

from mal_types import (
    Environment,
    MalAny,
    MalAtom,
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


def defmacro_handler(args: List[MalAny], env: Environment) -> EvalState:
    """Handle the special form defmacro!."""
    if len(args) == 2 and isinstance(args[0], MalSym):
        val = EVAL(args[1], env)
        if isinstance(val, MalFunc):
            val.is_macro = True
            env.set(args[0], val)
            return EvalState(val, env, EvalMode.FINISHED)
        raise mal_errors.EvalError("Non-function in defmacro!", str(args))
    raise mal_errors.EvalError("Bad arguments for defmacro!", str(args))


def do_handler(args: List[MalAny], env: Environment) -> EvalState:
    """Handle the special form do."""
    if len(args) >= 1:
        for arg in args[:-1]:  # Evaluate all args except the last one
            EVAL(arg, env)
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


def macroexpand_handler(args: List[MalAny], env: Environment) -> EvalState:
    """Handle the special form macroexpand."""
    if len(args) == 1:
        return EvalState(macroexpand(args[0], env), env, EvalMode.FINISHED)
    raise mal_errors.EvalError("Bad arguments for quote", str(args))


def quote_handler(args: List[MalAny], env: Environment) -> EvalState:
    """Handle the special form quote."""
    if len(args) == 1:
        return EvalState(args[0], env, EvalMode.FINISHED)
    raise mal_errors.EvalError("Bad arguments for quote", str(args))


def quasiquote_handler(args: List[MalAny], env: Environment) -> EvalState:
    """Handle the special form quasiquote."""
    if len(args) == 1:

        return EvalState(quasiquote(args[0]), env, EvalMode.CONTINUING)
    raise mal_errors.EvalError("Bad arguments for quasiquote", str(args))


def try_handler(args: List[MalAny], env: Environment) -> EvalState:
    """Handle the special form try*."""
    if len(args) == 2:
        try:
            return EvalState(EVAL(args[0], env), env, EvalMode.FINISHED)
        except mal_errors.MalError as err:
            if isinstance(args[1], MalSeq):
                catch_clause: List[MalAny] = args[1].value
                if (
                    len(catch_clause) == 3
                    and catch_clause[0] == MalSym("catch*")
                    and isinstance(catch_clause[1], MalSym)
                ):
                    catch_binding: MalSym = catch_clause[1]
                    catch_body: MalAny = catch_clause[2]
                    err_value: MalAny = err.value if isinstance(
                        err, mal_errors.UserException
                    ) else str(err)
                    return EvalState(
                        catch_body,
                        Environment([catch_binding], [err_value], outer=env),
                        EvalMode.CONTINUING,
                    )
                raise mal_errors.EvalError("Bad catch clause in try*", str(args))
            raise mal_errors.EvalError("Missing catch clause in try*", str(args))
    raise mal_errors.EvalError("Bad arguments for try*", str(args))


special_form_handlers: Dict[str, Callable[[List[MalAny], Environment], EvalState]] = {
    "def!": def_handler,
    "defmacro!": defmacro_handler,
    "do": do_handler,
    "fn*": fn_handler,
    "if": if_handler,
    "let*": let_handler,
    "macroexpand": macroexpand_handler,
    "quote": quote_handler,
    "quasiquote": quasiquote_handler,
    "try*": try_handler,
}


def EVAL(entry_ast: MalAny, entry_env: Environment) -> MalAny:
    """Top-level eval function that handles apply."""
    current = EvalState(entry_ast, entry_env, EvalMode.CONTINUING)

    while current.mode == EvalMode.CONTINUING:

        current = EvalState(
            macroexpand(current.ast, current.env), current.env, EvalMode.CONTINUING
        )

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
            if evaluated_head.is_macro:
                raise mal_errors.EvalError("Using a macro as a function")
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


def quasiquote(ast: MalAny) -> MalAny:
    """Quasiquote the ast."""
    if not isinstance(ast, MalSeq) or len(ast.value) == 0:
        return MalList([MalSym("quote"), ast])
    elems: List[MalAny] = ast.value
    if elems[0] == MalSym("unquote"):
        if len(elems) == 2:
            return elems[1]
        raise mal_errors.EvalError("Too many arguments to unquote", str(ast))
    if isinstance(elems[0], MalSeq) and len(elems[0].value) > 0:
        sub_elems: List[MalAny] = elems[0].value
        if sub_elems[0] == MalSym("splice-unquote"):
            if len(sub_elems) == 2:
                return MalList(
                    [MalSym("concat"), sub_elems[1], quasiquote(MalList(elems[1:]))]
                )
            raise mal_errors.EvalError("Too many arguments to splice-unquote", str(ast))
    return MalList(
        [MalSym("cons"), quasiquote(elems[0]), quasiquote(MalList(elems[1:]))]
    )


def macroexpand(ast: MalAny, env: Environment) -> MalAny:
    """Macro expand the ast."""
    while is_macro(ast, env):
        assert isinstance(ast, MalList)
        head = ast.value[0]
        assert isinstance(head, MalSym)
        macro = env.get(head)
        assert isinstance(macro, MalFunc)
        assert macro.is_macro
        ast = EVAL(
            macro.ast, Environment(macro.params, ast.value[1:], outer=macro.env,)
        )
    return ast


def is_macro(ast: MalAny, env: Environment) -> bool:
    """Test whether the ast is a macro call in the environment."""
    if isinstance(ast, MalList) and len(ast.value) > 0:
        head: MalAny = ast.value[0]
        if isinstance(head, MalSym) and env.find(head) is not None:
            head_value: MalAny = env.get(head)
            if isinstance(head_value, MalFunc) and head_value.is_macro:
                return True
    return False


def mal_swap(args: List[MalAny]) -> MalAny:
    """Python definition of the mal swap! function."""
    if (
        len(args) >= 2
        and isinstance(args[0], MalAtom)
        and (isinstance(args[1], MalFunc) or isinstance(args[1], MalBuiltin))
    ):
        target_atom: MalAtom = args[0]
        if isinstance(args[1], MalFunc):
            update_func: MalFunc = args[1]
            target_atom.value = EVAL(
                update_func.ast,
                Environment(
                    update_func.params,
                    [target_atom.value] + args[2:],
                    outer=update_func.env,
                ),
            )
        else:
            update_fn = cast(Callable[[List[MalAny]], MalAny], args[1].value)
            target_atom.value = update_fn([target_atom.value] + args[2:])
        return target_atom.value
    raise mal_errors.EvalError("Bad arguments for swap!", str(args))


def READ(input_string: str) -> Optional[MalAny]:
    """Read a mal element from the given string."""
    return reader.read_str(input_string)


def PRINT(ast: MalAny) -> None:
    """Print the string form of its argument to stdout."""
    print(printer.pr_str(ast, True))


def rep(input_string: str, env: Environment) -> None:
    """Call read-eval-print on its argument."""
    form = read_eval(input_string, env)
    if form is not None:
        PRINT(form)


def read_eval(input_string: str, env: Environment) -> Optional[MalAny]:
    """Call read-eval on its argument."""
    try:
        input_form = READ(input_string)
        if input_form is not None:
            return EVAL(input_form, env)
    except (mal_errors.UserException) as user_exception:
        print("User Exception: ", end="")
        PRINT(user_exception.value)
    except (mal_errors.MalError) as err:
        print(err)
    return None


def mal_eval(args: List[MalAny], env: Environment) -> MalAny:
    """Python definition of eval function."""
    if len(args) == 1:
        return EVAL(args[0], env)
    raise mal_errors.EvalError("Bad arguments to eval")


prelude: str = """
(do
    (def! not
        (fn* (a)
            (if a false true)))
    (def! load-file
        (fn* (f)
            (eval (read-string
                (str "(do " (slurp f) "\nnil)")))))
    (defmacro! cond
        (fn* (& xs)
            (if (> (count xs) 0)
                (list 'if (first xs) (if (> (count xs) 1)
                                        (nth xs 1)
                                        (throw "odd number of forms to cond"))
                (cons 'cond (rest (rest xs)))))))
;    (def! swap!
;        (fn* [a f & rest]
;            (reset! a (apply f @a rest))))
)
"""


def main() -> None:
    """Set up environment and then load given file or start REPL."""
    repl_env = Environment()
    core_ns = core.create_ns()
    for sym_name in core_ns:
        repl_env.set(MalSym(sym_name), core_ns[sym_name])

    repl_env.set(MalSym("eval"), MalBuiltin(lambda xs: mal_eval(xs, repl_env)))
    repl_env.set(MalSym("swap!"), MalBuiltin(mal_swap))
    repl_env.set(MalSym("*ARGV*"), MalList([]))

    prelude_form = READ(prelude)
    assert prelude_form is not None
    EVAL(prelude_form, repl_env)

    if len(argv) > 1:
        argv_list: Sequence[MalAny] = argv[2:]
        repl_env.set(MalSym("*ARGV*"), MalList(argv_list))
        read_eval('(load-file "' + argv[1] + '")', repl_env)
    else:
        while True:
            try:
                rep(input("user> "), repl_env)
            except EOFError:
                break


if __name__ == "__main__":
    main()
