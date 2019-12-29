"""Implements step 2 of https://github.com/kanaka/mal - eval"""


from typing import cast, Dict

from mal_errors import EvalError, ReaderError
from mal_types import MalAny, MalFunc, MalList, MalVec, MalMap, MalSeq, MalSym, MalNum
from printer import pr_str
from reader import read_str

Environment = Dict[str, MalAny]

repl_env: Environment = {
    '+': MalFunc(lambda xs: MalNum(cast(MalNum, xs[0]).value +  cast(MalNum, xs[1]).value)),
    '-': MalFunc(lambda xs: MalNum(cast(MalNum, xs[0]).value -  cast(MalNum, xs[1]).value)),
    '*': MalFunc(lambda xs: MalNum(cast(MalNum, xs[0]).value *  cast(MalNum, xs[1]).value)),
    '/': MalFunc(lambda xs: MalNum(cast(MalNum, xs[0]).value // cast(MalNum, xs[1]).value))
    }


def EVAL(ast: MalAny, env: Environment) -> MalAny:
    """Top-level eval function that handles apply"""

    if isinstance(ast, MalSeq) and len(ast.value) > 0:
        evaluated = eval_ast(ast, env)
        if isinstance(evaluated, MalSeq) and len(evaluated.value) > 0:
            head = evaluated.value[0]
            if isinstance(head, MalFunc):
                return head.value(evaluated.value[1:])
        return evaluated

    return eval_ast(ast, env)


def eval_ast(ast: MalAny, env: Environment) -> MalAny:
    """Eval function"""

    # A symbol evaluates to its value in the environment
    if isinstance(ast, MalSym):
        if ast.value in env:
            return env[ast.value]
        raise EvalError("Unknown symbol", str(ast))

    # A list or vector has all of its contents evaluated
    if isinstance(ast, MalSeq):
        evaluated = list(map(lambda x: EVAL(x, env), ast.value))
        return MalVec(evaluated) if isinstance(ast, MalVec) else MalList(evaluated)

    # A map has its value evaluated
    if isinstance(ast, MalMap):
        evaluated = list(map(lambda x: EVAL(x, env), ast.value.values()))
        return MalMap((list(ast.value.keys()), evaluated))

    return ast


def READ(input_string: str) -> MalAny:
    """Read a mal element from the given string"""
    return read_str(input_string)


def PRINT(ast: MalAny) -> None:
    """Prints the string form of its argument to stdout"""
    print(pr_str(ast, True))


def rep(input_string: str) -> None:
    """Calls read-eval-print on its argument"""

    try:
        PRINT(EVAL(READ(input_string), repl_env))
    except (EvalError, ReaderError) as err:
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
