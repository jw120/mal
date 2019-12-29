"""Implements step 0 of https://github.com/kanaka/mal - the repl"""


def READ(input_string: str) -> str:
    """Dummy read function - just passes through its argument"""
    return input_string


def EVAL(ast: str) -> str:
    """Dummy eval function - just passes through its argument"""
    return ast


def PRINT(ast: str) -> None:
    """Minimal print function - just prints its argument"""
    print(ast)


def rep(input_string: str) -> None:
    """Calls read-eval-print on its argument"""
    PRINT(EVAL(READ(input_string)))


def rep_loop() -> None:
    """Repeatedly provides user prompt and passes the input to read-eval-print"""
    while True:
        try:
            rep(input("user> "))
        except EOFError:
            break


if __name__ == "__main__":
    rep_loop()
