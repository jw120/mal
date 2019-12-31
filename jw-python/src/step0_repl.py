"""Implements step 0 of https://github.com/kanaka/mal - the repl."""


def READ(input_string: str) -> str:
    """Pass though (dummy)."""
    return input_string


def EVAL(ast: str) -> str:
    """Pass though (dummy)."""
    return ast


def PRINT(ast: str) -> None:
    """Print argument (minimal print function)."""
    print(ast)


def rep(input_string: str) -> None:
    """Call read-eval-print on its argument."""
    PRINT(EVAL(READ(input_string)))


def rep_loop() -> None:
    """Repeatedly provide user prompt and passes the input to read-eval-print."""
    while True:
        try:
            rep(input("user> "))
        except EOFError:
            break


if __name__ == "__main__":
    rep_loop()
