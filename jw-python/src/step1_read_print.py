"""step1"""

def mal_read(input_string: str) -> str:
    """Dummy read function - just passes through its argument"""
    return input_string

def mal_eval(ast: str) -> str:
    """Dummy eval function - just passes through its argument"""
    return ast

def mal_print(ast: str) -> None:
    """Minimal print function - just prints its argument"""
    print(ast)

def rep(input_string: str) -> None:
    """Calls read-eval-print on its argument"""
    mal_print(mal_eval(mal_read(input_string)))

def rep_loop() -> None:
    """Reapeatedly provides user prompt and passes the input to read-eval-print"""
    while True:
        rep(input("user> "))

if __name__ == "__main__":
    rep_loop()
