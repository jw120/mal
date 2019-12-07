"""step0"""

def mal_read(input_string: str) -> str:
    """read"""
    return input_string

def mal_eval(ast: str) -> str:
    """read"""
    return ast

def mal_print(ast: str) -> None:
    "print"
    print(ast)

def rep(input_string: str) -> None:
    """print"""
    mal_print(mal_eval(mal_read(input_string)))

def rep_loop() -> None:
    """loop"""
    while True:
        rep(input("user> "))

if __name__ == "__main__":
    rep_loop()
