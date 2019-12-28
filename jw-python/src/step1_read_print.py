"""step1"""

from mal import MalType
from printer import pr_str
from reader import read_str

def mal_eval(ast: MalType) -> MalType:
    """Dummy eval function - just passes through its argument"""
    return ast

def mal_print(ast: MalType) -> None:
    """Minimal print function - just prints its argument"""
    print(pr_str(ast))

def rep(input_string: str) -> None:
    """Calls read-eval-print on its argument"""
    mal_print(mal_eval(read_str(input_string)))

def rep_loop() -> None:
    """Reapeatedly provides user prompt and passes the input to read-eval-print"""
    while True:
        rep(input("user> "))

if __name__ == "__main__":
    rep_loop()
