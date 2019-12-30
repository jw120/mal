"""Environment"""

# Delays evaluation in Python 3.7
# (to allow find to return an Environment before class is defined)
from __future__ import annotations

from typing import Optional, List

from mal_errors import EvalError
from mal_types import MalAny, Mal_Environment, MalSym


class Environment:
    """Environment"""

    def __init__(
        self,
        binds: Optional[List[MalSym]] = None,
        exprs: Optional[List[MalAny]] = None,
        *,
        outer: Optional[Environment] = None,
    ):
        self.data: Mal_Environment = {}
        self.outer: Optional[Environment] = outer
        if binds is None or exprs is None:
            return
        if binds is None or exprs is None or len(binds) != len(exprs):
            raise EvalError("Bad arguments to environment constructore")
        for sym, expr in zip(binds, exprs):
            self.data[sym.value] = expr

    def set(self, sym: MalSym, value: MalAny) -> None:
        """Adds the symbol and value to the environment"""
        self.data[sym.value] = value

    def find(self, sym: MalSym) -> Optional[Environment]:
        """Return an evironment in the chain that includes the given symbol"""
        if sym.value in self.data:
            return self
        if self.outer is None:
            return None
        return self.outer.find(sym)

    def get(self, sym: MalSym):
        """Return the symbol's value in the environement or its chain"""
        env = self.find(sym)
        if env is None:
            raise EvalError("Symbol '" + str(sym) + "' not found in environment")
        return env.data[sym.value]
