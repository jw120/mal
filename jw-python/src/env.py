"""Environment"""

# Delays evaluation in Python 3.7 (to allow find to return an Environment before class is defined)
from __future__ import annotations

from typing import Optional

from mal_errors import EvalError
from mal_types import MalAny, Mal_Environment


class Environment:
    """Environment"""

    def __init__(self, outer: Optional[Environment] = None):
        self.data: Mal_Environment = {}
        self.outer: Optional[Environment] = outer

    def set(self, sym: str, value: MalAny) -> None:
        """Adds the symbol and value to the environment"""
        self.data[sym] = value
        self.outer = None

    def find(self, sym: str) -> Optional[Environment]:
        """Return an evironment in the chain that includes the given symbol"""
        if sym in self.data:
            return self
        if self.outer is None:
            return None
        return self.outer.find(sym)

    def get(self, sym: str):
        """Return the value associated with the symbol in the environement or its chain"""
        env = self.find(sym)
        if env is None:
            raise EvalError("Symbol not found in environment", str(sym))
        return env.data[sym]
