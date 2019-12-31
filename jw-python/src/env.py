"""Environment definition for mal."""

# Delays evaluation in Python 3.7
# (to allow find to return an Environment before class is defined)
from __future__ import annotations

from typing import List, Optional

from mal_errors import EvalError

from mal_types import MalAny, MalList, MalSym, Mal_Environment


class Environment:
    """Environment holding symbols and values."""

    def __init__(
        self,
        binds: Optional[List[MalSym]] = None,
        exprs: Optional[List[MalAny]] = None,
        *,
        outer: Optional[Environment] = None,
    ):
        """Create a new environment.

        Attributes:
            binds, exprs -- list of symbols and list of expression to bind them to
                (optional - but both must be provided if either is)
            outer -- outer environment (for lookup chain)
                (optional and must be a keyword argument)
        """
        self.data: Mal_Environment = {}
        self.outer: Optional[Environment] = outer
        if binds is None and exprs is None:
            return
        if binds is None or exprs is None:
            raise EvalError("Bad args to env - missing list")
        found_ampersand = False
        for i in range(len(binds)):
            bind = binds[i]
            if not isinstance(bind, MalSym):
                raise EvalError("Bad args to env - non-symbol")
            if found_ampersand:
                self.data[bind.value] = MalList(exprs[i - 1 :])
                break
            if bind.value == "&":
                found_ampersand = True
            else:
                if i < len(exprs):
                    self.data[bind.value] = exprs[i]
                else:
                    raise EvalError("Bad args to env - missing expr")

    def set(self, sym: MalSym, value: MalAny) -> None:
        """Add the symbol and value to the environment."""
        self.data[sym.value] = value

    def find(self, sym: MalSym) -> Optional[Environment]:
        """Return an evironment in the chain that includes the given symbol."""
        if sym.value in self.data:
            return self
        if self.outer is None:
            return None
        return self.outer.find(sym)

    def get(self, sym: MalSym) -> MalAny:
        """Return the symbol's value in the environement or its chain."""
        env = self.find(sym)
        if env is None:
            raise EvalError("Symbol '" + str(sym) + "' not found in environment")
        return env.data[sym.value]
