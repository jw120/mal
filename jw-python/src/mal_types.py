"""Provide type defintions for mal's AST and the Environment.

AST types defines a value equality and a to_string method which allows string
conversion with and without prettifying strings

## AST type class hierarchy:

* MalAny
    + MalSeq (provides shared implementation and supports functions which work on both)
        - MalList
        - MalVec
    + MalMap
    + MalBuiltin (a function define in python)
    + MalFunc (a function defined by fn* in Mal)
    + MalKey (valid keys for a map)
        - MalKeyword
        - MalStr
    + MalSym
    + MalNum
    + MalBool
    + MalNil

"""

# pylint: disable=too-few-public-methods

from __future__ import annotations

from abc import ABC, abstractmethod
from typing import Any, Callable, Dict, List, Optional, Tuple, Union, cast

import mal_errors

import utils


class MalAny(ABC):
    """Abstract type for any Mal element."""

    def __str__(self) -> str:
        """Convert to string using to_string(False). Inherited by all subclasses."""
        return self.to_string(False)

    @abstractmethod
    def to_string(self, _print_readably: bool) -> str:
        """Provide a string version with or without strings made readable."""
        pass


class MalKey(MalAny):
    """Type for mal that can be used as a hash-map key."""

    def __init__(self, value: str) -> None:
        """Create a MalKey."""
        self.value: str = value
        super().__init__()

    def __eq__(self, other: Any) -> bool:
        """Equality based on value."""
        return isinstance(other, self.__class__) and self.value == other.value

    def __hash__(self) -> int:
        """Hash based on value. (needed as defining __eq__ disables default)."""
        return hash(str(self))


class MalKeyword(MalKey):
    """Keyword type for mal."""

    def to_string(self, _print_readably: bool) -> str:
        """Convert to a string."""
        return ":" + self.value


class MalStr(MalKey):
    """String type for mal."""

    def to_string(self, print_readably: bool) -> str:
        """Convert to a string, optionally adding quotes and escapes."""
        if print_readably:
            return '"' + utils.add_escapes(self.value) + '"'
        return self.value


Mal_Environment = Dict[str, MalAny]
Mal_Function = Callable[[List[MalAny]], MalAny]
Mal_Map = Dict[MalKey, MalAny]


class MalSeq(MalAny):
    """Sequence type for Mal - lists or vectors."""

    def __init__(self, value: List[MalAny]) -> None:
        """Create a MalSeq. Called via super() from subclases."""
        self.value: List[MalAny] = value
        super().__init__()

    def __eq__(self, other: Any) -> bool:
        """Value equality considering all MalSeq sub-classses to be equal."""
        return isinstance(other, MalSeq) and self.value == other.value


class MalList(MalSeq):
    """List type for mal."""

    def to_string(self, print_readably: bool) -> str:
        """Convert to a string, optionally adding quotes and escapes."""
        str_values = map(lambda x: x.to_string(print_readably), self.value)
        return "(" + " ".join(str_values) + ")"


class MalVec(MalSeq):
    """Vector type for mal."""

    def to_string(self, print_readably: bool) -> str:
        """Convert to a string, optionally adding quotes and escapes."""
        str_values = map(lambda x: x.to_string(print_readably), self.value)
        return "[" + " ".join(str_values) + "]"


class MalMap(MalAny):
    """Mal type for mal."""

    def __init__(
        self, elements: Union[List[MalAny], Tuple[List[MalKey], List[MalAny]]]
    ) -> None:
        """Create a MalMap.

        Argument:
            element - either an alternating list of symbols and values or a tuple
            of a list of symbls and a list of values
        """
        # Construct from an alternating key-value list
        if isinstance(elements, list):
            if len(elements) % 2 == 1:
                raise mal_errors.EvalError("Unmatched key for map", str(elements[-1]))
            self.value: Mal_Map = {}
            for k, v in zip(elements[0::2], elements[1::2]):
                if not isinstance(k, MalKey):
                    raise mal_errors.EvalError("Bad key type for map", str(k))
                self.value[k] = v

        # Construct from a tuple of keys and values
        else:
            keys, values = elements
            self.value = dict(zip(keys, values))

        super().__init__()

    def to_string(self, print_readably: bool) -> str:
        """Convert to a string, optionally adding quotes and escapes."""
        accumulated: List[str] = []
        for k in self.value:
            accumulated.append(k.to_string(print_readably))
            accumulated.append(self.value[k].to_string(print_readably))
        return "{" + " ".join(map(str, accumulated)) + "}"

    def __eq__(self, other: Any) -> bool:
        """Value equality."""
        return isinstance(other, self.__class__) and self.value == other.value


class MalSym(MalAny):
    """Symbol type for mal."""

    def __init__(self, value: str):
        """Create a MalSym."""
        self.value: str = value
        super().__init__()

    def to_string(self, _print_readably: bool) -> str:
        """Convert to a string, optionally adding quotes and escapes."""
        return self.value

    def __eq__(self, other: Any) -> bool:
        """Value equality."""
        return isinstance(other, self.__class__) and self.value == other.value


class MalBuiltin(MalAny):
    """Type for builtin mal functions (defined in python)."""

    def __init__(self, value: Mal_Function) -> None:
        """Create a MalBuiltin."""
        self.value: Mal_Function = value

    def to_string(self, _print_readably: bool) -> str:
        """Convert to a string."""
        return "#<function>"

    def __eq__(self, _other: Any) -> bool:
        """Equality for functions always false."""
        return False


class MalFunc(MalAny):
    """Type for mal functions defined in mal by fn*."""

    def __init__(self, ast: MalAny, params: List[MalSym], env: Environment,) -> None:
        """Create a mal-defined function.

        Argument:
            ast - the body of the function
            params - paramater names of the function (added to env before calling)
            env - the environment to use when calling
        """
        self.ast: MalAny = ast
        self.params: List[MalSym] = params
        self.env: Environment = env

    def to_string(self, _print_readably: bool) -> str:
        """Convert to a string."""
        return "#<function>"

    def __eq__(self, _other: Any) -> bool:
        """Equality for functions always false."""
        return False


class MalNum(MalAny):
    """Number type for mal."""

    def __init__(self, value: int) -> None:
        """Create a MalNum."""
        self.value: int = value
        super().__init__()

    def to_string(self, _print_readably: bool) -> str:
        """Convert to a string."""
        return str(self.value)

    def __eq__(self, other: Any) -> bool:
        """Value equality."""
        return isinstance(other, self.__class__) and self.value == other.value


class MalBool(MalAny):
    """Boolean type for mal."""

    def __init__(self, value: bool) -> None:
        """Create a MalBool."""
        self.value: bool = value
        super().__init__()

    def to_string(self, _print_readably: bool) -> str:
        """Convert to a string."""
        return "true" if self.value else "false"

    def __eq__(self, other: Any) -> bool:
        """Value equality."""
        return isinstance(other, self.__class__) and self.value == other.value


class MalNil(MalAny):
    """Nil type for mal."""

    def to_string(self, _print_readably: bool) -> str:
        """Convert to a string."""
        return "nil"

    def __eq__(self, other: Any) -> bool:
        """Value equality."""
        return isinstance(other, self.__class__)


class Environment:
    """Environment holding symbols and values."""

    def __init__(
        self,
        binds: Optional[List[MalSym]] = None,
        exprs: Optional[List[MalAny]] = None,
        *,
        outer: Optional[Environment] = None,
    ):
        """Create a new environmenmt.

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
            raise mal_errors.EvalError("Bad args to env - missing list")
        found_ampersand = False
        for i in range(len(binds)):
            bind = binds[i]
            if not isinstance(bind, MalSym):
                raise mal_errors.EvalError("Bad args to env - non-symbol")
            if found_ampersand:
                self.data[bind.value] = MalList(exprs[i - 1 :])
                break
            if bind.value == "&":
                found_ampersand = True
            else:
                if i < len(exprs):
                    self.data[bind.value] = exprs[i]
                else:
                    raise mal_errors.EvalError("Bad args to env - missing expr")

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
            raise mal_errors.EvalError(
                "Symbol '" + str(sym) + "' not found in environment"
            )
        return env.data[sym.value]


def to_symlist(xs: MalAny) -> List[MalSym]:
    """Convert a mal value to a list of symbols suitable.

    Mal value should be a list of symbols. Useful for creating a new environment.
    """
    if not isinstance(xs, MalSeq):
        raise mal_errors.EvalError("Expected a sequence", str(xs))
    for x in xs.value:
        if not isinstance(x, MalSym):
            raise mal_errors.EvalError("Expected a symbol", str(x))
    return cast(List[MalSym], xs.value)
