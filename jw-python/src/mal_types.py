"""Provide type defintions for mal's AST and the Environment.

AST types defines a value equality and a to_string method which allows string
conversion with and without prettifying strings

## AST type class hierarchy:

MalAny is a union of the following
    + MalSeq with subclasses MalList and MalVec
    + MalMap
    + MalBuiltin (a function defined in python)
    + MalFunc (a function defined by fn* in Mal)
    + MalKeyword
    + MalSym
    + MalNil
    + MalAtom (mutable reference to a value)
    + Callable
    + str (builtin)
    + int (builtin)
    + bool (builtin)


"""

# pylint: disable=too-few-public-methods

from __future__ import annotations

from typing import (
    Any,
    Callable,
    Dict,
    List,
    NamedTuple,
    Optional,
    Sequence,
    Tuple,
    Union,
    cast,
)


class MalKeyword(NamedTuple):
    """Keyword type for mal."""

    value: str

    def __str__(self) -> str:
        """Convert to a string."""
        return ":" + self.value

    def __eq__(self, other: object) -> bool:
        """Equality based on value."""
        return (
            self.__class__ == other.__class__
            and isinstance(other, self.__class__)  # For typechecker
            and self.value == other.value
        )

    def __hash__(self) -> int:
        """Hash based on value. (needed as defining __eq__ disables default)."""
        return hash(str(self))


class MalAtom:
    """Atom type for mal. Mutable."""

    def __init__(self, value: MalAny) -> None:
        """Create an atom."""
        self.value: MalAny = value

    def __eq__(self, other: object) -> bool:
        """Equality based on value."""
        return (
            self.__class__ == other.__class__
            and isinstance(other, self.__class__)  # For typechecker
            and self.value == other.value
        )


class MalSeq:
    """Sequence type for Mal - lists or vectors."""

    def __init__(self, value: Sequence[MalAny]) -> None:
        """Create a MalSeq. Called via super() from subclases."""
        self.value: List[MalAny] = list(value)

    def __eq__(self, other: Any) -> bool:
        """Value equality considering all MalSeq sub-classses to be equal."""
        return isinstance(other, MalSeq) and self.value == other.value


class MalList(MalSeq):
    """List type for mal."""

    pass


class MalVec(MalSeq):
    """Vector type for mal."""

    pass


class MalNil:
    """Nil type for mal."""

    def __eq__(self, other: Any) -> bool:
        """Value equality."""
        return self.__class__ == other.__class__ and isinstance(
            other, self.__class__
        )  # For typechecker


class MalMap:
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
                raise MalException("Unmatched key for map: ", str(elements[-1]))
            self.value: Dict[MalKey, MalAny] = {}
            for k, v in zip(elements[0::2], elements[1::2]):
                if not (isinstance(k, str) or isinstance(k, MalKeyword)):
                    raise MalException("Bad key type for map: ", str(k))
                self.value[k] = v

        # Construct from a tuple of keys and values
        else:
            keys, values = elements
            self.value = dict(zip(keys, values))

        super().__init__()

    def __eq__(self, other: Any) -> bool:
        """Value equality."""
        return isinstance(other, self.__class__) and self.value == other.value


class MalSym(NamedTuple):
    """Symbol type for mal."""

    value: str

    def __str__(self) -> str:
        """Convert to a string."""
        return self.value

    def __eq__(self, other: Any) -> bool:
        """Equality based on value."""
        return (
            self.__class__ == other.__class__
            and isinstance(other, self.__class__)  # For typechecker
            and self.value == other.value
        )

    def __hash__(self) -> int:
        """Hash based on value. (needed as defining __eq__ disables default)."""
        return hash(str(self))


class MalBuiltin(NamedTuple):
    """Type for mal functions defined in mal by fn*."""

    value: Callable[..., Any]  # Should be Mal_Callable (but gives circular defn)

    def __eq__(self, _other: Any) -> bool:
        """Equality for functions always false."""
        return False


class MalFunc:
    """Type for mal functions defined in mal by fn*."""

    def __init__(
        self,
        ast: MalAny,
        params: List[MalSym],
        env: Environment,
        is_macro: bool = False,
    ) -> None:
        """Create a mal-defined function.

        Argument:
            ast - the body of the function
            params - paramater names of the function (added to env before calling)
            env - the environment to use when calling
            is_macro - is the function a macro
        """
        self.ast: MalAny = ast
        self.params: List[MalSym] = params
        self.env: Environment = env
        self.is_macro: bool = is_macro

    def __eq__(self, _other: Any) -> bool:
        """Equality for functions always false."""
        return False


MalAny = Union[
    MalSeq,
    MalMap,
    MalFunc,
    MalAtom,
    MalKeyword,
    MalBuiltin,
    MalSym,
    MalNil,
    str,
    int,
    bool,
]
Mal_Environment = Dict[str, MalAny]
MalKey = Union[MalKeyword, str]
Mal_Callable = Callable[[List[MalAny]], MalAny]


class MalException(Exception):
    """Exception thrown during mal execution."""

    def __init__(self, value: MalAny, extra: Optional[str] = None):
        """Initialize with value describing the exception or with two strings."""
        if isinstance(value, str) and extra is not None:
            self.value: MalAny = value + ": " + extra
        else:
            self.value = value
        super().__init__()


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
            raise MalException("Bad args to env - missing list")
        found_ampersand = False
        for i in range(len(binds)):
            bind = binds[i]
            if not isinstance(bind, MalSym):
                raise MalException("Bad args to env - non-symbol")
            if found_ampersand:
                self.data[bind.value] = MalList(exprs[i - 1 :])
                break
            if bind.value == "&":
                found_ampersand = True
            else:
                if i < len(exprs):
                    self.data[bind.value] = exprs[i]
                else:
                    raise MalException("Bad args to env - missing expr")

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
            raise MalException("'" + str(sym) + "' not found")
        return env.data[sym.value]


def to_symlist(xs: MalAny) -> List[MalSym]:
    """Convert a mal value to a list of symbols.

    Mal value should be a sequence of symbols. Useful for creating a new environment.
    """
    if isinstance(xs, MalSeq):
        for x in xs.value:
            if not isinstance(x, MalSym):
                raise MalException("Expected a symbol: ", str(x))
        return cast(List[MalSym], xs.value)
    raise MalException("Expected a sequence: ", str(xs))


def is_pair(xs: MalAny) -> bool:
    """Test if the argument a non-empty list."""
    return isinstance(xs, MalList) and len(xs.value) > 0
