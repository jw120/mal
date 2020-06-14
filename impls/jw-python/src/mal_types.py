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

from copy import deepcopy
from typing import (
    Any,
    Callable,
    Dict,
    List,
    NamedTuple,
    NoReturn,
    Optional,
    Sequence,
    Tuple,
    Union,
    cast,
)


class MetaMixin:
    """Add meta variable to a mal class."""

    meta: Optional[MalAny]

    def __init__(self, val: Optional[MalAny] = None) -> None:
        """Initialize the meta value."""
        self.meta = val

    def get_meta(self) -> MalAny:
        """Return the current meta value."""
        if self.meta is None:
            return MalNil()
        return self.meta

    def with_meta(self, value: MalAny) -> MetaMixin:
        """Create a new copy with the new meta value."""
        new_obj = deepcopy(self)
        new_obj.meta = value
        return new_obj


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


class MalAtom(MetaMixin):
    """Atom type for mal. Mutable."""

    def __init__(self, value: MalAny) -> None:
        """Create an atom."""
        super().__init__()
        self.value: MalAny = value

    def __eq__(self, other: object) -> bool:
        """Equality based on value."""
        return (
            self.__class__ == other.__class__
            and isinstance(other, self.__class__)  # For typechecker
            and self.value == other.value
        )


class MalSeq(MetaMixin):
    """Sequence type for Mal - lists or vectors."""

    def __init__(self, value: Sequence[MalAny]) -> None:
        """Create a MalSeq. Called via super() from subclases."""
        super().__init__()
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


class MalMap(MetaMixin):
    """Mal type for mal."""

    def __init__(
        self, elements: Union[List[MalAny], Tuple[List[MalKey], List[MalAny]]]
    ) -> None:
        """Create a MalMap.

        Argument:
            element - either an alternating list of symbols and values or a tuple
            of a list of symbols and a list of values
        """
        # Construct from an alternating key-value list
        super().__init__()
        if isinstance(elements, list):
            if len(elements) % 2 == 1:
                raise MalException("Unmatched key for map: ", elements[-1])
            self.value: Dict[MalKey, MalAny] = {}
            for k, v in zip(elements[0::2], elements[1::2]):
                if not (isinstance(k, str) or isinstance(k, MalKeyword)):
                    raise MalException("Bad key type for map: ", k)
                self.value[k] = v

        # Construct from a tuple of keys and values
        else:
            keys, values = elements
            self.value = dict(zip(keys, values))

        super().__init__()

    def assoc(self, other: MalMap) -> MalMap:
        """Return a new MalMap that merges in keys."""
        new_map = MalMap([])
        new_map.value = {**self.value, **other.value}
        return new_map

    def dissoc(self, keys: List[MalKey]) -> MalMap:
        """Return a new MalMap that removes keys."""
        new_map = MalMap([])
        new_map.value = deepcopy(self.value)
        for k in keys:
            new_map.value.pop(k, None)
        return new_map

    def get(self, key: MalKey) -> Optional[MalAny]:
        """Return the associated value if present."""
        if key in self.value:
            return self.value[key]
        return None

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


class MalBuiltin(MetaMixin):
    """Type for mal functions defined in mal by fn*."""

    def __init__(self, val: Callable[..., Any]) -> None:
        """Create a python-defined function."""
        super().__init__()
        self.value: Callable[..., Any] = val  # Should be Mal_Callable

    def __eq__(self, _other: Any) -> bool:
        """Equality for functions always false."""
        return False


class MalFunc(MetaMixin):
    """Type for mal functions defined in mal by fn*."""

    def __init__(
        self,
        ast: MalAny,
        params: List[MalSym],
        env: Environment,
        *,
        closure: Optional[Callable[..., Any]] = None,
        is_macro: bool = False,
    ) -> None:
        """Create a mal-defined function.

        Argument:
            ast - the body of the function
            params - paramater names of the function (added to env before calling)
            env - the environment to use when calling
            closure - for calling in map and apply
            is_macro - is the function a macro
        """
        super().__init__()
        self.ast: MalAny = ast
        self.params: List[MalSym] = params
        self.env: Environment = env
        if closure is None:
            self.value: Callable[..., Any] = raise_no_closure
        else:
            self.value = closure
        self.is_macro: bool = is_macro

    def __eq__(self, _other: Any) -> bool:
        """Equality for functions always false."""
        return False


def raise_no_closure() -> NoReturn:
    """Raise a no closure exception."""
    raise MalException("No closure!")


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


def isMalKey(x: MalAny) -> bool:
    """Test whether this is a MalKey."""
    return isinstance(x, MalKeyword) or isinstance(x, str)


class MalException(Exception):
    """Exception thrown during mal execution."""

    def __init__(
        self, value: MalAny, context: Optional[Union[MalAny, List[MalAny]]] = None
    ):
        """Initialize with a mal value and an optional context value."""
        self.value: MalAny = value
        self.context: Optional[Union[MalAny, List[MalAny]]] = context
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

    def add(self, sym: MalSym, value: MalAny) -> None:
        """Add the symbol and value to the environment."""
        self.data[sym.value] = value

    def find(self, sym: MalSym) -> Optional[Environment]:
        """Return an evironment in the chain that includes the given symbol."""
        if sym.value in self.data:
            return self
        if self.outer is None:
            return None
        return self.outer.find(sym)

    def get(self, sym: MalSym, *, context: Optional[MalAny] = None) -> MalAny:
        """Return the symbol's value in the environement or its chain."""
        env = self.find(sym)
        if env is None:
            raise MalException("'" + str(sym) + "' not found", context)
        return env.data[sym.value]


def to_symlist(xs: MalAny) -> List[MalSym]:
    """Convert a mal value to a list of symbols.

    Mal value should be a sequence of symbols. Useful for creating a new environment.
    """
    if isinstance(xs, MalSeq):
        for x in xs.value:
            if not isinstance(x, MalSym):
                raise MalException("Expected a symbol: ", x)
        return cast(List[MalSym], xs.value)
    raise MalException("Expected a sequence: ", xs)


def is_pair(xs: MalAny) -> bool:
    """Test if the argument a non-empty list."""
    return isinstance(xs, MalList) and len(xs.value) > 0
