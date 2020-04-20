# My implementations of mal (https://github.com/kanaka/mal)

I have implemented the mal ("make a lisp") interpreter in several languages as
a programmer exercise and a way to learn (or re-learn) those languages.

In order of completion these were

* Haskell
* Python
* Racket (excludes meta-data support)
* Elixir
* C
* Racket-br (failed attempt to make a mal-to-racket compliler a la Beautiful Racket)

Each should be able to be run by cd'ing into the directory and running `make` to build and run tests (implementations
include only limited native tests and rely mostly on the mal project step tests)

Relative peformance of my implemntations is: (use `jw-perf.sh` to run)

C > Racket (9869) > elixir (6433) > haskell (3953) > python (1051)

TODO
- Use macros in elixir (to replace wrapping functions and to simplify `list()`)
- Compare performance of my versions with provided versions
- Compare my versions with provided versions


## Haskell

Implementation choices:
* Uses an AST to hold mal values (`ASTInt Int` etc)
* Uses parser combinators (Megaparsec library) instead of regular expression for parsing/reading
* Uses `Text` as the data type (rather than strings)
* Relies on native TCO (so step 5 largely skipped)
* Written around an application monad that combines

    - `IO` so we can do IO and use `IORef` to allow for mutation of environments and atoms
    - `ExceptT` to catch exceptions (we don't use Haskell exceptions)
    - `ReaderT` to hold confiugation (just whether to show debug information)

Tools used:
* stack
* brittany (code formatting)
* hspec

Edited with VS Code using the Simple GHC integration and Haskell Syntax Highlighting addons

Installed on my mac using `brew install stack`


## Python

Implementation choices
* Python 3
* mypy typing
* Mal values held with a union of native types (str, int, bool) and class types

Tools uses:
* flake8 linting
* black formatting
* pyenv for python versions
* venv to keep a dedicated set of python libraries

Edited with VS Code and the Microsoft Python addon


## Racket

Implementation choices
* Uses native racket types to hold mal values
* Adds a nil symbol (racket just has `null` which is `()` and `#f`. Mal has `nil`, `'()` and `#f` all different).
* Meta-data not implemented as we use native lists, maps and vectors which don't have meta data
* Relies on native TCO (so step 5 largely skipped)

Edited with emacs and racket-mode.

Installed on my mac using `brew cask install racket`


## Elixir

Implementation choices
* Uses native elixir types when available with structs wrapping lists, vectors,
  maps, and functions so meta can be supported. Vectors held as maps with integer keys.
* Mutability provided with `Agent` (for atoms) and ETS (for environments)
* Relies on native TCO (so step 5 largely skipped)
* Relies on elixir pattern matching to catch argument type errors (passing a string to `+` etc)

Edited with VS Code using the ElixirLS addons

Installed on my mac using `brew install elixir`


## C

Implementation choices:
- Use a discriminated union for main `mal` type
- Use GNU readline and PCRE libraries
- Use modern C (C11/C17) as much as possible
- More unit tests that other implementations as C is error-prone
- Exceptions handled by returning an exception type and manually ensuring
  exceptions propogate during evaluation
- No garbage collection is done, we call `malloc()` many times but almost never `free()`

Edited with VS Code and the Microsoft C/C++ addon
