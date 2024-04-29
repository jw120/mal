# 2024 Notes

* working on jw-rust
* using devbox (single environment for all jw languages at top-level)
* TODO
  * test my old implementations
  * Fix Syntax warning in python testing code

* Working (`make "test^IMPL"` and `make MAL_IMPL=IMPL "test^mal"`) (showing `make "perf^IMPL"` numbers)
  * python (3ms, 9ms, 4266/10s)
  * jw-python (5ms, 19ms, 1862/10s)

* Rust notes
  * eval as a special form not a bound function (with hack to find outermost environment) - borrowed from supplied rust implementation

* Note - how to update my repo for changes in the upstream <https://github.com/kanaka/mal>

git fetch upstream  # Fetches upstream changes into local branch remotes/upstream/master (git branch -va to see)
git checkout master # Make sure we are on our master branch
git merge remotes/upstream/master

## My implementations of mal (<https://github.com/kanaka/mal>)

I have implemented the mal ("make a lisp") interpreter in several languages as
a programmer exercise and a way to learn (or re-learn) those languages.

In order of completion these were

* Haskell
* Python
* Racket (excludes meta-data support)
* Elixir
* C
* Racket-br (failed attempt to make a mal-to-racket compiler a la Beautiful Racket)
* Swift
* Typed racket (with meta-data support)

Each should be able to be run by cd'ing into the directory and running `make` to build and run tests (implementations
include only limited native tests and rely mostly on the mal project step tests)

Relative performance of my implementations is: (use `jw-perf.sh` to run)

haskell (59k) > typed-racket (33k) > C (27k) > Racket (10k) > elixir (6k) >  swift (3k) > python (1k)

TODO

* Use macros in elixir (to replace wrapping functions and to simplify `list()`)
* Compare performance of my versions with provided versions
* Compare my versions with provided versions

## Haskell

Implementation choices:

* Uses an AST to hold mal values (`ASTInt Int` etc)
* Uses parser combinators (Megaparsec library) instead of regular expression for parsing/reading
* Uses `Text` as the data type (rather than strings)
* Relies on native TCO (so step 5 largely skipped)
* Written around an application monad that combines

  * `IO` so we can do IO and use `IORef` to allow for mutation of environments and atoms
  * `ExceptT` to catch exceptions (we don't use Haskell exceptions)
  * `ReaderT` to hold configuration (just whether to show debug information)

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

Implementation choices:

* Uses native elixir types when available with structs wrapping lists, vectors,
  maps, and functions so meta can be supported. Vectors held as maps with integer keys.
* Mutability provided with `Agent` (for atoms) and ETS (for environments)
* Relies on native TCO (so step 5 largely skipped)
* Relies on elixir pattern matching to catch argument type errors (passing a string to `+` etc)

Edited with VS Code using the ElixirLS addons

Installed on my mac using `brew install elixir`

## C

Implementation choices:

* Use a discriminated union for main `mal` type
* Use GNU readline and PCRE libraries
* Use modern C (C11/C17) as much as possible
* More unit tests that other implementations as C is error-prone
* Exceptions handled by returning an exception type and manually ensuring
  exceptions propagate during evaluation
* No garbage collection is done, we call `malloc()` many times but almost never `free()`

## Swift

Implementation choices:

* Uses DIY parser combinator library (instead of reggaes) for parsing
* Swift package (can build from command line)
* Mal arrays and lists implemented as swift array slices

Git notes. After adding `https://github.com/kanaka/mal` as an upstream remote

```{sh}
git fetch upstream
git checkout master # if needed
git merge upstream/master
# Resolve conflicts if any
git push origin master
```

## Typed racket

Implementation choices:

* Use a mix of native types and structs as mal values. Structs for mal's nil, collection and function types
* Uses rackets void type to signal no value from reader
* Relies on native TCO (so step 5 largely skipped)
