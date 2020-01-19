# Python implementation of Make a Lisp

Written in Python 3 with mypy typing.

# To setup

First setup a pythton development environment. For me this means

```sh
use_pyenv
use_venc
```

Then install the required modules

```sh
pip install -r requirements.txt
```

Then use make (from this directory) to check and run all the tests. Fuller tests are run from the parent
directory

```sh
make "test^jw-python^step4" # Run one step's tests (from 1..9,A)
make "test^jw-python" # Run all the tests in python
make MAL_IMPL=jw-python "test^mal" | grep failing  # Run all the tests in hosted mal
make "perf^jw-python"  # Performance tests
```

# TODO

* Should we use more realistic underlying types - a list with O(1) cons? deque
* Add python-eval and tests
* Allow iterators to __init__ to save list() calls
* Improve Benchmark
* What is the difference between EVAL and eval_ast? (why do instuctions suggest eval_ast in do?)
* Move VS Code settings to workspace
* Can we mak typing better: Eliminate casts and workrounds, Properly type Callables
* Standardize __eq__ methods
* Better error messages - track source line numbers?
