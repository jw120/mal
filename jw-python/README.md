# TODO

Streamline step9 - move map/apply out? move special forms out?

Allow iterators to __init__ to save list() calls


# To setup

use_pyenv
use_venc
pip install -r requirements.txt

# To develop



# Open questions

What is the difference between EVAL and eval_ast? (why do instuctions suggest eval_ast in do?)

Move VS Code settings to workspace

Simplify errors - ? one class for all errors we catch, Python errors for internal

Can we mak typing better
- Eliminate casts and workrounds
- Properly type Callables

Can we use immutable types

Should we use more realistic underlying types - a list with O(1) cons?

Standardize __eq__ methods

Neater way of handling Optionals (than if is not None)

Remove doctests
