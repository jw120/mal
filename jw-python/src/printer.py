"""Printer"""

from mal_types import MalAny


def pr_str(element: MalAny, print_readably: bool) -> str:
    """Convert a mal value into a string"""

    if print_readably:
        return element.str_readable()
    return str(element)
