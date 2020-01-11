"""Printing functionality for mal."""

from typing import List

from mal_types import MalAny, MalList, MalMap, MalNil, MalVec

import utils


def pr_str(element: MalAny, print_readably: bool) -> str:
    """Convert a mal value into a string."""
    if isinstance(element, MalList):
        str_values = map(lambda x: pr_str(x, print_readably), element.value)
        return "(" + " ".join(str_values) + ")"

    if isinstance(element, MalVec):
        str_values = map(lambda x: pr_str(x, print_readably), element.value)
        return "[" + " ".join(str_values) + "]"

    if isinstance(element, MalMap):
        accumulated: List[str] = []
        for k in element.value:
            accumulated.append(pr_str(k, print_readably))
            accumulated.append(pr_str(element.value[k], print_readably))
        return "{" + " ".join(map(str, accumulated)) + "}"

    if isinstance(element, MalNil):
        return "nil"

    if isinstance(element, str):
        if print_readably:
            return '"' + utils.add_escapes(element) + '"'
        return element

    if isinstance(element, bool):
        return "true" if element else "false"

    return str(element)
