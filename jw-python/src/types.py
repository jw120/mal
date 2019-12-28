"""Type definitions - mainly the AST"""

#pylint: disable=too-few-public-methods

class MalType:
    """Abstract type for any AST element"""

class MalList(MalType):
    """List"""

    def __init__(self, elements):
        self.elements = elements
        super().__init__()

class MalNil(MalType):
    """Nil"""
