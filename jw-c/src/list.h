#ifndef LIST_H
#define LIST_H

#include <stdbool.h>

#include "mal_types.h"

// return a new list whose head is the given element and whose tail is the given list (which may be NULL)
list_node *list_cons(mal, list_node *);

// Given a pointer to the last element of a list (or NULL), add the given element and return the new
// last element
list_node *list_extend(mal, list_node *n);

// Length of the list
int list_len(list_node *);

// Is the list empty
bool list_empty(list_node *);

// Are the two lists equal
bool list_equals(list_node *, list_node *);

#endif
