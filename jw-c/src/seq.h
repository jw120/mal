#ifndef SEQ_H
#define SEQ_H

#include <stdbool.h>

#include "types.h"

/**
 *
 * Sequence functions (for both a list of vector)
 *
 */

// Count the elements in a sequence
int seq_count(mal);

// Is the sequence empty
bool seq_empty(mal m);

// Are the two sequences equal
bool seq_equals(mal, mal);

/**
 *
 * List functions
 *
 */

int list_count(list_node *);
bool list_empty(list_node *);
bool list_equals(list_node *, list_node *);

// return a new list whose head is the given element and whose tail is the given list (which may be NULL)
list_node *list_cons(mal, list_node *);

// Given a pointer to the last element of a list (or NULL), add the given element and return the new
// last element
list_node *list_extend(mal, list_node *n);

/**
 *
 * Vector functions
 *
 */

int vec_count(vec *);
bool vec_empty(vec *);

// Create a vector of the given size with elements from the given list
vec *create_vec(size_t, list_node *);

#endif
