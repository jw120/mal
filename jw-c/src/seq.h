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
count_t seq_count(mal);

// Is the sequence empty
bool seq_empty(mal m);

// Are the two sequences equal
bool seq_equals(mal, mal);

// Convert a list or vector to list
list_node *seq_to_list(mal);

/**
 *
 * List functions
 *
 */

count_t list_count(list_node *);
bool list_empty(list_node *);
bool list_equals(list_node *, list_node *);

// return a new list whose head is the given element and whose tail is the given
// list (which may be NULL)
list_node *list_cons(mal, list_node *);

// generate a list from an array of mal values
list_node *array_to_list(count_t, mal[]);

// Given a pointer to the last element of a list (or NULL), add the given
// element and return the new last element
list_node *list_extend(mal, list_node *n);

// Versions that work on a mal type
mal mal_cons(mal, mal);
mal mal_first(mal);
mal mal_rest(mal);

// Helper function for quasiquote - is this a non-empty list
bool is_pair(mal m);

/**
 *
 * Vector functions
 *
 */

count_t vec_count(vec *);
bool vec_empty(vec *);
bool vec_equals(vec *, vec *);

// Create a vector of the given size with elements from the given list
vec *list_to_vec(count_t, list_node *);
// Create a vector with unititialized entries
vec *uninitialized_vec(count_t);

#endif
