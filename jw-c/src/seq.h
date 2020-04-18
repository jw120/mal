#ifndef SEQ_H
#define SEQ_H

#include <stdbool.h>

#include "types.h"

/**
 *
 * Sequence functions (for both a list or vector)
 *
 */

// Count the elements in a sequence
count_t seq_count(mal);

// Is the sequence empty
bool seq_empty(mal m);

// Convert a list or vector to list
list_node *seq_to_list(mal);

/**
 *
 * List functions
 *
 */

// Count the elements in a list
count_t list_count(list_node *);

// Is the list empty
bool list_empty(list_node *);

// Are the two vectors equal (i.e., the same size and with corresponding
// elements mal_equal)
bool list_equals(list_node *, list_node *);

// return a new list with given head and tail
list_node *list_cons(mal, list_node *);

// generate a list from an array of mal values
list_node *array_to_list(count_t, mal[]);

// Given a pointer to the last element of a list (or NULL), add the given
// element and return the new last element
list_node *list_extend(mal, list_node *n);

// Create a new list consisting of (at most) the given number of elements
// from the front of the given list
list_node *list_take(list_node *, count_t);

// Return the last value in a list (or an exception if NULL)
mal list_last(list_node *);

// Create a new list that combines both lists
list_node *list_append(list_node *, list_node *);

// Create a list whose head is the first argument and whose tail is the second
// argument Returns an exception if the second argument is not a list
mal mal_cons(mal, mal);

// Return the first value of the given mal value which should be a list, vector
// or nil. Gives nil if the sequence is empty (or nil) and an exception if the
// argument is a different type
mal mal_first(mal);

// Return the first value of the given mal value which should be a list, vector
// or nil. Gives nil if the sequence is empty (or nil) and an exception if the
// argument is a different type
mal mal_rest(mal);

// Helper function for quasiquote - is this a non-empty list
bool is_pair(mal m);

// Does the list include a value that is mal_equal to the given value
bool list_contains(list_node *, mal);

/**
 *
 * Vector functions
 *
 */

// How many elements in the vector
count_t vec_count(vec *);

// Is the vector empty
bool vec_empty(vec *);

// Are the two vectors equal (i.e., the same size and with corresponding
// elements mal_equal)
bool vec_equals(vec *, vec *);

// Do the list and vector have the same length with the correpsonding elements
// all mal_equal? Handles NULL lists (which are equal to zero element vectors)
bool list_vec_equals(list_node *n, vec *v);

// Create a vector of the given size with elements from the given list
vec *list_to_vec(count_t, list_node *);

// Create a vector with given size but unititialized entries
vec *uninitialized_vec(count_t);

#endif
