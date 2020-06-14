#ifndef UTILS_H
#define UTILS_H

#include <stdbool.h>
#include <stdnoreturn.h>

#include "types.h"

// Send the message (printf-style) to stderr and abort()
_Noreturn void internal_error(const char *restrict fmt, ...);

// Call malloc, failing with given message (printf-style) if malloc fails
void *checked_malloc(size_t, const char *restrict, ...);

// Helper function to concatenate a list of strings with given separator,
// opener, closer. Takes counts of number of characters in all the strings in
// the list  and the number of strings in the list
const char *str_join(list_node *s, size_t chars, count_t elements,
                     const char *sep, const char *opener, const char *closer);

#endif
