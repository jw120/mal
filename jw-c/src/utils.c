/**
 *
 * utils.c - misc utility functions
 *
 **/

#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils.h"

_Noreturn static void
print_err_and_exit(const char *prefix, const char *restrict fmt, va_list args) {
  fprintf(stderr, "%s - ", prefix);
  vfprintf(stderr, fmt, args);
  fprintf(stderr, "\n");
  fflush(stderr);
  exit(EXIT_FAILURE);
}

void *checked_malloc(size_t size, const char *restrict fmt, ...) {
  void *ptr = malloc(size);
  if (ptr != NULL)
    return ptr;
  va_list args;
  va_start(args, fmt);
  print_err_and_exit("Malloc failed", fmt, args);
}

_Noreturn void internal_error(const char *restrict fmt, ...) {
  va_list args;
  va_start(args, fmt);
  print_err_and_exit("Internal error", fmt, args);
}

// Helper function to concat a list of strings with given separator, opener,
// closer takes counts of number of strings in the list and number of chars
// (excluding the separators)
const char *str_join(list_node *s, size_t chars, count_t elements,
                     const char *sep, const char *opener, const char *closer) {

  count_t num_sep = elements > 0 ? elements - 1 : 0;
  size_t buf_size =
      1 + chars + strlen(opener) + strlen(closer) + num_sep * strlen(sep);
  size_t buf_capacity_remaining = buf_size;
  char *buf = checked_malloc(buf_size, "str_join");

  strncpy(buf, opener, buf_size);
  buf_capacity_remaining -= strlen(opener);
  while (s != NULL) {
    strncat(buf, s->val.s, buf_capacity_remaining);
    buf_capacity_remaining -= strlen(s->val.s);
    if (s->next != NULL) {
      strncat(buf, sep, buf_capacity_remaining);
      buf_capacity_remaining -= strlen(sep);
    }
    s = s->next;
  }
  strncat(buf, closer, buf_capacity_remaining);
  return buf;
}
