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

// Concatenates the source string onto the end of the buffer which is
// already initailized as a null-terminated string. Count is the maximum
// number of characters to include (excluding the null)
void str_concat(char *buf, const char *source, size_t count) {
  if (source != NULL && buf != NULL) {
    size_t current_len = strlen(buf);
    strncat(buf + current_len, source, count - current_len);
  }
}

// Are both arguments strings that are equal
bool str_equals(mal a, mal b) {
  return is_str(a) && is_str(b) && strcmp(a.s, b.s) == 0;
}

// Remove escape sequences from a string
mal remove_escapes(mal m) {
  if (!is_str(m))
    return mal_exception_str("remove_escapes on a non-string");
  char *buf = checked_malloc(strlen(m.s) + 1, "remove_escapes");
  bool in_escape = false;
  char *p = buf;
  const char *s = m.s;
  while (*s) {
    if (in_escape) {
      if (*s == 'n') {
        *p++ = '\n';
        s++;
        in_escape = false;
      } else if (*s == '\\') {
        *p++ = '\\';
        s++;
        in_escape = false;
      } else if (*s == '\"') {
        *p++ = '\"';
        s++;
        in_escape = false;
      } else
        return mal_exception_str("Bad escape sequence");
    } else {
      if (*s == '\\') {
        in_escape = true;
        s++;
      } else
        *p++ = *s++;
    }
  }
  if (in_escape) // trailing slashj or bad escape
    return mal_exception_str("EOF in escape sequence");
  *p = *s; // null terminate;
  return mal_str(buf);
}

// add escape sequences to a string
mal add_escapes(mal m) {
  if (!is_str(m))
    return mal_exception_str("add_escapes on a non-string");

  // need to make one extra char space for every ", \, \n
  const char *s = m.s;
  size_t extras_count = 0;
  while (*s) {
    if (*s == '\\' || *s == '\"' || *s == '\n')
      extras_count++;
    s++;
  }
  char *buf = checked_malloc(strlen(m.s) + 1UL + extras_count, "add_escapes");

  char *p = buf;
  s = m.s;
  while (*s) {
    if (*s == '\\') {
      *p++ = '\\';
      *p++ = '\\';
      s++;
    } else if (*s == '\"') {
      *p++ = '\\';
      *p++ = '\"';
      s++;
    } else if (*s == '\n') {
      *p++ = '\\';
      *p++ = 'n';
      s++;
    } else
      *p++ = *s++;
  }
  *p = *s; // null terminate;
  return mal_str(buf);
}

// Helper function to concat a list of strings with given separator, opener,
// closer takes counts of number of strings in the list and number of chars
// (excluding the separators)
const char *str_join(list_node *s, size_t chars, count_t elements,
                     const char *sep, const char *opener, const char *closer) {

  count_t num_sep = elements > 0 ? elements - 1 : 0;
  size_t buf_size =
      1 + chars + strlen(opener) + strlen(closer) + num_sep * strlen(sep);
  char *buf = checked_malloc(buf_size, "str_join");

  strncpy(buf, opener, buf_size);
  while (s != NULL) {
    str_concat(buf, s->val.s, buf_size - 1);
    if (s->next != NULL) {
      str_concat(buf, sep, buf_size - 1);
    }
    s = s->next;
  }
  str_concat(buf, closer, buf_size - 1);
  return buf;
}
