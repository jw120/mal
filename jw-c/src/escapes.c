/**
 *
 * escapes.c - add and remove escape sequences from shrings
 *
 **/

#include <string.h>

#include "escapes.h"
#include "utils.h"

mal remove_escapes(const char *s) {
  char *buf = checked_malloc(strlen(s) + 1, "remove_escapes");
  bool in_escape = false;
  char *p = buf;
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

const char *add_escapes(const char *s) {

  // need to make one extra char space for every ", \, \n
  size_t extras_count = 0;
  for (const char *s2 = s; *s2; s2++) {
    if (*s2 == '\\' || *s2 == '\"' || *s2 == '\n')
      extras_count++;
  }
  char *buf = checked_malloc(strlen(s) + 1UL + extras_count, "add_escapes");

  char *p = buf;
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
  return buf;
}
