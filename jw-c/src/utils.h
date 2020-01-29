#ifndef UTILS_H
#define UTILS_H

#include <stdbool.h>
#include <stdnoreturn.h>

#include "types.h"

extern bool debug_mode;

bool is_number(const char *);

noreturn void internal_error(const char *restrict, ...);
void debug(const char *, const char *restrict, ...);
void *checked_malloc(size_t, const char *restrict, ...);
void str_concat(char *, const char *, size_t);

mal add_escapes(mal);
mal remove_escapes(mal);
bool str_equals(mal, mal);

#endif