#ifndef UTILS_H
#define UTILS_H

#include <stdnoreturn.h>

extern bool debug_mode;

noreturn void internal_error(const char * restrict, ...);
void debug(const char *, const char * restrict, ...);
void *checked_malloc(size_t, const char * restrict, ...);


#endif