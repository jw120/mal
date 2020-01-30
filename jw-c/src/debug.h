#ifndef DEBUG_H
#define DEBUG_H

#include "types.h"

typedef enum
{
  DEBUG_OFF = 0,
  DEBUG_HIGH = 1,
  DEBUG_ALL = 2
} debug_level;

void set_debug_level(const char *);
#define DEBUG_INTERNAL_FMT(...) debug_fmt(__func__, DEBUG_ALL, __VA_ARGS__)
#define DEBUG_HIGH_FMT(...) debug_fmt(__func__, DEBUG_HIGH, __VA_ARGS__)
#define DEBUG_INTERNAL_MAL(msg, m) debug_mal(__func__, DEBUG_ALL, msg, m)
#define DEBUG_HIGH_MAL(msg, m) debug_mal(__func__, DEBUG_HIGH, msg, m)
#define DEBUG_INTERNAL_MAL2(msg, m1, m2) debug_mal2(__func__, DEBUG_ALL, msg, m1, m2)
#define DEBUG_HIGH_MAL2(msg, m1, m2) debug_mal2(__func__, DEBUG_HIGH, msg, m1, m2)
#define DEBUG_HIGH_ENV(e) debug_env(DEBUG_HIGH, e)

void debug_fmt(const char *, debug_level, const char *restrict, ...);
void debug_mal(const char *, debug_level, const char *, mal);
void debug_mal2(const char *, debug_level, const char *, mal, mal);
void debug_env(debug_level, env *);

#endif