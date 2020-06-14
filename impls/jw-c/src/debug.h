#ifndef DEBUG_H
#define DEBUG_H

#include "types.h"

// Available debug levels
typedef enum {
  DEBUG_OFF = 0,  // No debugging messages
  DEBUG_HIGH = 1, // High-level debugging messages only
  DEBUG_ALL = 2   // All debugging messages ("internal")
} debug_level;

// set the global debugging level from the DEBUG environment variable
void set_debug_level_from_env(void);

// Print debug message using vprintf format template if debug level is
// DEBUG_ALL
#define DEBUG_INTERNAL_FMT(...) debug_fmt(__func__, DEBUG_ALL, __VA_ARGS__)

// Print debug message using vprintf format template if debug level is
// DEBUG_HIGH
#define DEBUG_HIGH_FMT(...) debug_fmt(__func__, DEBUG_HIGH, __VA_ARGS__)

// Print debug message and a mal value if debug level is
// DEBUG_ALL
#define DEBUG_INTERNAL_MAL(msg, m) debug_mal(__func__, DEBUG_ALL, msg, m)

// Print debug message and a mal value if debug level is
// DEBUG_HIGH
#define DEBUG_HIGH_MAL(msg, m) debug_mal(__func__, DEBUG_HIGH, msg, m)

// Print debug message and two mal values if debug level is
// DEBUG_ALL
#define DEBUG_INTERNAL_MAL2(msg, m1, m2)                                       \
  debug_mal2(__func__, DEBUG_ALL, msg, m1, m2)

// Print debug message and two mal values if debug level is
// DEBUG_HIGH
#define DEBUG_HIGH_MAL2(msg, m1, m2)                                           \
  debug_mal2(__func__, DEBUG_HIGH, msg, m1, m2)

// Print the contents of the given environment if debug level is
// DEBUG_HIGH
#define DEBUG_HIGH_ENV(e) debug_env(DEBUG_HIGH, e)

// if the current debug level is at least as high as the level specified, print
// a debugging message using a vprintf format template
void debug_fmt(const char *, debug_level, const char *restrict, ...);

// if the current debug level is at least as high as the level specified,print a
// debugging message consisting of a string and a mal value (which is passed
// through pr_str)
void debug_mal(const char *, debug_level, const char *, mal);

// if the current debug level is at least as high as the level specified,print a
// debugging message consisting of a string and two mal values (which are passed
// through pr_str)
void debug_mal2(const char *, debug_level, const char *, mal, mal);

// if the current debug level is at least as high as the level specified,print
// the contents of the given environment
void debug_env(debug_level, env *);

#endif
