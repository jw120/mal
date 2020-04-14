/**
 *
 * debug.c - debug info functions
 *
 **/

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "core.h"
#include "debug.h"
#include "hash_table.h"
#include "printer.h"

static debug_level debug = DEBUG_OFF;

void set_debug_level_from_env() {
  const char *setting = getenv("DEBUG");
  if (setting == NULL)
    return;
  if (strncmp(setting, "1", 1) == 0)
    debug = DEBUG_HIGH;
  else if (strncmp(setting, "2", 1) == 0)
    debug = DEBUG_ALL;
}

void debug_fmt(const char *func, debug_level level, const char *restrict fmt,
               ...) {
  if (level > debug)
    return;
  va_list args;
  va_start(args, fmt);
  printf("%16s: ", func);
  vprintf(fmt, args);
  printf("\n");
  fflush(stdout);
}

void debug_mal(const char *func, debug_level level, const char *msg, mal m) {
  if (level > debug)
    return;
  printf("%16s: '%s' '%s'\n", func, msg, pr_str(m, true));
  fflush(stdout);
}

void debug_mal2(const char *func, debug_level level, const char *msg, mal m1,
                mal m2) {
  if (level > debug)
    return;
  printf("%16s: %s %s %s\n", func, msg, pr_str(m1, true), pr_str(m2, true));
  fflush(stdout);
}

void debug_env(debug_level level, env *e) {
  if (level > debug)
    return;
  assert(e != NULL);

  printf("environment: ");
  if (e == core_env())
    printf("core");
  else
    printf("%x", (unsigned)e & 0xffff);

  if (e->outer == NULL)
    printf(" (outer=NULL)\n");
  else if (e->outer == core_env())
    printf(" (outer=core)\n");
  else
    printf(" (outer=%x)\n", (unsigned)e->outer & 0xffff);

  assert(e->lookup != NULL);
  ht_debug_print(e->lookup, "  ");
}
