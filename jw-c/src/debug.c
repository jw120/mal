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
#include "printer.h"

static debug_level debug = DEBUG_OFF;

// set the global debugging level
void set_debug_level(const char *setting)
{
  if (setting == NULL)
    return;
  if (strncmp(setting, "1", 1) == 0)
    debug = DEBUG_HIGH;
  else if (strncmp(setting, "2", 1) == 0)
    debug = DEBUG_ALL;
}

// print a debugging message
void debug_fmt(const char *func, debug_level level, const char *restrict fmt, ...)
{
  if (level > debug)
    return;
  va_list args;
  va_start(args, fmt);
  printf("%16s: ", func);
  vprintf(fmt, args);
  printf("\n");
  fflush(stdout);
}

// print a debugging message and a mal value
void debug_mal(const char *func, debug_level level, const char *msg, mal m)
{
  if (level > debug)
    return;
  printf("%16s: %s %s\n", func, msg, pr_str(m, true));
  fflush(stdout);
}

// print a debugging message and two mal values
void debug_mal2(const char *func, debug_level level, const char *msg, mal m1, mal m2)
{
  if (level > debug)
    return;
  printf("%16s: %s %s %s\n", func, msg, pr_str(m1, true), pr_str(m2, true));
  fflush(stdout);
}

// Print the environment
void debug_env(debug_level level, env *e)
{
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
  map_record *sym_table = e->lookup->table;
  size_t sym_table_size = e->lookup->size;
  for (int i = 0; i < sym_table_size; i++)
    printf("  %s%s -> %s\n",
           sym_table[i].is_kw ? ":" : "",
           sym_table[i].key,
           pr_str(sym_table[i].val, true));
}