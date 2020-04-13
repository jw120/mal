/**
 *
 * core_num.c - defines number-related functions for core environment
 *
 **/

#include <string.h>

#include "core_num.h"

#include "debug.h"
#include "env.h"
#include "seq.h"

// Macro for C implementation for a two-integer argument function
#define DEFINE_NUM_FUNCTION(C_fn_name, mal_fn_name, num_fn)                    \
  static mal C_fn_name(list_node *n, UNUSED(env *e)) {                         \
    DEBUG_HIGH_MAL("called with", mal_list(n));                                \
    if (list_count(n) != 2 || !is_int(n->val) || !is_int(n->next->val))        \
      return mal_exception_str("Need two integers for " mal_fn_name);          \
    return num_fn(n->val.i, n->next->val.i);                                   \
  }

static mal fn_lt(int x, int y) { return mal_bool(x < y); }
static mal fn_le(int x, int y) { return mal_bool(x <= y); }
static mal fn_gt(int x, int y) { return mal_bool(x > y); }
static mal fn_ge(int x, int y) { return mal_bool(x >= y); }
static mal fn_plus(int x, int y) { return mal_int(x + y); }
static mal fn_subtract(int x, int y) { return mal_int(x - y); }
static mal fn_times(int x, int y) { return mal_int(x * y); }
static mal fn_divide(int x, int y) {
  if (y == 0)
    return mal_exception_str("Division by zero");
  return mal_int(x / y);
}

DEFINE_NUM_FUNCTION(core_lt, "<", fn_lt)
DEFINE_NUM_FUNCTION(core_le, "<=", fn_le)
DEFINE_NUM_FUNCTION(core_gt, ">", fn_gt)
DEFINE_NUM_FUNCTION(core_ge, ">=", fn_ge)
DEFINE_NUM_FUNCTION(core_add, "+", fn_plus)
DEFINE_NUM_FUNCTION(core_subtract, "-", fn_subtract)
DEFINE_NUM_FUNCTION(core_times, "*", fn_times)
DEFINE_NUM_FUNCTION(core_divide, "/", fn_divide)

// add number-related core functions to the environment
void add_num(env *e) {
  env_set(e, "+", mal_fn(core_add));
  env_set(e, "-", mal_fn(core_subtract));
  env_set(e, "*", mal_fn(core_times));
  env_set(e, "/", mal_fn(core_divide));
  env_set(e, "<", mal_fn(core_lt));
  env_set(e, "<=", mal_fn(core_le));
  env_set(e, ">", mal_fn(core_gt));
  env_set(e, ">=", mal_fn(core_ge));
}
