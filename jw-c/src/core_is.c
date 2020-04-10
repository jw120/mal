/**
 *
 * core_is - defines is? functions for the core environment
 *
 **/

// #include <errno.h>
// #include <stdio.h>
// #include <string.h>

#include "core_is.h"

#include "debug.h"
#include "env.h"
// #include "printer.h"
// #include "reader.h"
#include "seq.h"
// #include "utils.h"

// C implementation of mal list?
mal list_test(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1)
    return mal_exception_str("Need one argument for list?");
  return mal_bool(is_list(n->val));
}

// C implementation of mal nil?
static mal core_is_nil(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1)
    return mal_exception_str("Need one argument for nil?");
  return mal_bool(is_nil(n->val));
}

// C implementation of mal true?
static mal core_is_true(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1)
    return mal_exception_str("Need one argument for true?");
  return mal_bool(is_true(n->val));
}

// C implementation of mal false?
static mal core_is_false(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1)
    return mal_exception_str("Need one argument for false?");
  return mal_bool(is_false(n->val));
}

// C implementation of mal symbol?
static mal core_is_sym(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1)
    return mal_exception_str("Need one argument for symbol?");
  return mal_bool(is_sym(n->val));
}

// C implementation of mal keyword?
static mal core_is_kw(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1)
    return mal_exception_str("Need one argument for keyword?");
  return mal_bool(is_kw(n->val));
}

// C implementation of mal vector?
static mal core_is_vec(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1)
    return mal_exception_str("Need one argument for vector?");
  return mal_bool(is_vec(n->val));
}

// C implementation of mal sequential?
static mal core_is_seq(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1)
    return mal_exception_str("Need one argument for sequential?");
  return mal_bool(is_seq(n->val));
}
// C implementation of mal map?
static mal core_is_map(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1)
    return mal_exception_str("Need one argument for map?");
  return mal_bool(is_map(n->val));
}

// add is? core functions to the environment
void add_is(env *e) {
  env_set(e, "list?", mal_fn(list_test));
  env_set(e, "nil?", mal_fn(core_is_nil));
  env_set(e, "true?", mal_fn(core_is_true));
  env_set(e, "false?", mal_fn(core_is_false));
  env_set(e, "symbol?", mal_fn(core_is_sym));
  env_set(e, "keyword?", mal_fn(core_is_kw));
  env_set(e, "vector?", mal_fn(core_is_vec));
  env_set(e, "sequential?", mal_fn(core_is_seq));
  env_set(e, "map?", mal_fn(core_is_map));
}
