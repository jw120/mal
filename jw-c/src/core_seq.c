/**
 *
 * core_seq.c - defines sequence-related function for the standard environment
 *
 **/

#include "core_seq.h"

#include "debug.h"
#include "env.h"
#include "seq.h"

// C implementation of mal list
mal list(list_node *n, env *e) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  return mal_list(n);
}

// C implementation of mal list?
mal list_test(list_node *n, env *e) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  return mal_bool(is_list(n->val));
}

// C implementation of mal empty?
mal empty_test(list_node *n, env *e) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  return mal_bool(seq_empty(n->val));
}

// C implementation of mal count
mal count(list_node *n, env *e) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  return mal_int(seq_count(n->val));
}

// add sequence-related core functions to the environment
void add_seq(env *e) {
  env_set(e, "list", mal_fn(list));
  env_set(e, "list?", mal_fn(list_test));
  env_set(e, "empty?", mal_fn(empty_test));
  env_set(e, "count", mal_fn(count));
}
