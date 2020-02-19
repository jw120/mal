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

// C implementation of mal cons
mal core_cons(list_node *n, env *e) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 2)
    return mal_exception_str("Bad numbero ofarguments to cons");
  if (is_list(n->next->val))
    return mal_cons(n->val, n->next->val);
  if (is_vec(n->next->val)) {
    list_node *new_head = list_cons(n->val, NULL);
    list_node *new_ptr = new_head;
    for (int i = 0; i < n->next->val.v->size; i++) {
      new_ptr = list_extend(n->next->val.v->buf[i], new_ptr);
    }
    return mal_list(new_head);
  }
  return mal_exception_str("Bad arguments to cons");
}

// C implementation of mal concat
mal core_concat(list_node *n, env *e) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  list_node *new_head = NULL;
  list_node *new_ptr = NULL;
  while (n != NULL) {
    if (is_list(n->val)) {
      list_node *sub_list_ptr = n->val.n;
      while (sub_list_ptr != NULL) {
        new_ptr = list_extend(sub_list_ptr->val, new_ptr);
        if (new_head == NULL)
          new_head = new_ptr;
        sub_list_ptr = sub_list_ptr->next;
      }
    } else if (is_vec(n->val)) {
      for (int i = 0; i < n->val.v->size; i++) {
        new_ptr = list_extend(n->val.v->buf[i], new_ptr);
        if (new_head == NULL)
          new_head = new_ptr;
      }
    } else {
      return mal_exception_str("Expected a sequence in concat");
    }
    n = n->next;
  }
  return mal_list(new_head);
}

// C implementation of mal first
mal core_first(list_node *n, env *e) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1)
    return mal_exception_str("Bad arguments to first");
  return mal_first(n->val);
}

// C implementation of mal rest
mal core_rest(list_node *n, env *e) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1)
    return mal_exception_str("Bad arguments to rest");
  return mal_rest(n->val);
}

// add sequence-related core functions to the environment
void add_seq(env *e) {
  env_set(e, "list", mal_fn(list));
  env_set(e, "list?", mal_fn(list_test));
  env_set(e, "empty?", mal_fn(empty_test));
  env_set(e, "count", mal_fn(count));
  env_set(e, "cons", mal_fn(core_cons));
  env_set(e, "concat", mal_fn(core_concat));
  env_set(e, "first", mal_fn(core_first));
  env_set(e, "rest", mal_fn(core_rest));
}
