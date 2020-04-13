/**
 *
 * core_seq.c - defines sequence-related function for the standard environment
 *
 **/

#include "core_seq.h"

#include "debug.h"
#include "env.h"
#include "hash_table.h"
#include "seq.h"

// C implementation of mal list
static mal core_list(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  return mal_list(n);
}

// C implementation of mal empty?
static mal core_empty(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  return mal_bool(seq_empty(n->val));
}

// C implementation of mal count
static mal core_count(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  return mal_int((int)seq_count(n->val));
}

// C implementation of mal cons
static mal core_cons(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 2)
    return mal_exception_str("Bad numbero ofarguments to cons");
  if (is_list(n->next->val))
    return mal_cons(n->val, n->next->val);
  if (is_vec(n->next->val)) {
    list_node *new_head = list_cons(n->val, NULL);
    list_node *new_ptr = new_head;
    for (count_t i = 0; i < n->next->val.v->count; i++) {
      new_ptr = list_extend(n->next->val.v->buf[i], new_ptr);
    }
    return mal_list(new_head);
  }
  return mal_exception_str("Bad arguments to cons");
}

// C implementation of mal concat
static mal core_concat(list_node *n, UNUSED(env *e)) {
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
      for (count_t i = 0; i < n->val.v->count; i++) {
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
static mal core_first(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1)
    return mal_exception_str("Bad arguments to first");
  return mal_first(n->val);
}

// C implementation of mal rest
static mal core_rest(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1)
    return mal_exception_str("Bad arguments to rest");
  return mal_rest(n->val);
}

// C implementation of mal nth
static mal core_nth(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 2)
    return mal_exception_str("Need two arguments to nth");
  if (!is_int(n->next->val))
    return mal_exception_str("Second argument must be an integer for nth");
  mal target = n->val;
  if (n->next->val.i < 0)
    return mal_exception_str("Second argument must be non-negative for nth");
  count_t index = (count_t)n->next->val.i;

  if (is_list(target)) {
    list_node *p = target.n;
    while (index-- > 0 && p != NULL) {
      p = p->next;
    }
    return p != NULL ? p->val : mal_exception_str("Index out of range for nth");
  }

  if (is_vec(target)) {
    if (index < target.v->count)
      return target.v->buf[index];
    return mal_exception_str("Index out of range for vector nth");
  }

  return mal_exception_str("Non-sequence passed to nth");
}

// C implementation of mal vector
static mal core_vector(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  return mal_vec(list_to_vec(list_count(n), n));
}

// C implementation of mal hash-map
static mal core_hash_map(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) % 2)
    return mal_exception_str("Need even number of arguments to hash-map");
  return mal_map(ht_from_alternating_list(n));
}

// C implementation of mal get
static mal core_get(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 2)
    return mal_exception_str("Need two arguments to get");
  if (is_map(n->val) && is_str_or_kw(n->next->val))
    return (ht_has(n->val.m, n->next->val.s)) ? ht_get(n->val.m, n->next->val.s)
                                              : mal_nil();
  if (is_nil(n->val))
    return mal_nil();
  return mal_exception_str(
      "Need a map (or nil) and a string or keyword as arguments to get");
}

// C implementation of mal contains?
static mal core_contains(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 2)
    return mal_exception_str("Need two arguments to get");
  if (is_map(n->val) && is_str_or_kw(n->next->val))
    return mal_bool(ht_has(n->val.m, n->next->val.s));
  if (is_nil(n->val))
    return mal_nil();
  return mal_exception_str(
      "Need a map (or nil) and a string or keyword as arguments to contains?");
}

// C implementation of mal keys
static mal core_keys(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1 || !is_map(n->val))
    return mal_exception_str("Need a map for keys");
  return mal_list(ht_keys(n->val.m));
}

// C implementation of mal vals
static mal core_vals(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1 || !is_map(n->val))
    return mal_exception_str("Need a map for vals");
  return mal_list(ht_values(n->val.m));
}

// C implementation of mal assoc
static mal core_assoc(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  count_t args_count = list_count(n) - 1; // arguments after hash-map
  if (args_count % 2 || !is_map(n->val))
    return mal_exception_str(
        "Need a map and an even number of other arguments for assoc");
  hash_table *new_ht = ht_copy(n->val.m, n->val.m->entries + args_count / 2);
  list_node *arg = n->next;
  while (arg != NULL) {
    if (!is_str_or_kw(arg->val))
      return mal_exception_str("Keys must be strings or keywords");
    ht_put(new_ht, arg->val.s, arg->next->val);
    arg = arg->next->next;
  }
  return mal_map(new_ht);
}

// C implementation of mal dissoc
static mal core_dissoc(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (n == NULL || !is_map(n->val))
    return mal_exception_str("Need a map for dissoc");
  hash_table *old_ht = n->val.m;
  hash_table *new_ht = ht_new(old_ht->entries);
  list_node *dissoc_list = n->next;
  for (list_node *n = ht_keys(old_ht); n != NULL; n = n->next) {
    if (!list_contains(dissoc_list, n->val)) {
      ht_put(new_ht, n->val.s, ht_get(old_ht, n->val.s));
    }
  }
  return mal_map(new_ht);
}

void add_seq(env *e) {
  env_set(e, "list", mal_fn(core_list));
  env_set(e, "empty?", mal_fn(core_empty));
  env_set(e, "count", mal_fn(core_count));
  env_set(e, "cons", mal_fn(core_cons));
  env_set(e, "concat", mal_fn(core_concat));
  env_set(e, "first", mal_fn(core_first));
  env_set(e, "rest", mal_fn(core_rest));
  env_set(e, "nth", mal_fn(core_nth));
  env_set(e, "vector", mal_fn(core_vector));
  env_set(e, "hash-map", mal_fn(core_hash_map));
  env_set(e, "get", mal_fn(core_get));
  env_set(e, "contains?", mal_fn(core_contains));
  env_set(e, "keys", mal_fn(core_keys));
  env_set(e, "vals", mal_fn(core_vals));
  env_set(e, "assoc", mal_fn(core_assoc));
  env_set(e, "dissoc", mal_fn(core_dissoc));
}
