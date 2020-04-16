/**
 *
 * env.c - implement an environment of symbols
 *
 * Holds symbols using a map of mal_str
 *
 **/

#include <assert.h>
#include <stdio.h>

#include "env.h"

#include "debug.h"
#include "hash_table.h"
#include "seq.h"
#include "utils.h"

env *env_new(list_node *elems, env *outer) {
  DEBUG_HIGH_FMT("called with %d elements and outer %p", list_count(elems),
                 outer);

  env *e = checked_malloc(sizeof(env), "env_new");
  e->lookup = ht_from_alternating_list(elems);
  e->outer = outer;
  if (e->lookup == NULL)
    return NULL;

  DEBUG_INTERNAL_FMT("returning %p", e);
  return e;
}

env *env_new2(list_node *binds, list_node *vals, env *outer) {
  DEBUG_HIGH_FMT("called with outer %p", outer);

  env *e = checked_malloc(sizeof(env), "env_new2");
  e->lookup = ht_from_lists(binds, vals);
  e->outer = outer;
  if (e->lookup == NULL)
    return NULL;

  DEBUG_INTERNAL_FMT("returning %p", e);
  return e;
}

void env_free(env *e) {
  DEBUG_HIGH_FMT("called on %p", e);
  free(e->lookup);
  free(e);
}

void env_set(env *e, const char *sym, mal val) {
  DEBUG_INTERNAL_MAL2("setting", mal_sym(sym), val);
  assert(e != NULL);
  ht_put(e->lookup, sym, val);
}

env *env_find(env *e, const char *sym) {
  DEBUG_INTERNAL_FMT("finding %s in %p", sym, e);
  while (e != NULL) {
    if (ht_has(e->lookup, sym))
      return e;
    e = e->outer;
  }
  return NULL;
}

#define NOT_FOUND_BUF_SIZE 80
static char not_found_buf[NOT_FOUND_BUF_SIZE];

mal env_get(env *e, const char *sym) {
  DEBUG_INTERNAL_FMT("getting %s", sym);
  env *found_e = env_find(e, sym);
  DEBUG_INTERNAL_FMT("found %p", found_e);
  if (found_e == NULL) {
    snprintf(not_found_buf, NOT_FOUND_BUF_SIZE, "'%s' not found", sym);
    return mal_exception_str(not_found_buf);
  }
  mal ret = ht_get(found_e->lookup, sym);
  DEBUG_HIGH_MAL2("returning", mal_sym(sym), ret);
  return ret;
}
