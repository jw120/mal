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
#include "map.h"
#include "seq.h"
#include "utils.h"

// creata a new envionment given an alternating sym/val list (NULL if fails)
env *env_new(list_node *elems, env *outer)
{
  DEBUG_INTERNAL_FMT("called with %d elements and outer %p", list_count(elems), outer);

  env *e = checked_malloc(sizeof(env), "env_new");
  e->lookup = list_to_map(elems);
  e->outer = outer;
  if (e->lookup == NULL)
    return NULL;

  return e;
}

// NYI - set a value in the environment
void env_set(env *e, const char *sym, mal val)
{
  DEBUG_INTERNAL_MAL2("setting", mal_sym(sym), val);
  assert(e != NULL);
  map_set(e->lookup, mal_sym(sym), val);
}

// Return the environment where the sym is defined (in its our chain) or NULL
env *env_find(env *e, const char *sym)
{
  DEBUG_INTERNAL_FMT("finding %s in %p", sym, e);
  if (e == NULL)
    return NULL;
  if (map_contains(e->lookup, mal_str(sym)))
    return e;
  if (e->outer != NULL)
    return env_find(e->outer, sym);
  return NULL;
}

#define NOT_FOUND_BUF_SIZE 80
static char not_found_buf[NOT_FOUND_BUF_SIZE];

// Return the value associtated with the given symbol, exception if not found
mal env_get(env *e, const char *sym)
{
  DEBUG_INTERNAL_FMT("getting %s", sym);
  env *found_e = env_find(e, sym);
  DEBUG_INTERNAL_FMT("found %p", found_e);
  if (found_e == NULL)
  {
    snprintf(not_found_buf, NOT_FOUND_BUF_SIZE, "'%s' not found", sym);
    return mal_exception_str(not_found_buf);
  }
  mal ret = map_get(found_e->lookup, mal_str(sym));
  DEBUG_HIGH_MAL2("", mal_sym(sym), ret);
  return ret;
}

/*

env %p (outer=core)  / (outer=NULL) / (outer=%p)
  +: <function>
  x: 2

*/
