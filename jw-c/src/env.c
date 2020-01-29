/**
 *
 * env.c - implement an environment of symbols
 *
 * Holds symbols using a map of mal_str
 *
 **/

#include <assert.h>

#include "env.h"

#include "map.h"
#include "seq.h"
#include "utils.h"

// creata a new envionment given an alternating sym/val list (NULL if fails)
env *env_new(list_node *elems, env *outer)
{
  debug("env_new", "%d elements and outer %p", list_count(elems), outer);

  env *e = checked_malloc(sizeof(env), "env_new");
  e->lookup = list_to_map(elems);
  e->outer = outer;
  if (e->lookup == NULL)
    return NULL;

  debug("env_new", "lookup is %p and outer is %p", e->lookup, e->outer);

  return e;
}

// NYI - set a value in the environment
void env_set(env *e, const char *sym, mal val)
{
  assert(false); // NYI
}

// Return the environment where the sym is defined (in its our chain) or NULL
env *env_find(env *e, const char *sym)
{
  debug("env_find", "finding %s in %p", sym, e);
  if (e == NULL)
    return NULL;
  if (map_contains(e->lookup, mal_str(sym)))
    return e;
  if (e->outer != NULL)
    return env_find(e->outer, sym);
  return NULL;
}

// Return the value associtated with the given symbol, exception if not found
mal env_get(env *e, const char *sym)
{
  debug("env_get", "getting %s in %p", sym, e);
  env *found_e = env_find(e, sym);
  debug("env_get", "found %p", found_e);
  if (found_e == NULL)
    return mal_exception(mal_str("not found"));
  return map_get(found_e->lookup, mal_str(sym));
}