/**
 *
 * core_miscc - defines misc functions for the core environment
 *
 **/

#include <stdio.h>
#include <string.h>

#include "core_misc.h"

#include "debug.h"
#include "env.h"
#include "printer.h"

// C implementation of mal =
mal equals(list_node *n, env *e)
{
  DEBUG_HIGH_MAL("called with", mal_list(n));
  RETURN_IF_EXCEPTION(n->val);
  RETURN_IF_EXCEPTION(n->next->val);
  return mal_bool(mal_equals(n->val, n->next->val));
}

// C implementation of mal prn
mal prn(list_node *n, env *e)
{
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (n != NULL)
    puts(pr_str(n->val, true));
  return mal_nil();
}

// add misc core functions to the environment
void add_misc(env *e)
{
  env_set(e, "prn", mal_fn(prn));
  env_set(e, "=",  mal_fn(equals));
}
