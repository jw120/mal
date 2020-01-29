/**
 *
 * core.c - defines the standard environment
 *
 **/

#include "core.h"

#include "env.h"
#include "seq.h"

fn plus;
fn minus;
fn times;
fn divide;

env *create_startup_env()
{

  mal startup_array[] = {mal_sym("+"), mal_fn(plus), mal_sym("-"),
                         mal_fn(minus), mal_sym("*"), mal_fn(times),
                         mal_sym("/"), mal_fn(divide)};

  return env_new(array_to_list(8, startup_array), NULL);
}

mal plus(list_node *n, env *e)
{
  if (list_count(n) != 2 || !is_int(n->val) || !is_int(n->next->val))
    return mal_exception(mal_str("Bad args to plus"));

  return mal_int(n->val.i + n->next->val.i);
}

mal minus(list_node *n, env *e)
{
  if (list_count(n) != 2 || !is_int(n->val) || !is_int(n->next->val))
    return mal_exception(mal_str("Bad args to minus"));

  return mal_int(n->val.i - n->next->val.i);
}

mal times(list_node *n, env *e)
{
  if (list_count(n) != 2 || !is_int(n->val) || !is_int(n->next->val))
    return mal_exception(mal_str("Bad args to plus"));

  return mal_int(n->val.i * n->next->val.i);
}

mal divide(list_node *n, env *e)
{
  if (list_count(n) != 2 || !is_int(n->val) || !is_int(n->next->val))
    return mal_exception(mal_str("Bad args to divide"));

  if (n->next->val.i == 0)
    return mal_exception(mal_str("Divide by zero"));

  return mal_int(n->val.i / n->next->val.i);
}