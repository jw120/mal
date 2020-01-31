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

#define ERR_MSG_LEN 80
char err_msg[ERR_MSG_LEN + 1];

// macro to support validation of arguments
#define ret_if_args_invalid(s) \
  if (s != NULL)               \
    return mal_exception_str(s);

// does the list of arguments passed represent two ints
static char *validate_2_ints(list_node *n, const char *func_name)
{
  if (list_count(n) != 2 || !is_int(n->val) || !is_int(n->next->val))
  {
    strncpy(err_msg, "Expected two integer arguments for ", ERR_MSG_LEN);
    strncat(err_msg, func_name, ERR_MSG_LEN);
    return err_msg;
  }
  return NULL;
}

// C implementation of mal +
mal add(list_node *n, env *e)
{
  DEBUG_HIGH_MAL("called with", mal_list(n));
  ret_if_args_invalid(validate_2_ints(n, __func__));
  return mal_int(n->val.i + n->next->val.i);
}

// C implementation of mal -
mal subtract(list_node *n, env *e)
{
  DEBUG_HIGH_MAL("called with", mal_list(n));
  ret_if_args_invalid(validate_2_ints(n, __func__));
  return mal_int(n->val.i - n->next->val.i);
}

// C implementation of mal *
mal times(list_node *n, env *e)
{
  DEBUG_HIGH_MAL("called with", mal_list(n));
  ret_if_args_invalid(validate_2_ints(n, __func__));
  return mal_int(n->val.i * n->next->val.i);
}

// C implementation of mal /
mal divide(list_node *n, env *e)
{
  DEBUG_HIGH_MAL("called with", mal_list(n));
  ret_if_args_invalid(validate_2_ints(n, __func__));
  if (n->next->val.i == 0)
    return mal_exception_str("Divide by zero");

  return mal_int(n->val.i / n->next->val.i);
}

// C implementation of mal <
mal lt(list_node *n, env *e)
{
  DEBUG_HIGH_MAL("called with", mal_list(n));
  ret_if_args_invalid(validate_2_ints(n, __func__));
  return mal_bool(n->val.i < n->next->val.i);
}

// C implementation of mal <=
mal lte(list_node *n, env *e)
{
  DEBUG_HIGH_MAL("called with", mal_list(n));
  ret_if_args_invalid(validate_2_ints(n, __func__));
  return mal_bool(n->val.i <= n->next->val.i);
}

// C implementation of mal >
mal gt(list_node *n, env *e)
{
  DEBUG_HIGH_MAL("called with", mal_list(n));
  ret_if_args_invalid(validate_2_ints(n, __func__));
  return mal_bool(n->val.i > n->next->val.i);
}

// C implementation of mal >=
mal gte(list_node *n, env *e)
{
  DEBUG_HIGH_MAL("called with", mal_list(n));
  ret_if_args_invalid(validate_2_ints(n, __func__));
  return mal_bool(n->val.i >= n->next->val.i);
}

// add number-related core functions to the environment
void add_num(env *e)
{
  env_set(e, "+", mal_fn(add));
  env_set(e, "-",  mal_fn(subtract));
  env_set(e, "*", mal_fn(times));
  env_set(e, "/", mal_fn(divide));
  env_set(e, "<", mal_fn(lt));
  env_set(e, "<=",  mal_fn(lte));
  env_set(e, ">", mal_fn(gt));
  env_set(e, ">=", mal_fn(gte));
}
