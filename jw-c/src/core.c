/**
 *
 * core.c - defines the standard environment
 *
 **/

#include <assert.h>
#include <string.h>

#include "core.h"

#include "env.h"
#include "seq.h"

#define ERR_MSG_LEN 80
char err_msg[ERR_MSG_LEN + 1];

// macro to support validation of arguments
#define ret_if_args_invalid(s) \
  if (s != NULL)               \
    return mal_exception(mal_str(s));

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
mal plus(list_node *n, env *e)
{
  ret_if_args_invalid(validate_2_ints(n, __func__));
  return mal_int(n->val.i + n->next->val.i);
}

// C implementation of mal -
mal minus(list_node *n, env *e)
{
  ret_if_args_invalid(validate_2_ints(n, __func__));
  return mal_int(n->val.i - n->next->val.i);
}

// C implementation of mal *
mal times(list_node *n, env *e)
{
  ret_if_args_invalid(validate_2_ints(n, __func__));
  return mal_int(n->val.i * n->next->val.i);
}

// C implementation of mal /
mal divide(list_node *n, env *e)
{
  ret_if_args_invalid(validate_2_ints(n, __func__));
  if (n->next->val.i == 0)
    return mal_exception(mal_str("Divide by zero"));

  return mal_int(n->val.i / n->next->val.i);
}

// return the core environment, creating it if it does not already exist
env *core_env()
{

  const size_t symbol_array_size = 8;
  mal symbol_array[] = {mal_sym("+"), mal_fn(plus), mal_sym("-"),
                        mal_fn(minus), mal_sym("*"), mal_fn(times),
                        mal_sym("/"), mal_fn(divide), mal_nil()}; // nil is a sentinel
  assert(is_nil(symbol_array[symbol_array_size]));                // check array size is correct

  // create the environment if it does not yet exist
  static env *e = NULL;
  if (e == NULL)
    e = env_new(array_to_list(symbol_array_size, symbol_array), NULL);
  return e;
}
