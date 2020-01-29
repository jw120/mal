#include <stdio.h>

#include "eval.h"
#include "eval_test.h"
#include "minunit.h"

#include "core.h"
#include "seq.h"

const char *eval_test()
{

  mal a[] = {mal_sym("x"), mal_int(7), mal_sym("y"), mal_int(5)};
  env *e = env_new(array_to_list(4, a), create_startup_env());

  // eval_ast preserves atoms and empty lists
  mu_assert_eq("eval_ast int", eval_ast(mal_int(2), e), mal_int(2));
  mu_assert_eq("eval_ast ()", eval_ast(mal_list(NULL), e), mal_list(NULL));

  // eval_ast looks up the value of a symbol
  mu_assert_eq("eval_ast sym", eval_ast(mal_sym("x"), e), mal_int(7));

  // eval_ast applies along a list
  mal input_a_1[] = {mal_sym("y"), mal_int(0), mal_str("abc"), mal_sym("x"), mal_list(NULL)};
  mal expected_a_1[] = {mal_int(5), mal_int(0), mal_str("abc"), mal_int(7), mal_list(NULL)};
  mal input_1 = mal_list(array_to_list(5, input_a_1));
  mal expected_1 = mal_list(array_to_list(5, expected_a_1));
  mu_assert_eq("eval_ast list", eval_ast(input_1, e), expected_1);

  // eval does the same as eval_ast on atoms and empty lists
  mu_assert_eq("eval int", eval(mal_int(2), e), mal_int(2));
  mu_assert_eq("eval ()", eval(mal_list(NULL), e), mal_list(NULL));
  mu_assert_eq("eval sym", eval(mal_sym("x"), e), mal_int(7));

  // eval calls a function
  mal input_a_2[] = {mal_sym("+"), mal_int(3), mal_int(1)};
  mal input_2 = mal_list(array_to_list(3, input_a_2));
  mu_assert_eq("eval (+ 3 1)", eval(input_2, e), mal_int(4));
  mal input_a_3[] = {mal_sym("+"), mal_sym("y"), mal_sym("x")};
  mal input_3 = mal_list(array_to_list(3, input_a_3));
  mu_assert_eq("eval (+ x y)", eval(input_3, e), mal_int(12));

  return 0;
}
