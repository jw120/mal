#include <stdio.h>

#include "eval.h"
#include "eval_test.h"
#include "minunit.h"

#include "core.h"
#include "reader.h"
#include "seq.h"

const char *eval_test() {

  mal a[] = {mal_sym("x"), mal_int(7), mal_sym("y"), mal_int(5)};
  env *e = env_new(array_to_list(4, a), core_env());

  // eval_ast preserves atoms and empty lists
  mu_assert_eq("eval_ast int", eval_ast(mal_int(2), e), mal_int(2));
  mu_assert_eq("eval_ast ()", eval_ast(mal_list(NULL), e), mal_list(NULL));

  // eval_ast looks up the value of a symbol
  mu_assert_eq("eval_ast sym", eval_ast(mal_sym("x"), e), mal_int(7));

  // eval_ast applies along a list
  mal input_a_1[] = {mal_sym("y"), mal_int(0), mal_str("abc"), mal_sym("x"),
                     mal_list(NULL)};
  mal expected_a_1[] = {mal_int(5), mal_int(0), mal_str("abc"), mal_int(7),
                        mal_list(NULL)};
  mal input_1 = mal_list(array_to_list(5, input_a_1));
  mal expected_1 = mal_list(array_to_list(5, expected_a_1));
  mu_assert_eq("eval_ast list", eval_ast(input_1, e), expected_1);

  // eval_ast applies along a vector
  mal input_1v = mal_vec(list_to_vec(5, input_1.n));
  mal expected_1v = mal_vec(list_to_vec(5, expected_1.n));
  mu_assert_eq("eval_ast vec", eval_ast(input_1v, e), expected_1v);

  // eval_ast applies to values in a hashmap
  mal input_m = read_str("{:a 3 :b (* 3 4) :c () :d (+ x y)}");
  mal expected_m = read_str("{:a 3 :b 12 :c () :d 12}");
  mu_assert_eq("eval_ast map", eval_ast(input_m, e), expected_m);

  // eval does the same as eval_ast on atoms, empty lists, vectors and hashmaps
  mu_assert_eq("eval int", eval(mal_int(2), e), mal_int(2));
  mu_assert_eq("eval ()", eval(mal_list(NULL), e), mal_list(NULL));
  mu_assert_eq("eval sym", eval(mal_sym("x"), e), mal_int(7));
  mu_assert_eq("eval vec", eval(input_1v, e), expected_1v);
  mu_assert_eq("eval map", eval(input_m, e), expected_m);

  // eval calls a function
  mal input_a_2[] = {mal_sym("+"), mal_int(3), mal_int(1)};
  mal input_2 = mal_list(array_to_list(3, input_a_2));
  mu_assert_eq("eval (+ 3 1)", eval(input_2, e), mal_int(4));
  mal input_a_3[] = {mal_sym("+"), mal_sym("y"), mal_sym("x")};
  mal input_3 = mal_list(array_to_list(3, input_a_3));
  mu_assert_eq("eval (+ x y)", eval(input_3, e), mal_int(12));

  return 0;
}
