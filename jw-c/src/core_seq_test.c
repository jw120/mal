#include "core_misc_test.h"
#include "core_num.h"
#include "minunit.h"

#include "core.h"
#include "eval.h"
#include "reader.h"
#include "seq.h"

#define E(s) eval(read_str(s), e)

const char *core_seq_test() {
  env *e = core_env();

  // list
  list_node *n12 = list_cons(mal_int(1), list_cons(mal_int(2), NULL));
  mu_assert_eq("list 1 2", E("(list 1 2)"), mal_list(n12));
  mu_assert_eq("list ()", E("(list)"), E("()"));

  // list?
  mu_assert_eq("list? (1 2)", E("(list? '(1 2))"), mal_true());
  mu_assert_eq("list? ()", E("(list? ())"), mal_true());
  mu_assert_eq("list? nil", E("(list? nil)"), mal_false());
  mu_assert_eq("list? []", E("(list? [1 2])"), mal_false());

  // empty?
  mu_assert_eq("empty? (1 2)", E("(empty? '(1 2))"), mal_false());
  mu_assert_eq("empty? ()", E("(empty? ())"), mal_true());
  mu_assert_eq("empty? [1 2]", E("(empty? [1 2])"), mal_false());
  mu_assert_eq("empty? []", E("(empty? [])"), mal_true());
  mu_assert_eq("empty? nil", E("(empty? nil)"), mal_true());
  mu_assert_eq("empty? 33", E("(empty? 33)"), mal_true());

  // count
  mu_assert_eq("count (1 2)", E("(count '(1 2))"), mal_int(2));
  mu_assert_eq("count ()", E("(count ())"), mal_int(0));
  mu_assert_eq("count [1 2]", E("(count [1 2])"), mal_int(2));
  mu_assert_eq("count []", E("(count [])"), mal_int(0));
  mu_assert_eq("count nil", E("(count nil)"), mal_int(0));
  mu_assert_eq("count 44", E("(count 44)"), mal_int(0));

  return 0;
}
