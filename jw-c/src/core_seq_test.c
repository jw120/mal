#include "core_seq_test.h"
#include "core_misc_test.h"
#include "core_num.h"
#include "minunit.h"

#include "core.h"
#include "eval.h"
#include "reader.h"
#include "seq.h"

#include "printer.h"
#include <stdio.h>

#define E(s) eval(read_str(s), e)

const char *core_seq_test(void) {
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

  // cons
  mu_assert_eq(
      "cons 1 (2 3)", E("(cons 1 '(2 3))"),
      mal_cons(mal_int(1),
               mal_cons(mal_int(2), mal_cons(mal_int(3), mal_list(NULL)))));
  mu_assert_eq("cons 1 ()", E("(cons 1 ())"),
               mal_cons(mal_int(1), mal_list(NULL)));
  mu_assert_eq(
      "cons 1 [2 3]", E("(cons 1 [2 3])"),
      mal_cons(mal_int(1),
               mal_cons(mal_int(2), mal_cons(mal_int(3), mal_list(NULL)))));
  mu_assert_eq("cons 1 []", E("(cons 1 [])"),
               mal_cons(mal_int(1), mal_list(NULL)));

  // concat
  E("(def! a ())");
  E("(def! b '(1))");
  E("(def! c '(2 3))");
  E("(def! d '(5 6 7))");
  mal a = mal_list(NULL);
  mal ab = mal_cons(mal_int(1), mal_list(NULL));
  mal bc = mal_cons(mal_int(1),
                    mal_cons(mal_int(2), mal_cons(mal_int(3), mal_list(NULL))));
  mal bcd = mal_cons(
      mal_int(1),
      mal_cons(
          mal_int(2),
          mal_cons(mal_int(3),
                   mal_cons(mal_int(5),
                            mal_cons(mal_int(6),
                                     mal_cons(mal_int(7), mal_list(NULL)))))));
  mu_assert_eq("concat ", E("(concat)"), mal_list(NULL));
  mu_assert_eq("concat a", E("(concat a)"), a);
  mu_assert_eq("concat ab", E("(concat a b)"), ab);
  mu_assert_eq("concat abc", E("(concat b c)"), bc);
  mu_assert_eq("concat bc", E("(concat b c)"), bc);
  mu_assert_eq("concat bcd", E("(concat b c d)"), bcd);
  mu_assert_eq("concat bcda", E("(concat b c d a)"), bcd);
  mu_assert_eq("concat vec bcda", E("(concat [1] [2 3] [5 6 7] [])"), bcd);

  // first
  mu_assert_eq("first ()", E("(first ())"), mal_nil());
  mu_assert_eq("first nil", E("(first nil)"), mal_nil());
  mu_assert_eq("first (1)", E("(first '(1))"), mal_int(1));
  mu_assert_eq("first (2 3 4)", E("(first '(2 3 4))"), mal_int(2));
  mu_assert_eq("first []", E("(first [])"), mal_nil());
  mu_assert_eq("first [1]", E("(first [1])"), mal_int(1));
  mu_assert_eq("first [2 3 4]", E("(first [2 3 4])"), mal_int(2));

  // rest
  mu_assert_eq("rest ()", E("(rest ())"), mal_list(NULL));
  mu_assert_eq("rest nil", E("(rest nil)"), mal_list(NULL));
  mu_assert_eq("rest (1)", E("(rest '(1))"), mal_list(NULL));
  mu_assert_eq("rest (2 3 4)", E("(rest '(2 3 4))"),
               mal_cons(mal_int(3), mal_cons(mal_int(4), mal_list(NULL))));
  mu_assert_eq("rest []", E("(rest [])"), mal_list(NULL));
  mu_assert_eq("rest [1]", E("(rest [1])"), mal_list(NULL));
  mu_assert_eq("rest [2 3 4]", E("(rest [2 3 4])"),
               mal_cons(mal_int(3), mal_cons(mal_int(4), mal_list(NULL))));

  // nth
  mu_assert("nth () 0", is_exception(E("(nth () 0)")));
  mu_assert("nth () 2", is_exception(E("(nth () 2)")));
  mu_assert_eq("nth '(4 5 6) 0", E("(nth '(4 5 6) 0)"), mal_int(4));
  mu_assert_eq("nth '(4 5 6) 2 ", E("(nth '(4 5 6) 2)"), mal_int(6));
  mu_assert("nth '(4 5 6) 7", is_exception(E("(nth '(4 5 6) 7)")));
  mu_assert_eq("nth [3 4 5 6] 0", E("(nth [3 4 5 6] 0)"), mal_int(3));
  mu_assert_eq("nth [3 4 5 6] 2 ", E("(nth [3 4 5 6] 2)"), mal_int(5));
  mu_assert("nth [3 4 5 6] 7", is_exception(E("(nth [3 4 5 6] 7)")));

  // vector
  mu_assert_eq("(vector)", E("(vector)"), E("[]"));
  mu_assert_eq("(vector 2 3)", E("(vector 2 3)"), E("[2 3]"));

  return 0;
}
