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

const char *core_seq_test(void) {
  env *e = core_env();

  // list
  list_node *n12 = list_cons(mal_int(1), list_cons(mal_int(2), NULL));
  mu_assert_mal(e, "(list 1 2)", mal_list(n12));
  mu_assert_mal(e, "(list)", mal_list(NULL));

  // list?
  mu_assert_mal(e, "(list? '(1 2))", mal_true());
  mu_assert_mal(e, "(list? ())", mal_true());
  mu_assert_mal(e, "(list? nil)", mal_false());
  mu_assert_mal(e, "(list? [1 2])", mal_false());

  // empty?
  mu_assert_mal(e, "(empty? '(1 2))", mal_false());
  mu_assert_mal(e, "(empty? ())", mal_true());
  mu_assert_mal(e, "(empty? [1 2])", mal_false());
  mu_assert_mal(e, "(empty? [])", mal_true());
  mu_assert_mal(e, "(empty? nil)", mal_true());
  mu_assert_mal(e, "(empty? 33)", mal_true());

  // count
  mu_assert_mal(e, "(count '(1 2))", mal_int(2));
  mu_assert_mal(e, "(count ())", mal_int(0));
  mu_assert_mal(e, "(count [1 2])", mal_int(2));
  mu_assert_mal(e, "(count [])", mal_int(0));
  mu_assert_mal(e, "(count nil)", mal_int(0));
  mu_assert_mal(e, "(count 44)", mal_int(0));

  // cons
  mu_assert_mal(
      e, "(cons 1 '(2 3))",
      mal_cons(mal_int(1),
               mal_cons(mal_int(2), mal_cons(mal_int(3), mal_list(NULL)))));
  mu_assert_mal(e, "(cons 1 ())", mal_cons(mal_int(1), mal_list(NULL)));
  mu_assert_mal(
      e, "(cons 1 [2 3])",
      mal_cons(mal_int(1),
               mal_cons(mal_int(2), mal_cons(mal_int(3), mal_list(NULL)))));
  mu_assert_mal(e, "(cons 1 [])", mal_cons(mal_int(1), mal_list(NULL)));

  // concat
  eval(read_str(
           "(do (def! a ()) (def! b '(1)) (def! c '(2 3)) (def! d '(5 6 7)))"),
       e);
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
  mu_assert_mal(e, "(concat)", mal_list(NULL));
  mu_assert_mal(e, "(concat a)", a);
  mu_assert_mal(e, "(concat a b)", ab);
  mu_assert_mal(e, "(concat b c)", bc);
  mu_assert_mal(e, "(concat b c)", bc);
  mu_assert_mal(e, "(concat b c d)", bcd);
  mu_assert_mal(e, "(concat b c d a)", bcd);
  mu_assert_mal(e, "(concat [1] [2 3] [5 6 7] [])", bcd);

  // first
  mu_assert_mal(e, "(first ())", mal_nil());
  mu_assert_mal(e, "(first nil)", mal_nil());
  mu_assert_mal(e, "(first '(1))", mal_int(1));
  mu_assert_mal(e, "(first '(2 3 4))", mal_int(2));
  mu_assert_mal(e, "(first [])", mal_nil());
  mu_assert_mal(e, "(first [1])", mal_int(1));
  mu_assert_mal(e, "(first [2 3 4])", mal_int(2));

  // rest
  mu_assert_mal(e, "(rest ())", mal_list(NULL));
  mu_assert_mal(e, "(rest nil)", mal_list(NULL));
  mu_assert_mal(e, "(rest '(1))", mal_list(NULL));
  mu_assert_mal(e, "(rest '(2 3 4))",
                mal_cons(mal_int(3), mal_cons(mal_int(4), mal_list(NULL))));
  mu_assert_mal(e, "(rest [])", mal_list(NULL));
  mu_assert_mal(e, "(rest [1])", mal_list(NULL));
  mu_assert_mal(e, "(rest [2 3 4])",
                mal_cons(mal_int(3), mal_cons(mal_int(4), mal_list(NULL))));

  // nth
  mu_assert_exception(e, "(nth () 0)");
  mu_assert_exception(e, "(nth () 2)");
  mu_assert_mal(e, "(nth '(4 5 6) 0)", mal_int(4));
  mu_assert_mal(e, "(nth '(4 5 6) 2)", mal_int(6));
  mu_assert_exception(e, "(nth '(4 5 6) 7)");
  mu_assert_mal(e, "(nth [3 4 5 6] 0)", mal_int(3));
  mu_assert_mal(e, "(nth [3 4 5 6] 2)", mal_int(5));
  mu_assert_exception(e, "(nth [3 4 5 6] 7)");

  // vector
  mu_assert_mal(e, "(vector)", mal_vec(list_to_vec(0, NULL)));
  mu_assert_mal(e, "(vector 2 3)",
                mal_vec(list_to_vec(
                    2, list_cons(mal_int(2), list_cons(mal_int(3), NULL)))));

  // hash-map functions
  mu_assert_mal2(e, "(hash-map :a 2 :b 3)", "{:a 2 :b 3}");
  mu_assert_mal(e, "(get {:a 1 :b 2} :b)", mal_int(2));
  mu_assert_mal(e, "(get {:a 1 :b 2} \"d\")", mal_nil());
  mu_assert_mal(e, "(get {:a 1 :b 2} :c)", mal_nil());
  mu_assert_mal(e, "(get nil :c)", mal_nil());
  mu_assert_mal(e, "(contains? {:a 1 :b 2} :a)", mal_true());
  mu_assert_mal(e, "(contains? {:a 1 :b 2} :c)", mal_false());
  mu_assert_mal2(e, "(keys {:a 2})", "'(:a)");
  mu_assert_mal2(e, "(vals {:a 2 :b 2})", "'(2 2)");
  mu_assert_mal2(e, "(assoc {:a 2} :b 3 :c 4)", "{:a 2 :b 3 :c 4}");
  mu_assert_mal2(e, "(dissoc {:a 2 :b 3 :c 4} :c :b :d)", "{:a 2}");

  return 0;
}
