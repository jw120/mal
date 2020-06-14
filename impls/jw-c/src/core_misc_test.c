#include "core_misc_test.h"
#include "minunit.h"

#include "core.h"

const char *core_misc_test() {
  env *e = core_env();

  // Tests for =
  mu_assert_mal(e, "(= 2 2)", mal_true());
  mu_assert_mal(e, "(= 2 3)", mal_false());

  // Exceptions propogate in =
  mu_assert_exception(e, "(= 2 (/ 3 0))");
  mu_assert_exception(e, "(= (/ 3 0) 2)");

  // read-string
  mu_assert_mal(e, "(read-string \"22\")", mal_int(22));

  // throw
  mu_assert_mal(e, "(throw 22)", mal_exception(mal_int(22)));

  // symbol
  mu_assert_mal(e, "(symbol \"abc\")", mal_sym("abc"));

  // keyword
  mu_assert_mal(e, "(keyword \"abc\")", mal_kw("abc"));
  mu_assert_mal(e, "(keyword :ab)", mal_kw("ab"));
  mu_assert_exception(e, "(keyword 2)");

  // apply
  mu_assert_mal(e, "(apply + (list 2 3))", mal_int(5));
  mu_assert_mal(e, "(apply + 4 (list 5))", mal_int(9));
  mu_assert_mal(e, "(apply (fn* (a b) (* a b)) (list 2 3))", mal_int(6));
  mu_assert_mal(e, "(apply (fn* (a b) (/ a b)) 10 (list 5))", mal_int(2));

  // map
  mu_assert_mal2(e, "(map list? '(1 (2 3) 4))", "(list false true false)");
  mu_assert_mal2(e, "(map list? [1 '(2 3) '()])", "(list false true true)");
  mu_assert_mal2(e, "(map (fn* (a) (+ a 3)) '(1 2 3))", "(list 4 5 6)");
  mu_assert_mal2(e, "(map (fn* (a) (+ a 3)) [0 2 3])", "(list 3 5 6)");
  mu_assert_mal2(e, "(map (fn* (& args) (list? args)) [1 2])",
                 "(list true true)");

  // with-meta and meta
  mu_assert_mal2(e, "(meta (with-meta + 3))", "3");
  mu_assert_mal2(e, "(meta (with-meta (fn* [x] (* x x)) 4))", "4");
  mu_assert_mal2(e, "(meta (with-meta [2 3] 5))", "5");
  mu_assert_mal2(e, "(meta (with-meta {:x 2} 8))", "8");
  mu_assert_mal2(e, "(meta (with-meta '(1 2 3) 9))", "9");
  mu_assert_mal2(e, "(meta (with-meta (atom 7) 10))", "10");
  mu_assert_mal2(e, "(meta (with-meta '(1 2 3) {:y 7}))", "{:y 7}");
  mu_assert_mal2(e, "(meta '(1 2 3))", "nil");

  // time-ms
  mu_assert_mal(e, "(number? (time-ms))", mal_true());

  return 0;
}
