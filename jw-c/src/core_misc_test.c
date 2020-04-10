#include "core_misc_test.h"
#include "core_num.h"
#include "minunit.h"

#include "core.h"
#include "eval.h"
#include "reader.h"

const char *core_misc_test() {
  env *e = core_env();

  // no tests for print functions - as they print to the screen, rely on mal
  // tests

  // Tests for ==
  mu_assert_mal(e, "(= 2 2)", mal_true());
  mu_assert_mal(e, "(= 2 3)", mal_false());

  // Exceptions propogate in =
  mu_assert_exception(e, "(= 2 (/ 3 0))");
  mu_assert_exception(e, "(= (/ 3 0) 2)");

  // read-string
  const char *s = "(read-string \"22\")";
  mu_assert_mal(e, s, mal_int(22));

  // throw
  mu_assert_mal(e, "(throw 22)", mal_exception(mal_int(22)));

  // symbol
  mu_assert_mal(e, "(symbol \"abc\")", mal_sym("abc"));

  // keyword
  mu_assert_mal(e, "(keyword \"abc\")", mal_kw("abc"));
  mu_assert_mal(e, "(keyword :ab)", mal_kw("ab"));
  mu_assert_exception(e, "(keyword 2)");

  return 0;
}
