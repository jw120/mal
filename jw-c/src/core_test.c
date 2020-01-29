#include "core.h"
#include "core_test.h"
#include "minunit.h"

#include "eval.h"
#include "reader.h"

const char *core_test()
{
  env *e = core_env();

  mu_assert_eq("core (+ 3 2)", eval(read_str("(+ 3 2)"), e), mal_int(5));
  mu_assert_eq("core (- 4 3)", eval(read_str("(- 4 3)"), e), mal_int(1));
  mu_assert_eq("core (* 4 5)", eval(read_str("(* 4 5)"), e), mal_int(20));
  mu_assert_eq("core (/ 9 2)", eval(read_str("(/ 9 2)"), e), mal_int(4));
  mu_assert("core / 0", is_exception(eval(read_str("(/ 5 0)"), e)));

  mu_assert("core - no arg", is_exception(eval(read_str("(-)"), e)));
  mu_assert("core - one arg", is_exception(eval(read_str("(- 5)"), e)));
  mu_assert("core - three args", is_exception(eval(read_str("(- 5 4 3)"), e)));
  mu_assert("core - str arg", is_exception(eval(read_str("(- 3 \"x\")"), e)));

  return 0;
}
