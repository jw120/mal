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
  mu_assert_eq("core = true", eval(read_str("(= 2 2)"), e), mal_true());
  mu_assert_eq("core = false", eval(read_str("(= 2 3)"), e), mal_false());

  // Exceptions propogate in =
  mu_assert_eq("core = except2", eval(read_str("(= 2 (/ 3 0))"), e),
               eval(read_str("(/ 4 0)"), e));
  mu_assert_eq("core = except1", eval(read_str("(= (/ 3 0) 2)"), e),
               eval(read_str("(/ 4 0)"), e));

  // read-string
  const char *s = "(read-string \"22\")";
  mu_assert_eq("core read-string 22", eval(read_str(s), e), mal_int(22));

  return 0;
}
