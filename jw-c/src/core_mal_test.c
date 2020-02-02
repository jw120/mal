#include "core_mal.h"
#include "core_mal_test.h"
#include "minunit.h"

#include "core.h"
#include "eval.h"
#include "reader.h"

const char *core_mal_test()
{
  env *e = core_env();

  // Tests for not
  mu_assert_eq("core not true", eval(read_str("(not true)"), e), mal_false());
  mu_assert_eq("core not false", eval(read_str("(not false)"), e), mal_true());
  mu_assert_eq("core not false", eval(read_str("(not nil)"), e), mal_true());
  mu_assert_eq("core not 38", eval(read_str("(not 38)"), e), mal_false());

  return 0;
}
