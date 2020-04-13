#include "core_mal_test.h"
#include "core_mal.h"
#include "minunit.h"

#include "core.h"
#include "eval.h"
#include "reader.h"

const char *core_mal_test() {
  env *e = core_env();

  // not
  mu_assert_mal(e, "(not true)", mal_false());
  mu_assert_mal(e, "(not false)", mal_true());
  mu_assert_mal(e, "(not nil)", mal_true());
  mu_assert_mal(e, "(not 38)", mal_false());

  // cond
  mu_assert_mal(e, "(cond true 2 false 3)", mal_int(2));
  mu_assert_mal(e, "(cond false 2 true 3)", mal_int(3));
  mu_assert_mal(e, "(cond false 2 false 3 \"else\" 4)", mal_int(4));

  return 0;
}
