#include "core_mal_test.h"
#include "core_mal.h"
#include "minunit.h"

#include "core.h"
#include "eval.h"
#include "reader.h"

const char *core_mal_test() {
  env *e = core_env();

  // Tests for not
  mu_assert_mal(e, "(not true)", mal_false());
  mu_assert_mal(e, "(not false)", mal_true());
  mu_assert_mal(e, "(not nil)", mal_true());
  mu_assert_mal(e, "(not 38)", mal_false());

  return 0;
}
