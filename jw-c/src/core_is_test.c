#include "core_is_test.h"
#include "core.h"
#include "minunit.h"

const char *core_is_test(void) {
  env *e = core_env();

  mu_assert_mal(e, "(nil? nil)", mal_true());
  mu_assert_mal(e, "(nil? 0)", mal_false());

  mu_assert_mal(e, "(true? true)", mal_true());
  mu_assert_mal(e, "(true? false)", mal_false());
  mu_assert_mal(e, "(true? 1)", mal_false());
  mu_assert_mal(e, "(true? nil)", mal_false());

  mu_assert_mal(e, "(false? true)", mal_false());
  mu_assert_mal(e, "(false? false)", mal_true());
  mu_assert_mal(e, "(false? 1)", mal_false());
  mu_assert_mal(e, "(false? nil)", mal_false());

  mu_assert_mal(e, "(symbol? 'a)", mal_true());
  mu_assert_mal(e, "(symbol? \"a\")", mal_false());
  mu_assert_mal(e, "(symbol? 1)", mal_false());

  mu_assert_mal(e, "(keyword? :a)", mal_true());
  mu_assert_mal(e, "(keyword? 'a)", mal_false());

  mu_assert_mal(e, "(vector? [0 1])", mal_true());
  mu_assert_mal(e, "(vector? '(0 1))", mal_false());
  mu_assert_mal(e, "(vector? 12345)", mal_false());

  mu_assert_mal(e, "(sequential? [0 1])", mal_true());
  mu_assert_mal(e, "(sequential? '(0 1))", mal_true());
  mu_assert_mal(e, "(sequential? 12345)", mal_false());

  mu_assert_mal(e, "(map? {:a 1})", mal_true());
  mu_assert_mal(e, "(map? 12345)", mal_false());

  mu_assert_mal(e, "(atom? (atom 33))", mal_true());
  mu_assert_mal(e, "(atom? 33)", mal_false());

  return 0;
}
