#include "core_is_test.h"
#include "core_is.h"
#include "minunit.h"

#include "core.h"
#include "eval.h"
#include "reader.h"

const char *core_is_test() {
  env *e = core_env();

  mu_assert_eq("nil? nil", eval(read_str("(nil? nil)"), e), mal_true());
  mu_assert_eq("nil? 0", eval(read_str("(nil? 0)"), e), mal_false());

  mu_assert_eq("true? true", eval(read_str("(true? true)"), e), mal_true());
  mu_assert_eq("true? false", eval(read_str("(true? false)"), e), mal_false());
  mu_assert_eq("true? 1", eval(read_str("(true? 1)"), e), mal_false());
  mu_assert_eq("true? nil", eval(read_str("(true? nil)"), e), mal_false());

  mu_assert_eq("false? true", eval(read_str("(false? true)"), e), mal_false());
  mu_assert_eq("false? false", eval(read_str("(false? false)"), e), mal_true());
  mu_assert_eq("false? 1", eval(read_str("(false? 1)"), e), mal_false());
  mu_assert_eq("false? nil", eval(read_str("(false? nil)"), e), mal_false());

  mu_assert_eq("symbol? 'a", eval(read_str("(symbol? 'a)"), e), mal_true());
  mu_assert_eq("symbol? \"a\"", eval(read_str("(symbol? \"a\")"), e),
               mal_false());
  mu_assert_eq("symbol? 1", eval(read_str("(symbol? 1)"), e), mal_false());

  mu_assert_eq("keyword? :a", eval(read_str("(keyword? :a)"), e), mal_true());
  mu_assert_eq("keyword? 'a", eval(read_str("(keyword? 'a)"), e), mal_false());

  mu_assert_eq("vector? [0 1]", eval(read_str("(vector? [0 1])"), e),
               mal_true());
  mu_assert_eq("vector? '(0 1)", eval(read_str("(vector? '(0 1))"), e),
               mal_false());
  mu_assert_eq("vector? 12345", eval(read_str("(vector? 12345)"), e),
               mal_false());

  mu_assert_eq("sequential? [0 1]", eval(read_str("(sequential? [0 1])"), e),
               mal_true());
  mu_assert_eq("sequential? '(0 1)", eval(read_str("(sequential? '(0 1))"), e),
               mal_true());
  mu_assert_eq("sequential? 12345", eval(read_str("(sequential? 12345)"), e),
               mal_false());

  mu_assert_eq("map? {:a 1}", eval(read_str("(map? {:a 1})"), e), mal_true());
  mu_assert_eq("map? 12345", eval(read_str("(map? 12345)"), e), mal_false());

  return 0;
}
