#include "core_atom_test.h"
#include "core_num.h"
#include "core_num_test.h"
#include "minunit.h"

#include "core.h"
#include "eval.h"
#include "reader.h"

const char *core_atom_test() {
  env *e = core_env();

  mu_assert_mal(e, "(atom 33)", mal_atom(mal_int(33)));
  mu_assert_mal(e, "(deref (atom 33))", mal_int(33));
  mu_assert_mal(e, "(atom? (atom 33))", mal_true());
  mu_assert_mal(e, "(atom? 33)", mal_false());

  // reset
  eval(read_str("(def! a (atom 77))"), e);
  mu_assert_mal(e, "a", mal_atom(mal_int(77)));
  mu_assert_mal(e, "(reset! a 66)", mal_int(66));
  mu_assert_mal(e, "a", mal_atom(mal_int(66)));

  // swap
  eval(read_str("(def! x (atom 1))"), e);
  eval(read_str("(swap! x + 3)"), e);
  mu_assert_mal(e, "@x", mal_int(4));

  return 0;
}
