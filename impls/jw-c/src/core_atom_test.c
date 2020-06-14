#include "core_atom_test.h"
#include "core.h"
#include "minunit.h"

const char *core_atom_test() {
  env *e = core_env();

  // atom and deref
  mu_assert_mal(e, "(atom 33)", mal_atom(mal_int(33)));
  mu_assert_mal(e, "(deref (atom 33))", mal_int(33));

  // reset
  E("(def! a (atom 77))", e);
  mu_assert_mal(e, "a", mal_atom(mal_int(77)));
  mu_assert_mal(e, "(reset! a 66)", mal_int(66));
  mu_assert_mal(e, "a", mal_atom(mal_int(66)));

  // swap
  E("(def! x (atom 1))", e);
  E("(swap! x + 3)", e);
  mu_assert_mal(e, "@x", mal_int(4));

  // exception propogration
  mu_assert_mal(e, "(atom (throw 33))", mal_exception(mal_int(33)));
  mu_assert_mal(e, "(deref (throw 33))", mal_exception(mal_int(33)));
  mu_assert_mal(e, "(reset! (throw 33) 66)", mal_exception(mal_int(33)));
  mu_assert_mal(e, "(reset! a (throw 33))", mal_exception(mal_int(33)));
  mu_assert_mal(e, "(swap! x + (throw 22))", mal_exception(mal_int(22)));
  mu_assert_mal(e, "(swap! (throw 21) + 2)", mal_exception(mal_int(21)));
  mu_assert_mal(e, "(swap! x (throw 20) 2)", mal_exception(mal_int(20)));

  return 0;
}
