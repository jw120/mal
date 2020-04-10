#include "core_atom_test.h"
#include "core_num.h"
#include "core_num_test.h"
#include "minunit.h"

#include "core.h"
#include "eval.h"
#include "reader.h"

const char *core_atom_test() {
  env *e = core_env();

  mu_assert_eq("core atom", eval(read_str("(atom 33)"), e),
               mal_atom(mal_int(33)));
  mu_assert_eq("core deref", eval(read_str("(deref (atom 33))"), e),
               mal_int(33));
  mu_assert_eq("core atom? true", eval(read_str("(atom? (atom 33))"), e),
               mal_true());
  mu_assert_eq("core atom? false", eval(read_str("(atom? 33)"), e),
               mal_false());

  // reset
  eval(read_str("(def! a (atom 77))"), e);
  mu_assert_eq("core reset! pre", eval(read_str("a"), e),
               mal_atom(mal_int(77)));
  mu_assert_eq("core reset!", eval(read_str("(reset! a 66)"), e), mal_int(66));
  mu_assert_eq("core reset! post", eval(read_str("a"), e),
               mal_atom(mal_int(66)));

  // swap
  eval(read_str("(def! x (atom 1))"), e);
  eval(read_str("(swap! x + 3)"), e);
  mu_assert_eq("core swap x", eval(read_str("@x"), e), mal_int(4));

  return 0;
}
