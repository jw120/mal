#include "core_num.h"
#include "core_num_test.h"
#include "minunit.h"

#include "core.h"
#include "eval.h"
#include "reader.h"

const char *core_num_test()
{
  env *e = core_env();

  // Basic functionality works for arithmetic
  mu_assert_eq("core (+ 3 2)", eval(read_str("(+ 3 2)"), e), mal_int(5));
  mu_assert_eq("core (- 4 3)", eval(read_str("(- 4 3)"), e), mal_int(1));
  mu_assert_eq("core (* 4 5)", eval(read_str("(* 4 5)"), e), mal_int(20));
  mu_assert_eq("core (/ 9 2)", eval(read_str("(/ 9 2)"), e), mal_int(4));
  mu_assert("core / 0", is_exception(eval(read_str("(/ 5 0)"), e)));

  // Basic functionality works for logical comparators
  mu_assert_eq("core (<  1 2)", eval(read_str("(<  1 2)"), e), mal_true());
  mu_assert_eq("core (<= 1 2)", eval(read_str("(<= 1 2)"), e), mal_true());
  mu_assert_eq("core (>  1 2)", eval(read_str("(>  1 2)"), e), mal_false());
  mu_assert_eq("core (>= 1 2)", eval(read_str("(>= 1 2)"), e), mal_false());
  mu_assert_eq("core (<  2 2)", eval(read_str("(<  2 2)"), e), mal_false());
  mu_assert_eq("core (<= 2 2)", eval(read_str("(<= 2 2)"), e), mal_true());
  mu_assert_eq("core (>  2 2)", eval(read_str("(>  2 2)"), e), mal_false());
  mu_assert_eq("core (>= 2 2)", eval(read_str("(>= 2 2)"), e), mal_true());
  mu_assert_eq("core (<  3 2)", eval(read_str("(<  3 2)"), e), mal_false());
  mu_assert_eq("core (<= 3 2)", eval(read_str("(<= 3 2)"), e), mal_false());
  mu_assert_eq("core (>  3 2)", eval(read_str("(>  3 2)"), e), mal_true());
  mu_assert_eq("core (>= 3 2)", eval(read_str("(>= 3 2)"), e), mal_true());

  // Exception if no arguments
  mu_assert("core -  no arg", is_exception(eval(read_str("(- )"), e)));
  mu_assert("core +  no arg", is_exception(eval(read_str("(+ )"), e)));
  mu_assert("core *  no arg", is_exception(eval(read_str("(* )"), e)));
  mu_assert("core /  no arg", is_exception(eval(read_str("(/ )"), e)));
  mu_assert("core <  no arg", is_exception(eval(read_str("(< )"), e)));
  mu_assert("core <= no arg", is_exception(eval(read_str("(<=)"), e)));
  mu_assert("core >  no arg", is_exception(eval(read_str("(> )"), e)));
  mu_assert("core >= no arg", is_exception(eval(read_str("(>=)"), e)));

  // Exception if one argument
  mu_assert("core -  one arg", is_exception(eval(read_str("(- 4)"), e)));
  mu_assert("core +  one arg", is_exception(eval(read_str("(+ 4)"), e)));
  mu_assert("core *  one arg", is_exception(eval(read_str("(* 4)"), e)));
  mu_assert("core /  one arg", is_exception(eval(read_str("(/ 4)"), e)));
  mu_assert("core <  one arg", is_exception(eval(read_str("(< 4)"), e)));
  mu_assert("core <= one arg", is_exception(eval(read_str("(<= 4)"), e)));
  mu_assert("core >  one arg", is_exception(eval(read_str("(> 4)"), e)));
  mu_assert("core >= one arg", is_exception(eval(read_str("(>= 4)"), e)));

  // Exception if three arguments
  mu_assert("core -  3 args", is_exception(eval(read_str("(- 4 5 6)"), e)));
  mu_assert("core +  3 args", is_exception(eval(read_str("(+ 4 5 6)"), e)));
  mu_assert("core *  3 args", is_exception(eval(read_str("(* 4 5 6)"), e)));
  mu_assert("core /  3 args", is_exception(eval(read_str("(/ 4 5 6)"), e)));
  mu_assert("core <  3 args", is_exception(eval(read_str("(< 4 5 6)"), e)));
  mu_assert("core <= 3 args", is_exception(eval(read_str("(<= 4 5 6)"), e)));
  mu_assert("core >  3 args", is_exception(eval(read_str("(> 4 5 6)"), e)));
  mu_assert("core >= 3 args", is_exception(eval(read_str("(>= 4 5 6)"), e)));

  // Exception if a non-integer first arg
  mu_assert("core -  str1 arg", is_exception(eval(read_str("(- \"a\" 4)"), e)));
  mu_assert("core +  str1 arg", is_exception(eval(read_str("(+ \"a\" 4)"), e)));
  mu_assert("core *  str1 arg", is_exception(eval(read_str("(* \"a\" 4)"), e)));
  mu_assert("core /  str1 arg", is_exception(eval(read_str("(/ \"a\" 4)"), e)));
  mu_assert("core <  str1 arg", is_exception(eval(read_str("(< \"a\" 4)"), e)));
  mu_assert("core <= str1 arg", is_exception(eval(read_str("(<= \"a\" 4)"), e)));
  mu_assert("core >  str1 arg", is_exception(eval(read_str("(> \"a\" 4)"), e)));
  mu_assert("core >= str1 arg", is_exception(eval(read_str("(>= \"a\" 4)"), e)));

  // Exception if a non-integer second arg
  mu_assert("core -  bool2 arg", is_exception(eval(read_str("(- 5 true)"), e)));
  mu_assert("core +  bool2 arg", is_exception(eval(read_str("(+ 5 true)"), e)));
  mu_assert("core *  bool2 arg", is_exception(eval(read_str("(* 5 true)"), e)));
  mu_assert("core /  bool2 arg", is_exception(eval(read_str("(/ 5 true)"), e)));
  mu_assert("core <  bool2 arg", is_exception(eval(read_str("(< 5 true)"), e)));
  mu_assert("core <= bool2 arg", is_exception(eval(read_str("(<= 5 true)"), e)));
  mu_assert("core >  bool2 arg", is_exception(eval(read_str("(> 5 true)"), e)));
  mu_assert("core >= bool2 arg", is_exception(eval(read_str("(>= 5 true)"), e)));

  // Exceptions propogate
  mu_assert_eq("core + except2", eval(read_str("(+ 2 (/ 3 0))"), e), eval(read_str("(/ 4 0)"), e));
  mu_assert_eq("core + except1", eval(read_str("(+ (/ 3 0) 2)"), e), eval(read_str("(/ 4 0)"), e));

  return 0;
}
