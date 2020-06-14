#include "core_num_test.h"
#include "core_num.h"
#include "minunit.h"

#include "core.h"
#include "eval.h"
#include "reader.h"

const char *core_num_test() {
  env *e = core_env();

  // Basic functionality works for arithmetic
  mu_assert_mal(e, "(+ 3 2)", mal_int(5));
  mu_assert_mal(e, "(- 4 3)", mal_int(1));
  mu_assert_mal(e, "(* 4 5)", mal_int(20));
  mu_assert_mal(e, "(/ 9 2)", mal_int(4));
  mu_assert_exception(e, "(/ 5 0)");

  // Basic functionality works for logical comparators
  mu_assert_mal(e, "(<  1 2)", mal_true());
  mu_assert_mal(e, "(<= 1 2)", mal_true());
  mu_assert_mal(e, "(>  1 2)", mal_false());
  mu_assert_mal(e, "(>= 1 2)", mal_false());
  mu_assert_mal(e, "(<  2 2)", mal_false());
  mu_assert_mal(e, "(<= 2 2)", mal_true());
  mu_assert_mal(e, "(>  2 2)", mal_false());
  mu_assert_mal(e, "(>= 2 2)", mal_true());
  mu_assert_mal(e, "(<  3 2)", mal_false());
  mu_assert_mal(e, "(<= 3 2)", mal_false());
  mu_assert_mal(e, "(>  3 2)", mal_true());
  mu_assert_mal(e, "(>= 3 2)", mal_true());

  // Exception if no arguments
  mu_assert_exception(e, "(- )");
  mu_assert_exception(e, "(+ )");
  mu_assert_exception(e, "(* )");
  mu_assert_exception(e, "(/ )");
  mu_assert_exception(e, "(< )");
  mu_assert_exception(e, "(<=)");
  mu_assert_exception(e, "(> )");
  mu_assert_exception(e, "(>=)");

  // Exception if one argument
  mu_assert_exception(e, "(- 4)");
  mu_assert_exception(e, "(+ 4)");
  mu_assert_exception(e, "(* 4)");
  mu_assert_exception(e, "(/ 4)");
  mu_assert_exception(e, "(< 4)");
  mu_assert_exception(e, "(<= 4)");
  mu_assert_exception(e, "(> 4)");
  mu_assert_exception(e, "(>= 4)");

  // Exception if three arguments
  mu_assert_exception(e, "(- 4 5 6)");
  mu_assert_exception(e, "(+ 4 5 6)");
  mu_assert_exception(e, "(* 4 5 6)");
  mu_assert_exception(e, "(/ 4 5 6)");
  mu_assert_exception(e, "(< 4 5 6)");
  mu_assert_exception(e, "(<= 4 5 6)");
  mu_assert_exception(e, "(> 4 5 6)");
  mu_assert_exception(e, "(>= 4 5 6)");

  // Exception if a non-integer first arg
  mu_assert_exception(e, "(- \"a\" 4)");
  mu_assert_exception(e, "(+ \"a\" 4)");
  mu_assert_exception(e, "(* \"a\" 4)");
  mu_assert_exception(e, "(/ \"a\" 4)");
  mu_assert_exception(e, "(< \"a\" 4)");
  mu_assert_exception(e, "(<= \"a\" 4)");
  mu_assert_exception(e, "(> \"a\" 4)");
  mu_assert_exception(e, "(>= \"a\" 4)");

  // Exception if a non-integer second arg
  mu_assert_exception(e, "(- 5 true)");
  mu_assert_exception(e, "(+ 5 true)");
  mu_assert_exception(e, "(* 5 true)");
  mu_assert_exception(e, "(/ 5 true)");
  mu_assert_exception(e, "(< 5 true)");
  mu_assert_exception(e, "(<= 5 true)");
  mu_assert_exception(e, "(> 5 true)");
  mu_assert_exception(e, "(>= 5 true)");

  // Exceptions propogate
  mu_assert_exception(e, "(+ 2 (/ 3 0)");
  mu_assert_exception(e, "(+ (/ 3 0) 2)");

  return 0;
}
