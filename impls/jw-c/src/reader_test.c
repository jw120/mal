#include <string.h>

#include "minunit.h"
#include "reader.h"
#include "reader_test.h"

#include "hash_table.h"
#include "printer.h"
#include "seq.h"

const char *reader_test() {

  mu_assert_eq("reader '456'", read_str("456"), mal_int(456));
  mu_assert_eq("reader '  456'", read_str("  456"), mal_int(456));
  mu_assert_eq("reader '  456 '", read_str("  456 "), mal_int(456));
  mu_assert_eq("reader 'qwe'", read_str("qwe"), mal_sym("qwe"));
  mu_assert_eq("reader '\"lkj\"'", read_str("\"lkj\""), mal_str("lkj"));
  mu_assert_eq("reader a\\\\b", read_str("\"a\\\\b\""), mal_str("a\\b"));

  mal actual = read_str("(+ 2 3)");
  mal expected = mal_list(list_cons(
      mal_sym("+"), list_cons(mal_int(2), list_cons(mal_int(3), NULL))));
  mu_assert("reader (+ 2 3) list", is_list(actual));
  mu_assert_eq("reader (+ 2 3) head", actual.n->val, mal_sym("+"));
  mu_assert_eq("reader (+ 2 3) 2", actual.n->next->val, mal_int(2));
  mu_assert_eq("reader (+ 2 3) 3", actual.n->next->next->val, mal_int(3));
  mu_assert("reader (+ 2 3) NULL", actual.n->next->next->next == NULL);
  mu_assert_eq("reader (+ 2 3) whole", actual, expected);

  actual = read_str("{\"a\" 2 \"qq\" nil}");
  expected = mal_map(ht_from_alternating_list(
      list_cons(mal_str("a"),
                list_cons(mal_int(2), list_cons(mal_str("qq"),
                                                list_cons(mal_nil(), NULL))))));
  mu_assert_eq("reader map", actual, expected);

  mu_assert("reader empty string", is_missing(read_str("")));
  mu_assert("reader spaces", is_missing(read_str("   ")));

  mu_assert("reader \"", is_exception(read_str("\"")));
  mu_assert("reader \"abc", is_exception(read_str("\"abc")));
  mu_assert("reader (", is_exception(read_str("(")));
  mu_assert("reader (+ 2", is_exception(read_str("(+ 2")));
  mu_assert("reader [", is_exception(read_str("[")));
  mu_assert("reader [+ 2", is_exception(read_str("[+ 2")));

  actual = read_str("'3");
  expected = mal_cons(mal_sym("quote"), mal_cons(mal_int(3), mal_list(NULL)));
  mu_assert_eq("reader '3", actual, expected);
  actual = read_str("'(1 2)");
  expected = mal_cons(
      mal_sym("quote"),
      mal_cons(mal_cons(mal_int(1), mal_cons(mal_int(2), mal_list(NULL))),
               mal_list(NULL)));
  mu_assert_eq("reader '(1 2)", actual, expected);

  return 0;
}
