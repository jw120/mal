#include <string.h>

#include "minunit.h"
#include "printer.h"
#include "printer_test.h"

#include "map.h"
#include "seq.h"

const char *printer_test() {

  mal e = mal_exception(mal_str("bad"));
  mal m = mal_list(list_cons(
      mal_sym("plus"), list_cons(mal_int(4), list_cons(mal_int(5), NULL))));
  mal v = mal_vec(list_to_vec(3, m.n));
  list_node *hm_list =
      list_cons(mal_str("a"),
                list_cons(mal_int(1), list_cons(mal_str("last"),
                                                list_cons(mal_true(), NULL))));
  mal hm_map = mal_map(list_to_map(hm_list));

  mu_assert("pr_str missing",
            strncmp(pr_str(mal_missing(), true), "Internal", 8) == 0);
  mu_assert_str("pr_str exception", pr_str(e, true), "\"bad\"");
  mu_assert_str("pr_str true", pr_str(mal_true(), true), "true");
  mu_assert_str("pr_str false", pr_str(mal_false(), true), "false");
  mu_assert_str("pr_str nil", pr_str(mal_nil(), true), "nil");
  mu_assert_str("pr_str int", pr_str(mal_int(123), true), "123");
  mu_assert_str("pr_str str", pr_str(mal_str("abc"), true), "\"abc\"");
  mu_assert_str("pr_str str esc true", pr_str(mal_str("a\\bc"), true),
                "\"a\\\\bc\"");
  mu_assert_str("pr_str str esc false", pr_str(mal_str("a\\bc"), false),
                "\"a\\bc\"");
  mu_assert_str("pr_str sym", pr_str(mal_sym("def!"), true), "def!");
  mu_assert_str("pr_str kw", pr_str(mal_kw("cat"), true), ":cat");
  mu_assert_str("pr_str list null", pr_str(mal_list(NULL), true), "()");
  mu_assert_str("pr_str list", pr_str(m, true), "(plus 4 5)");
  mu_assert_str("pr_str vec", pr_str(v, true), "[plus 4 5]");
  mu_assert_str("pr_str map", pr_str(hm_map, true), "{\"a\" 1 \"last\" true}");

  return 0;
}
