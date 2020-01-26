#include <string.h>
#include "minunit.h"

#include "reader_test.h"
#include "reader.h"
#include "seq.h"

const char *reader_test() {

    mu_assert("reader '456'", mal_equals(read_str("456"), mal_int(456)));
    mu_assert("reader '  456'", mal_equals(read_str("  456"), mal_int(456)));
    mu_assert("reader '  456 '", mal_equals(read_str("  456 "), mal_int(456)));
    mu_assert("reader 'qwe'", mal_equals(read_str("qwe"), mal_sym("qwe")));
    mu_assert("reader '\"lkj\"'", mal_equals(read_str("\"lkj\""), mal_str("lkj")));

    mal expected = mal_list(
        list_cons(mal_sym("+"),
            list_cons(mal_int(2),
                list_cons(mal_int(3), NULL))));
    mal actual = read_str("(+ 2 3)");
    mu_assert("reader (+ 2 3) list", is_list(actual));
    mu_assert("reader (+ 2 3) head", mal_equals(actual.n->val, mal_sym("+")));
    mu_assert("reader (+ 2 3) 2", mal_equals(actual.n->next->val, mal_int(2)));
    mu_assert("reader (+ 2 3) 3", mal_equals(actual.n->next->next->val, mal_int(3)));
    mu_assert("reader (+ 2 3) NULL", actual.n->next->next->next == NULL);
    mu_assert("reader (+ 2 3) whole", mal_equals(actual, expected));

    mu_assert("reader empty string", is_missing(read_str("")));
    mu_assert("reader spaces", is_missing(read_str("   ")));

    return 0;
}
