#include <string.h>
#include "minunit.h"

#include "list.h"
#include "reader_test.h"
#include "reader.h"

const char *reader_test() {

    mu_assert("reader '456'", mal_equals(read_str("456"), make_int(456)));
    mu_assert("reader '  456'", mal_equals(read_str("  456"), make_int(456)));
    mu_assert("reader '  456 '", mal_equals(read_str("  456 "), make_int(456)));
    mu_assert("reader 'qwe'", mal_equals(read_str("qwe"), make_sym("qwe")));
    mu_assert("reader '\"lkj\"'", mal_equals(read_str("\"lkj\""), make_str("lkj")));

    mal expected = make_list(
        list_cons(make_sym("+"),
            list_cons(make_int(2),
                list_cons(make_int(3), NULL))));
    mal actual = read_str("(+ 2 3)");
    mu_assert("reader (+ 2 3) list", actual.tag == LIST);
    mu_assert("reader (+ 2 3) head", mal_equals(actual.n->val, make_sym("+")));
    mu_assert("reader (+ 2 3) 2", mal_equals(actual.n->next->val, make_int(2)));
    mu_assert("reader (+ 2 3) 3", mal_equals(actual.n->next->next->val, make_int(3)));
    mu_assert("reader (+ 2 3) NULL", actual.n->next->next->next == NULL);
    mu_assert("reader (+ 2 3) whole", mal_equals(actual, expected));

    mu_assert("reader empty string", read_str("").tag == MISSING);
    mu_assert("reader spaces", read_str("   ").tag == MISSING);

    return 0;
}
