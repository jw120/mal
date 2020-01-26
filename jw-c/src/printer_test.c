#include <string.h>
#include "minunit.h"

#include "printer_test.h"
#include "printer.h"
#include "seq.h"

const char *printer_test() {

    mal e = mal_exception(mal_str("bad"));
    mal m = mal_list(
        list_cons(mal_sym("plus"),
            list_cons(mal_int(4),
                list_cons(mal_int(5), NULL))));
    mal v = mal_vec(create_vec(3, m.n));

    mu_assert("pr_str missing", strncmp(pr_str(mal_missing()), "Internal", 8) == 0);
    mu_assert("pr_str exception", strcmp(pr_str(e), "\"bad\"") == 0);
    mu_assert("pr_str true", strcmp(pr_str(mal_true()), "true") == 0);
    mu_assert("pr_str false", strcmp(pr_str(mal_false()), "false") == 0);
    mu_assert("pr_str nil", strcmp(pr_str(mal_nil()), "nil") == 0);
    mu_assert("pr_str int", strcmp(pr_str(mal_int(123)), "123") == 0);
    mu_assert("pr_str str", strcmp(pr_str(mal_str("abc")), "\"abc\"") == 0);
    mu_assert("pr_str sym", strcmp(pr_str(mal_sym("def!")), "def!") == 0);
    mu_assert("pr_str kw", strcmp(pr_str(mal_kw("cat")), ":cat") == 0);
    mu_assert("pr_str list null", strcmp(pr_str(mal_list(NULL)), "()") == 0);
    mu_assert("pr_str list", strcmp(pr_str(m), "(plus 4 5)") == 0);
    mu_assert("pr_str vec", strcmp(pr_str(v), "[plus 4 5]") == 0);

    return 0;
}
