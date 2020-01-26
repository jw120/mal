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

    mu_assert("pr_str missing", strncmp(pr_str(mal_missing(), true), "Internal", 8) == 0);
    mu_assert("pr_str exception", strcmp(pr_str(e, true), "\"bad\"") == 0);
    mu_assert("pr_str true", strcmp(pr_str(mal_true(), true), "true") == 0);
    mu_assert("pr_str false", strcmp(pr_str(mal_false(), true), "false") == 0);
    mu_assert("pr_str nil", strcmp(pr_str(mal_nil(), true), "nil") == 0);
    mu_assert("pr_str int", strcmp(pr_str(mal_int(123), true), "123") == 0);
    mu_assert("pr_str str", strcmp(pr_str(mal_str("abc"), true), "\"abc\"") == 0);
    mu_assert("pr_str str esc true", strcmp(pr_str(mal_str("a\\bc"), true), "\"a\\\\bc\"") == 0);
    mu_assert("pr_str str esc false", strcmp(pr_str(mal_str("a\\bc"), false), "\"a\\bc\"") == 0);
    mu_assert("pr_str sym", strcmp(pr_str(mal_sym("def!"), true), "def!") == 0);
    mu_assert("pr_str kw", strcmp(pr_str(mal_kw("cat"), true), ":cat") == 0);
    mu_assert("pr_str list null", strcmp(pr_str(mal_list(NULL), true), "()") == 0);
    mu_assert("pr_str list", strcmp(pr_str(m, true), "(plus 4 5)") == 0);
    mu_assert("pr_str vec", strcmp(pr_str(v, true), "[plus 4 5]") == 0);

    return 0;
}
