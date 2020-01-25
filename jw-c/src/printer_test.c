#include <string.h>
#include "minunit.h"

#include "printer_test.h"
#include "printer.h"
#include "seq.h"

const char *printer_test() {

    mu_assert("pr_str 123", strcmp(pr_str(make_int(123)), "123") == 0);
    mu_assert("pr_str \"abc\"", strcmp(pr_str(make_str("abc")), "\"abc\"") == 0);
    mu_assert("pr_str def!", strcmp(pr_str(make_sym("def!")), "def!") == 0);

    mu_assert("pr_str ()", strcmp(pr_str(make_list(NULL)), "()") == 0);
    mal m = make_list(
        list_cons(make_sym("plus"),
            list_cons(make_int(4),
                list_cons(make_int(5), NULL))));
    mu_assert("pr_str (plus 4 5)", strcmp(pr_str(m), "(plus 4 5)") == 0);

    return 0;
}
