#include <string.h>
#include "minunit.h"

#include "list.h"
#include "reader_test.h"
#include "reader.h"

const char *types_test() {

    mu_assert("types missing", is_missing(make_missing()));
    mu_assert("types int", is_int(make_int(5)));
    mu_assert("types sym", is_sym(make_sym("abc")));
    mu_assert("types str", is_str(make_str("def")));
    mu_assert("types list", is_list(make_list(NULL)));
    mu_assert("types exception", is_exception(make_exception(make_list(NULL))));

    mu_assert("types !missing", !is_missing(make_int(2)));
    mu_assert("types !int", !is_int(make_str("Q")));
    mu_assert("types !sym", !is_sym(make_int(35)));
    mu_assert("types !str", !is_str(make_int(22)));
    mu_assert("types !list", !is_list(make_int(2)));
    mu_assert("types !exception", !is_exception(make_int(2)));

    mu_assert("match_sym match", match_sym(make_sym("qq"), "qq"));
    mu_assert("match_sym non-match sym", !match_sym(make_sym("qq"), "a"));
    mu_assert("match_sym non-match str", !match_sym(make_str("qq"), "qq"));

    return 0;
}
