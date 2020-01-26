#include <string.h>
#include "minunit.h"

#include "reader_test.h"
#include "reader.h"
#include "seq.h"

const char *types_test() {

    mu_assert("types missing", is_missing(mal_missing()));
    mu_assert("types exception", is_exception(mal_exception(mal_list(NULL))));
    mu_assert("types true", is_true(mal_true()));
    mu_assert("types false", is_false(mal_false()));
    mu_assert("types true bool", is_bool(mal_true()));
    mu_assert("types false bool", is_bool(mal_false()));
    mu_assert("types nil", is_nil(mal_nil()));
    mu_assert("types int", is_int(mal_int(5)));
    mu_assert("types str", is_str(mal_str("def")));
    mu_assert("types sym", is_sym(mal_sym("abc")));
    mu_assert("types kw", is_sym(mal_sym("def!")));
    mu_assert("types list", is_list(mal_list(NULL)));
    mu_assert("types vec", is_vec(mal_vec(NULL)));
    mu_assert("types seq list", is_seq(mal_list(NULL)));
    mu_assert("types seq vec", is_seq(mal_vec(NULL)));

    mu_assert("types !missing", !is_missing(mal_int(2)));
    mu_assert("types !exception", !is_exception(mal_int(2)));
    mu_assert("types !true", !is_true(mal_int(2)));
    mu_assert("types !false", !is_false(mal_int(2)));
    mu_assert("types !bool", !is_bool(mal_int(2)));
    mu_assert("types !nil", !is_nil(mal_int(2)));
    mu_assert("types !int", !is_int(mal_str("Q")));
    mu_assert("types !str", !is_str(mal_int(22)));
    mu_assert("types !sym", !is_sym(mal_int(35)));
    mu_assert("types !kw", !is_kw(mal_int(35)));
    mu_assert("types !list", !is_list(mal_int(2)));
    mu_assert("types !vec", !is_vec(mal_int(2)));
    mu_assert("types !seq", !is_seq(mal_int(2)));

    mu_assert("match_sym match", match_sym(mal_sym("qq"), "qq"));
    mu_assert("match_sym non-match sym", !match_sym(mal_sym("qq"), "a"));
    mu_assert("match_sym non-match str", !match_sym(mal_str("qq"), "qq"));

    return 0;
}
