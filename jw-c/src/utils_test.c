#include <string.h>
#include "minunit.h"

#include "utils_test.h"
#include "utils.h"

#define MAX_CHARS 8

const char *utils_test() {

    char buf[MAX_CHARS + 1] = "abc";

    str_concat(buf, "de", MAX_CHARS);
    mu_assert("str_concat de", strcmp(buf, "abcde") == 0);
    str_concat(buf, NULL, MAX_CHARS);
    mu_assert("str_concat NULL 2", strcmp(buf, "abcde") == 0);
    str_concat(buf, "fg", MAX_CHARS);
    mu_assert("str_concat fg", strcmp(buf, "abcdefg") == 0);
    str_concat(buf, "hi", MAX_CHARS);
    mu_assert("str_concat hi", strcmp(buf, "abcdefgh") == 0);

    str_concat(NULL, "abc", MAX_CHARS);
    mu_assert("str_concat NULL first", strcmp(buf, "abcdefgh") == 0);

    mal n = mal_str("no escapes");

    mal a = mal_str("she said \\\"hi\\\"");
    mal b = mal_str("she said \"hi\"");
    mal c = mal_str("one\\ntwo\\n");
    mal d = mal_str("one\ntwo\n");
    mal e = mal_str("slash\\\\ing");
    mal f = mal_str("slash\\ing");

    mal g = mal_str("bad \\escape");
    mal h = mal_str("end escape\\");

    mu_assert("remove_escapes non-str", is_exception(remove_escapes(mal_int(3))));
    mu_assert("remove_escapes nothing", str_equals(remove_escapes(n), n));
    // printf("a=%s, r(a)=%s, b=%s\n", a.s, remove_escapes(a).s, b.s);
    mu_assert("remove_escapes quote", str_equals(remove_escapes(a), b));
    mu_assert("remove_escapes nl", str_equals(remove_escapes(c), d));
    mu_assert("remove_escapes slash", str_equals(remove_escapes(e), f));
    mu_assert("remove_escapes bad esc", is_exception(remove_escapes(g)));
    mu_assert("remove_escapes end esc", is_exception(remove_escapes(h)));

    mu_assert("remove_escapes non-str", is_exception(add_escapes(mal_int(3))));
    mu_assert("remove_escapes nothing", str_equals(add_escapes(n), n));
    mu_assert("add_escapes quote", str_equals(add_escapes(b), a));
    mu_assert("add_escapes nl", str_equals(add_escapes(d), c));
    mu_assert("add_escapes slash", str_equals(add_escapes(f), e));

    return 0;
}
