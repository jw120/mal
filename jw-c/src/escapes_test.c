#include <stdio.h>
#include <string.h>

#include "escapes.h"
#include "escapes_test.h"
#include "minunit.h"

// Helper functions are both mal arguments strings that are equal
static bool str_equals(mal a, mal b) {
  return is_str(a) && is_str(b) && (strcmp(a.s, b.s) == 0);
}

const char *escapes_test() {

  const char *n = "no escapes";

  const char *a = "she said \\\"hi\\\"";
  const char *b = "she said \"hi\"";
  const char *c = "one\\ntwo\\n";
  const char *d = "one\ntwo\n";
  const char *e = "slash\\\\ing";
  const char *f = "slash\\ing";

  const char *g = "bad \\escape";
  const char *h = "end escape\\";

  mu_assert("remove_escapes nothing",
            str_equals(remove_escapes(n), mal_str(n)));
  mu_assert("remove_escapes quote", str_equals(remove_escapes(a), mal_str(b)));
  mu_assert("remove_escapes nl", str_equals(remove_escapes(c), mal_str(d)));
  mu_assert("remove_escapes slash", str_equals(remove_escapes(e), mal_str(f)));
  mu_assert("remove_escapes bad esc", is_exception(remove_escapes(g)));
  mu_assert("remove_escapes end esc", is_exception(remove_escapes(h)));

  mu_assert("add_escapes nothing", strcmp(add_escapes(n), n) == 0);
  mu_assert("add_escapes quote", strcmp(add_escapes(b), a) == 0);
  mu_assert("add_escapes nl", strcmp(add_escapes(d), c) == 0);
  mu_assert("add_escapes slash", strcmp(add_escapes(f), e) == 0);

  return 0;
}
