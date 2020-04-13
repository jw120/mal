#include "minunit.h"
#include <string.h>

#include "hash_table.h"
#include "reader.h"
#include "reader_test.h"
#include "types_test.h"

#include "seq.h"

// dummy function for mal_fn test
static mal dummy_fn(UNUSED(list_node *n), UNUSED(env *e)) { return mal_int(0); }

const char *types_test() {

  mu_assert("types missing", is_missing(mal_missing()));
  mu_assert("types exception", is_exception(mal_exception(mal_list(NULL))));
  mu_assert("types exception_str", is_exception(mal_exception_str("hi!")));
  mu_assert("types true", is_true(mal_true()));
  mu_assert("types false", is_false(mal_false()));
  mu_assert("types true bool", is_true(mal_bool(true)));
  mu_assert("types false bool", is_false(mal_bool(false)));
  mu_assert("types falsey false", is_falsey(mal_false()));
  mu_assert("types falsey nil", is_falsey(mal_nil()));
  mu_assert("types falsey true", !is_falsey(mal_true()));
  mu_assert("types falsey 33", !is_falsey(mal_int(33)));
  mu_assert("types true bool", is_bool(mal_true()));
  mu_assert("types false bool", is_bool(mal_false()));
  mu_assert("types nil", is_nil(mal_nil()));
  mu_assert("types int", is_int(mal_int(5)));
  mu_assert("types str", is_str(mal_str("def")));
  mu_assert("types sym", is_sym(mal_sym("abc")));
  mu_assert("types kw", is_sym(mal_sym("def!")));
  mu_assert("types list", is_list(mal_list(NULL)));
  mu_assert("types vec", is_vec(mal_vec(list_to_vec(0, NULL))));
  mu_assert("types seq list", is_seq(mal_list(NULL)));
  mu_assert("types seq vec", is_seq(mal_vec(list_to_vec(0, NULL))));
  mu_assert("types map", is_map(mal_map(ht_from_alternating_list(NULL))));
  mu_assert("types fn", is_fn(mal_fn(dummy_fn)));

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
  mu_assert("types !map", !is_map(mal_int(2)));
  mu_assert("types !fn", !is_fn(mal_int(2)));

  mu_assert("match_sym match", match_sym(mal_sym("qq"), "qq"));
  mu_assert("match_sym non-match sym", !match_sym(mal_sym("qq"), "a"));
  mu_assert("match_sym non-match str", !match_sym(mal_str("qq"), "qq"));

  mu_assert("str \"a\"", is_str(mal_str("a")));
  mu_assert("str \")\"", is_str(mal_str(")")));
  mu_assert("str :b", !is_str(mal_kw("a")));
  mu_assert("kw \"a\"", !is_kw(mal_str("a")));
  mu_assert("kw \")\"", !is_kw(mal_str(")")));
  mu_assert("kw :b", is_kw(mal_kw("a")));
  mu_assert("str_or_kw \"a\"", is_str_or_kw(mal_str("a")));
  mu_assert("str_or_kw \")\"", is_str_or_kw(mal_str(")")));
  mu_assert("str_or_kw :b", is_str_or_kw(mal_kw("a")));
  mu_assert("str_or_kw 'a", !is_str_or_kw(mal_sym("a")));
  mu_assert("str_or_kw 2", !is_str_or_kw(mal_int(2)));
  mu_assert("string_like \"a\"", is_string_like(mal_str("a")));
  mu_assert("string_like \")\"", is_string_like(mal_str(")")));
  mu_assert("string_like :b", is_string_like(mal_kw("a")));
  mu_assert("string_like 'a", is_string_like(mal_sym("a")));
  mu_assert("string_like 2", !is_string_like(mal_int(2)));

  return 0;
}
