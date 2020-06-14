/**
 *
 * core_is - defines is? functions for the core environment
 *
 **/

#include "core_is.h"

#include "debug.h"
#include "env.h"
#include "seq.h"

// Macro for C implementation for an is? mal function
#define DEFINE_IS_SOMETHING_FN(C_fn_name, mal_fn_name, test_fn)                \
  static mal C_fn_name(list_node *n, UNUSED(env *e)) {                         \
    DEBUG_HIGH_MAL("called with", mal_list(n));                                \
    if (list_count(n) != 1)                                                    \
      return mal_exception_str("Need one argument for " mal_fn_name);          \
    return mal_bool(test_fn(n->val));                                          \
  }

DEFINE_IS_SOMETHING_FN(core_is_list, "list?", is_list)
DEFINE_IS_SOMETHING_FN(core_is_nil, "nil?", is_nil)
DEFINE_IS_SOMETHING_FN(core_is_true, "true?", is_true)
DEFINE_IS_SOMETHING_FN(core_is_false, "false?", is_false)
DEFINE_IS_SOMETHING_FN(core_is_sym, "symbol?", is_sym)
DEFINE_IS_SOMETHING_FN(core_is_kw, "keyword?", is_kw)
DEFINE_IS_SOMETHING_FN(core_is_vec, "vector?", is_vec)
DEFINE_IS_SOMETHING_FN(core_is_seq, "seq?", is_seq)
DEFINE_IS_SOMETHING_FN(core_is_map, "map?", is_map)
DEFINE_IS_SOMETHING_FN(core_is_atom, "atom?", is_atom)
DEFINE_IS_SOMETHING_FN(core_is_string, "string?", is_str)
DEFINE_IS_SOMETHING_FN(core_is_number, "number?", is_int)
DEFINE_IS_SOMETHING_FN(core_is_fn, "fn?", is_non_macro_callable)
DEFINE_IS_SOMETHING_FN(core_is_macro, "macro?", is_macro)

void add_is(env *e) {
  env_set(e, "list?", mal_fn(core_is_list));
  env_set(e, "nil?", mal_fn(core_is_nil));
  env_set(e, "true?", mal_fn(core_is_true));
  env_set(e, "false?", mal_fn(core_is_false));
  env_set(e, "symbol?", mal_fn(core_is_sym));
  env_set(e, "keyword?", mal_fn(core_is_kw));
  env_set(e, "vector?", mal_fn(core_is_vec));
  env_set(e, "sequential?", mal_fn(core_is_seq));
  env_set(e, "map?", mal_fn(core_is_map));
  env_set(e, "atom?", mal_fn(core_is_atom));
  env_set(e, "number?", mal_fn(core_is_number));
  env_set(e, "string?", mal_fn(core_is_string));
  env_set(e, "fn?", mal_fn(core_is_fn));
  env_set(e, "macro?", mal_fn(core_is_macro));
}
