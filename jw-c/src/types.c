/**
 *
 * types.c - basic support for the mal type defined in mal.h
 *
 **/

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "types.h"

#include "hash_table.h"
#include "seq.h"
#include "utils.h"

static const char *kw_prefix =
    "\xCA\x9E"; // UTF8 for LATIN SMALL LETTER TURNED K
const char *skip_kw_prefix(const char *s) { return s + strlen(kw_prefix); }

bool mal_equals(mal a, mal b) {
  switch (a.tag) {
  case MISSING:
    internal_error("mal_equals called on a missing value");
  case EXCEPTION:
    return a.tag == b.tag && mal_equals(*a.e, *b.e);
  case MAL_TRUE:
  case MAL_FALSE:
  case NIL:
    return a.tag == b.tag;
  case INT:
    return a.tag == b.tag && a.i == b.i;
  case SYM:
  case STR_OR_KW:
    return a.tag == b.tag && strcmp(a.s, b.s) == 0;
  case LIST:
    if (b.tag == LIST)
      return list_equals(a.n, b.n);
    if (b.tag == VEC)
      return list_vec_equals(a.n, b.v);
    return false;
  case VEC:
    if (b.tag == LIST)
      return list_vec_equals(b.n, a.v);
    if (b.tag == VEC)
      return vec_equals(a.v, b.v);
    return false;
  case MAP:
    return a.tag == b.tag && ht_equals(a.m, b.m);
  case FN:
  case CLOSURE:
    return false;
  case ATOM:
    return a.tag == b.tag && mal_equals(**a.a, **b.a);
  }
}

/**
 *
 * Convenience functions to test the type
 *
 */

bool is_missing(mal m) { return m.tag == MISSING; }

bool is_exception(mal m) { return m.tag == EXCEPTION; }

bool is_bool(const mal m) { return m.tag == MAL_TRUE || m.tag == MAL_FALSE; }

bool is_true(const mal m) { return m.tag == MAL_TRUE; }

bool is_false(const mal m) { return m.tag == MAL_FALSE; }

bool is_falsey(const mal m) { return m.tag == MAL_FALSE || m.tag == NIL; }

bool is_nil(const mal m) { return m.tag == NIL; }

bool is_int(const mal m) { return m.tag == INT; }

bool is_str(const mal m) {
  return m.tag == STR_OR_KW && strncmp(m.s, kw_prefix, strlen(kw_prefix)) != 0;
}

bool is_sym(const mal m) { return m.tag == SYM; }

bool is_kw(const mal m) {
  return m.tag == STR_OR_KW && strncmp(m.s, kw_prefix, strlen(kw_prefix)) == 0;
}

bool is_str_or_kw(const mal m) { return m.tag == STR_OR_KW; };

bool is_string_like(const mal m) { return m.tag == STR_OR_KW || m.tag == SYM; }

bool is_list(const mal m) { return m.tag == LIST; }

bool is_vec(const mal m) { return m.tag == VEC; }

bool is_seq(const mal m) { return m.tag == LIST || m.tag == VEC; }

bool is_map(const mal m) { return m.tag == MAP; }

bool is_fn(const mal m) { return m.tag == FN; }

bool is_closure(const mal m) { return m.tag == CLOSURE; }

bool is_non_macro_callable(const mal m) {
  return m.tag == FN || (m.tag == CLOSURE && m.c != NULL && !m.c->is_macro);
}

bool is_macro(const mal m) {
  return m.tag == CLOSURE && m.c != NULL && m.c->is_macro;
}

bool is_atom(const mal m) { return m.tag == ATOM; }

bool match_sym(const mal m, const char *s) {
  return m.tag == SYM && strcmp(m.s, s) == 0;
}

/**
 * Convenience constructor functions
 *
 */

mal mal_missing() {
  mal val = {MISSING, {.i = 0}};
  return val;
}

mal mal_exception(mal m) {
  mal *m_ptr = checked_malloc(sizeof(mal), "mal_exception");
  *m_ptr = m;
  mal val = {EXCEPTION, {.e = m_ptr}};
  return val;
}

mal mal_exception_str(const char *s) {
  mal *m_ptr = checked_malloc(sizeof(mal), "mal_exception_str");
  m_ptr->tag = STR_OR_KW;
  m_ptr->s = s;
  mal val = {EXCEPTION, {.e = m_ptr}};
  return val;
}

mal mal_true() {
  mal val = {MAL_TRUE, {.i = 0}};
  return val;
}

mal mal_false() {
  mal val = {MAL_FALSE, {.i = 0}};
  return val;
}

mal mal_bool(bool b) { return b ? mal_true() : mal_false(); }

mal mal_nil() {
  mal val = {NIL, {.i = 0}};
  return val;
}

mal mal_int(int i) {
  mal val = {INT, {.i = i}};
  return val;
}

mal mal_str(const char *s) {
  if (s == NULL)
    return mal_exception_str("Null in mal_str");
  mal val = {STR_OR_KW, {.s = s}};
  return val;
}

mal mal_sym(const char *s) {
  if (s == NULL)
    return mal_exception_str("Null in mal_sym");
  mal val = {SYM, {.s = s}};
  return val;
}

mal mal_kw(const char *s) {
  if (s == NULL)
    return mal_exception_str("Null in mal_kw");
  size_t buf_size = 1 + strlen(kw_prefix) + strlen(s);
  char *buf = checked_malloc(buf_size, "mal_kw");
  strncpy(buf, kw_prefix, buf_size);
  strncat(buf, s, buf_size - strlen(buf));
  mal val = {STR_OR_KW, {.s = buf}};
  return val;
}

mal mal_list(list_node *n) {
  mal val = {LIST, {.n = n}};
  return val;
}

mal mal_vec(vec *v) {
  if (v == NULL)
    return mal_exception_str("Null in mal_vec");
  mal val = {VEC, {.v = v}};
  return val;
}

mal mal_map(hash_table *m) {
  if (m == NULL)
    return mal_exception_str("Null in mal_map");
  mal val = {MAP, {.m = m}};
  return val;
}

mal mal_fn(fn *f) {
  if (f == NULL)
    return mal_exception_str("Null in mal_fn");
  mal val = {FN, {.f = f}};
  return val;
}

mal mal_closure(closure *c) {
  if (c == NULL)
    return mal_exception_str("Null in mal_closure");
  mal val = {CLOSURE, {.c = c}};
  return val;
}

mal mal_atom(mal m) {
  mal *m_ptr = checked_malloc(sizeof(mal), "mal_exception m_ptr");
  mal **m_ptr_ptr = checked_malloc(sizeof(mal *), "mal_exception m_ptr_ptr");
  *m_ptr = m;
  *m_ptr_ptr = m_ptr;
  mal val = {ATOM, {.a = m_ptr_ptr}};
  return val;
}
