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

#include "map.h"
#include "seq.h"
#include "utils.h"

// Value equality
bool mal_equals(mal a, mal b)
{
  switch (a.tag)
  {
  case MISSING:
    assert(0); // Missing tag in mal_equals
    return false;
  case EXCEPTION:
    return a.tag == b.tag && mal_equals(*a.e, *b.e);
  case TRUE:
  case FALSE:
  case NIL:
    return a.tag == b.tag;
  case INT:
    return a.tag == b.tag && a.i == b.i;
  case SYM:
  case STR:
  case KW:
    return a.tag == b.tag && strcmp(a.s, b.s) == 0;
  case LIST:
  case VEC:
    if (b.tag == LIST || b.tag == VEC)
    {
      return seq_equals(a, b);
    }
    return false;
  case MAP:
    return a.tag == b.tag && map_equals(a.m, b.m);
  case FN:
    return false;
  }
}

/**
 *
 * Convenience functions to test the type
 *
 */

bool is_missing(mal m) { return m.tag == MISSING; }

bool is_exception(mal m) { return m.tag == EXCEPTION; }

bool is_bool(const mal m) { return m.tag == TRUE || m.tag == FALSE; }

bool is_true(const mal m) { return m.tag == TRUE; }

bool is_false(const mal m) { return m.tag == FALSE; }

bool is_nil(const mal m) { return m.tag == NIL; }

bool is_int(const mal m) { return m.tag == INT; }

bool is_str(const mal m) { return m.tag == STR; }

bool is_sym(const mal m) { return m.tag == SYM; }

bool is_kw(const mal m) { return m.tag == KW; }

bool is_list(const mal m) { return m.tag == LIST; }

bool is_vec(const mal m) { return m.tag == VEC; }

bool is_seq(const mal m) { return m.tag == LIST || m.tag == VEC; }

bool is_map(const mal m) { return m.tag == MAP; }

bool is_fn(const mal m) { return m.tag == FN; }

bool match_sym(const mal m, const char *s)
{
  return m.tag == SYM && strcmp(m.s, s) == 0;
}

/**
 * Convenience constructor functions
 *
 */

mal mal_missing()
{
  mal val = {MISSING};
  return val;
}

mal mal_exception(mal m)
{
  mal *m_ptr = checked_malloc(sizeof(mal), "mal_exception");
  *m_ptr = m;
  mal val = {EXCEPTION, {.e = m_ptr}};
  return val;
}

mal mal_exception_str(const char *s)
{
  mal *m_ptr = checked_malloc(sizeof(mal), "mal_exception_str");
  m_ptr->tag = STR;
  m_ptr->s = s;
  mal val = {EXCEPTION, {.e = m_ptr}};
  return val;
}

mal mal_true()
{
  mal val = {TRUE};
  return val;
}

mal mal_false()
{
  mal val = {FALSE};
  return val;
}

mal mal_nil()
{
  mal val = {NIL};
  return val;
}

mal mal_int(int i)
{
  mal val = {INT, {.i = i}};
  return val;
}

mal mal_str(const char *s)
{
  mal val = {STR, {.s = s}};
  return val;
}

mal mal_sym(const char *s)
{
  mal val = {SYM, {.s = s}};
  return val;
}

mal mal_kw(const char *s)
{
  mal val = {KW, {.s = s}};
  return val;
}

mal mal_list(list_node *n)
{
  mal val = {LIST, {.n = n}};
  return val;
}

mal mal_vec(vec *v)
{
  mal val = {VEC, {.v = v}};
  return val;
}

mal mal_map(map *m)
{
  mal val = {MAP, {.m = m}};
  return val;
}

mal mal_fn(fn *f)
{
  mal val = {FN, {.f = f}};
  return val;
}

// Constants to simplify evaluation
mal opening_paren = {SYM, {.s = "("}};
mal closing_paren = {SYM, {.s = ")"}};
