/**
 *
 * core_num.c - defines number-related functions for core environment
 *
 **/

#include "core_atom.h"

#include "debug.h"
#include "env.h"
#include "eval.h"
#include "seq.h"

// C implementation of mal function atom
// Takes a Mal value and returns a new atom which points to that Mal value.
static mal core_atom_create(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1)
    return mal_exception_str("Bad arguments to atom");
  RETURN_IF_EXCEPTION(n->val);
  return mal_atom(n->val);
}

// C implementation of mal function deref
// Takes an atom argument and returns the Mal value referenced by this atom
static mal core_atom_deref(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  RETURN_IF_EXCEPTION(n->val);
  if (list_count(n) != 1 || !is_atom(n->val))
    return mal_exception_str("Bad arguments to deref");
  return **n->val.a;
}

// C implementation of mal function reset!
// Takes an atom and a Mal value; the atom is modified to refer to the
// given Mal value. The Mal value is returned.
static mal core_atom_reset(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 2)
    return mal_exception_str("Need two arguments to deref");
  RETURN_IF_EXCEPTION(n->val);
  RETURN_IF_EXCEPTION(n->next->val);
  if (!is_atom(n->val))
    return mal_exception_str("Bad arguments to deref");
  **n->val.a = n->next->val;
  return n->next->val;
}

// C implementation of mal function swap
// Takes an atom, a function, and zero or more function arguments. The
// atom's value is modified to the result of applying the function with the
// atom's value as the first argument and the optionally given function
// arguments as the rest of the arguments. The new atom's value is returned.
static mal core_atom_swap(list_node *n, env *e) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) < 2)
    return mal_exception_str("Too few arguments to swap");

  mal a = n->val;
  mal f = n->next->val;
  list_node *rest = n->next->next;
  RETURN_IF_EXCEPTION(a);
  RETURN_IF_EXCEPTION(f);
  if (rest != NULL)
    RETURN_IF_EXCEPTION(rest->val);

  if (!is_atom(a))
    return mal_exception_str("Expected an atom in swap");
  if (!is_fn(f) && !is_closure(f))
    return mal_exception_str("Expected a function in swap");

  mal result = apply(f, list_cons(**a.a, rest), e);
  RETURN_IF_EXCEPTION(result);
  **a.a = result;
  return result;
}

void add_atom(env *e) {
  env_set(e, "atom", mal_fn(core_atom_create));
  env_set(e, "deref", mal_fn(core_atom_deref));
  env_set(e, "reset!", mal_fn(core_atom_reset));
  env_set(e, "swap!", mal_fn(core_atom_swap));
}
