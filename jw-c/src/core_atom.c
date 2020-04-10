/**
 *
 * core_num.c - defines number-related functions for core environment
 *
 **/

#include <string.h>

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
  return mal_atom(n->val);
}

// C implementation of mal function atom?
// Takes an argument and returns true if the argument is an atom.
static mal core_atom_test(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1)
    return mal_exception_str("Bad arguments to atom?");
  return mal_bool(is_atom(n->val));
}

// C implementation of mal function deref
// Takes an atom argument and returns the Mal value referenced by this atom
static mal core_atom_deref(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 1 || !is_atom(n->val))
    return mal_exception_str("Bad arguments to deref");
  return **n->val.a;
}

// C implementation of mal function reset!
// Takes an atom and a Mal value; the atom is modified to refer to the
// given Mal value. The Mal value is returned.
static mal core_atom_reset(list_node *n, UNUSED(env *e)) {
  DEBUG_HIGH_MAL("called with", mal_list(n));
  if (list_count(n) != 2 || !is_atom(n->val))
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
  if (!is_atom(a))
    return mal_exception_str("Expected an atom in swap");
  if (!is_fn(f) && !is_closure(f))
    return mal_exception_str("Expected a function in swap");

  mal fn_with_args = mal_cons(f, mal_cons(**a.a, mal_list(n->next->next)));
  mal result = eval(fn_with_args, e);
  **a.a = result;
  return result;
}

// add number-related core functions to the environment
void add_atom(env *e) {
  env_set(e, "atom", mal_fn(core_atom_create));
  env_set(e, "atom?", mal_fn(core_atom_test));
  env_set(e, "deref", mal_fn(core_atom_deref));
  env_set(e, "reset!", mal_fn(core_atom_reset));
  env_set(e, "swap!", mal_fn(core_atom_swap));
}
