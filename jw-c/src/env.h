#ifndef ENV_H
#define ENV_H

#include "types.h"

// create a new envionment given an alternating sym/val list and an
// outer environment (returns NULL if fails)
env *env_new(list_node *, env *);

// create a new envionment given lists of symbols and values and an
// outer environment (returns NULL if fails)
env *env_new2(list_node *symbols, list_node *values, env *);

// free the environment and its contents
void env_free(env *);

// set the symbol to the given value in the enviroment (which is mutated)
void env_set(env *, const char *symbol, mal value);

// return a pointer to the environment containing the given symbol following the
// starting environment's outer chain. Returns NULL if not found
env *env_find(env *, const char *);

// return the value associated with the given symbol in the environment or
// an exception if the symbol is not present
mal env_get(env *, const char *);

#endif
