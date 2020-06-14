#ifndef CORE_MISC_H
#define CORE_MISC_H

#include "types.h"

// add misc core functions to the environment
void add_misc(env *);

mal core_apply(list_node *n, env *e);

#endif
