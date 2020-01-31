#ifndef ENV_H
#define ENV_H

#include "types.h"

env *env_new(list_node *, env *);
void env_free(env *);
void env_set(env *, const char *, mal);
env *env_find(env *, const char *);
mal env_get(env *, const char *);

#endif