#ifndef ENV_H
#define ENV_H

#include "types.h"

typedef struct env_struct {
    map *lookup;
    struct env_struct *outer;
} env;

env *env_new(list_node *, env *);
void env_set(env *, const char *, mal);
env *env_find(env *, const char *);
mal env_get(env *, const char *);

#endif