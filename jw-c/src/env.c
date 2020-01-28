/**
 *
 * env.c - implement an environment of symbols
 *
 * Holds symbols using a map of mal_str
 *
 **/


#include "types.h"
#include "env.h"

#include "map.h"
#include "utils.h"

env *env_new(list_node *elems, env *outer) {

    env *e = checked_malloc(sizeof(env), "env_new");
    e->lookup = list_to_map(elems);
    e->outer = outer;

    return e;

}

void env_set(env *e, const char *sym, mal val) {
    internal_error("env_set NYI");
}

env *env_find(env *e, const char *sym) {
    if (e == NULL) {
        return NULL;
    }
    if (map_contains(e->lookup, mal_str(sym))) {
        return e;
    }
    if (e->outer != NULL) {
        return env_find(e->outer, sym);
    }
    return NULL;
}

mal env_get(env *e, const char *sym) {
    env *found_e = env_find(e, sym);
    if (e == NULL) {
        return mal_exception(mal_str("not found"));
    }
    return map_get(found_e->lookup, mal_str(sym));
}