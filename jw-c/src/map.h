#ifndef MAP_H
#define MAP_H

#include "types.h"

map *list_to_map(list_node *);
bool map_equals(map *, map *);
mal map_get(map *, const char *);
bool map_contains(map *, const char *);

#endif