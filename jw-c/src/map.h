#ifndef MAP_H
#define MAP_H

#include "types.h"

bool map_equals(map *, map *);
mal map_get(map *, mal);
bool map_contains(map *, mal);

map *uninitialized_map(size_t);
map *list_to_map(list_node *);

#endif