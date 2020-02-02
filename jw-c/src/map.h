#ifndef MAP_H
#define MAP_H

#include "types.h"

bool map_equals(map *, map *);
mal map_get(map *, mal);
void map_set(map *, mal, mal);
bool map_contains(map *, mal);

map *uninitialized_map(size_t);
map *list_to_map(list_node *);
map *list2_to_map(list_node *, list_node *);

#endif