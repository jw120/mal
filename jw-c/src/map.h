#ifndef MAP_H
#define MAP_H

#include "types.h"

map *list_to_map(list_node *);
bool map_equals(map *, map *);
mal map_get(map *, mal);
bool map_contains(map *, mal);

#endif