/**
 *
 * map.c - implement an immutable map data type
 *
 * Holds keys as a sorted array and use bsearch to look up. Keys can be
 * str, sym or kw mal values, but sym/str values are both treated as strs
 *
 **/

#include <string.h>

#include "map.h"

#include "debug.h"
#include "seq.h"
#include "types.h"
#include "utils.h"

// ordering on - string, keyword
int map_record_cmp(void const *lhs, void const *rhs)
{

  map_record const *const l = lhs;
  map_record const *const r = rhs;
  DEBUG_INTERNAL_FMT("comparing %s %s", l->key, r->key);

  int compare_strings = strcmp(l->key, r->key);
  if (compare_strings)
    return compare_strings;
  return l->is_kw - r->is_kw;
}

// ordering on - string, keyword, index
int map_record_cmp_with_index(void const *lhs, void const *rhs)
{

  map_record const *const l = lhs;
  map_record const *const r = rhs;
  DEBUG_INTERNAL_FMT("comparing %s %s", l->key, r->key);

  int compare_strings = strcmp(l->key, r->key);
  if (compare_strings)
    return compare_strings;
  int compare_kw = l->is_kw - r->is_kw;
  if (compare_kw)
    return compare_kw;
  return l->index - r->index;
}

// return a new map with no duplicates from the given map (which is sorted)
map *dedup(map *m)
{

  DEBUG_INTERNAL_FMT("given map of %d elems", m->size);
  qsort(m->table, m->size, sizeof(map_record), map_record_cmp_with_index);

  // count the unique elements
  int unique_elems = 1;
  for (int i = 1; i < m->size; i++)
  {
    unique_elems += map_record_cmp(m->table + i - 1, m->table + i) != 0;
  }
  DEBUG_INTERNAL_FMT("%d unique elems", unique_elems);

  // create the new map
  map *new_map = checked_malloc(sizeof(map), "dedup map");
  new_map->size = unique_elems;
  new_map->table =
      checked_malloc(new_map->size * sizeof(map_record), "dedup table");

  // copy over the elemets
  new_map->table[0] = m->table[0];
  DEBUG_INTERNAL_FMT("copied element with key %s", m->table[0].key);
  int j = 0;
  for (int i = 1; i < m->size; i++)
  {
    if (map_record_cmp(m->table + i, new_map->table + j) == 0)
    {
      DEBUG_INTERNAL_FMT("overwrote element on key %s", m->table[i].key);
      new_map->table[j] = m->table[i];
    }
    else
    {
      DEBUG_INTERNAL_FMT("copied element on key %s", m->table[i].key);
      new_map->table[++j] = m->table[i];
    }
  }

  return new_map;
}

// Create a map with unititialized entries
map *uninitialized_map(size_t count)
{
  map *new_map = checked_malloc(sizeof(map), "uninit_map map");
  new_map->size = count;
  new_map->table =
      checked_malloc(count * sizeof(map_record), "uninit_map table");
  return new_map;
}

// create a map from an alternating list of elements [key, val, key, val...]
map *list_to_map(list_node *n)
{

  DEBUG_INTERNAL_FMT("called with %d elements", list_count(n));

  // list must have an even number of elements
  const int list_size = list_count(n);
  if (list_size % 2 == 1)
  {
    return NULL;
  }

  // create the new map
  map *new_map = uninitialized_map(list_size / 2);
  if (new_map->size == 0)
  {
    DEBUG_INTERNAL_FMT("returning empty map");
    return new_map;
  }

  // copy the elements from the list into the table
  int i = 0;
  while (n != NULL)
  {
    if (!is_str(n->val) && !is_kw(n->val) && !is_sym(n->val))
    {
      DEBUG_INTERNAL_FMT("returning NULL as found non-str/kw");
      return NULL;
    }
    new_map->table[i].key = n->val.s;
    new_map->table[i].is_kw = is_kw(n->val);
    new_map->table[i].index = i;
    n = n->next;
    new_map->table[i].val = n->val;
    n = n->next;
    i++;
  }

  // deduplicate
  map *dedup_map = dedup(new_map);
  free(new_map);
  return dedup_map;
}

// equality for two maps
bool map_equals(map *m1, map *m2)
{
  DEBUG_INTERNAL_FMT("called on maps of length %d and %d", m1->size, m2->size);
  if (m1->size != m2->size)
    return false;
  for (int i = 0; i < m1->size; i++)
  {
    map_record r1 = m1->table[i];
    map_record r2 = m2->table[i];
    if (strcmp(r1.key, r2.key) != 0)
      return false;
    if (r1.is_kw != r2.is_kw)
      return false;
    if (!mal_equals(r1.val, r2.val))
      return false;
  }
  return true;
}

mal map_get(map *hm, mal m)
{
  DEBUG_INTERNAL_MAL("called with", m);
  if (!is_str(m) && !is_kw(m) && !is_sym(m))
    return mal_nil();
  map_record key_record = {.key = m.s, .is_kw = is_kw(m)};
  map_record *res = bsearch(&key_record, hm->table, hm->size,
                            sizeof(map_record), map_record_cmp);
  mal ret = res ? res->val : mal_nil();
  DEBUG_INTERNAL_MAL("returning", ret);
  return ret;
}

bool map_contains(map *hm, mal m)
{
  DEBUG_INTERNAL_MAL2("called with", mal_map(hm), m);
  if (!is_str(m) && !is_kw(m) && !is_sym(m))
    return false;

  map_record key_record = {.key = m.s, .is_kw = is_kw(m)};
  map_record *res = bsearch(&key_record, hm->table, hm->size,
                            sizeof(map_record), map_record_cmp);
  bool ret = res != NULL;
  DEBUG_INTERNAL_FMT("returning", ret ? "true" : "false");
  return ret;
}
