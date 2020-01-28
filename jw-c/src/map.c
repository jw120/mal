/**
 *
 * map.c - implement an immutable map data type
 *
 * Holds keys as a sorted array and use bsearch to look up
 *
 **/

#include <string.h>

#include "map.h"

#include "seq.h"
#include "types.h"
#include "utils.h"

// ordering on - string, keyword, duplicate
int map_record_cmp(void const *lhs, void const *rhs) {

    map_record const *const l = lhs;
    map_record const *const r = rhs;

    int compare_strings = strcmp(l->key, r->key);
    if (compare_strings) {
        return compare_strings;
    }
    int compare_kw = l->is_kw - r->is_kw;
    if (compare_kw) {
        return compare_kw;
    }
    return l->is_dup - r->is_dup;
}

// create a map from an alternating list of elements [key, val, key, val...]
map *list_to_map(list_node *n) {

    // list must have an even number of elements
    const int list_size = list_count(n);
    if (list_size % 2 == 1) {
        return NULL;
    }

    // create the new map
    map *new_map = checked_malloc(sizeof(map), "mal_map map");
    new_map->size = list_size / 2;
    new_map->table = checked_malloc(new_map->size * sizeof(map_record), "mal_map table");

    // copy the elements from the list into the table
    int i = 0;
    while (n != NULL) {
        if (!is_str(n->val) && !is_kw(n->val)) {
            return NULL;
        }
        new_map->table[i].key = n->val.s;
        new_map->table[i].is_kw = n->val.tag == KW;
        new_map->table[i].is_dup = false; // updated below
        n = n->next;
        new_map->table[i].val = n->val;
        n = n->next;
        i++;
    }

    // sort the keys and return
    qsort(new_map->table, new_map->size, sizeof(map_record), map_record_cmp);
    return new_map;
}

bool map_equals(map *m1, map *m2) {
    if (m1->size != m2->size) {
        return false;
    }
    for (int i=0; i < m1->size; i++) {
        map_record r1 = m1->table[i];
        map_record r2 = m2->table[i];
        if (strcmp(r1.key, r2.key) != 0) {
            return false;
        }
        if (r1.is_kw != r2.is_kw) {
            return false;
        }
        if (!mal_equals(r1.val, r2.val)) {
            return false;
        }
    }
    return true;
}

mal map_get(map *hm, mal m) {
    if (!is_str(m) && !is_kw(m)) {
        return mal_nil();
    }
    map_record key_record = { .key = m.s, .is_kw = is_kw(m), .is_dup = false };
    map_record *res = bsearch(&key_record, hm->table, hm->size, sizeof(map_record), map_record_cmp);
    return res ? res->val : mal_nil();
}

bool map_contains(map *hm, mal m) {
    if (!is_str(m) && !is_kw(m)) {
        return false;
    }
    map_record key_record = { .key = m.s, .is_kw = is_kw(m), .is_dup = false };
    map_record *res = bsearch(&key_record, hm->table, hm->size, sizeof(map_record), map_record_cmp);
    return res != NULL;
}

