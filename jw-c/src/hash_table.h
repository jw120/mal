#ifndef HASH_TABLE_H
#define HASH_TABLE_H

#include <stdbool.h>

#include "types.h"

typedef struct hash_row {
  const char *key;
  mal value;
} hash_row;

struct hash_table {
  count_t size;
  count_t entries;
  hash_row *table;
};

hash_table *ht_new(unsigned);
hash_table *ht_copy(hash_table *, unsigned);

bool ht_has(hash_table *, const char *);
mal ht_get(hash_table *, const char *);
void ht_put(hash_table *, const char *, mal);

hash_table *ht_from_alternating_list(list_node *);
hash_table *ht_from_lists(list_node *, list_node *);

list_node *ht_keys(hash_table *);
list_node *ht_values(hash_table *);

bool ht_equals(hash_table *, hash_table *);

void ht_debug_print(hash_table *, const char *);

#endif
