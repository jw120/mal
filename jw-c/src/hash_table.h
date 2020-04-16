#ifndef HASH_TABLE_H
#define HASH_TABLE_H

#include <stdbool.h>

#include "types.h"

typedef struct hash_row {
  const char *key; // NULL if this row is empty
  mal value;
} hash_row;

struct hash_table {
  count_t size;    // size of the table
  count_t entries; // number of non-null entries in the table
  hash_row *table;
};

// Create a new hash table with capacity for the given number of entries
hash_table *ht_new(unsigned);

// Create a new hash table with cpacity for the given number of entries and
// add all the entries of the given hash table
hash_table *ht_copy(hash_table *, unsigned);

// Does the hash table include the given key
bool ht_has(hash_table *, const char *);

// Get the value associated with the given key or an exception
// if the key is missing. Intended to be called only when
// the key is known to be present (e.g., after calling ht_has)
mal ht_get(hash_table *, const char *);

// Add the given key and value to the hash table
void ht_put(hash_table *, const char *, mal);

// Create a new hash table from an alternating list of elements [key, val, key,
// val,...]. Keys must be mal strings, keywords or symbols. Returns NULL if list
// is invalid
hash_table *ht_from_alternating_list(list_node *);

// Create a new hash table from a pair of lists [key, key, ...] [val, val, ...]
// Keys must be mal strings, keywords or symbols. Returns NULL if list is
// invalid
hash_table *ht_from_lists(list_node *keys, list_node *values);

// Return all the hash table's keys as a mal linked list
list_node *ht_keys(hash_table *);

// Return all the hash table's values as a mal linked list
list_node *ht_values(hash_table *);

// Do the two hash tables have the same keys with the same values
bool ht_equals(hash_table *, hash_table *);

// Print all the hash table's contents to stdio for debugging purposes
void ht_debug_print(hash_table *, const char *);

#endif
