/**
 *
 * hash_table.c - implements hash tables with const char *keys and void* values
 *
 **/

// Uses an open addressing table
//
// https://en.wikipedia.org/wiki/Hash_table
//

#include "hash_table.h"
#include "debug.h"
#include "seq.h"
#include "utils.h"

#include <stdio.h>
#include <string.h>

// When the load factor increases above RESIZE_LOAD_FACTOR we re-create the
// hash table with a larger size
#define RESIZE_LOAD_FACTOR 0.7
#define RESIZE_SIZE_MULTIPLE 2

// When we create a hash-map from a list of keys, how big should the table be
// relative to the number of keys in the list
#define CREATE_SIZE_MULTIPLE 1.5
#define CREATE_SIZE_MIN 8

// Jenkins hash function (wikipedia via stackoverflow)
uint32_t hash(const char *s) {
  uint32_t hash = 0;
  uint32_t len = (uint32_t)strlen(s);
  for (uint32_t i = 0; i < len; ++i) {
    hash += (uint32_t)s[i];
    hash += (hash << 10);
    hash ^= (hash >> 6);
  }
  hash += (hash << 3);
  hash ^= (hash >> 11);
  hash += (hash << 15);
  return hash;
}

// Create a new hash_table with capacity for the given number of entries
hash_table *ht_new(count_t entries) {
  DEBUG_HIGH_FMT("called for %u entries", entries);

  count_t size = (count_t)(CREATE_SIZE_MULTIPLE * (double)entries);
  if (size < CREATE_SIZE_MIN)
    size = CREATE_SIZE_MIN;
  hash_table *ht = checked_malloc(sizeof(hash_table), "ht_new struct");
  ht->size = size;
  ht->entries = 0;
  ht->table = checked_malloc(sizeof(hash_row) * size, "ht_new table");
  for (count_t i = 0; i < size; i++) {
    ht->table[i].key = NULL;
  }
  return ht;
}

// Add key and value to the hash table
void ht_put(hash_table *ht, const char *key, mal value) {
  DEBUG_HIGH_FMT("called for key %s", key);

  count_t i = hash(key) % ht->size;
  // printf("ht_put %s hash is %u ", key, i);
  while (ht->table[i].key != NULL)
    i = (i + 1) % ht->size;
  ht->table[i].key = key;
  ht->table[i].value = value;
  ht->entries++;
  // printf("added to slot %u (entries %u, size %u)\n", i, ht->entries,
  // ht->size);

  if (ht->entries > ht->size * RESIZE_LOAD_FACTOR) {
    // printf("ht_put resizing (size was %u, will be %u)\n", ht->entries,
    //  ht->size * RESIZE_SIZE_MULTIPLE);
    hash_table *new_ht = ht_new(ht->size * RESIZE_SIZE_MULTIPLE);
    for (count_t j = 0; j < ht->size; j++) {
      if (ht->table[j].key != NULL) {
        // printf("transferring %s: ", ht->table[j].key);
        ht_put(new_ht, ht->table[j].key, ht->table[j].value);
      }
    }
    free(ht->table);
    ht->size = new_ht->size;
    ht->entries = new_ht->entries;
    ht->table = new_ht->table;
    free(new_ht);
  }
}

// Get the value associated with the given key
// Crashes if the key is missing - use ht_has first
mal ht_get(hash_table *ht, const char *key) {
  DEBUG_HIGH_FMT("called for key %s", key);

  count_t i = hash(key) % ht->size;
  while (true) {
    if (ht->table[i].key == NULL)
      return mal_exception_str("No such key in ht_get");
    if (strcmp(key, ht->table[i].key) == 0)
      return ht->table[i].value;
    i = (i + 1) % ht->size;
  }
}

// Is the key in the hash table
bool ht_has(hash_table *ht, const char *key) {
  DEBUG_HIGH_FMT("called for key %s", key);

  count_t i = hash(key) % ht->size;
  while (true) {
    if (ht->table[i].key == NULL)
      return false;
    if (strcmp(key, ht->table[i].key) == 0)
      return true;
    i = (i + 1) % ht->size;
  }
}

// create a map from an alternating list of elements [key, val, key, val,...]
// returns NULL if list is invalid
hash_table *ht_from_alternating_list(list_node *n) {

  DEBUG_HIGH_FMT("called with %d elements", list_count(n));

  const count_t list_size = list_count(n);
  if (list_size % 2 == 1)
    return NULL;

  hash_table *ht = ht_new(list_size / 2);

  while (n != NULL) {
    if (!is_str_or_kw(n->val)) {
      return NULL;
    }
    ht_put(ht, n->val.s, n->next->val);
    n = n->next->next;
  }
  return ht;
}

// create a map from a pair of lists [key, key, ...] [val, val, ...]
// returns NULL if list is invalid
hash_table *ht_from_lists(list_node *ks, list_node *vs) {

  count_t ks_count = list_count(ks);
  count_t vs_count = list_count(vs);

  DEBUG_HIGH_FMT("called with %d, %d elements", ks_count, vs_count);

  if (ks_count != vs_count)
    return NULL;

  hash_table *ht = ht_new(ks_count);

  while (ks != NULL && vs != NULL) {
    if (!is_str(ks->val)) {
      return NULL;
    }
    ht_put(ht, ks->val.s, vs->val);
    ks = ks->next;
    vs = vs->next;
  }
  return ht;
}

// Return all the table's keys as a mal linked list
list_node *ht_keys(hash_table *ht) {
  DEBUG_HIGH_FMT("called on table with %u entries", ht->entries);

  list_node *n = NULL;
  for (count_t i = 0; i < ht->size; i++) {
    const char *key = ht->table[i].key;
    if (key != NULL) {
      list_node *new_n = checked_malloc(sizeof(list_node), "ht_keys");
      new_n->next = n;
      new_n->val = mal_str(key);
      n = new_n;
    }
  }
  return n;
}

// Return all the table's values as a mal linked list
list_node *ht_values(hash_table *ht) {
  DEBUG_HIGH_FMT("called on table with %u entries", ht->entries);

  list_node *n = NULL;
  for (count_t i = 0; i < ht->size; i++) {
    const char *key = ht->table[i].key;
    if (key != NULL) {
      list_node *new_n = checked_malloc(sizeof(list_node), "ht_keys");
      new_n->next = n;
      new_n->val = ht->table[i].value;
      n = new_n;
    }
  }
  return n;
}

// Do the two hash tables have the same keys and values
bool ht_equals(hash_table *a, hash_table *b) {

  if (a->entries != b->entries)
    return FALSE;
  for (count_t i = 0; i < a->size; i++) {
    const char *a_key = a->table[i].key;
    if (a_key != NULL) {
      if (!ht_has(b, a_key))
        return FALSE;
      mal a_value = a->table[i].value;
      mal b_value = ht_get(b, a_key);
      if (!mal_equals(a_value, b_value))
        return FALSE;
    }
  }
  return TRUE;
}
