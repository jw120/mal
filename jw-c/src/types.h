#ifndef MAL_TYPES_H
#define MAL_TYPES_H

#include <stdbool.h>
#include <stdlib.h>

/**
 *
 * mal.h - Header to define our main mal data type
 *
 * mal values are intended to be held on the stack (not via a pointer)
 *
**/

// Forward declaration
struct list_node;
typedef struct list_node list_node;
typedef struct vec vec;
typedef struct map map;
typedef struct map_record map_record;

enum mal_tag {
    MISSING, // reader may return a missing value, should not be passed to eval or print
    EXCEPTION, // an error or thrown by the user
    TRUE,
    FALSE,
    NIL,
    INT,
    STR,
    SYM,
    KW,
    LIST,
    VEC,
    MAP
};

struct mal_struct {
    enum mal_tag tag;
    union {
        struct mal_struct *e; // for EXCEPTION
        int i; // for INT
        const char * s; // for STR, SYM AND KEYWORD
        list_node *n; // for LIST
        vec *v; // for VEC
        map *m; // FOR MAP
    };
};
typedef struct mal_struct mal;

struct list_node {
    mal val;
    list_node *next;
};

struct vec {
    size_t size;
    mal *buf;
};

struct map_record {
    const char *key;
    bool is_kw; // is the key a keyword (not just a string)
    int index; // used to de-duplicate the map (by keeping highest index)
    mal val;
};

struct map {
    size_t size; // size of table (including duplicate keys)
    map_record *table;
};

// Test functions
bool is_missing(const mal);
bool is_exception(const mal);
bool is_bool(const mal);
bool is_true(const mal);
bool is_false(const mal);
bool is_nil(const mal);
bool is_int(const mal);
bool is_str(const mal);
bool is_sym(const mal);
bool is_kw(const mal);
bool is_list(const mal);
bool is_vec(const mal);
bool is_seq(const mal);
bool is_map(const mal);
bool match_sym(const mal, const char *);

// Constructor functions
mal mal_missing();
mal mal_exception(mal);
mal mal_true();
mal mal_false();
mal mal_nil();
mal mal_int(int);
mal mal_str(const char *);
mal mal_sym(const char *);
mal mal_kw(const char *);
mal mal_kw(const char*);
mal mal_list(list_node *);
mal mal_vec(vec *);
mal mal_map(map *);

// Equality
bool mal_equals(mal, mal);

#endif