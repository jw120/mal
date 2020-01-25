#ifndef MAL_TYPES_H
#define MAL_TYPES_H

#include <stdbool.h>

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

enum mal_tag {
    MISSING, // reader may return a missing value, should not be passed to eval or print
    EXCEPTION, // an error or thrown by the user
    INT,
    STR,
    SYM,
    LIST,
    VEC
};

struct mal_struct {
    enum mal_tag tag;
    union {
        int i;
        const char * s;
        list_node *n;
        vec *v;
        struct mal_struct *e; // for an exception
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

// Test functions
bool is_missing(const mal);
bool is_exception(const mal);
bool is_int(const mal);
bool is_str(const mal);
bool is_sym(const mal);
bool is_list(const mal);
bool is_vec(const mal);
bool is_seq(const mal);
bool match_sym(const mal, const char *);

// Constructor functions
mal make_missing();
mal make_int(int);
mal make_str(const char *);
mal make_sym(const char *);
mal make_list(list_node *);
mal make_vec(vec *);
mal make_exception(mal);

// Equality
bool mal_equals(mal, mal);

#endif