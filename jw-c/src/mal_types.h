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

enum mal_tag {
    INT,
    STR,
    SYM,
    LIST
};

typedef struct {
    enum mal_tag tag;
    union {
        int i;
        const char * s;
        list_node *n;
    };
} mal;

struct list_node {
    mal val;
    list_node *next;
};

// Equality
bool mal_equals(mal, mal);

// Constructor functions
mal make_int(int i);
mal make_str(const char *s);
mal make_sym(const char *s);
mal make_list();

// Constants to simplify evaluation
extern mal opening_paren;
extern mal closing_paren;

#endif