#ifndef MAL_H
#define MAL_H

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
        char * s;
        list_node *p;
    };
} mal;

typedef struct {
    mal *val;
    list_node *next;
} list_node;

#endif