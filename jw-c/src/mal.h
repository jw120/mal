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
    };
} mal;

#endif