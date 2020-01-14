#ifndef MAL_LIST_H
#define MAL_LIST_H

#include "mal.h"

struct mal_list {
    mal list_val;
    next *mal_list;
}
typedef struct mal_list mal_list;

// returns a point to a newly malloc'ed mal_list element (with next set to NULL)
mal_list *new_list_element(mal val);


#endif
