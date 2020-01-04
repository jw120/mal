#include <pcre.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>

#include "reader.h"
#include "tokenize.h"
#include "utils.h"

const char *reader_peek(reader_state state) {
    return state.current;
}

const char *reader_next(reader_state *state_ptr) {
    const char *current = state_ptr->current;
    if (state_ptr->offset < state_ptr->input_length) {
        const token_result *next = tokenize(state_ptr->input_string, state_ptr->offset);
        if (next == NULL) {
           internal_error("Got null from tokenize");
        }
        printf("Fetched token %s\n", next->val);
        state_ptr->current = next->val;
        state_ptr->offset = next->next_offset;
    } else {
        state_ptr->current = NULL;
    }
    printf("Returned token %s\n", next->val);
    return current;
}


void read_str(const char *input_string) {

    static reader_state *state_ptr;
    state_ptr = malloc(sizeof(reader_state));

    state_ptr->input_string = input_string;
    state_ptr->input_length = strlen(input_string);
    state_ptr->offset = 0;
    state_ptr->current = reader_next(state_ptr);


}
