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

    debug("reader_next", "called with state {'%s', %d, '%s', %d}",
        state_ptr->input_string, state_ptr->input_length,
        state_ptr->current == NULL ? "NULL" : state_ptr->current,
        state_ptr->offset);

    // We return this pre-fetched current match
    const char *pre_fetched_current = state_ptr->current;

    const token_result *next;
    if (state_ptr->offset < state_ptr->input_length) {
        debug("reader_next", "calling tokenize on '%s' at %d", state_ptr->input_string, state_ptr->offset);
        next = tokenize(state_ptr->input_string, state_ptr->offset);
        if (next == NULL) {
           internal_error("Got null from tokenize");
        }
        debug("reader_next", "fetched '%s' with %d", next->val, next->next_offset);
        state_ptr->current = next->val;
        state_ptr->offset = next->next_offset;
        debug("reader_next", "updated state_ptr");
    } else {
        state_ptr->current = NULL;
    }
    debug("reader_next", "leaving with state {'%s', %d, '%s', %d}",
        state_ptr->input_string, state_ptr->input_length,
        state_ptr->current == NULL ? "NULL" : state_ptr->current,
        state_ptr->offset);

    debug("reader_next", "returning '%s'\n", pre_fetched_current == NULL ? "NULL" : pre_fetched_current);
    return pre_fetched_current;
}

void read_str(const char *input_string) {

    debug("read_str", "called on '%s'", input_string);

    static reader_state *state_ptr;
    state_ptr = checked_malloc(sizeof(reader_state), "state allocation in read_str");

    debug("read_str", "allocated %p", state_ptr);

    state_ptr->input_string = input_string;
    debug("read_str", "set input_string '%s'", state_ptr->input_string);

    state_ptr->input_length = strlen(input_string);
    debug("read_str", "set input_length %d", state_ptr->input_length);

    state_ptr->offset = 0;
    debug("read_str", "set offset %d", state_ptr->offset);

    state_ptr->current = NULL;
    debug("read_str", "set current '%s'", state_ptr->current == NULL ? "NULL" : state_ptr->current);


    debug("read_str", "set to {'%s', %d, '%s', %d}",
        state_ptr->input_string, state_ptr->input_length,
        state_ptr->current == NULL ? "NULL" : state_ptr->current,
        state_ptr->offset);

    reader_next(state_ptr);
}
