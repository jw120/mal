#include <pcre.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>

#include "mal.h"
#include "reader.h"
#include "tokenize.h"
#include "utils.h"

/**
 *
 *
 *
 *
 **/

typedef struct reader_state reader_state;
struct reader_state {
    const char *input_string;
    int input_length;
    const char *current;
    int offset;
};

const char *reader_peek(const reader_state *state_ptr) {
    return state_ptr->current;
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

    debug("reader_next", "returning '%s'", pre_fetched_current == NULL ? "NULL" : pre_fetched_current);
    return pre_fetched_current;
}

mal read_list(reader_state *state_ptr) {

    const char * token = reader_next(state_ptr);
    if (strcmp(token, "(") != 0) {
        internal_error("read_list", "called without leading paren: '%s'", token);
    }

    while (strcmp(token, ")") != 0) {
        token = reader_next(state_ptr);
        if (token == NULL) {
            internal_error("Missing end of list");
        }
        debug("read_list", "found element '%s'", token);
    }

    mal m = { LIST };
    return m;

}


mal read_atom(reader_state *state_ptr) {

    const char * const token = reader_next(state_ptr);
    const int token_len = strlen(token);
    mal value;

    if (token_len > 0 && is_number(token)) {
        debug("read_atom", "found an int in '%s'", token);
        value.tag = INT;
        value.i = atoi(token);
        debug("read_atom", "returning %d", value.i);
        return value;
    }

    if (token_len >=2 && token[0] == '\"' && token[token_len - 1] == '\"') {
        value.tag = STR;
        value.s = checked_malloc(token_len - 1, "STR in read_atom");
        strncpy(value.s, (char *) token + 1, token_len - 2);
        return value;
    }

    if (token_len >= 1) {
        value.tag = SYM;
        value.s = token;
        return value;
    }

    internal_error("read_atom", "zero length token");

}

mal read_form(reader_state *state_ptr) {
    if (strcmp(reader_peek(state_ptr), "(") == 0) {
        return read_list(state_ptr);
    } else {
        return read_atom(state_ptr);
    }
}

// Top-level
mal read_str(const char *input_string) {

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

    debug("read_str", "set to {'%s', %d, '%s', %d}",
        state_ptr->input_string, state_ptr->input_length,
        state_ptr->current == NULL ? "NULL" : state_ptr->current,
        state_ptr->offset);

    mal m = read_form(state_ptr);
    debug("read_str", "returning with tag %d", m.tag);
    return m;
}



