#include <pcre.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <stdnoreturn.h>

#include "list.h"
#include "reader.h"
#include "tokenize.h"
#include "utils.h"

/**
 *
 * reader.c - Provides the read_str function which reads a mal form from a string
 *
 * Operates by tokenizing the input and holding a state which holds the input string,
 * an index into it for the current location and a current token.
 *
 **/

// Reader state - used only within this module
typedef struct {
    const char *input_string;
    int input_length;
    const char *current;
    int offset;
} reader_state;

// Forward definitions of founctions used within this module
mal read_form(reader_state *);
mal read_atom(reader_state *);
mal read_list(reader_state *);
noreturn void reader_error(const char *, reader_state *);

// Query the current token without advancing it
const char *reader_peek(const reader_state *state_ptr) {
    return state_ptr->current == NULL ? "" : state_ptr->current;
}

// Read the current token and advance the reader
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
           reader_error("reader_next got null from tokenize", state_ptr);
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

// Read a list
mal read_list(reader_state *state_ptr) {

    mal current = read_atom(state_ptr);

    if (!mal_equals(current, opening_paren)) {
        reader_error("read_list called without leading paren", state_ptr);
    }

    debug("read_list", "started");
    list_node *head = NULL;
    list_node *last = NULL;

    while (true) {
        current = read_form(state_ptr);
        if (mal_equals(current, closing_paren)) {
            break;
        }
        if (current.tag == MISSING) {
            internal_error("EOF in list");
        }
        last = list_extend(current, last);
        if (head == NULL) {
            head = last;
        }
        debug("read_list", "found element tag %d", current.tag);
    }
    debug("read_list", "finished");

    return make_list(head);


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
        debug("read_atom", "found an string %s", token);
        value.tag = STR;
        value.s = checked_malloc(token_len - 1, "STR in read_atom");
        strncpy((char *)value.s, token + 1, token_len - 2);
        debug("read_atom", "returning str %s", value.s);
        return value;
    }

    if (token_len >= 1) {
        debug("read_atom", "found a symbol %s", token);
        value.tag = SYM;
        value.s = token;
        debug("read_atom", "returning sym %s", value.s);
        return value;
    }

    puts("!!!");
    reader_error("read_atom received zero length token", state_ptr);

}

mal read_form(reader_state *state_ptr) {
    if (strcmp(reader_peek(state_ptr), "") == 0) {
        debug("read_form", "starting read_list");
        return make_missing();
    }
    if (strcmp(reader_peek(state_ptr), "(") == 0) {
        debug("read_form", "starting read_list");
        return read_list(state_ptr);
    } else {
        debug("read_form", "starting read_atom");
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


noreturn void reader_error(const char * msg, reader_state *state_ptr) {
    internal_error("Reader error %s at %s", msg, state_ptr->input_string + state_ptr->offset);
}


