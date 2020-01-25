/**
 *
 * reader.c - Provides the read_str function which reads a mal form from a string
 *
 * Operates by tokenizing the input and holding a state which holds the input string,
 * an index into it for the current location and a current token.
 *
 **/

#include <pcre.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <stdnoreturn.h>

#include "reader.h"
#include "seq.h"
#include "tokenize.h"
#include "utils.h"

// Reader state - used only within this module
typedef struct {
    const char *input_string; // original input string provided to read_str
    int input_length; // length of input_string
    const char *current;
    int offset;
} reader_state;

// Forward definitions of founctions used within this module
mal read_form(reader_state *);
mal read_atom(reader_state *);
mal read_list(reader_state *);
mal make_reader_exception(const char *, reader_state *);

// Return the pre-fetched token without advancing it
const char *reader_peek(const reader_state *state_ptr) {
    return state_ptr->current == NULL ? "" : state_ptr->current;
}

// Return the previously fetched token and fetch the next one
const char *reader_next(reader_state *state_ptr) {

    // We return this pre-fetched current match
    const char *pre_fetched_current = state_ptr->current;

    // Read the next token (if input string is not exhausted) and store as current
    if (state_ptr->offset < state_ptr->input_length) {
        const token_result *next = tokenize(state_ptr->input_string, state_ptr->offset);
        // if (next == NULL) {
        //     return make_reader_exception("reader_next got null from tokenize", state_ptr);
        // }
        state_ptr->current = next->val;
        state_ptr->offset = next->next_offset;
    } else {
        state_ptr->current = NULL;
    }

    return pre_fetched_current;
}

// Read a list or vector
mal read_seq(reader_state *state_ptr) {

    mal current = read_atom(state_ptr);
    bool reading_list;
    if (match_sym(current, "(")) {
        reading_list = true;
    } else if (match_sym(current, "]")) {
        reading_list = false;
    } else {
        return make_reader_exception("read_seq called without opener", state_ptr);
    }

    // Read the elements as a list, keeping a count of the number read
    debug("read_seq", "started");
    list_node *head = NULL;
    list_node *last = NULL;
    int nodes_count = 0;
    while (true) {
        current = read_form(state_ptr);
        if ((reading_list && match_sym(current, ")")) ||
            (!reading_list && match_sym(current, "]"))) {
            break;
        }
        if (is_missing(current)) {
            return make_reader_exception("EOF in list", state_ptr);
        }
        nodes_count++;
        last = list_extend(current, last);
        if (head == NULL) {
            head = last;
        }
    }

    if (reading_list) {
        return make_list(head);
    }
    return make_vec(create_vec(nodes_count, head));
}

// read a non-list
mal read_atom(reader_state *state_ptr) {

    const char * const token = reader_next(state_ptr);
    const int token_len = strlen(token);
    mal value;

    if (token_len > 0 && is_number(token)) {
        value.tag = INT;
        value.i = atoi(token);
        debug("read_atom", "returning %d", value.i);
        return value;
    }

    if (token_len >=2 && token[0] == '\"' && token[token_len - 1] == '\"') {
        value.tag = STR;
        value.s = checked_malloc(token_len - 1, "STR in read_atom");
        strncpy((char *)value.s, token + 1, token_len - 2);
        debug("read_atom", "returning str %s", value.s);
        return value;
    }

    if (token_len >= 1) {
        value.tag = SYM;
        value.s = checked_malloc(token_len + 1, "SYM in read_atom");
        strncpy((char *)value.s, token, token_len);
        debug("read_atom", "returning sym %s", value.s);
        return value;
    }

    return make_reader_exception("read_atom received zero length token", state_ptr);
}

mal read_form(reader_state *state_ptr) {
    const char *next_token=reader_peek(state_ptr);
    if (strcmp(next_token, "") == 0) {
        debug("read_form", "returning missing");
        return make_missing();
    }
    if (strcmp(next_token, "(") == 0 || strcmp(next_token, ")") == 0) {
        debug("read_form", "starting read_seq");
        return read_seq(state_ptr);
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
    state_ptr->input_string = input_string;
    state_ptr->input_length = strlen(input_string);
    state_ptr->current = NULL;
    state_ptr->offset = 0;

    // first call of reader_next sets current and provides no output
    reader_next(state_ptr);

    return read_form(state_ptr);
}

static const char *reader_error_msg = "Reader error:";

mal make_reader_exception(const char * msg, reader_state *state_ptr) {
    const int buf_len =
        strlen(reader_error_msg) + strlen(msg)
        + state_ptr->input_length - state_ptr->offset
        + 2; // two spaces
    char *buf = checked_malloc(buf_len + 1, "make_reader_exception");
    snprintf(buf, buf_len, "%s %s %s", reader_error_msg, msg, state_ptr->input_string + state_ptr->offset);
    return make_exception(make_str(buf));
}


