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

#include "map.h"
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
static mal read_form(reader_state *);
static mal read_atom(reader_state *);
static mal mal_reader_exception(const char *, reader_state *);

// Return the pre-fetched token without advancing it
static const char *reader_peek(const reader_state *state_ptr) {
    return state_ptr->current == NULL ? "" : state_ptr->current;
}

// Return the previously fetched token and fetch the next one
static const char *reader_next(reader_state *state_ptr) {

    // We return this pre-fetched current match
    const char *pre_fetched_current = state_ptr->current;

    // Read the next token (if input string is not exhausted) and store as current
    if (state_ptr->offset < state_ptr->input_length) {
        const token_result *next = tokenize(state_ptr->input_string, state_ptr->offset);
        // if (next == NULL) {
        //     return mal_reader_exception("reader_next got null from tokenize", state_ptr);
        // }
        state_ptr->current = next->val;
        state_ptr->offset = next->next_offset;
    } else {
        state_ptr->current = NULL;
    }

    return pre_fetched_current;
}

enum read_type { READ_LIST, READ_VEC, READ_MAP};

// Read a list, vector or map
static mal read_extended(reader_state *state_ptr) {

    mal current = read_atom(state_ptr);
    enum read_type reading_mode;
    char *closing_char;
    if (match_sym(current, "(")) {
        reading_mode = READ_LIST;
        closing_char = ")";
    } else if (match_sym(current, "[")) {
        reading_mode = READ_VEC;
        closing_char = "]";
    } else if (match_sym(current, "{")) {
        reading_mode = READ_MAP;
        closing_char = "}";
    } else {
        return mal_reader_exception("read_extended called without opener", state_ptr);
    }

    // Read the elements as a list, keeping a count of the number read
    debug("read_extended", "started");
    list_node *head = NULL;
    list_node *last = NULL;
    int nodes_count = 0;
    while (true) {
        current = read_form(state_ptr);
        if (match_sym(current, closing_char)) {
            break;
        }
        if (is_missing(current)) {
            return mal_reader_exception("EOF in list", state_ptr);
        }
        nodes_count++;
        last = list_extend(current, last);
        if (head == NULL) {
            head = last;
        }
    }

    map *m;
    switch (reading_mode) {
        case READ_LIST:
            return mal_list(head);
        case READ_VEC:
            return mal_vec(list_to_vec(nodes_count, head));
        case READ_MAP:
            m = list_to_map(head);
            if (m == NULL) {
                return mal_exception(mal_str("Map creation failed"));
            }
            return mal_map(m);
        default:
            internal_error("bad mode at end of read_seq");
    }
}

// read a non-list
static mal read_atom(reader_state *state_ptr) {

    const char * const token = reader_next(state_ptr);
    const int token_len = strlen(token);
    mal value;
    debug("read_atom", "received token %s, length %d", token, token_len);

    if (token_len > 0 && is_number(token)) {
        value.tag = INT;
        value.i = atoi(token);
        debug("read_atom", "returning %d", value.i);
        return value;
    }

    if (token[0] == '\"') {
        if (token_len >=2 && token[token_len - 1] == '\"') {
            char *buf = checked_malloc(token_len - 1, "STR in read_atom");
            strncpy(buf, token + 1, token_len - 2);
            buf[token_len - 1] = '\0';
            mal val = remove_escapes(mal_str(buf));
            debug("read_atom", "returning str %s", val.s);
            return val;
        }
        return mal_reader_exception("unbalanced string quote", state_ptr);
    }

    if (token_len >= 2 && strncmp(token, ":", 1) == 0) {
        debug("read_atom", "returning keyword :%s", token + 1);
        return mal_kw(token + 1);
    }

    if (strcmp(token, "true") == 0) {
        debug("read_atom", "returning true");
        return mal_true();
    }

    if (strcmp(token, "false") == 0) {
        debug("read_atom", "returning false");
        return mal_false();
    }

    if (strcmp(token, "nil") == 0) {
        debug("read_atom", "returning nil");
        return mal_nil();
    }

    if (strcmp(token, "'") == 0) {
        debug("read_atom", "expanding quote");
        return mal_cons(mal_sym("quote"), mal_cons(read_form(state_ptr), mal_list(NULL)));
    }

    if (strcmp(token, "`") == 0) {
        debug("read_atom", "expanding quasiquote");
        return mal_cons(mal_sym("quasiquote"), mal_cons(read_form(state_ptr), mal_list(NULL)));
    }

    if (strcmp(token, "~") == 0) {
        debug("read_atom", "expanding unquote");
        return mal_cons(mal_sym("unquote"), mal_cons(read_form(state_ptr), mal_list(NULL)));
    }

    if (strcmp(token, "@") == 0) {
        debug("read_atom", "expanding deref");
        return mal_cons(mal_sym("deref"), mal_cons(read_form(state_ptr), mal_list(NULL)));
    }

    if (strcmp(token, "~@") == 0) {
        debug("read_atom", "expanding spliceunquote");
        return mal_cons(mal_sym("splice-unquote"), mal_cons(read_form(state_ptr), mal_list(NULL)));
    }

    if (strcmp(token, "^") == 0) {
        debug("read_atom", "expanding deref");
        mal m1 = read_form(state_ptr);
        mal m2 = read_form(state_ptr);
        return mal_cons(mal_sym("with-meta"), mal_cons(m2, mal_cons(m1, mal_list(NULL))));
    }

    if (token_len >= 1) {
        value.tag = SYM;
        value.s = checked_malloc(token_len + 1, "SYM in read_atom");
        strncpy((char *)value.s, token, token_len + 1);
        debug("read_atom", "returning sym %s", value.s);
        return value;
    }

    return mal_reader_exception("read_atom received zero length token", state_ptr);
}

static mal read_form(reader_state *state_ptr) {
    const char *next_token=reader_peek(state_ptr);
    if (strcmp(next_token, "") == 0) {
        debug("read_form", "returning missing");
        return mal_missing();
    }
    if (strcmp(next_token, "(") == 0 || strcmp(next_token, "[") == 0 || strcmp(next_token, "{") == 0) {
        debug("read_form", "starting read_extended");
        return read_extended(state_ptr);
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

#define READER_ERROR_MSG "Reader error:"

static mal mal_reader_exception(const char * msg, reader_state *state_ptr) {
    const int buf_len =
        strlen(READER_ERROR_MSG) + strlen(msg)
        + state_ptr->input_length - state_ptr->offset
        + 2; // two spaces
    char *buf = checked_malloc(buf_len + 1, "mal_reader_exception");
    snprintf(buf, buf_len, "%s %s %s", READER_ERROR_MSG, msg, state_ptr->input_string + state_ptr->offset);
    return mal_exception(mal_str(buf));
}


