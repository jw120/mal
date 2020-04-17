/**
 *
 * reader.c - Provides the read_str function which reads a mal form from a
 *string
 *
 * Operates by tokenizing the input and holding a state which holds the input
 *s tring, an index into it for the current location and a current token.
 *
 **/

#include <assert.h>
#include <ctype.h>
#include <pcre.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdnoreturn.h>
#include <string.h>

#include "reader.h"

#include "debug.h"
#include "escapes.h"
#include "hash_table.h"
#include "seq.h"
#include "tokenize.h"
#include "utils.h"

// Reader state - used only within this module
typedef struct {
  const char *input_string; // original input string provided to read_str
  size_t input_length;      // length of input_string
  const char *current;
  size_t offset;
} reader_state;

// Forward definitions of founctions used within this module
static mal read_form(reader_state *);
static mal read_atom(reader_state *);
static mal mal_reader_exception(const char *, reader_state *);
static bool is_number(const char *s);

// Return the pre-fetched token without advancing it
static const char *reader_peek(const reader_state *state_ptr) {
  return state_ptr->current == NULL ? "" : state_ptr->current;
}

// Return the previously fetched token and fetch the next one
static const char *reader_next(reader_state *state_ptr) {

  DEBUG_INTERNAL_FMT("called with state_ptr %p", state_ptr);
  DEBUG_INTERNAL_FMT("called with state_ptr->input_string %s",
                     state_ptr->input_string);
  DEBUG_INTERNAL_FMT("called with state_ptr->input_length %d",
                     state_ptr->input_length);
  DEBUG_INTERNAL_FMT("called with state_ptr->current %p", state_ptr->current);
  DEBUG_INTERNAL_FMT("called with state_ptr->offset %p", state_ptr->offset);

  // We return this pre-fetched current match
  const char *pre_fetched_current = state_ptr->current;

  // Read the next token (if input string is not exhausted) and store as current
  if (state_ptr->offset < state_ptr->input_length) {
    const token_result *next =
        tokenize(state_ptr->input_string, state_ptr->offset);
    state_ptr->current = next->val;
    state_ptr->offset = next->next_offset;
  } else
    state_ptr->current = NULL;

  return pre_fetched_current;
}

enum read_type { READ_LIST, READ_VEC, READ_MAP };

// Read a list, vector or map
static mal read_extended(reader_state *state_ptr) {

  mal current = read_atom(state_ptr);
  enum read_type reading_mode;
  char *closing_char;
  if (match_sym(current, "(")) {
    DEBUG_INTERNAL_FMT("list");
    reading_mode = READ_LIST;
    closing_char = ")";
  } else if (match_sym(current, "[")) {
    DEBUG_INTERNAL_FMT("vec");
    reading_mode = READ_VEC;
    closing_char = "]";
  } else if (match_sym(current, "{")) {
    DEBUG_INTERNAL_FMT("map");
    reading_mode = READ_MAP;
    closing_char = "}";
  } else
    return mal_reader_exception("read_extended no opener", state_ptr);

  // Read the elements as a list, keeping a count of the number read
  list_node *head = NULL;
  list_node *last = NULL;
  count_t nodes_count = 0;
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

  switch (reading_mode) {
  case READ_LIST:
    return mal_list(head);
  case READ_VEC:
    return mal_vec(list_to_vec(nodes_count, head));
  case READ_MAP:
    return mal_map(ht_from_alternating_list(head));
  }
}

// read a non-list
static mal read_atom(reader_state *state_ptr) {

  DEBUG_INTERNAL_FMT("top of loop");
  const char *const token = reader_next(state_ptr);
  if (token == NULL) {
    return mal_missing(); // EOF
  }
  const size_t token_len = strlen(token);
  mal value;
  DEBUG_INTERNAL_FMT("received token %s, length %d", token, token_len);

  if (token_len > 0 && is_number(token)) {
    value = mal_int(atoi(token));
    DEBUG_INTERNAL_FMT("returning int %d", value.i);
    return value;
  }

  if (token[0] == '\"') {
    if (token_len >= 2 && token[token_len - 1] == '\"') {
      char *buf = checked_malloc(token_len - 1, "string in read_atom");
      strncpy(buf, token + 1, token_len - 2);
      buf[token_len - 1] = '\0';
      DEBUG_INTERNAL_FMT("before remove_escapes '%s'", buf);
      mal val = remove_escapes(buf);
      DEBUG_INTERNAL_FMT("returning str '%s'", val.s);
      return val;
    }
    return mal_reader_exception("unbalanced string quote", state_ptr);
  }

  if (token_len >= 2 && strncmp(token, ":", 1) == 0) {
    DEBUG_INTERNAL_FMT("returning keyword :%s", token + 1);
    return mal_kw(token + 1);
  }

  if (strcmp(token, "true") == 0) {
    DEBUG_INTERNAL_FMT("returning true");
    return mal_true();
  }

  if (strcmp(token, "false") == 0) {
    DEBUG_INTERNAL_FMT("returning false");
    return mal_false();
  }

  if (strcmp(token, "nil") == 0) {
    DEBUG_INTERNAL_FMT("returning nil");
    return mal_nil();
  }

  if (strcmp(token, "'") == 0) {
    DEBUG_INTERNAL_FMT("expanding quote");
    return mal_cons(mal_sym("quote"),
                    mal_cons(read_form(state_ptr), mal_list(NULL)));
  }

  if (strcmp(token, "`") == 0) {
    DEBUG_INTERNAL_FMT("expanding quasiquote");
    return mal_cons(mal_sym("quasiquote"),
                    mal_cons(read_form(state_ptr), mal_list(NULL)));
  }

  if (strcmp(token, "~") == 0) {
    DEBUG_INTERNAL_FMT("expanding unquote");
    return mal_cons(mal_sym("unquote"),
                    mal_cons(read_form(state_ptr), mal_list(NULL)));
  }

  if (strcmp(token, "@") == 0) {
    DEBUG_INTERNAL_FMT("expanding deref");
    return mal_cons(mal_sym("deref"),
                    mal_cons(read_form(state_ptr), mal_list(NULL)));
  }

  if (strcmp(token, "~@") == 0) {
    DEBUG_INTERNAL_FMT("expanding spliceunquote");
    return mal_cons(mal_sym("splice-unquote"),
                    mal_cons(read_form(state_ptr), mal_list(NULL)));
  }

  if (strcmp(token, "^") == 0) {
    DEBUG_INTERNAL_FMT("expanding deref");
    mal m1 = read_form(state_ptr);
    mal m2 = read_form(state_ptr);
    return mal_cons(mal_sym("with-meta"),
                    mal_cons(m2, mal_cons(m1, mal_list(NULL))));
  }

  if (token_len >= 1) {
    value = mal_sym(checked_malloc(token_len + 1, "SYM in read_atom"));
    strncpy((char *)value.s, token, token_len + 1);
    DEBUG_INTERNAL_FMT("returning sym %s", value.s);
    return value;
  }

  return mal_reader_exception("read_atom received zero length token",
                              state_ptr);
}

static mal read_form(reader_state *state_ptr) {

  while (true) { // loop to skip over comments

    const char *next_token = reader_peek(state_ptr);
    if (strcmp(next_token, "") == 0) {
      DEBUG_INTERNAL_FMT("returning missing");
      return mal_missing();
    }
    if (next_token[0] == ';') {
      DEBUG_INTERNAL_FMT("read comment");
      reader_next(state_ptr);
      continue;
    }
    if (strcmp(next_token, "(") == 0 || strcmp(next_token, "[") == 0 ||
        strcmp(next_token, "{") == 0) {
      DEBUG_INTERNAL_FMT("starting read_extended");
      return read_extended(state_ptr);
    } else {
      DEBUG_INTERNAL_FMT("starting read_atom");
      return read_atom(state_ptr);
    }
  }
}

// Top-level
mal read_str(const char *input_string) {

  DEBUG_INTERNAL_FMT("called on '%s'", input_string);

  static reader_state *state_ptr;
  state_ptr =
      checked_malloc(sizeof(reader_state), "state allocation in read_str");
  state_ptr->input_string = input_string;
  state_ptr->input_length = strlen(input_string);
  state_ptr->current = NULL;
  state_ptr->offset = 0;

  // first call of reader_next sets current and provides no output
  reader_next(state_ptr);

  return read_form(state_ptr);
}

#define READER_ERROR_MSG "Reader error:"

static mal mal_reader_exception(const char *msg, reader_state *state_ptr) {
  const size_t buf_len = strlen(READER_ERROR_MSG) + strlen(msg) +
                         state_ptr->input_length - state_ptr->offset +
                         2; // two spaces
  char *buf = checked_malloc(buf_len + 1, "mal_reader_exception");
  snprintf(buf, buf_len, "%s %s %s", READER_ERROR_MSG, msg,
           state_ptr->input_string + state_ptr->offset);
  return mal_exception_str(buf);
}

// Helper function - does the string represent a valid number
static bool is_number(const char *s) {
  assert(s != NULL);

  // Leading minus sign is OK; just skip
  if (*s == '-')
    s++;

  // Fail if any empty string
  if (strlen(s) == 0)
    return false;

  // Fail if any character is not a digit
  for (const char *p = s; *p; p++) {
    if (!isdigit(*p))
      return false;
  }

  return true;
}
