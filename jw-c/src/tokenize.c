/**
 *
 * tokenize.c - returns a token from the input_string
 *
 * When called repeatedly on a fixed input string, it returns the matched tokens
 * one by one using the mal PCRE regex.
 *
 * Uses pcre2 library
 *
 **/

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

// Use pcre2 library without support for wide characters
#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>

#include "tokenize.h"

#include "debug.h"
#include "utils.h"

// Regex for mal tokens (given in mal instructions)
#define TOKEN_REGEX                                                            \
  "[\\s,]*"                                                                    \
  "(~@|"                                                                       \
  "[\\[\\]{}()'`~^@]|"                                                         \
  "\"(?:\\\\.|[^\\\\\"])*\"?|"                                                 \
  ";.*|"                                                                       \
  "[^\\s\\[\\]{}('\"`,;)]*)"

// Search input_string from offset, returning the next token and the next offset
const token_result *tokenize(const char *input_string, const size_t offset) {

  DEBUG_INTERNAL_FMT("called");
  DEBUG_INTERNAL_FMT("called on %s", input_string + offset);

  static pcre2_code *token_regexp = NULL;
  static pcre2_match_data *match_data = NULL;

  if (token_regexp == NULL) { // Compile the regexp if has not been done

    int errornumber;
    size_t erroroffset;

    token_regexp =
        pcre2_compile((PCRE2_SPTR8)TOKEN_REGEX, // Our regex
                      PCRE2_ZERO_TERMINATED,    // ... which is zero-terminated
                      0,                        // No options
                      &errornumber, &erroroffset, // For error return values
                      NULL);                      // Use default compile context
    assert(token_regexp != NULL);

    // Reserve space for matches that will be returned
    match_data = pcre2_match_data_create_from_pattern(token_regexp, NULL);
  }

  int regexp_result = pcre2_match(
      token_regexp, (PCRE2_SPTR8)input_string, strlen(input_string), offset,
      0 /* default options */, match_data, NULL /* default match context */);

  // Nothing to return if no match or a match with no group (which is for
  // whitespace only)
  if (regexp_result == PCRE2_ERROR_NOMATCH || regexp_result == 1) {
    DEBUG_INTERNAL_FMT("found nothing");
    return NULL;
  }

  // Fail if anyother error or more than one matched group
  assert(regexp_result == 2);

  // Get the matches
  // ovector[0] and ovector[1] are offsets of start and end of whole match
  // ovector[2] and ovector[3] are offsets of start and end of (first and only )
  // group
  size_t *ovector = pcre2_get_ovector_pointer(match_data);

  const char *token_start = input_string + ovector[2];
  size_t token_length = ovector[3] - ovector[2];
  char *token =
      checked_malloc(token_length + 1, "token allocation in tokenize");
  strncpy(token, token_start, token_length);
  token[token_length] = '\0';
  token_result *result =
      checked_malloc(sizeof(token_result), "result allocation in tokenize");
  result->val = token;
  result->next_offset = ovector[1];
  DEBUG_INTERNAL_FMT("returning %s", result);
  return result;
}
