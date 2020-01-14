#include <stdbool.h>
#include <string.h>
#include <stdio.h>

// Use pcre2 library without support for wide characters
#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>

#include "tokenize.h"
#include "utils.h"

/***
 *
 * Tokenize
 *
 * When called repeatedly on a fixed input string, it returns the matched tokens
 * one by one using the mal PCRE regex.
 *
 * Uses pcre2 library (installed by default on mac)
 *
 **/

// Regex for mal tokens (given in mal instructions)
#define TOKEN_REGEX \
    "[\\s,]*" \
    "(~@|" \
    "[\\[\\]{}()'`~^@]|" \
    "\"(?:\\\\.|[^\\\\\"])*\"?|" \
    ";.*|" \
    "[^\\s\\[\\]{}('\"`,;)]*)"
#define OVEC_SIZE 6

// Search input_string from offset, returning the next token and the next offset
const token_result *tokenize(const char *input_string, const int offset) {

    static pcre2_code *token_regexp = NULL;
    static pcre2_match_data *match_data = NULL;

    debug("tokenize", "called on '%s' at offset %d", input_string, offset);

    if (token_regexp == NULL) { // Compile the regexp if has not been done

        int errornumber;
        size_t erroroffset;

        token_regexp = pcre2_compile(
            (PCRE2_SPTR8) TOKEN_REGEX,  // Our regex
            PCRE2_ZERO_TERMINATED,      // ... which is zero-terminated
            0,                          // No options
            &errornumber, &erroroffset, // For error return values
            NULL);                      // Use default compile context

        if (token_regexp == NULL) {
            PCRE2_UCHAR buffer[256];
            pcre2_get_error_message(errornumber, buffer, sizeof(buffer));
            internal_error("PCRE2 compilation failed at offset %d: %s\n",
                (int)erroroffset, buffer);
        }

        // Reserve space for matches that will be returned
        match_data = pcre2_match_data_create_from_pattern(token_regexp, NULL);
    }

    int regexp_result = pcre2_match(
        token_regexp, (PCRE2_SPTR8) input_string, strlen(input_string), offset,
        0 /* default options */, match_data, NULL /* default match context */);

    // Nothing to return if no match or a match with no group (which is for whitespace only)
    if (regexp_result == PCRE2_ERROR_NOMATCH || regexp_result == 1) {
        debug("tokenize", "found nothing");
        return NULL;
    }

    // Fail if anyother error or more than one matched group
    if (regexp_result < 0 || regexp_result > 2) {
        internal_error("Regexp match failed (return code %d)", regexp_result);
    }

    // Get the matches
    // ovector[0] and ovector[1] are offsets of start and end of whole match
    // ovector[2] and ovector[3] are offsets of start and end of (first and only ) group
    size_t *ovector = pcre2_get_ovector_pointer(match_data);

    const char *token_start = input_string + ovector[2];
    int token_length = ovector[3] - ovector[2];
    char *token = checked_malloc(token_length + 1, "token allocation in tokenize");
    strncpy(token, token_start, token_length);
    debug("tokenize", "found token '%s'", token);

    token_result *result = checked_malloc(sizeof(token_result), "result allocation in tokenize");
    result->val = token;
    result->next_offset = ovector[1] + 1;
    return result;
}