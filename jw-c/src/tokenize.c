#include <pcre.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>

#include "tokenize.h"
#include "utils.h"

#define TOKEN_REGEX \
    "[\\s,]*" \
    "(~@|" \
    "[\\[\\]{}()'`~^@]|" \
    "\"(?:\\\\.|[^\\\\\"])*\"?|" \
    ";.*|" \
    "[^\\s\\[\\]{}('\"`,;)]*)"
#define OVEC_SIZE 6

// [\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)

// Return the next token
const token_result *tokenize(const char *input_string, const int offset) {

    static pcre *token_regexp = NULL;
    static int ovec[OVEC_SIZE];

    printf("tokenize at offset %d\n", offset);

    if (token_regexp == NULL) { // Compile the regexp if has not been done
        const char * error;
        int erroffset;
        token_regexp = pcre_compile(TOKEN_REGEX, 0, & error, &erroffset, NULL);
        if (token_regexp == NULL) {
            internal_error("Regexp compilation failed");
        }
    }

    int rc = pcre_exec(
        token_regexp, NULL,
        input_string, strlen(input_string),
        offset,
        PCRE_ANCHORED,
        ovec, OVEC_SIZE);

    // Our regexp has one mandatory grouping so we expect two matches to be returned
    if (rc == 0) {
        return NULL;
    }
    if (rc != 2) {
        internal_error("Regexp match failed %d", rc);
    }

    // ovec[0] and ovec[1] are offsets of start and end of whole match
    // ovec[2] and ovec[3] are offsets of start and end of (first and only ) group
    // ovec[4] and ovec[5] are working space for the library

    const char *token_start = input_string + ovec[2];
    int token_length = ovec[3] - ovec[2];
    char *token = malloc(token_length + 1);
    strncpy(token, token_start, token_length);
    printf("Token '%s'\n", token);

    token_result *result = malloc(sizeof(token_result));
    result->val = token;
    result->next_offset = ovec[1] + 1;
    return result;
}