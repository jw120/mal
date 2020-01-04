#include <pcre.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>

#include "utils.h"

#define TOKEN_REGEX " *(Q.Q)" // "[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)"
#define OVEC_COUNT 300 // Must be a multiple of 3

#define REGEX_OPTIONS PCRE_ANCHORED

pcre *token_regexp;

void reader_initialize(void) {

    // Compile the regexp
    const char * error;
    int erroffset;
    token_regexp = pcre_compile(TOKEN_REGEX, 0, & error, &erroffset, NULL);
    if (token_regexp == NULL) {
        internal_error("Regexp compilation failed");
    }


}

const char **tokenize(const char *input_string) {

    int ovec[OVEC_COUNT];

    int offset = 0;
    bool finished = false;

    while (!finished) {

        int rc = pcre_exec(
            token_regexp, NULL,
            input_string, strlen(input_string),
            offset, 0,
            ovec, OVEC_COUNT);

        // ovec[0] and ovec[1] are offsets of start and end of whole match
        // ovec[2] and ovec[3] are offsets of start and end of (first and only ) group


        if (rc < 0) {
            internal_error("Regexp match failed %d", rc);
        }

        if (rc == 0) {
            internal_error("Regexp output vector not big enough");
        }

        if (rc > 2) {
            internal_error("Expected one group to match: %d", rc);
        }

        if (rc == 1) {
            finished = true;
            // Spaces only
        } else {

            const char *token_start = input_string + ovec[2];
            int token_length = ovec[3] - ovec[2];
            char *token = malloc(token_length + 1);
            strncpy(token, token_start, token_length);
            printf("Token '%s'\n", token);
            offset = ovec[1] + 1;
            finished = offset > strlen(input_string);
        }
    }

    return NULL;
}