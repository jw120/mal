// Step 0 - repl

#include <editline/readline.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "reader.h"
#include "tokenize.h"
#include "utils.h"

#define INPUT_BUFFER_SIZE 200

const char *READ(const char * s)
{
    read_str(s);
    debug("READ", "read_str finished (have '%s')", s || "NULL");
    return s;
}

const char *EVAL(const char * s)
{
    return s;
}

const char *PRINT(const char* s)
{
    return s;
}

const char *rep(const char *s)
{
    return PRINT(EVAL(READ(s)));
}

int main()
{
    mal m = read_str("23");
    debug("main", "got %d %d", m.tag, m.i);
    printf("---\n");
    m = read_str("\"abc\"");
    debug("main", "got %d %s", m.tag, m.s);
    printf("---\n");
    m = read_str("def");
    debug("main", "got %d %s", m.tag, m.s);
    printf("---\n");
    m = read_str("(qq 2 3)");
    debug("main", "got %d", m.tag);
    return 0;

    // while (1) {

    //     const char *input = readline("user> ");
    //     if (input == NULL) {
    //         puts("");
    //         break;
    //     }
    //     add_history(input);
    //     puts(rep(input));
    //     free((void *) input);
    // }
    // return 0;
}
