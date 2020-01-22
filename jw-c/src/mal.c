// Step 0 - repl

#include <editline/readline.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "printer.h"
#include "reader.h"
#include "utils.h"

#define INPUT_BUFFER_SIZE 200

mal READ(const char * s)
{
    mal m = read_str(s);
    debug("READ", "read_str finished (have '%s')", s || "NULL");
    return m;
}

mal EVAL(mal m)
{
    return m;
}

const char *PRINT(mal m)
{
    return pr_str(m);
}

const char *rep(const char *s)
{
    return PRINT(EVAL(READ(s)));
}

int main()
{
    mal m = read_str("23");
    debug("main", "got %d %d", m.tag, m.i);
    puts(PRINT(m));
    printf("---\n");
    m = read_str("\"abc\"");
    debug("main", "got %d %s", m.tag, m.s);
    puts(PRINT(m));
    printf("---\n");
    m = read_str("def");
    debug("main", "got %d %s", m.tag, m.s);
    puts(PRINT(m));
    printf("---\n");
    m = read_str("(qq 2 3)");
    debug("main", "got %d", m.tag);
    puts(PRINT(m));
    if (m.tag == LIST) {
        list_node * n = m.n;
        while (n != NULL) {
            puts(PRINT(n->val));
            n = n->next;
        }
    }
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
