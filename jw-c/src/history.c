/**
 *
 * history.c - handles start up and close down of readline history file
 *
 **/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <readline/history.h>

#include "history.h"

#include "utils.h"

#define HISTORY_FILENAME ".c_mal_history"
#define HISTORY_MAX_SIZE 100

char *history_filename = NULL;

void start_history() {

    // set the file name for our history file
    const char *home = getenv("HOME");
    if (home == NULL) {
        internal_error("Could not find $HOME for history file");
    }
    history_filename = checked_malloc(strlen(home) + strlen(HISTORY_FILENAME) + 2, "start_history");
    strncat(history_filename, home, strlen(home));
    strncat(history_filename + strlen(home), "/", 1);
    strncat(history_filename + strlen(home) + 1, HISTORY_FILENAME, strlen(HISTORY_FILENAME) + 1);

    // If the history file exists, read from it
    FILE *fp = fopen(history_filename, "r");
    if (fp != NULL) {
        fclose(fp);
        int err = read_history(history_filename);
        if (err) {
            internal_error("Error %d reading history file %s", err, history_filename);
        }
    }

}

void end_history() {

    if (!history_filename) {
        internal_error("History filename not set in end_history");
    }

    int err = write_history(history_filename);
    if (err) {
        internal_error("Error %d writing history file %d\n", err, history_filename);
    }
    err = history_truncate_file(history_filename, HISTORY_MAX_SIZE);
    if (err) {
        internal_error("Error %d truncating history file %d\n", err, history_filename);
    }
}



