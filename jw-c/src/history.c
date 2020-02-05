/**
 *
 * history.c - handles start up and close down of readline history file
 *
 **/

#include <stdio.h> // stdio has to be before readline/history.h

#include <assert.h>
#include <readline/history.h>
#include <stdlib.h>
#include <string.h>

#include "debug.h"
#include "history.h"

#include "utils.h"

#define HISTORY_FILENAME ".c_mal_history"
#define HISTORY_MAX_SIZE 100

char *history_filename = NULL;

// called before history tracking starts, reads history file
void pre_history() {

  // set the file name for our history file
  const char *home = getenv("HOME");
  assert(home != NULL);
  history_filename = checked_malloc(strlen(home) + strlen(HISTORY_FILENAME) + 2,
                                    "start_history");
  strncat(history_filename, home, strlen(home));
  strncat(history_filename + strlen(home), "/", 1);
  strncat(history_filename + strlen(home) + 1, HISTORY_FILENAME,
          strlen(HISTORY_FILENAME) + 1);

  int history_errno = read_history(history_filename);
  if (history_errno == 0)
    DEBUG_INTERNAL_FMT("History file %s read OK\n", history_filename);
  else
    DEBUG_INTERNAL_FMT("History file %s read failed, error %d\n",
                       history_filename, history_errno);
}

// called after history tracking finishes, writes history file
void post_history() {

  assert(history_filename != NULL);

  int err = write_history(history_filename);
  assert(err == 0);

  err = history_truncate_file(history_filename, HISTORY_MAX_SIZE);
  assert(err == 0);
}
