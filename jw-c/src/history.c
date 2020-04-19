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
#define HISTORY_SEP "/"

char *history_filename = NULL;

void pre_history() {

  // set the file name for our history file
  const char *home = getenv("HOME");
  assert(home != NULL);
  const size_t buf_size =
      strlen(home) + strlen(HISTORY_FILENAME) + strlen(HISTORY_SEP) + 1;
  history_filename = checked_malloc(buf_size, "start_history");
  strncpy(history_filename, home, buf_size);
  strncat(history_filename, HISTORY_SEP, buf_size - strlen(history_filename));
  strncat(history_filename, HISTORY_FILENAME,
          buf_size - strlen(HISTORY_FILENAME));

  int history_errno = read_history(history_filename);
  if (history_errno == 0)
    DEBUG_INTERNAL_FMT("History file %s read OK\n", history_filename);
  else
    DEBUG_INTERNAL_FMT("History file %s read failed, error %d\n",
                       history_filename, history_errno);
}

void post_history() {

  assert(history_filename != NULL);

  int err = write_history(history_filename);
  assert(err == 0);

  err = history_truncate_file(history_filename, HISTORY_MAX_SIZE);
  assert(err == 0);
}
