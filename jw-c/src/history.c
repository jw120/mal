/**
 *
 * history.c - handles start up and close down of readline history file
 *
 **/

#include <assert.h>
#include <stdio.h> // stdio has to be before readline/history.h

#include <readline/history.h>
#include <stdlib.h>
#include <string.h>

#include "history.h"

#include "utils.h"

#define HISTORY_FILENAME ".c_mal_history"
#define HISTORY_MAX_SIZE 100

char *history_filename = NULL;

// called before history tracking starts, reads history file
void pre_history()
{

  // set the file name for our history file
  const char *home = getenv("HOME");
  assert(home != NULL);
  history_filename = checked_malloc(strlen(home) + strlen(HISTORY_FILENAME) + 2,
                                    "start_history");
  strncat(history_filename, home, strlen(home));
  strncat(history_filename + strlen(home), "/", 1);
  strncat(history_filename + strlen(home) + 1, HISTORY_FILENAME,
          strlen(HISTORY_FILENAME) + 1);

  // If the history file exists, read from it
  FILE *fp = fopen(history_filename, "r");
  if (fp != NULL)
  {
    fclose(fp);
    int err = read_history(history_filename);
    assert(err == 0);
  }
}

// called after history tracking finishes, writes history file
void post_history()
{

  assert(history_filename != NULL);

  int err = write_history(history_filename);
  assert(err == 0);

  err = history_truncate_file(history_filename, HISTORY_MAX_SIZE);
  assert(err == 0);
}
