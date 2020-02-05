/**
 *
 * core.c - defines the standard environment
 *
 **/

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "core.h"

#include "core_atom.h"
#include "core_mal.h"
#include "core_misc.h"
#include "core_num.h"
#include "core_seq.h"

#include "debug.h"
#include "env.h"
#include "printer.h"
#include "seq.h"

// return the core environment, creating it if it does not already exist
env *core_env() {

  // create the environment if it does not yet exist
  static env *e = NULL;
  if (e == NULL) {
    DEBUG_INTERNAL_FMT("created new core environment");
    e = env_new(NULL, NULL);
    add_atom(e);
    add_misc(e);
    add_num(e);
    add_seq(e);
    add_mal(e);
  }
  return e;
}
