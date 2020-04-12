#ifndef MAL_TYPES_H
#define MAL_TYPES_H

#include <stdbool.h>
#include <stdlib.h>

/**
 *
 * mal.h - Header to define our main mal data type
 *
 * mal values are intended to be held on the stack (not via a pointer)
 *
 **/

// Forward declaration
struct list_node;
typedef struct list_node_struct list_node;
typedef struct vec_struct vec;
typedef struct map_struct map;
typedef struct map_record_struct map_record;
typedef struct mal_struct mal;
typedef struct env_struct env;
typedef struct closure_struct closure;
typedef struct hash_table hash_table;

typedef mal fn(list_node *, env *);

typedef unsigned count_t; // For number of elements in a vector or map

enum mal_tag {
  MISSING, // reader may return a missing value, should not be passed to eval or
           // print
  EXCEPTION, // an error or thrown by the user
  MAL_TRUE,
  MAL_FALSE,
  NIL,
  INT,
  STR_OR_KW, // Keywords are held as a prefixed string
  SYM,
  LIST,
  VEC,
  MAP,
  FN,      // A C-defined builtin function
  CLOSURE, // A mal-defined closure (or macro)
  ATOM
};

struct mal_struct {
  enum mal_tag tag;
  union {
    struct mal_struct *e;  // for EXCEPTION
    int i;                 // for INT
    const char *s;         // for STR_OR_KW, SYM
    list_node *n;          // for LIST
    vec *v;                // for VEC
    hash_table *m;         // FOR MAP
    fn *f;                 // FOR FN
    closure *c;            // FOR CLOSURE
    struct mal_struct **a; // FOR ATOM
  };
};

struct list_node_struct {
  mal val;
  list_node *next;
};

struct vec_struct {
  count_t count; // Number of mal elements in the vector
  mal *buf;
};

struct map_record_struct {
  const char *key;
  bool is_kw; // is the key a keyword (not just a string)
  int index;  // used to de-duplicate the map (by keeping highest index)
  mal val;
};

struct map_struct {
  count_t count; // number of mal elements  (including duplicate keys)
  map_record *table;
};

struct env_struct {
  map *lookup;
  struct env_struct *outer;
};

struct closure_struct {
  mal body;
  list_node *binds;
  env *e;
  bool is_macro;
};

// Test functions
bool is_missing(const mal);
bool is_exception(const mal);
bool is_bool(const mal);
bool is_true(const mal);
bool is_false(const mal);
bool is_falsey(const mal);
bool is_nil(const mal);
bool is_int(const mal);
bool is_str(const mal);
bool is_sym(const mal);
bool is_kw(const mal);
bool is_str_or_kw(const mal);
bool is_list(const mal);
bool is_vec(const mal);
bool is_seq(const mal);
bool is_map(const mal);
bool is_fn(const mal);
bool is_closure(const mal);
bool is_atom(const mal);
bool match_sym(const mal, const char *);

// Constructor functions
mal mal_missing(void);
mal mal_exception(mal);
mal mal_exception_str(const char *s);
mal mal_true(void);
mal mal_false(void);
mal mal_bool(bool);
mal mal_nil(void);
mal mal_int(int);
mal mal_str(const char *);
mal mal_sym(const char *);
mal mal_kw(const char *);
mal mal_kw(const char *);
mal mal_list(list_node *);
mal mal_vec(vec *);
mal mal_map(hash_table *);
mal mal_fn(fn *);
mal mal_closure(closure *);
mal mal_atom(mal);

// Helper function to skip keyword prefi
const char *skip_kw_prefix(const char *);

// Equality
bool mal_equals(mal, mal);

// Helper macro to propogate exceptions
#define RETURN_IF_EXCEPTION(x)                                                 \
  if (is_exception(x))                                                         \
  return x

// Helper macro to mark function parameters as unused to avoid warnings
// The unused parameters are needed in the function definition so the
// function pointer will type check (for fn* above)
#define UNUSED(x) __attribute__((unused)) x

#endif
