Notes:
- Extern functions are documented in the header files (which are then picked up
  by VS Code and shown in mouseovers), static functions documented in their .c files
- Name prefix `core_` used for C functions which provide mal functionality and
  are bound to the repl environment from core.c
- Name prefix `mal_` used for C functions which manipluate the main `mal` type
  which is a discriminated union


Potential TODOs:
- Add asserts more thoroughly
- Check exception propogation more thorougly - as done in core_atom
- See if intermittent failures reeappear in tests 6, 9, A
- Add some `free()` calls when we are sure we don't need the value any more
- Add refernece counting or similar for proper memory management
