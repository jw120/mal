# Elixir implementation of mal

TODO
- Try exdoc

Maybe macros instead of wrap functions in core?

Choices
 - Vectors as maps
 - Environments (and atoms?) with ETS
 - Nothing done for TCO (as elixir does automatically?)

  Be careful with ^ and $ as they match start/end of line, not string endings. If you want to match the whole string use: \A and \z. [link]

