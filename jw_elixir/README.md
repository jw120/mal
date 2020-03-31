# Elixir implementation of mal



Choices
 - Vectors as maps
 - Environments with ETS
 - Atoms with agent (as cross-process)
 - Nothing done for TCO (as elixir does automatically?)
 - Ambiguity between [Mal] as a Mal list type and also a list of Mal types

  Be careful with ^ and $ as they match start/end of line, not string endings. If you want to match the whole string use: \A and \z. [link]

Macros for :symbol - can we avoid ""
Structs for vectors etc
