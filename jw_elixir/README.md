# Elixir implementation of mal



Choices
 - Vectors as maps
 - Environments with ETS
 - Atoms with agent (as cross-process)
 - Nothing done for TCO (as elixir does automatically?)

  Be careful with ^ and $ as they match start/end of line, not string endings. If you want to match the whole string use: \A and \z. [link]

Should we use native types instead of :list, :string etc
