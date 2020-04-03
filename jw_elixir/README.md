# Elixir implementation of mal

TODO - review docs

Choices
 - Vectors as maps
 - Structs for list/map/vector (so we can have meta)
 - Environments with ETS
 - Atoms with agent (as cross-process)
 - Nothing done for TCO (as elixir does automatically?)
 - Ambiguity between [Mal] as a Mal list type and also a list of Mal types
 - Rely on elixir pattern matching to avoid type checking/raise everywhere

  Be careful with ^ and $ as they match start/end of line, not string endings. If you want to match the whole string use: \A and \z. [link]

Macros for :symbol - can we avoid ""
macro for %Mal.List
macro for set_wrapped_match(pattern)

Simplify core - more lambdas?

Can we have a tigher spec
  @spec def_form(Mal.arguments(), Env.t()) :: Mal.t()
  def def_form([sym(s), val], env) do