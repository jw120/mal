TODO

Should we be using '!' as a suffic for functions that mutate the environment - elixir convetion is ! for functions that throw

Can we have a tigher spec
  @spec def_form(Mal.arguments(), Env.t()) :: Mal.t()
  def def_form([sym(s), val], env) do