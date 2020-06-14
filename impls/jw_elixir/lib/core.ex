defmodule Core do
  @moduledoc """
  Provide the core environment for mal that holds all the pre-defined functions
  """

  # Functions defined in mal code
  @mal_prelude [
    "(def! not (fn* (a) (if a false true)))",
    "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\\nnil)\")))))",
    "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))",
    "(def! *host-language* \"jw_elixir\")"
  ]

  @doc """
  Create a new core environment. Intended to be called only once.
  """
  @spec new_env :: Env.t()
  def new_env do
    env = Env.new()

    Core.Atom.add(env)
    Core.HashMap.add(env)
    Core.IO.add(env)
    Core.Meta.add(env)
    Core.Misc.add(env)
    Core.Numeric.add(env)
    Core.Sequence.add(env)

    @mal_prelude
    |> Enum.each(&Eval.eval(Reader.read_str(&1), env))

    env
  end
end
