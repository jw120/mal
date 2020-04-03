defmodule Core.Numeric do
  @moduledoc """
  Provide numeric functionality for the core environment.
  """

  import Core.Helpers

  @doc """
  Add functions for this module to the environment
  """
  def add(env) do
    wrap2(env, "+", fn x, y when is_integer(x) and is_integer(y) -> x + y end)
    wrap2(env, "-", &Kernel.-/2)
    wrap2(env, "*", &Kernel.*/2)
    wrap2(env, "/", &Kernel.div/2)
    wrap2(env, ">", &Kernel.>/2)
    wrap2(env, ">=", &Kernel.>=/2)
    wrap2(env, "<", &Kernel.</2)
    wrap2(env, "<=", &Kernel.<=/2)
  end
end
