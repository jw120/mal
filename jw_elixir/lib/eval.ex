defmodule Eval do
  @moduledoc """
  Mal evaluation
  """

  @doc """
  Evaludates a mal expression
  """
  @spec eval(Mal.t, Env.t) :: Mal.t
  def eval(x, _env) do
    x
  end

end


