defmodule Core.Meta do
  @moduledoc """
  Provide meta functionality for the core environment.

  """

  import Core.Helpers

  @doc """
  Add functions for this module to the environment
  """
  def add(env) do
    wrap1(env, "meta", fn
      %Mal.Function{meta: m} -> m
      %Mal.List{meta: m} -> m
      %Mal.Vector{meta: m} -> m
      %Mal.HashMap{meta: m} -> m
      %Mal.Atom{meta: m} -> m
    end)

    wrap2(env, "with-meta", fn x, y ->
      case x do
        %Mal.Function{} -> %{x | meta: y}
        %Mal.List{} -> %{x | meta: y}
        %Mal.Vector{} -> %{x | meta: y}
        %Mal.HashMap{} -> %{x | meta: y}
        %Mal.Atom{} -> %{x | meta: y}
      end
    end)
  end
end
