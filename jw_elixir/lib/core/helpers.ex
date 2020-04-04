defmodule Core.Helpers do
  @moduledoc """
  Provide helper functions to streamline the binding of elixir-defined funtions into a mal
  environment
  """

  @doc """
  Adds a mal -> mal function to the environment
  """
  @spec wrap1(Env.t(), String.t(), (Mal.t() -> Mal.t())) :: Env.t()
  def wrap1(env, mal_name, f) do
    Env.set!(env, mal_name, %Mal.Function{
      closure: fn [x] -> f.(x) end,
      name: mal_name,
      is_macro: false
    })
  end

  @doc """
  Adds a mal x mal -> mal function to the environment
  """
  @spec wrap2(Env.t(), String.t(), (Mal.t(), Mal.t() -> Mal.t())) :: Env.t()
  def wrap2(env, mal_name, f) do
    Env.set!(env, mal_name, %Mal.Function{
      closure: fn [x, y] -> f.(x, y) end,
      name: mal_name,
      is_macro: false
    })
  end

  @doc """
  Adds a [mal] -> mal function to the environment
  """
  @spec wrap_list(Env.t(), String.t(), Mal.closure()) :: Env.t()
  def wrap_list(env, mal_name, f) do
    Env.set!(env, mal_name, %Mal.Function{
      closure: f,
      name: mal_name,
      is_macro: false
    })
  end
end
