defmodule Core do
  @moduledoc """
  Provide the core environment for mal that holds all the pre-defined functions
  """

  @doc """
  Create a new core environment. Intended to be called only once.
  """
  @spec new_env :: Env.t()
  def new_env() do
    env = Env.new()
    Env.set!(env, "+", {:function, &Core.add/1})
    Env.set!(env, "-", {:function, &Core.subtract/1})
    Env.set!(env, "*", {:function, &Core.multiply/1})
    Env.set!(env, "/", {:function, &Core.divide/1})
    env
  end

  @spec add([Mal.t()]) :: Mal.t()
  def add([{:number, x}, {:number, y}]), do: {:number, x + y}
  def add(args), do: raise(MalException, "Bad arguments to +: #{inspect(args)}")

  @spec subtract([Mal.t()]) :: Mal.t()
  def subtract([{:number, x}, {:number, y}]), do: {:number, x - y}
  def subtract(args), do: raise(MalException, "Bad arguments to -: #{inspect(args)}")

  @spec multiply([Mal.t()]) :: Mal.t()
  def multiply([{:number, x}, {:number, y}]), do: {:number, x * y}
  def multiply(args), do: raise(MalException, "Bad arguments to *: #{inspect(args)}")

  @spec divide([Mal.t()]) :: Mal.t()
  def divide([{:number, x}, {:number, y}]), do: {:number, div(x, y)}
  def divide(args), do: raise(MalException, "Bad arguments to /: #{inspect(args)}")
end
