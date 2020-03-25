defmodule Core do
  @moduledoc """
  Provide the core environment for mal that holds all the pre-defined functions
  """

  @doc """
  Create a new core environment. Intended to be called only once.
  """
  @spec new_env :: Env.t()
  def new_env do
    env = Env.new()
    Env.set!(env, "+", {:function, &Core.add/1})
    Env.set!(env, "-", {:function, &Core.subtract/1})
    Env.set!(env, "*", {:function, &Core.multiply/1})
    Env.set!(env, "/", {:function, &Core.divide/1})
    Env.set!(env, ">", {:function, &Core.gt/1})
    Env.set!(env, ">=", {:function, &Core.ge/1})
    Env.set!(env, "<", {:function, &Core.lt/1})
    Env.set!(env, "<=", {:function, &Core.le/1})
    Env.set!(env, "prn", {:function, &Core.prn/1})
    Env.set!(env, "list", {:function, &Core.list/1})
    Env.set!(env, "list?", {:function, &Core.list?/1})
    Env.set!(env, "empty?", {:function, &Core.mal_empty?/1})
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

  @spec gt([Mal.t()]) :: Mal.t()
  def gt([{:number, x}, {:number, y}]), do: {:boolean, x > y}
  def gt(args), do: raise(MalException, "Bad arguments to >: #{inspect(args)}")

  @spec ge([Mal.t()]) :: Mal.t()
  def ge([{:number, x}, {:number, y}]), do: {:boolean, x >= y}
  def ge(args), do: raise(MalException, "Bad arguments to >=: #{inspect(args)}")

  @spec lt([Mal.t()]) :: Mal.t()
  def lt([{:number, x}, {:number, y}]), do: {:boolean, x < y}
  def lt(args), do: raise(MalException, "Bad arguments to <: #{inspect(args)}")

  @spec le([Mal.t()]) :: Mal.t()
  def le([{:number, x}, {:number, y}]), do: {:boolean, x <= y}
  def le(args), do: raise(MalException, "Bad arguments to <=: #{inspect(args)}")

  #  @spec mal_equal?([Mal.t()]) :: Mal.t()
  #  def mal_equal?([{:list, xs}, {:list, ys}]) do
  # def divide(args), do: raise(MalException, "Bad arguments to /: #{inspect(args)}")

  @spec prn([Mal.t()]) :: {nil}
  def prn([x | _]) do
    x
    |> Printer.pr_str(true)
    |> IO.puts()

    {nil}
  end

  def prn([]), do: raise(MalException, "No argument for prn")

  @spec list([Mal.t()]) :: Mal.t()
  def list(xs), do: {:list, xs}

  @spec list?([Mal.t()]) :: Mal.t()
  def list?({:list, _}), do: {:boolean, true}
  def list?(_), do: {:boolean, false}

  @spec mal_empty?({:list, [Mal.t()]}) :: {:boolean, boolean()}
  def mal_empty?({:list, []}), do: {:boolean, true}
  def mal_empty?({:list, _}), do: {:boolean, false}
  def mal_empty?(_), do: raise(MalException, "Non-list passed to empty?")

  @spec mal_count({:list, [Mal.t()]}) :: {:number, number()}
  def mal_count({:list, xs}), do: {:number, length(xs)}
  def mal_count(_), do: raise(MalException, "Non-list passed to count")
end
