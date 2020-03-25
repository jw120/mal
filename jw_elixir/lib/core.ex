defmodule Core do

  @spec new_env :: Env.t()
  def new_env() do
    Env.new
    |> Env.set("+", {:function, &Core.add/1})
    |> Env.set("-", {:function, &Core.subtract/1})
    |> Env.set("*", {:function, &Core.multiply/1})
    |> Env.set("/", {:function, &Core.divide/1})
  end

  @spec add([Mal.t]) :: Mal.t
  def add([{:number, x}, {:number, y}]), do: {:number, x + y}
  def add(args), do: raise MalException, "Bad arguments to +: #{inspect(args)}"

  @spec subtract([Mal.t]) :: Mal.t
  def subtract([{:number, x}, {:number, y}]), do: {:number, x - y}
  def subtract(args), do: raise MalException, "Bad arguments to -: #{inspect(args)}"

  @spec multiply([Mal.t]) :: Mal.t
  def multiply([{:number, x}, {:number, y}]), do: {:number, x * y}
  def multiply(args), do: raise MalException, "Bad arguments to *: #{inspect(args)}"

  @spec divide([Mal.t]) :: Mal.t
  def divide([{:number, x}, {:number, y}]), do: {:number, div(x, y)}
  def divide(args), do: raise MalException, "Bad arguments to /: #{inspect(args)}"

end
