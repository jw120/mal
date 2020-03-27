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
    Env.set!(env, "+", {:function, wrap_int2_int(&(&1 + &2), "+")})
    Env.set!(env, "-", {:function, wrap_int2_int(&(&1 - &2), "-")})
    Env.set!(env, "*", {:function, wrap_int2_int(&(&1 * &2), "*")})
    Env.set!(env, "/", {:function, wrap_int2_int(&(div(&1, &2)), "/")})
    Env.set!(env, ">", {:function, wrap_int2_bool(&(&1 > &2), ">")})
    Env.set!(env, ">=", {:function, wrap_int2_bool(&(&1 >= &2), ">=")})
    Env.set!(env, "<", {:function, wrap_int2_bool(&(&1 < &2), "<")})
    Env.set!(env, "<=", {:function, wrap_int2_bool(&(&1 <= &2), "<=")})
    Env.set!(env, "=", {:function, wrap_mal2_bool(&mal_equal?/2, "=")})
    Env.set!(env, "prn", {:function, &Core.prn/1})
    Env.set!(env, "list", {:function, &Core.list/1})
    Env.set!(env, "list?", {:function, &Core.list?/1})
    Env.set!(env, "empty?", {:function, &Core.mal_empty?/1})
    Env.set!(env, "count", {:function, &Core.mal_count/1})
    Eval.eval(Reader.read_str("(def! not (fn* (a) (if a false true)))"), env)
    env
  end

  # To save boilerplate we use wrapping functions to convert simple elixir functions
  # to functions which act on a list of mal types

  @typep int2_int :: (integer(), integer() -> integer())
  @spec wrap_int2_int(int2_int, String.t()) :: Mal.closure()
  defp wrap_int2_int(f, mal_name) do
    fn
      [{:number, x}, {:number, y}] -> {:number, f.(x, y)}
      args -> raise(MalException, "Bad arguments to " <> mal_name <> ": #{inspect(args)}")
    end
  end

  @typep int2_bool :: (boolean(), boolean() -> boolean())
  @spec wrap_int2_bool(int2_bool, String.t()) :: Mal.closure()
  defp wrap_int2_bool(f, mal_name) do
    fn
      [{:number, x}, {:number, y}] -> {:boolean, f.(x, y)}
      args -> raise(MalException, "Bad arguments to " <> mal_name <> ": #{inspect(args)}")
    end
  end

  @typep mal2_bool :: (Mal.t(), Mal.t() -> boolean())
  @spec wrap_mal2_bool(mal2_bool, String.t()) :: Mal.closure()
  defp wrap_mal2_bool(f, mal_name) do
    fn
      [x, y] -> {:boolean, f.(x, y)}
      args -> raise(MalException, "Bad arguments to " <> mal_name <> ": #{inspect(args)}")
    end
  end

  @spec mal_equal?(Mal.t(), Mal.t()) :: boolean()
  def mal_equal?({:list, xs}, {:list, ys}), do: list_equal?(xs, ys)
  def mal_equal?({:list, xs}, {:vector, ys}), do: list_equal?(xs, Seq.vector_to_list(ys))
  def mal_equal?({:vector, xs}, {:list, ys}), do: list_equal?(Seq.vector_to_list(xs), ys)
  def mal_equal?({:vector, xs}, {:vector, ys}), do: list_equal?(Seq.vector_to_list(xs), Seq.vector_to_list(ys))
  def mal_equal?(a, b), do: a == b
  defp list_equal?([x | xs], [y | ys]), do: mal_equal?(x, y) && list_equal?(xs, ys)
  defp list_equal?([], []), do: true
  defp list_equal?(_, _), do: false

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
  def list?([{:list, _}]), do: {:boolean, true}
  def list?([_]), do: {:boolean, false}
  def list?(args), do: raise(MalException, "list? expects one argument: : #{inspect(args)}")

  @spec mal_empty?([{:list, [Mal.t()]}]) :: {:boolean, boolean()}
  def mal_empty?([{:list, xs}]), do: {:boolean, Enum.empty?(xs)}
  def mal_empty?([{:vector, xs}]), do: {:boolean, Enum.empty?(xs)}
  def mal_empty?(args), do: raise(MalException, "empty? expects one sequence argument: #{inspect(args)}")

  @spec mal_count([{:list, [Mal.t()]} | {:nil}]) :: {:number, number()}
  def mal_count([{:list, xs}]), do: {:number, length(xs)}
  def mal_count([{:vector, xs}]), do: {:number, map_size(xs)}
  def mal_count([{:nil}]), do: {:number, 0}
  def mal_count(args), do: raise(MalException, "count expects one sequence argument: #{inspect(args)}")
end
