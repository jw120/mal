defmodule Core do
  @moduledoc """
  Provide the core environment for mal that holds all the pre-defined functions
  """

  # Functions defined in mal code
  @mal_prelude [
    "(def! not (fn* (a) (if a false true)))",
    ~S/(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))/
  ]

  @doc """
  Create a new core environment. Intended to be called only once.
  """
  @spec new_env :: Env.t()
  def new_env do
    env = Env.new()

    # Numeric and logical functions
    set_wrapped_int2!(env, "+", &{:number, &1 + &2})
    set_wrapped_int2!(env, "-", &{:number, &1 - &2})
    set_wrapped_int2!(env, "*", &{:number, &1 * &2})
    set_wrapped_int2!(env, "/", &{:number, div(&1, &2)})
    set_wrapped_int2!(env, ">", &{:boolean, &1 > &2})
    set_wrapped_int2!(env, ">=", &{:boolean, &1 >= &2})
    set_wrapped_int2!(env, "<", &{:boolean, &1 < &2})
    set_wrapped_int2!(env, "<=", &{:boolean, &1 <= &2})

    # IO functions
    Env.set!(env, "prn", {:function, &mal_prn/1})
    Env.set!(env, "println", {:function, &mal_println/1})
    Env.set!(env, "pr-str", {:function, &mal_pr_str/1})
    Env.set!(env, "str", {:function, &mal_str/1})
    set_wrapped_str1!(env, "read-string", &mal_read_string/1)
    set_wrapped_str1!(env, "slurp", &mal_slurp/1)

    # Sequence functions
    Env.set!(env, "list", {:function, &{:list, &1}})
    Env.set!(env, "empty?", {:function, &mal_empty?/1})
    Env.set!(env, "count", {:function, &mal_count/1})
    set_wrapped_mal1!(env, "list?", &{:boolean, mal_list?(&1)})

    # Other functions
    set_wrapped_mal2_bool!(env, "=", &mal_equal?/2)
    set_wrapped_mal1!(env, "eval", &Eval.eval(&1, env))

    # Mal-defined functions
    @mal_prelude
    |> Enum.each(&Eval.eval(Reader.read_str(&1), env))

    env
  end

  # To save boilerplate we use wrapping functions to convert simple elixir functions
  # to functions which act on a list of mal types.

  # Add to the environment a wrapped version of an int x int -> mal function
  @spec set_wrapped_int2!(Env.t(), String.t(), (integer(), integer() -> Mal.t())) :: Env.t()
  defp set_wrapped_int2!(env, mal_name, f) do
    Env.set!(
      env,
      mal_name,
      {:function,
       fn
         [{:number, x}, {:number, y}] -> f.(x, y)
         args -> raise(MalException, {"Bad arguments to " <> mal_name, args})
       end}
    )
  end

  # This adds a mal x mal -> bool function (used for mal_equal?)
  @spec set_wrapped_mal2_bool!(Env.t(), String.t(), (Mal.t(), Mal.t() -> boolean())) :: Env.t()
  defp set_wrapped_mal2_bool!(env, mal_name, f) do
    Env.set!(
      env,
      mal_name,
      {:function,
       fn
         [x, y] -> {:boolean, f.(x, y)}
         args -> raise(MalException, {"Bad arguments to " <> mal_name, args})
       end}
    )
  end

  # This adds a mal -> mal function
  @spec set_wrapped_mal1!(Env.t(), String.t(), (Mal.t() -> Mal.t())) :: Env.t()
  defp set_wrapped_mal1!(env, mal_name, f) do
    Env.set!(
      env,
      mal_name,
      {:function,
       fn
         [x] -> f.(x)
         args -> raise(MalException, {"Bad arguments to " <> mal_name, args})
       end}
    )
  end

  # This adds a string -> mal function
  @spec set_wrapped_str1!(Env.t(), String.t(), (String.t() -> Mal.t())) :: Env.t()
  defp set_wrapped_str1!(env, mal_name, f) do
    Env.set!(
      env,
      mal_name,
      {:function,
       fn
         [{:string, s}] -> f.(s)
         args -> raise(MalException, {"Bad arguments to " <> mal_name, args})
       end}
    )
  end

  # IO functions

  @spec mal_prn([Mal.t()]) :: {nil}
  defp mal_prn(xs) do
    xs
    |> Enum.map(&Printer.pr_str(&1, true))
    |> Enum.join(" ")
    |> IO.puts()

    {nil}
  end

  @spec mal_println([Mal.t()]) :: {nil}
  defp mal_println(xs) do
    xs
    |> Enum.map(&Printer.pr_str(&1, false))
    |> Enum.join(" ")
    |> IO.puts()

    {nil}
  end

  @spec mal_pr_str([Mal.t()]) :: {:string, String.t()}
  defp mal_pr_str(xs) do
    xs
    |> Enum.map(&Printer.pr_str(&1, true))
    |> Enum.join(" ")
    |> (fn s -> {:string, s} end).()
  end

  @spec mal_str([Mal.t()]) :: {:string, String.t()}
  defp mal_str(xs) do
    xs
    |> Enum.map(&Printer.pr_str(&1, false))
    |> Enum.join("")
    |> (fn s -> {:string, s} end).()
  end

  @spec mal_slurp(String.t()) :: {:string, String.t()}
  defp mal_slurp(file_name) do
    {:ok, file} = File.open(file_name, [:read])
    contents = IO.read(file, :all)
    {:string, contents}
  end

  @spec mal_read_string(String.t()) :: Mal.t()
  defp mal_read_string(s) do
    case Reader.read_str(s) do
      {:void} ->
        {nil}

      x ->
        x
    end
  end

  #
  # Sequence functions
  #

  #  @spec list([Mal.t()]) :: Mal.t()
  # def list(xs), do: {:list, xs}

  @spec mal_list?(Mal.t()) :: boolean()
  defp mal_list?({:list, _}), do: true
  defp mal_list?(_), do: false

  @spec mal_empty?([{:list, [Mal.t()]}]) :: {:boolean, boolean()}
  defp mal_empty?([{:list, xs}]), do: {:boolean, Enum.empty?(xs)}
  defp mal_empty?([{:vector, xs}]), do: {:boolean, Enum.empty?(xs)}

  defp mal_empty?(args),
    do: raise(MalException, "empty? expects one sequence argument: #{inspect(args)}")

  @spec mal_count([{:list, [Mal.t()]} | {nil}]) :: {:number, number()}
  defp mal_count([{:list, xs}]), do: {:number, length(xs)}
  defp mal_count([{:vector, xs}]), do: {:number, map_size(xs)}
  defp mal_count([{nil}]), do: {:number, 0}

  defp mal_count(args),
    do: raise(MalException, "count expects one sequence argument: #{inspect(args)}")

  #
  # Other functions
  #

  @spec mal_equal?(Mal.t(), Mal.t()) :: boolean()
  defp mal_equal?({:list, xs}, {:list, ys}), do: list_equal?(xs, ys)
  defp mal_equal?({:list, xs}, {:vector, ys}), do: list_equal?(xs, Seq.vector_to_list(ys))
  defp mal_equal?({:vector, xs}, {:list, ys}), do: list_equal?(Seq.vector_to_list(xs), ys)

  defp mal_equal?({:vector, xs}, {:vector, ys}),
    do: list_equal?(Seq.vector_to_list(xs), Seq.vector_to_list(ys))

  defp mal_equal?(a, b), do: a == b
  defp list_equal?([x | xs], [y | ys]), do: mal_equal?(x, y) && list_equal?(xs, ys)
  defp list_equal?([], []), do: true
  defp list_equal?(_, _), do: false
end
