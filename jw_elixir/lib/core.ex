defmodule Core do
  @moduledoc """
  Provide the core environment for mal that holds all the pre-defined functions
  """

  alias Core.Atom

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

    atom_pid = Atom.initialize()

    # Numeric and logical functions
    set_wrapped2!(env, "+", &Kernel.+/2)
    set_wrapped2!(env, "-", &Kernel.-/2)
    set_wrapped2!(env, "*", &Kernel.*/2)
    set_wrapped2!(env, "/", &Kernel.div/2)
    set_wrapped2!(env, ">", &Kernel.>/2)
    set_wrapped2!(env, ">=", &Kernel.>=/2)
    set_wrapped2!(env, "<", &Kernel.</2)
    set_wrapped2!(env, "<=", &Kernel.<=/2)

    # IO functions
    set_wrappedN!(env, "prn", &mal_prn/1)
    set_wrappedN!(env, "println", &mal_println/1)
    set_wrappedN!(env, "pr-str", &mal_pr_str/1)
    set_wrappedN!(env, "str", &mal_str/1)
    set_wrapped1!(env, "read-string", &mal_read_string/1)
    set_wrapped1!(env, "slurp", &mal_slurp/1)

    # Sequence functions
    set_wrappedN!(env, "list", &Function.identity/1)
    set_wrapped1!(env, "empty?", &mal_empty?/1)
    set_wrapped1!(env, "count", &mal_count/1)
    set_wrapped1!(env, "list?", &is_list/1)
    set_wrapped2!(env, "cons", &mal_cons/2)
    set_wrappedN!(env, "concat", &mal_concat/1)

    # Atom functions
    set_wrapped1!(env, "atom", &Atom.mal_atom(&1, atom_pid))
    set_wrapped1!(env, "atom?", &Atom.mal_atom?/1)
    set_wrapped1!(env, "deref", &Atom.mal_deref/1)
    set_wrapped2!(env, "reset!", &Atom.mal_reset!/2)
    set_wrappedN!(env, "swap!", &Atom.mal_swap!/1)

    # Other functions
    set_wrapped2!(env, "=", &mal_equal?/2)
    set_wrapped1!(env, "eval", &Eval.eval(&1, env))

    # Mal-defined functions
    @mal_prelude
    |> Enum.each(&Eval.eval(Reader.read_str(&1), env))

    env
  end

  # To save boilerplate we use wrapping functions to convert simple elixir functions
  # to functions which act on a list of mal types.

  # This adds a mal x mal -> mal function
  @spec set_wrapped2!(Env.t(), String.t(), (Mal.t(), Mal.t() -> Mal.t())) :: Env.t()
  defp set_wrapped2!(env, mal_name, f) do
    Env.set!(env, mal_name, %Mal.Function{
      closure: fn
        [x, y] -> f.(x, y)
        args -> raise(MalException, {"Bad arguments to " <> mal_name, args})
      end,
      is_macro: false
    })
  end

  # This adds a mal -> mal function
  @spec set_wrapped1!(Env.t(), String.t(), (Mal.t() -> Mal.t())) :: Env.t()
  defp set_wrapped1!(env, mal_name, f) do
    Env.set!(env, mal_name, %Mal.Function{
      closure: fn
        [x] -> f.(x)
        args -> raise(MalException, {"Bad arguments to " <> mal_name, args})
      end,
      is_macro: false
    })
  end

  # This adds a [mal] -> mal function
  @spec set_wrappedN!(Env.t(), String.t(), Mal.closure()) :: Env.t()
  defp set_wrappedN!(env, mal_name, f) do
    Env.set!(env, mal_name, %Mal.Function{closure: f, is_macro: false})
  end

  # IO functions

  @spec mal_prn(Mal.arguments()) :: nil
  defp mal_prn(xs) do
    xs
    |> Enum.map(&Printer.pr_str(&1, true))
    |> Enum.join(" ")
    |> IO.puts()

    nil
  end

  @spec mal_println(Mal.arguments()) :: nil
  defp mal_println(xs) do
    xs
    |> Enum.map(&Printer.pr_str(&1, false))
    |> Enum.join(" ")
    |> IO.puts()

    nil
  end

  @spec mal_pr_str(Mal.arguments()) :: String.t()
  defp mal_pr_str(xs) do
    xs
    |> Enum.map(&Printer.pr_str(&1, true))
    |> Enum.join(" ")
  end

  @spec mal_str(Mal.arguments()) :: String.t()
  defp mal_str(xs) do
    xs
    |> Enum.map(&Printer.pr_str(&1, false))
    |> Enum.join("")
  end

  @spec mal_slurp(String.t()) :: String.t()
  defp mal_slurp(file_name) do
    {:ok, file} = File.open(file_name, [:read])
    IO.read(file, :all)
  end

  @spec mal_read_string(String.t()) :: Mal.t()
  defp mal_read_string(s) do
    case Reader.read_str(s) do
      :void ->
        nil

      x ->
        x
    end
  end

  #
  # Sequence functions
  #

  @spec mal_empty?(Mal.t()) :: boolean()
  defp mal_empty?(xs) when is_list(xs), do: Enum.empty?(xs)
  defp mal_empty?({:vector, v}), do: Enum.empty?(v)
  defp mal_empty?(args), do: raise(MalException, "empty? takes a sequence : #{inspect(args)}")

  @spec mal_count(Mal.t()) :: number()
  defp mal_count(xs) when is_list(xs), do: length(xs)
  defp mal_count({:vector, xs}), do: map_size(xs)
  defp mal_count(nil), do: 0
  defp mal_count(args), do: raise(MalException, "count takes a sequence: #{inspect(args)}")

  @spec mal_cons(Mal.t(), Mal.t()) :: [Mal.t()]
  def mal_cons(x, xs) when is_list(xs), do: [x | xs]
  def mal_cons(x, {:vector, v}), do: [x | Seq.vector_to_list(v)]
  def mal_cons(x, y), do: raise(MalException, "bad args for cons: #{inspect(x)} #{inspect(y)}")

  @spec mal_concat(Mal.arguments()) :: [Mal.t()]
  def mal_concat(args) when is_list(args) do
    args
    |> Enum.map(fn
      xs when is_list(xs) -> xs
      {:vector, v} -> Seq.vector_to_list(v)
      other -> raise(MalException, "concat arguments must be lists: #{other}")
    end)
    |> Enum.concat()
  end

  #
  # Other functions
  #

  @spec mal_equal?(Mal.t(), Mal.t()) :: boolean()
  defp mal_equal?(xs, ys) when is_list(xs) and is_list(ys), do: list_equal?(xs, ys)
  defp mal_equal?(xs, {:vector, ys}) when is_list(xs), do: list_equal?(xs, Seq.vector_to_list(ys))
  defp mal_equal?({:vector, xs}, ys) when is_list(ys), do: list_equal?(Seq.vector_to_list(xs), ys)

  defp mal_equal?({:vector, xs}, {:vector, ys}),
    do: list_equal?(Seq.vector_to_list(xs), Seq.vector_to_list(ys))

  defp mal_equal?(a, b), do: a == b
  defp list_equal?([x | xs], [y | ys]), do: mal_equal?(x, y) && list_equal?(xs, ys)
  defp list_equal?([], []), do: true
  defp list_equal?(_, _), do: false
end
