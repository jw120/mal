defmodule Core do
  @moduledoc """
  Provide the core environment for mal that holds all the pre-defined functions
  """

  alias Core.Atom

  # Functions defined in mal code
  @mal_prelude [
    "(def! not (fn* (a) (if a false true)))",
    "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\\nnil)\")))))",
    "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))"
  ]

  @doc """
  Create a new core environment. Intended to be called only once.
  """
  @spec new_env :: Env.t()
  def new_env do
    env = Env.new()

    # Start the agent for our atoms and capture its pid (for use in mal_atom below)
    agent_pid = Atom.initialize()

    # Numeric and logical functions
    set_wrapped2!(env, "+", fn x, y when is_integer(x) and is_integer(y) -> x + y end)
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
    set_wrappedN!(env, "list", &%Mal.List{contents: &1})
    set_wrapped1!(env, "empty?", &mal_empty?/1)
    set_wrapped1!(env, "count", &mal_count/1)
    set_wrapped1!(env, "list?", &mal_list?/1)
    set_wrapped2!(env, "cons", &mal_cons/2)
    set_wrappedN!(env, "concat", &mal_concat/1)
    set_wrapped1!(env, "first", &mal_first/1)
    set_wrapped1!(env, "rest", &mal_rest/1)
    set_wrapped2!(env, "nth", &mal_nth/2)

    # Atom functions
    set_wrapped1!(env, "atom", &Atom.mal_atom(&1, agent_pid))
    set_wrapped1!(env, "atom?", &Atom.mal_atom?/1)
    set_wrapped1!(env, "deref", &Atom.mal_deref/1)
    set_wrapped2!(env, "reset!", &Atom.mal_reset!/2)
    set_wrappedN!(env, "swap!", &Atom.mal_swap!/1)

    # Other functions
    set_wrapped2!(env, "=", &mal_equal?/2)
    set_wrapped1!(env, "eval", &Eval.eval(&1, env))
    set_wrapped1!(env, "throw", &raise(MalException, &1))
    set_wrapped1!(env, "nil?", &mal_nil?/1)
    set_wrapped1!(env, "true?", &mal_true?/1)
    set_wrapped1!(env, "false?", &mal_false?/1)
    set_wrapped1!(env, "symbol?", &mal_symbol?/1)


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
      closure: fn [x, y] -> f.(x, y) end,
      name: mal_name,
      is_macro: false
    })
  end

  # This adds a mal -> mal function
  @spec set_wrapped1!(Env.t(), String.t(), (Mal.t() -> Mal.t())) :: Env.t()
  defp set_wrapped1!(env, mal_name, f) do
    Env.set!(env, mal_name, %Mal.Function{
      closure: fn [x] -> f.(x) end,
      name: mal_name,
      is_macro: false
    })
  end

  # This adds a [mal] -> mal function
  @spec set_wrappedN!(Env.t(), String.t(), Mal.closure()) :: Env.t()
  defp set_wrappedN!(env, mal_name, f) do
    Env.set!(env, mal_name, %Mal.Function{
      closure: f,
      name: mal_name,
      is_macro: false
    })
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

  @spec mal_empty?(Mal.List.t() | Mal.Vector.t()) :: boolean()
  defp mal_empty?(%Mal.List{contents: xs}), do: Enum.empty?(xs)
  defp mal_empty?(%Mal.Vector{vector_map: v}), do: Enum.empty?(v)

  @spec mal_count(Mal.List.t() | Mal.Vector.t() | nil) :: number()
  defp mal_count(%Mal.List{contents: xs}), do: length(xs)
  defp mal_count(%Mal.Vector{vector_map: v}), do: map_size(v)
  defp mal_count(nil), do: 0

  @spec mal_list?(Mal.t()) :: boolean()
  defp mal_list?(%Mal.List{}), do: true
  defp mal_list?(_), do: false

  @spec mal_cons(Mal.t(), Mal.List.t() | Mal.Vector.t()) :: Mal.t()
  def mal_cons(x, %Mal.List{contents: xs}), do: %Mal.List{contents: [x | xs]}

  def mal_cons(x, %Mal.Vector{vector_map: v}),
    do: %Mal.List{contents: [x | Seq.vector_map_to_list(v)]}

  @spec mal_concat(Mal.arguments()) :: Mal.t()
  def mal_concat(args) do
    args
    |> Enum.map(fn
      %Mal.List{contents: xs} ->
        xs

      %Mal.Vector{vector_map: v} ->
        Seq.vector_map_to_list(v)
    end)
    |> Enum.concat()
    |> (fn xs -> %Mal.List{contents: xs} end).()
  end

  @spec mal_first(nil | Mal.List.t() | Mal.Vector.t()) :: Mal.t()
  def mal_first(%Mal.List{contents: xs}), do: List.first(xs)
  def mal_first(%Mal.Vector{vector_map: v}) when map_size(v) == 0, do: nil
  def mal_first(%Mal.Vector{vector_map: v}), do: v[0]
  def mal_first(nil), do: nil

  @spec mal_rest(nil | Mal.List.t() | Mal.Vector.t()) :: Mal.List.t()
  def mal_rest(%Mal.List{contents: []}), do: %Mal.List{contents: []}
  def mal_rest(%Mal.List{contents: [_ | xs]}), do: %Mal.List{contents: xs}

  def mal_rest(%Mal.Vector{vector_map: v}),
    do: mal_rest(%Mal.List{contents: Seq.vector_map_to_list(v)})

  def mal_rest(nil), do: %Mal.List{contents: []}

  @spec mal_nth(Mal.List.t() | Mal.Vector.t(), non_neg_integer()) :: Mal.t()
  def mal_nth(%Mal.List{contents: xs}, i) do
    case Enum.fetch(xs, i) do
      {:ok, x} -> x
      :error -> raise MalException, "Index out of range"
    end
  end

  def mal_nth(%Mal.Vector{vector_map: v}, i) do
    case Map.fetch(v, i) do
      {:ok, x} -> x
      :error -> raise MalException, "Index out of range"
    end
  end

  #
  # Other functions
  #

  # Equality that treats lists and vectors as equal
  @spec mal_equal?(Mal.t(), Mal.t()) :: boolean()
  defp mal_equal?(%Mal.List{contents: xs}, %Mal.List{contents: ys}),
    do: list_equal?(xs, ys)

  defp mal_equal?(%Mal.List{contents: xs}, %Mal.Vector{vector_map: ys}),
    do: list_equal?(xs, Seq.vector_map_to_list(ys))

  defp mal_equal?(%Mal.Vector{vector_map: xs}, %Mal.List{contents: ys}),
    do: list_equal?(Seq.vector_map_to_list(xs), ys)

  defp mal_equal?(%Mal.Vector{vector_map: xs}, %Mal.Vector{vector_map: ys}),
    do: list_equal?(Seq.vector_map_to_list(xs), Seq.vector_map_to_list(ys))

  defp mal_equal?(a, b),
    do: a == b

  defp list_equal?([x | xs], [y | ys]), do: mal_equal?(x, y) && list_equal?(xs, ys)
  defp list_equal?([], []), do: true
  defp list_equal?(_, _), do: false

  @spec mal_nil?(Mal.t()) :: boolean()
  defp mal_nil?(nil), do: true
  defp mal_nil?(_), do: false

  @spec mal_true?(Mal.t()) :: boolean()
  defp mal_true?(true), do: true
  defp mal_true?(_), do: false

  @spec mal_false?(Mal.t()) :: boolean()
  defp mal_false?(false), do: true
  defp mal_false?(_), do: false

  @spec mal_symbol?(Mal.t()) :: boolean()
  defp mal_symbol?({:symbol, _}), do: true
  defp mal_symbol?(_), do: false

end
