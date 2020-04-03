defmodule Core.Misc do
  @moduledoc """
  Provide miscellaneous functionality for the core environment.
  """

  import Core.Helpers

  @doc """
  Add functions for this module to the environment
  """
  def add(env) do
    wrap2(env, "=", &mal_equal?/2)
    wrap1(env, "eval", &Eval.eval(&1, env))
    wrap1(env, "throw", &raise(MalException, &1))
    wrapN(env, "apply", &mal_apply/1)

    wrap2(env, "map", fn
      %Mal.Function{closure: f}, %Mal.List{contents: xs} ->
        %Mal.List{contents: Enum.map(xs, fn x -> f.([x]) end)}

      %Mal.Function{closure: f}, %Mal.Vector{vector_map: v} ->
        %Mal.List{contents: Enum.map(v, fn {_i, x} -> f.([x]) end)}
    end)

    wrap1(env, "nil?", fn
      nil -> true
      _ -> false
    end)

    wrap1(env, "true?", fn
      true -> true
      _ -> false
    end)

    wrap1(env, "false?", fn
      false -> true
      _ -> false
    end)

    wrap1(env, "symbol?", fn
      {:symbol, _} -> true
      _ -> false
    end)

    wrap1(env, "symbol", fn
      s when is_bitstring(s) -> {:symbol, s}
    end)

    wrap1(env, "keyword", fn
      s when is_bitstring(s) -> {:keyword, s}
      {:keyword, k} -> {:keyword, k}
    end)

    wrap1(env, "keyword?", fn
      {:keyword, _} -> true
      _ -> false
    end)

    wrap1(env, "fn?", fn
      %Mal.Function{is_macro: false} -> true
      _ -> false
    end)

    wrap1(env, "macro?", fn
      %Mal.Function{is_macro: true} -> true
      _ -> false
    end)

    wrap1(env, "string?", fn
      x when is_bitstring(x) -> true
      _ -> false
    end)

    wrap1(env, "number?", fn
      x when is_integer(x) -> true
      _ -> false
    end)

    wrapN(env, "time-ms", fn [] -> System.system_time(:millisecond) end)

  end

  @spec mal_equal?(Mal.t(), Mal.t()) :: boolean()
  defp mal_equal?(%Mal.List{contents: xs}, %Mal.List{contents: ys}),
    do: list_equal?(xs, ys)

  defp mal_equal?(%Mal.List{contents: xs}, %Mal.Vector{vector_map: ys}),
    do: list_equal?(xs, Seq.vector_map_to_list(ys))

  defp mal_equal?(%Mal.Vector{vector_map: xs}, %Mal.List{contents: ys}),
    do: list_equal?(Seq.vector_map_to_list(xs), ys)

  defp mal_equal?(%Mal.Vector{vector_map: xs}, %Mal.Vector{vector_map: ys}),
    do: list_equal?(Seq.vector_map_to_list(xs), Seq.vector_map_to_list(ys))

  defp mal_equal?(%Mal.HashMap{hashmap_map: m1}, %Mal.HashMap{hashmap_map: m2}) do
    map_size(m1) == map_size(m2) and
      Enum.all?(m1, fn
        {k1, v1} -> Map.has_key?(m2, k1) && mal_equal?(v1, Map.get(m2, k1))
      end)
  end

  defp mal_equal?(a, b),
    do: a == b

  defp list_equal?([x | xs], [y | ys]), do: mal_equal?(x, y) && list_equal?(xs, ys)
  defp list_equal?([], []), do: true
  defp list_equal?(_, _), do: false

  @spec mal_apply(Mal.arguments()) :: Mal.t()
  defp mal_apply([%Mal.Function{closure: f} | args]) do
    last_arg =
      case List.last(args) do
        %Mal.List{contents: xs} -> xs
        %Mal.Vector{vector_map: v} -> Seq.vector_map_to_list(v)
        nil -> []
        _ -> raise "Last argument to apply must be a sequece"
      end

    other_args = Enum.take(args, length(args) - 1)
    apply(f, [other_args ++ last_arg])
  end
end
