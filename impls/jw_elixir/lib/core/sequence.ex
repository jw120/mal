defmodule Core.Sequence do
  @moduledoc """
  Provide list and vector functionality for the core environment.
  """

  import Core.Helpers

  @doc """
  Add functions for this module to the environment
  """
  @spec add(Env.t()) :: Env.t()
  def add(env) do
    wrap_list(env, "list", &%Mal.List{contents: &1})

    wrap1(env, "empty?", fn
      %Mal.List{contents: xs} -> Enum.empty?(xs)
      %Mal.Vector{vector_map: v} -> Enum.empty?(v)
    end)

    wrap1(env, "count", fn
      %Mal.List{contents: xs} -> length(xs)
      %Mal.Vector{vector_map: v} -> map_size(v)
      nil -> 0
    end)

    wrap1(env, "list?", fn
      %Mal.List{} -> true
      _ -> false
    end)

    wrap2(env, "cons", fn
      x, %Mal.List{contents: xs} -> %Mal.List{contents: [x | xs]}
      x, %Mal.Vector{vector_map: v} -> %Mal.List{contents: [x | Seq.vector_map_to_list(v)]}
    end)

    wrap_list(env, "concat", fn args ->
      args
      |> Enum.map(fn
        %Mal.List{contents: xs} ->
          xs

        %Mal.Vector{vector_map: v} ->
          Seq.vector_map_to_list(v)
      end)
      |> Enum.concat()
      |> (fn xs -> %Mal.List{contents: xs} end).()
    end)

    wrap1(env, "first", fn
      %Mal.List{contents: xs} -> List.first(xs)
      %Mal.Vector{vector_map: []} -> nil
      %Mal.Vector{vector_map: v} -> v[0]
      nil -> nil
    end)

    wrap1(env, "rest", &mal_rest/1)

    wrap2(env, "nth", &mal_nth/2)

    wrap1(env, "vec", fn
      %Mal.List{contents: xs} -> %Mal.Vector{vector_map: Seq.list_to_vector_map(xs)}
      %Mal.Vector{vector_map: v} -> %Mal.Vector{vector_map: v}
    end)

    wrap_list(env, "vector", fn
      xs when is_list(xs) -> %Mal.Vector{vector_map: Seq.list_to_vector_map(xs)}
    end)

    wrap1(env, "vector?", fn
      %Mal.Vector{} -> true
      _ -> false
    end)

    wrap1(env, "sequential?", fn
      %Mal.Vector{} -> true
      %Mal.List{} -> true
      _ -> false
    end)

    wrap1(env, "seq", fn
      %Mal.List{contents: []} -> nil
      %Mal.Vector{vector_map: v} when map_size(v) == 0 -> nil
      "" -> nil
      nil -> nil
      %Mal.List{contents: xs} -> %Mal.List{contents: xs}
      %Mal.Vector{vector_map: v} -> %Mal.List{contents: Seq.vector_map_to_list(v)}
      s when is_bitstring(s) -> %Mal.List{contents: String.graphemes(s)}
    end)

    wrap_list(env, "conj", fn
      [%Mal.List{contents: xs} | args] ->
        %Mal.List{contents: Enum.reverse(args) ++ xs}

      [%Mal.Vector{vector_map: v} | args] ->
        %Mal.Vector{vector_map: Seq.list_to_vector_map(Seq.vector_map_to_list(v) ++ args)}
    end)
  end

  @spec mal_rest(nil | Mal.List.t() | Mal.Vector.t()) :: Mal.List.t()
  defp mal_rest(%Mal.List{contents: []}), do: %Mal.List{contents: []}
  defp mal_rest(%Mal.List{contents: [_ | xs]}), do: %Mal.List{contents: xs}

  defp mal_rest(%Mal.Vector{vector_map: v}),
    do: mal_rest(%Mal.List{contents: Seq.vector_map_to_list(v)})

  defp mal_rest(nil), do: %Mal.List{contents: []}

  @spec mal_nth(Mal.List.t() | Mal.Vector.t(), non_neg_integer()) :: Mal.t()
  defp mal_nth(%Mal.List{contents: xs}, i) do
    case Enum.fetch(xs, i) do
      {:ok, x} -> x
      :error -> raise MalException, "Index out of range"
    end
  end

  defp mal_nth(%Mal.Vector{vector_map: v}, i) do
    case Map.fetch(v, i) do
      {:ok, x} -> x
      :error -> raise MalException, "Index out of range"
    end
  end
end
