defmodule Core.HashMap do
  @moduledoc """
  Provide hashmap functionality for the core environment.
  """

  import Core.Helpers

  @doc """
  Add functions for this module to the environment
  """
  def add(env) do
    wrapN(env, "hash-map", fn
      args when is_list(args) -> %Mal.HashMap{hashmap_map: Seq.list_to_hashmap_map(args)}
    end)

    wrap1(env, "map?", fn
      %Mal.HashMap{} -> true
      _ -> false
    end)

    wrap2(env, "get", fn
      %Mal.HashMap{hashmap_map: m}, k -> Map.get(m, k)
      nil, _k -> nil
    end)

    wrap2(env, "contains?", fn
      %Mal.HashMap{hashmap_map: m}, k -> Map.has_key?(m, k)
      nil, _k -> false
    end)

    wrap1(env, "keys", fn
      %Mal.HashMap{hashmap_map: m} -> %Mal.List{contents: Map.keys(m)}
      nil -> %Mal.List{contents: []}
    end)

    wrap1(env, "vals", fn
      %Mal.HashMap{hashmap_map: m} -> %Mal.List{contents: Map.values(m)}
      nil -> %Mal.List{contents: []}
    end)

    wrapN(env, "assoc", fn [%Mal.HashMap{hashmap_map: m} | assoc_list] ->
      %Mal.HashMap{hashmap_map: Map.merge(m, Seq.list_to_hashmap_map(assoc_list))}
    end)

    wrapN(env, "dissoc", fn [%Mal.HashMap{hashmap_map: m} | key_list] ->
      %Mal.HashMap{hashmap_map: Map.drop(m, key_list)}
    end)
  end
end
