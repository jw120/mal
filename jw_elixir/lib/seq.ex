defmodule Seq do
  @moduledoc """
  Provides utilities for handling our sequence types
  """

  @doc """
  Converts a mal list to a mal vector (represented as
  an elixir map).

  ## Examples

      iex> Seq.list_to_vector_map([{:symbol, "a"}, 3])
      %{0 => {:symbol, "a"}, 1 => 3}

  """
  @spec list_to_vector_map([Mal.t()]) :: Mal.vector_map()
  def list_to_vector_map(xs) do
    indices = Stream.iterate(0, &(&1 + 1))
    Map.new(Stream.zip(indices, xs))
  end

  @doc """
  Converts a mal list to a mal hash-map (represented as
  an elixir map). The list has alternating keys and values

  ## Examples

      iex> Seq.list_to_hashmap_map([{:symbol, "a"}, 3])
      %{{:symbol, "a"} => 3}

  """
  @spec list_to_hashmap_map([Mal.t()]) :: Mal.hashmap_map()
  def list_to_hashmap_map(xs) do
    xs
    |> Enum.chunk_every(2)
    |> Enum.map(fn [x, y] -> {x, y} end)
    |> Map.new()
  end

  @doc """
  Converts a mal vector (represented as a map) to a list (which is in
  element order)

  ## Examples

      iex> Seq.vector_map_to_list(%{0 => {:symbol, "a"}, 1 => 2})
      [{:symbol, "a"}, 2]

  """
  @spec vector_map_to_list(Mal.vector_map()) :: [Mal.t()]
  def vector_map_to_list(v) when map_size(v) == 0, do: []

  def vector_map_to_list(v) do
    0..(map_size(v) - 1)
    |> Enum.map(&v[&1])
  end

  @doc """
  Converts a mal hash_map (represented as a map) to a list (of alternating keys
  and values, with keys unordered)

  ## Examples

      iex> Seq.hashmap_map_to_list(%{"q" => true})
      ["q", true]

  """
  @spec hashmap_map_to_list(Mal.hashmap_map()) :: [Mal.t()]
  def hashmap_map_to_list(m) do
    m
    |> Map.to_list()
    |> Enum.flat_map(fn {x, y} -> [x, y] end)
  end

  # Helper function to apply eval to the values in a map
  @spec eval_map_values(Mal.hashmap_map(), Env.t()) :: Mal.hashmap_map()
  def eval_map_values(m, env) do
    m
    |> Enum.map(fn {k, v} -> {k, Eval.eval(v, env)} end)
    |> Enum.into(%{})
  end
end
