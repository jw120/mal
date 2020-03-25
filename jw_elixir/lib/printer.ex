defmodule Printer do
  @moduledoc """
  Printer module provides pr_str to convert a mal value into a string.
  """

  @doc """
  Converts a mal value to a string. When the second argument is true, then
  string escape sequences and double-quotes are escaped inside strings.

  Used in our error handler, so should not raise MalException.

  ## Examples

      iex> Printer.pr_str({:number, 23}, true)
      "23"

  """
  @spec pr_str(Mal.t(), boolean()) :: String.t()
  def pr_str(x, print_readably) do
    case x do
      {:string, s} -> "\"" <> string_escape(s, print_readably) <> "\""
      {:symbol, s} -> s
      {:keyword, s} -> ":" <> s
      {:number, n} -> Integer.to_string(n)
      {:boolean, true} -> "true"
      {:boolean, false} -> "false"
      {nil} -> "nil"
      {:function, _} -> "<function>"
      {:list, xs} -> "(" <> join_list(xs, print_readably) <> ")"
      {:vector, m} -> "[" <> join_list(vector_map_to_list(m), print_readably) <> "]"
      {:map, m} -> "{" <> join_list(map_to_list(m), print_readably) <> "}"
      _ -> raise "Failure in pr_str: Unknown type in pr_str #{inspect(x)}"
    end
  end

  @spec string_escape(String.t(), boolean()) :: String.t()
  defp string_escape(s, false), do: s
  defp string_escape(s, true) do
    String.codepoints(s)
      |> Enum.map(
        fn c ->
          case c do
            "\"" -> "\\\""
            "\n" -> "\\n"
            "\\" -> "\\\\"
            _ -> c
          end
        end)
      |> Enum.join
  end

  @spec join_list([Mal.t], boolean()) :: String.t()
  def join_list(xs, print_readably) do
    xs
    |>Enum.map(&pr_str(&1, print_readably))
    |>Enum.join(" ")
  end

  @spec vector_map_to_list(map()) :: [Mal.t]
  def vector_map_to_list(m) when map_size(m) == 0, do: []
  def vector_map_to_list(m) do

    0..(map_size(m) - 1)
    |> Enum.map(fn i -> m[i] end)
  end

  @spec map_to_list(map()) :: [Mal.t]
  def map_to_list(m) do
    m
    |> Map.to_list
    |> Enum.flat_map(fn {x, y} -> [x, y] end)
  end
end


