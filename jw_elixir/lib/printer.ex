defmodule Printer do
  @moduledoc """
  Printer module provides pr_str to convert a mal value into a string.
  """

  @doc """
  Converts a mal value to a string.

  Used in our error handler, so should not raise MalException.

  ## Examples

      iex> Printer.pr_str({:number, 23})
      "23"

  """
  @spec pr_str(Mal.t()) :: String.t()
  def pr_str(x) do
    case x do
      {:string, s} -> "\"" <> s <> "\""
      {:symbol, s} -> s
      {:keyword, s} -> ":" <> s
      {:number, n} -> Integer.to_string(n)
      {:boolean, true} -> "true"
      {:boolean, false} -> "false"
      {nil} -> "nil"
      {:list, xs} -> "(" <> Enum.join(Enum.map(xs, &pr_str(&1)), " ") <> ")"
      {:vector, xs} -> "[" <> Enum.join(Enum.map(xs, &pr_str(&1)), " ") <> "]"
      {:map, xs} -> "{" <> Enum.join(Enum.map(xs, &pr_str(&1)), " ") <> "}"
      _ -> raise "Failure in pr_str: Unknown type in pr_str"
    end
  end
end
