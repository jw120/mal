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
  # credo:disable-for-next-line Credo.Check.Refactor.CyclomaticComplexity
  def pr_str(x, print_readably) do
    case x do
      {:string, s} ->
        case print_readably do
          true -> "\"" <> string_escape(s) <> "\""
          false -> s
        end
      {:symbol, s} -> s
      {:keyword, s} -> ":" <> s
      {:number, n} -> Integer.to_string(n)
      {:boolean, true} -> "true"
      {:boolean, false} -> "false"
      {nil} -> "nil"
      {:function, _} -> "<function>"
      {:list, xs} -> "(" <> print_and_join(xs, print_readably) <> ")"
      {:vector, m} -> "[" <> print_and_join(Seq.vector_to_list(m), print_readably) <> "]"
      {:hash_map, m} -> "{" <> print_and_join(Seq.hash_map_to_list(m), print_readably) <> "}"
      _ -> raise "Failure in pr_str: Unknown type in pr_str #{inspect(x)}"
    end
  end

  # Add slash escapes to a string
  @spec string_escape(String.t()) :: String.t()
  defp string_escape(s) do
    s
    |> String.codepoints()
    |> Enum.map(fn c ->
      case c do
        "\"" -> "\\\""
        "\n" -> "\\n"
        "\\" -> "\\\\"
        _ -> c
      end
    end)
    |> Enum.join()
  end

  # Print the list elements and join with spaces
  @spec print_and_join([Mal.t()], boolean()) :: String.t()
  def print_and_join(xs, print_readably) do
    xs
    |> Enum.map(&pr_str(&1, print_readably))
    |> Enum.join(" ")
  end
end
