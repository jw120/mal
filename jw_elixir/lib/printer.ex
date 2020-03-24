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
      {:list, xs} -> "(" <> Enum.join(Enum.map(xs, &pr_str(&1, print_readably)), " ") <> ")"
      {:vector, xs} -> "[" <> Enum.join(Enum.map(xs, &pr_str(&1, print_readably)), " ") <> "]"
      {:map, xs} -> "{" <> Enum.join(Enum.map(xs, &pr_str(&1, print_readably)), " ") <> "}"
      _ -> raise "Failure in pr_str: Unknown type in pr_str"
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
end


