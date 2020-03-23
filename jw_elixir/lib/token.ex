defmodule Token do
  @moduledoc """
  Provides functions to extract mal-language tokens from a string.

  Used the regex provided on the mal site.
  """

  @mal_regex ~R"""
    [\s,]*
    (
      ~@ |
      [\[\]{}()'`~@] |
      "(?:\\.|[^\"])*"? |
      ;.* |
      [^\s\[\]{}('"`,;)]*
    )
  """x

  @doc """
  Returns the leading token in the given string and the remaining portion of the string.

  ## Examples

      iex> Token.next("  12  ab")
      {"12", "  ab"}

  """
  @spec next(String.t()) :: {String.t(), String.t()}
  def next(target) do
    case Regex.run(@mal_regex, target, return: :index) do
      [{_, _}, {group_start, group_len}] ->
        {_before, group_and_rest} = String.split_at(target, group_start)
        String.split_at(group_and_rest, group_len)

      _ ->
        {"Failed", "failed"}
    end
  end

  @doc """
  Returns the leading token in the given string.

  ## Examples

      iex> Token.peek("  12  ab")
      "12"

  """
  @spec peek(String.t()) :: String.t()
  def peek(target) do
    {group_match, _new_target} = next(target)
    group_match
  end
end
