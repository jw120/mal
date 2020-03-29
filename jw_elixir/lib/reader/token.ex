defmodule Reader.Token do
  @moduledoc """
  Provides functions to extract mal-language tokens from a string.

  Based on the the regex provided on the mal site, but modified to automatically skip
  white space (including comments).
  """

  # Whitespace is spaces (including commas) and line comments. Note non-capturing regex groups
  @mal_whitespace ~r/^(?:(?:[\s,]+)|(?:;[^\n]*))+/

  @mal_token ~R"""
    ~@ |
    [\[\]{}()'`~@] |
    "(?:\\.|[^\"])*"? |
    [^\s\[\]{}('"`,;)]*
  """x

  @doc """
  Returns the leading token in the given string and the remaining portion of the string, or :void
  if there is no token.

  ## Examples

      iex> Reader.Token.next("  12  ab")
      {"12", "  ab"}

  """
  @spec next(String.t()) :: {String.t() | :void, String.t()}
  def next(target) do
    target_after_leading_whitespace = skip_whitespace(target)

    if target_after_leading_whitespace == "" do
      {:void, ""}
    else
      case Regex.run(@mal_token, target_after_leading_whitespace, return: :index) do
        [{0, token_len}] ->
          String.split_at(target_after_leading_whitespace, token_len)

        _ ->
          raise MalException,
                "Internal failure in Token.next with: #{target_after_leading_whitespace}"
      end
    end
  end

  @doc """
  Returns the leading token in the given string.

  ## Examples

      iex> Reader.Token.peek("  12  ab")
      "12"

  """
  @spec peek(String.t()) :: String.t() | :void
  def peek(target) do
    {token, _new_target} = next(target)
    token
  end

  @doc """
  Is the string empty, i.e, whitespace (inclduing comments) only

  ## Examples

      iex> Reader.Token.empty?("  ; Comment")
      true

  """
  @spec empty?(String.t()) :: boolean()
  def empty?(s) do
    "" == skip_whitespace(s)
  end

  # Target string without leading white space
  @spec skip_whitespace(String.t()) :: String.t()
  def skip_whitespace(s) do
    case Regex.run(@mal_whitespace, s, return: :index) do
      [{0, white_space_len}] ->
        String.slice(s, white_space_len, String.length(s) - white_space_len)

      _ ->
        s
    end
  end
end
