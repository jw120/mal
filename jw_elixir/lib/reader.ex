defmodule Reader do
  @moduledoc """
  Provides conversion of a string into a mal type
  """

  @doc """
  Converts a string into a mal type.

  Returns an error if there is anything other than whitespace following the
  mal value that has been read

  ## Examples

      iex> Reader.read_str("  12")
      {:number, 12}

  """
  @spec read_str(String.t()) :: Mal.t()
  def read_str(s) do
    {val, remaining} = read_form(s)
    # Remaining must be only trailing whitespace (including commas) and an optional comment
    unless Regex.match?(~r/^[\s,]*(;.*)?$/, remaining) do
      raise "Unexpected leftovers in read_str"
    end

    val
  end

  @spec read_form(String.t()) :: {Mal.t(), String.t()}
  defp read_form(""), do: {{:void}, ""}

  defp read_form(s) do
    case Token.peek(s) do
      "(" ->
        {contents, remainder} = read_list(s)
        {{:list, contents}, remainder}

      "[" ->
        {contents, remainder} = read_vector(s)
        {{:vector, list_to_vector(contents)}, remainder}

      "{" ->
        {contents, remainder} = read_map(s)
        {{:map, list_to_map(contents)}, remainder}

      _ ->
        read_atom(s)
    end
  end

  @spec read_list(String.t()) :: {[Mal.t()], String.t()}
  def read_list(s) do
    {"(", after_open} = Token.next(s)
    read_until(after_open, {:symbol, ")"})
  end

  @spec read_vector(String.t()) :: {[Mal.t()], String.t()}
  def read_vector(s) do
    {"[", after_open} = Token.next(s)
    read_until(after_open, {:symbol, "]"})
  end

  @spec read_map(String.t()) :: {[Mal.t()], String.t()}
  def read_map(s) do
    {"{", after_open} = Token.next(s)
    read_until(after_open, {:symbol, "}"})
  end

  @spec read_until(String.t(), Mal.t()) :: {[Mal.t()], String.t()}
  def read_until(s, closer) do
    case read_form(s) do
      {^closer, after_closer} ->
        {[], after_closer}

      {{:void}, _} ->
        raise MalException, "EOF during read"

      {tok, after_tok} ->
        {rest, after_rest} = read_until(after_tok, closer)
        {[tok | rest], after_rest}
    end
  end

  @spec read_atom(String.t()) :: {Mal.t(), String.t()}
  def read_atom(s) do
    {tok, after_tok} = Token.next(s)

    cond do
      "true" == tok ->
        {{:boolean, true}, after_tok}

      "false" == tok ->
        {{:boolean, false}, after_tok}

      "nil" == tok ->
        {{nil}, after_tok}

      "'" == tok ->
        {next_form, after_next_form} = read_form(after_tok)
        {{:list, [{:symbol, "quote"}, next_form]}, after_next_form}

      "`" == tok ->
        {next_form, after_next_form} = read_form(after_tok)
        {{:list, [{:symbol, "quasiquote"}, next_form]}, after_next_form}

      "~" == tok ->
        {next_form, after_next_form} = read_form(after_tok)
        {{:list, [{:symbol, "unquote"}, next_form]}, after_next_form}

      "~@" == tok ->
        {next_form, after_next_form} = read_form(after_tok)
        {{:list, [{:symbol, "splice-unquote"}, next_form]}, after_next_form}

      "@" == tok ->
        {next_form, after_next_form} = read_form(after_tok)
        {{:list, [{:symbol, "deref"}, next_form]}, after_next_form}

      "^" == tok ->
        {next_form, after_next_form} = read_form(after_tok)
        {next_next_form, after_next_next_form} = read_form(after_next_form)
        {{:list, [{:symbol, "with-meta"}, next_next_form, next_form]}, after_next_next_form}

      Regex.match?(~r/^-?[[:digit:]]+$/, tok) ->
        {{:number, String.to_integer(tok)}, after_tok}

      Regex.match?(~r/^[\[\]{}()'`~@]$/, tok) ->
        {{:symbol, tok}, after_tok}

      Regex.match?(~r/^\"[^\"]*\"$/, tok) ->
        {{:string, remove_escapes(String.slice(tok, 1, String.length(tok) - 2))}, after_tok}

      Regex.match?(~r/^\"[^\"]*$/, tok) ->
        raise MalException, "EOF found in string"

      Regex.match?(~r/^:[^\s\[\]{}('"`,;)]+$/, tok) ->
        {{:keyword, String.slice(tok, 1, String.length(tok) - 1)}, after_tok}

      true ->
        {{:symbol, tok}, after_tok}
    end
  end

  @spec remove_escapes(String.t()) :: String.t()
  defp remove_escapes(s) do
    {new_s, final_in_escape} = String.codepoints(s)
      |> Enum.reduce({"", false}, fn x, {acc, in_escape} ->
        case {x, in_escape} do
          {"\\", true} -> { acc <> "\\", false }
          {"n", true} -> { acc <> "\n", false }
          {_, true} -> raise MalException, "Bad escape sequence in remove_escapes"
          {"\\", false} -> {acc, true}
          {_, false} -> {acc <> x, false}
        end
      end)

    case final_in_escape do
      true -> raise MalException, "EOF in escape sequence in remove_escapes"
      false -> new_s
    end
  end

  @spec list_to_vector([Mal.t]) :: map()
  def list_to_vector(xs) do
    Stream.zip(Stream.iterate(0, &(&1 + 1)), xs)
      |> Map.new
  end

  @spec list_to_map([Mal.t]) :: map()
  def list_to_map(xs) do
    Enum.chunk_every(xs, 2)
    |> Enum.map(fn [x, y] -> {x, y} end)
    |> Map.new
  end

end
