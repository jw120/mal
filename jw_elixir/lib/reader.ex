defmodule Reader do
  @moduledoc """
  Provides conversion of a string into a mal type
  """

  alias Reader.Token
  import Mal, only: :macros

  @doc """
  Converts a string into a mal type.

  Returns an error if there is anything other than whitespace following the
  mal value that has been read

  ## Examples

      iex> Reader.read_str("  12")
      12

  """
  @spec read_str(String.t()) :: Mal.t()
  def read_str(s) do
    {val, remaining} = read_form(s)

    if Reader.Token.empty?(remaining) do
      val
    else
      raise MalException,
            "Unexpected leftovers in read_str reading '#{s}' left over '#{remaining}'"
    end
  end

  # Top-level internal reading function
  @spec read_form(String.t()) :: {Mal.t(), String.t()}
  defp read_form(s) do
    case Token.peek(s) do
      "(" ->
        {contents, remainder} = read_list(s)
        {%Mal.List{contents: contents}, remainder}

      "[" ->
        {contents, remainder} = read_vector(s)
        {%Mal.Vector{vector_map: Seq.list_to_vector_map(contents)}, remainder}

      "{" ->
        {contents, remainder} = read_map(s)
        {%Mal.HashMap{hashmap_map: Seq.list_to_hashmap_map(contents)}, remainder}

      :void ->
        {:void, ""}

      _ ->
        read_atom(s)
    end
  end

  @spec read_list(String.t()) :: {[Mal.t()], String.t()}
  defp read_list(s) do
    {"(", after_open} = Token.next(s)
    read_until(after_open, sym(")"))
  end

  @spec read_vector(String.t()) :: {[Mal.t()], String.t()}
  defp read_vector(s) do
    {"[", after_open} = Token.next(s)
    read_until(after_open, sym("]"))
  end

  @spec read_map(String.t()) :: {[Mal.t()], String.t()}
  defp read_map(s) do
    {"{", after_open} = Token.next(s)
    read_until(after_open, sym("}"))
  end

  @spec read_until(String.t(), Mal.t()) :: {[Mal.t()], String.t()}
  defp read_until(s, closer) do
    case read_form(s) do
      {^closer, after_closer} ->
        {[], after_closer}

      {:void, _} ->
        raise MalException, "EOF during read"

      {tok, after_tok} ->
        {rest, after_rest} = read_until(after_tok, closer)
        {[tok | rest], after_rest}
    end
  end

  @spec read_atom(String.t()) :: {Mal.t(), String.t()}
  # credo:disable-for-next-line Credo.Check.Refactor.CyclomaticComplexity
  defp read_atom(s) do
    {tok, after_tok} = Token.next(s)

    cond do
      "true" == tok ->
        {true, after_tok}

      "false" == tok ->
        {false, after_tok}

      "nil" == tok ->
        {nil, after_tok}

      "'" == tok ->
        {next_form, after_next_form} = read_form(after_tok)
        {%Mal.List{contents: [sym("quote"), next_form]}, after_next_form}

      "`" == tok ->
        {next_form, after_next_form} = read_form(after_tok)
        {%Mal.List{contents: [sym("quasiquote"), next_form]}, after_next_form}

      "~" == tok ->
        {next_form, after_next_form} = read_form(after_tok)
        {%Mal.List{contents: [sym("unquote"), next_form]}, after_next_form}

      "~@" == tok ->
        {next_form, after_next_form} = read_form(after_tok)
        {%Mal.List{contents: [sym("splice-unquote"), next_form]}, after_next_form}

      "@" == tok ->
        {next_form, after_next_form} = read_form(after_tok)
        {%Mal.List{contents: [sym("deref"), next_form]}, after_next_form}

      "^" == tok ->
        {next_form, after_next_form} = read_form(after_tok)
        {next_next_form, after_next_next_form} = read_form(after_next_form)
        {%Mal.List{contents: [sym("with-meta"), next_next_form, next_form]}, after_next_next_form}

      Regex.match?(~r/^-?[[:digit:]]+$/, tok) ->
        {String.to_integer(tok), after_tok}

      Regex.match?(~r/^[\[\]{}()'`~@]$/, tok) ->
        {sym(tok), after_tok}

      # Note /s makes . match include newlines
      Regex.match?(~r/^".*"$/s, tok) ->
        {remove_escapes(String.slice(tok, 1, String.length(tok) - 2)), after_tok}

      Regex.match?(~r/^\"[^\"]*$/, tok) ->
        raise MalException, "EOF found in string"

      Regex.match?(~r/^:[^\s\[\]{}('"`,;)]+$/, tok) ->
        {{:keyword, String.slice(tok, 1, String.length(tok) - 1)}, after_tok}

      true ->
        {sym(tok), after_tok}
    end
  end

  # Remove slash escapes from a string
  @spec remove_escapes(String.t()) :: String.t()
  defp remove_escapes(s) do
    {new_s, final_in_escape} =
      s
      |> String.codepoints()
      |> Enum.reduce({"", false}, fn x, {acc, in_escape} ->
        case {x, in_escape} do
          {"\\", true} -> {acc <> "\\", false}
          {"n", true} -> {acc <> "\n", false}
          {"\"", true} -> {acc <> "\"", false}
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
end
