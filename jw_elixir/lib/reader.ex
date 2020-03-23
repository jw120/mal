defmodule Reader do

  @spec read_str(String.t()) :: any
  def read_str(s) do
    {val, remaining} = read_form(s)
    if String.trim(remaining) != "" do
      raise "Unexpected leftovers in read_str"
    end
    val
  end


  @spec read_form(String.t()) :: {any, String.t()}
  def read_form(s) do
    case Token.peek(s) do
      "(" -> read_list(s)
      _ -> read_atom(s)
    end
  end

  @spec read_list(String.t()) :: {any, String.t()}
  def read_list(s) do
    {"(", after_open} = Token.next(s)
    read_until(after_open, ")")
  end

  @spec read_until(String.t(), String.t()) :: {[any], String.t()}
  def read_until(s, closer) do
    case read_form(s) do
      {^closer, after_closer} ->
        {[], after_closer}
      {tok, after_tok} ->
        {rest, after_rest} = read_until(after_tok, closer)
        {[tok | rest], after_rest}
    end
  end

  @spec read_atom(String.t()) :: {any, String.t()}
  def read_atom(s) do
    {tok, after_tok} = Token.next(s)
    cond do
      Regex.match?(~r/^-?[[:digit:]]+$/, tok) ->
        {String.to_integer(tok), after_tok}
      true ->
        {tok, after_tok}
      end
  end

end
