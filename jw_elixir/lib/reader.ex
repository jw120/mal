defmodule Reader do

  @spec read_str(String.t()) :: Mal.t
  def read_str(s) do
    {val, remaining} = read_form(s)
    unless Regex.match?(~r/^[\s,]*$/, remaining) do # Trailing whitespace (including commas)
      raise "Unexpected leftovers in read_str"
    end
    val
  end

  @spec read_form(String.t()) :: {Mal.t, String.t()}
  def read_form(s) do
    case Token.peek(s) do
      "(" ->
        {contents, remainder} = read_list(s)
        {{:list, contents}, remainder}
      _ -> read_atom(s)
    end
  end

  @spec read_list(String.t()) :: {[Mal.t], String.t()}
  def read_list(s) do
    {"(", after_open} = Token.next(s)
    read_until(after_open, {:symbol, ")"})
  end

  @spec read_until(String.t(), Mal.t) :: {[Mal.t], String.t()}
  def read_until(s, closer) do
    case read_form(s) do
      {^closer, after_closer} ->
        {[], after_closer}
      {tok, after_tok} ->
        {rest, after_rest} = read_until(after_tok, closer)
        {[tok | rest], after_rest}
    end
  end

  @spec read_atom(String.t()) :: {Mal.t, String.t()}
  def read_atom(s) do
    {tok, after_tok} = Token.next(s)
    cond do
      Regex.match?(~r/^-?[[:digit:]]+$/, tok) ->
        {{:number, String.to_integer(tok)}, after_tok}
      Regex.match?(~r/^[\[\]{}()'`~@]$/, tok) ->
        {{:symbol, tok}, after_tok}
      Regex.match?(~r/^\"[^\"]*\"$/, tok) ->
        {{:string, String.slice(tok, 1, String.length(tok) -2)}, after_tok}
      Regex.match?(~r/^:[^\s\[\]{}('"`,;)]+$/, tok) ->
        {{:keyword, String.slice(tok, 1, String.length(tok) - 1)}, after_tok}
      "true" == tok ->
        {{:boolean, true}, after_tok}
      "false" == tok ->
        {{:boolean, false}, after_tok}
      "nil" == tok ->
        {{:nil}, after_tok}
      true ->
        {{:symbol, tok}, after_tok}
      end
  end

end
