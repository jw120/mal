defmodule Token do

  @mal_regex ~R"""
    [\s,]*
    (
      [\[\]{}()'`~@] |
      "(?:\\.|[^\"])*"? |
      ;.* |
      [^\s\[\]{}('"`,;)]*
    )
  """x

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

  @spec peek(String.t()) :: String.t()
  def peek(target) do
    {group_match, _new_target} = next(target)
    group_match
  end

end

