defmodule Printer do

  @spec pr_str(Mal.t) :: String.t()
  def pr_str(x) do
    case x do
      { :string, s } -> "\"" <> s <> "\""
      { :symbol, s } -> s
      { :keyword, s} -> ":" <> s
      { :number, n } -> Integer.to_string(n)
      { :boolean, true } -> "true"
      { :boolean, false } -> "false"
      { :nil } -> "nil"
      { :list, xs } -> "(" <> Enum.join(Enum.map(xs, &(pr_str(&1))), " ") <> ")"
      true ->
        raise "Unknown type in pr_str"
    end
  end
end
