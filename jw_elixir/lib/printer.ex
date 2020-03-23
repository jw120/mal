defmodule Printer do

  @spec pr_str(any) :: String.t()
  def pr_str(s) do
    cond do
      is_integer(s) ->
        Integer.to_string(s)
      is_list(s) ->
        "(" <> Enum.join(Enum.map(s, &(pr_str(&1))), " ") <> ")"
      is_bitstring(s) ->
        s
      true ->
        raise "Unknown type in pr_str"
    end

  end

end
