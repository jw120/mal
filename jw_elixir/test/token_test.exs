defmodule TokenTest do
  use ExUnit.Case
  doctest Token

  test "matches a symbol" do
    assert Token.next("abc def") == {"abc", " def"}
    assert Token.peek("abc def") == "abc"
  end
end
