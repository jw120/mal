defmodule JwElixirTest do
  use ExUnit.Case
  doctest JwElixir

  test "greets the world" do
    assert JwElixir.hello() == :world
  end
end
