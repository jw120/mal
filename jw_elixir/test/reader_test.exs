defmodule ReaderTest do
  use ExUnit.Case
  doctest Reader

  test "reads atoms" do
    assert Reader.read_str("a") == "a"
    assert Reader.read_str("23") == 23
    assert Reader.read_str("-34") == -34
    assert Reader.read_str("  -34  ") == -34
  end

  test "reads list" do
    assert Reader.read_str("()") == []
    assert Reader.read_str("(1 2 3)") == [1, 2, 3]
    assert Reader.read_str("(plus 2 3)") == ["plus", 2, 3]
  end


end
