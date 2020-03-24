defmodule ReaderTest do
  use ExUnit.Case
  doctest Reader

  test "reads atoms" do
    assert Reader.read_str("23") == {:number, 23}
    assert Reader.read_str("-34") == {:number, -34}
    assert Reader.read_str("  -34  ") == {:number, -34}
    assert Reader.read_str("true") == {:boolean, true}
    assert Reader.read_str("false") == {:boolean, false}
    assert Reader.read_str("nil") == {nil}
    assert Reader.read_str("abc") == {:symbol, "abc"}
    assert Reader.read_str(":abc") == {:keyword, "abc"}
    assert Reader.read_str("\"qwe\"") == {:string, "qwe"}
  end

  test "reads list" do
    assert Reader.read_str("()") == {:list, []}
    assert Reader.read_str("(1 3)") == {:list, [number: 1, number: 3]}
    assert Reader.read_str("(plus 2)") == {:list, [{:symbol, "plus"}, {:number, 2}]}
  end

end
