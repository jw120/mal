defmodule ReaderTest do
  use ExUnit.Case
  doctest Reader

  test "reads atoms" do
    assert Reader.read_str("23") == 23
    assert Reader.read_str("-34") == -34
    assert Reader.read_str("  -34  ") == -34
    assert Reader.read_str("true") == true
    assert Reader.read_str("false") == false
    assert Reader.read_str("nil") == nil
    assert Reader.read_str("abc") == {:symbol, "abc"}
    assert Reader.read_str(":abc") == {:keyword, "abc"}
    assert Reader.read_str("\"qwe\"") == "qwe"
  end

  test "reads list" do
    assert Reader.read_str("()") == []
    assert Reader.read_str("(1 3)") == [1, 3]
    assert Reader.read_str("(plus 2)") == [{:symbol, "plus"}, 2]
  end

  test "skips comments" do
    assert Reader.read_str(";; AA") == :void
    assert Reader.read_str(";; BB\n") == :void
    assert Reader.read_str("  ;; CC\n;;  DD") == :void
    assert Reader.read_str(";; CC\n;;DD\n23") == 23
  end
end
