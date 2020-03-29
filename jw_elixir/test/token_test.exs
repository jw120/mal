defmodule ReaderTokenTest do
  use ExUnit.Case
  doctest Reader.Token

  test "matches a symbol" do
    assert Reader.Token.next("abc") == {"abc", ""}
    assert Reader.Token.next("abc def") == {"abc", " def"}
    assert Reader.Token.peek("abc def") == "abc"
  end

  test "skips comments" do
    assert Reader.Token.next(";; AA") == {:void, ""}
    assert Reader.Token.next("  ;; BB\n") == {:void, ""}
    assert Reader.Token.next("  ;; CC\n;;  DD") == {:void, ""}
    assert Reader.Token.next(";; CC\n;;DD\n23 ;more") == {"23", " ;more"}
  end
end
