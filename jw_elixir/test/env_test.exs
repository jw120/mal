defmodule EnvTest do
  use ExUnit.Case
  doctest Env

  test "works" do
    a = Env.new
    Env.set!(a, "x", {:number, 7})
    Env.set!(a, "y", {:number, 8})
    b = Env.new(a)
    Env.set!(b, "y", {:number, 4})
    Env.set!(b, "z", {:number, 6})
    assert Env.get(a, "x") == {:number, 7}
    assert Env.get(a, "y") == {:number, 8}
    assert_raise MalException, fn -> Env.get(a, "z") == {:number, 7} end
    assert Env.get(b, "x") == {:number, 7}
    assert Env.get(b, "y") == {:number, 4}
    assert Env.get(b, "z") == {:number, 6}
    assert Env.find(b, "x") == a
    assert Env.find(b, "y") == b
    assert Env.find(b, "q") == nil

  end

end
