defmodule EnvTest do
  use ExUnit.Case
  doctest Env

  test "works" do
    a = Env.new()
    Env.set!(a, "x", 7)
    Env.set!(a, "y", 8)
    b = Env.new(a)
    Env.set!(b, "y", 4)
    Env.set!(b, "z", 6)
    assert Env.get(a, "x") == 7
    assert Env.get(a, "y") == 8
    assert_raise MalException, fn -> Env.get(a, "z") == 7 end
    assert Env.get(b, "x") == 7
    assert Env.get(b, "y") == 4
    assert Env.get(b, "z") == 6
    assert Env.find(b, "x") == a
    assert Env.find(b, "y") == b
    assert Env.find(b, "q") == nil
  end
end
