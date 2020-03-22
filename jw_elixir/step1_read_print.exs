defmodule Step1 do

  def read(s) do
    s
  end

  def eval(s) do
    s
  end

  def print(s) do
    IO.puts(s)
  end

  def rep(s) do
    s |> read |> eval |> print
  end

  def repl() do
    s = IO.gets("user> ")
    unless s == :eof do
      s |> String.trim |> rep
      repl()
    end
  end

end

Step0.repl()
