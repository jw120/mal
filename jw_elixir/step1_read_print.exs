defmodule Step1 do

  def read(s) do
    Reader.read_str(s)
  end

  def eval(s) do
    s
  end

  def print(s) do
    Printer.pr_str(s)
  end

  def rep(s) do
    s |> read |> eval |> print
  end

  def repl() do
    s = IO.gets("user> ")
    unless s == :eof do
      s |> String.trim |> rep |> IO.puts
      repl()
    end
  end

end

Step1.repl()
