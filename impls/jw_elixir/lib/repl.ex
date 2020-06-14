defmodule Repl do
  @moduledoc """
  Provides read-eval-print loop functionality
  """

  @repl_startup ~s/(println (str "Mal [" *host-language* "]"))/

  @doc """
  Repeatedly prompts the user for input, and then applies the given real, evaluate and print
  functions to the input before printing the output
  """
  @spec run((String.t() -> Mal.t()), (Mal.t() -> Mal.t()), (Mal.t() -> String.t())) :: no_return()
  def run(read_fn, eval_fn, print_fn) do
    s = IO.gets("user> ")

    unless s == :eof do
      try do
        read_val = read_fn.(String.trim(s))

        unless read_val == :void do
          print_val = print_fn.(eval_fn.(read_val))
          IO.puts(print_val)
        end
      rescue
        e in MalException -> IO.puts("Exception: #{e.message}")
      end

      run(read_fn, eval_fn, print_fn)
    end
  end

  @doc """
  Handles mal startup. If System.argv() is empty then run the repl, otherwise load the given file
  """
  @spec start() :: no_return()
  def start do
    repl_env = Core.new_env()

    case System.argv() do
      [mal_prog | other_args] ->
        Env.set!(repl_env, "*ARGV*", %Mal.List{contents: other_args})

        "(load-file \"#{mal_prog}\")"
        |> Reader.read_str()
        |> Eval.eval(repl_env)

        exit(:normal)

      [] ->
        Env.set!(repl_env, "*ARGV*", %Mal.List{contents: []})
        Eval.eval(Reader.read_str(@repl_startup), repl_env)

        run(
          &Reader.read_str/1,
          &Eval.eval(&1, repl_env),
          &Printer.pr_str(&1, true)
        )
    end
  end
end
