defmodule Repl do
  @moduledoc """
  Provides read-eval-print-loop functionality
  """

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

        unless read_val == {:void} do
          print_val = print_fn.(eval_fn.(read_val))
          IO.puts(print_val)
        end
      rescue
        e in MalException -> IO.puts(e.message)
      end

      run(read_fn, eval_fn, print_fn)
    end
  end
end
