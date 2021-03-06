defmodule Core.IO do
  @moduledoc """
  Provide IO functionality for the core environment.
  """

  import Core.Helpers

  @doc """
  Add functions for this module to the environment
  """
  @spec add(Env.t()) :: Env.t()
  def add(env) do
    wrap_list(env, "prn", fn xs when is_list(xs) ->
      xs
      |> Enum.map(&Printer.pr_str(&1, true))
      |> Enum.join(" ")
      |> IO.puts()

      nil
    end)

    wrap_list(env, "println", fn xs when is_list(xs) ->
      xs
      |> Enum.map(&Printer.pr_str(&1, false))
      |> Enum.join(" ")
      |> IO.puts()

      nil
    end)

    wrap_list(env, "pr-str", fn xs when is_list(xs) ->
      xs
      |> Enum.map(&Printer.pr_str(&1, true))
      |> Enum.join(" ")
    end)

    wrap_list(env, "str", fn xs when is_list(xs) ->
      xs
      |> Enum.map(&Printer.pr_str(&1, false))
      |> Enum.join("")
    end)

    wrap1(env, "read-string", fn s when is_bitstring(s) ->
      case Reader.read_str(s) do
        :void ->
          nil

        x ->
          x
      end
    end)

    wrap1(env, "slurp", fn file_name when is_bitstring(file_name) ->
      {:ok, file} = File.open(file_name, [:read])
      IO.read(file, :all)
    end)

    wrap1(env, "readline", fn
      prompt when is_bitstring(prompt) ->
        case IO.gets(prompt) do
          :eof -> nil
          s -> String.replace_suffix(s, "\n", "")
        end
    end)
  end
end
