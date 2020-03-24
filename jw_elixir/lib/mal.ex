defmodule Mal do
  @typedoc """
  Type for mal language values
  """
  @type t ::
          {:string, String.t()}
          | {:symbol, String.t()}
          | {:keyword, String.t()}
          | {:number, integer()}
          | {:boolean, boolean()}
          | {nil}
          # void is used when there is no input
          | {:void}
          | {:list, list(t)}
          # vector held as a map with keys 0, 1, 2...
          | {:vector, map()}
          | {:map, map()}
end

defmodule MalException do
  @moduledoc """
  Exception type for our mal langauge. Either a plain string (for internal failures)
  or a mal value (from mal's raise) can be used.
  """

  defexception [:val, :message]

  @impl true
  def exception(value) when is_bitstring(value) do
    %MalException{val: {:string, value}, message: value}
  end

  def exception(value) do
    %MalException{val: value, message: Printer.pr_str(value, true)}
  end
end
