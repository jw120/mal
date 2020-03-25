defmodule MalException do
  @moduledoc """
  Exception type for our mal langauge. Either a plain string (for runtime failures)
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
