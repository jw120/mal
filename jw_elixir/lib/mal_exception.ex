defmodule MalException do
  @moduledoc """
  Exception type for our mal langauge. Either a plain string (for runtime failures)
  or a mal value (from mal's raise) can be used.
  """

  defexception [:val, :message]

  @impl true

  # Raising with a string and a mal mal value for a failure message
  #   raise(MalException, {"Bad arguments", args}
  def exception({prefix, value}) do
    combined = prefix <> ": " <> Printer.pr_str(value, true)
    %MalException{val: combined, message: combined}
  end

  # Raising with a string - for a simple failure message
  #   raise(MalException, "Something has gone wrong")
  def exception(value) when is_bitstring(value) do
    %MalException{val: value, message: value}
  end

  # Raising with a mal value - for mal throw
  def exception(value) do
    %MalException{val: value, message: Printer.pr_str(value, true)}
  end
end
