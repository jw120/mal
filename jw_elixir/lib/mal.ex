defmodule Mal do
  @moduledoc """
  Define the basic type for our language
  """

  @typedoc "Type for mal language values"
  @type t ::
          String.t()
          | {:symbol, String.t()}
          | {:keyword, String.t()}
          | integer()
          | boolean()
          | nil
          | :void
          | list(t)
          | {:vector, vector_map}
          | {:hash_map, hash_map_map}
          | Mal.Function.t
          | {:atom, pid, non_neg_integer()}

  @typedoc "Type for a mal function"
  @type closure :: ([t] -> t)

  defmodule Function do
    defstruct closure: nil, is_macro: false
    @type t :: %__MODULE__{closure: Mal.closure(), is_macro: boolean()}
  end

  @typedoc "Elixir map used to hold a mal vector (keys are 0...)"
  @type vector_map :: %{optional(non_neg_integer()) => t}

  @typedoc "Elixir map used to hold a mal hash-map"
  @type hash_map_map :: %{optional(t) => t}

  defmacro sym(s) do
    quote do
      {:symbol, unquote(s)}
    end
  end
end
