defmodule Mal do
  @moduledoc """
  Define the basic type for our language
  """

  @typedoc "Type for mal language values"
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
          | {:vector, vector_map}
          | {:hash_map, hash_map_map}
          | {:function, closure}
          | {:atom, pid, non_neg_integer()}

  @typedoc "Type for a mal function"
  @type closure :: ([t] -> t)

  @typedoc "Elixir map used to hold a mal vector"
  @type vector_map :: %{optional(non_neg_integer()) => t}

  @typedoc "Elixir map used to hold a mal hash-map"
  @type hash_map_map :: %{optional(t) => t}
end
