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
          | Mal.List.t()
          | Mal.Vector.t()
          | Mal.HashMap.t()
          | Mal.Function.t()
          | Mal.Atom.t()

  @typedoc "Type for a mal function"
  @type closure :: ([t] -> t)

  @typedoc "Type for arguments of a mal function"
  @type arguments :: [t]

  @typedoc "Elixir map used to hold a mal vector (keys are 0...)"
  @type vector_map :: %{optional(non_neg_integer()) => t}

  @typedoc "Elixir map used to hold a mal hash-map"
  @type hashmap_map :: %{optional(t) => t}

  defmacro sym(s) do
    quote do
      {:symbol, unquote(s)}
    end
  end
end

defmodule Mal.Atom do
  @enforce_keys [:agent, :key]
  defstruct [:agent, :key]
  @type t :: %__MODULE__{agent: pid, key: non_neg_integer()}
end

defmodule Mal.Function do
  @enforce_keys [:closure, :is_macro]
  defstruct [:closure, :is_macro]
  @type t :: %__MODULE__{closure: Mal.closure(), is_macro: boolean()}
end

defmodule Mal.HashMap do
  @enforce_keys [:hashmap_map]
  defstruct [:hashmap_map, meta: nil]
  @type t :: %__MODULE__{hashmap_map: Mal.hashmap_map(), meta: Mal.t()}
end

defmodule Mal.List do
  @enforce_keys [:contents]
  defstruct [:contents, meta: nil]
  @type t :: %__MODULE__{contents: [any], meta: Mal.t()}
end

defmodule Mal.Vector do
  @enforce_keys [:vector_map]
  defstruct [:vector_map, meta: nil]
  @type t :: %__MODULE__{vector_map: Mal.vector_map(), meta: Mal.t()}
end
