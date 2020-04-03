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

  @doc """
  Convenience macro to allow writing `sym("x")` instead of
  `{:symbol, "x"}`
  """
  defmacro sym(s) do
    quote do
      {:symbol, unquote(s)}
    end
  end
end

defmodule Mal.Atom do
  @enforce_keys [:agent, :key]

  @doc """
  Struct for mal atoms
  """
  defstruct [:agent, :key, meta: nil]
  @type t :: %__MODULE__{agent: pid, key: non_neg_integer(), meta: Mal.t()}
end

defmodule Mal.Function do
  @enforce_keys [:closure, :name, :is_macro]

  @doc """
  Struct for mal functions and macros
  """
  defstruct [:closure, :name, :is_macro, meta: nil]

  @type t :: %__MODULE__{
          closure: Mal.closure(),
          name: String.t(),
          is_macro: boolean(),
          meta: Mal.t()
        }
end

defmodule Mal.HashMap do
  @enforce_keys [:hashmap_map]

  @doc """
  Struct for mal hashmaps
  """
  defstruct [:hashmap_map, meta: nil]
  @type t :: %__MODULE__{hashmap_map: Mal.hashmap_map(), meta: Mal.t()}
end

defmodule Mal.List do
  @enforce_keys [:contents]

  @doc """
  Struct for mal lists
  """
  defstruct [:contents, meta: nil]
  @type t :: %__MODULE__{contents: [any], meta: Mal.t()}
end

defmodule Mal.Vector do
  @enforce_keys [:vector_map]

  @doc """
  Struct for mal vector
  """
  defstruct [:vector_map, meta: nil]
  @type t :: %__MODULE__{vector_map: Mal.vector_map(), meta: Mal.t()}
end
