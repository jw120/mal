defmodule Env do
  defstruct outer: nil, data: %{}

  @type t :: %Env{outer: t, data: %{optional(String.t) => Mal.t}}

  @spec new(String.t | nil) :: t
  def new(o \\ nil) do
    %Env{outer: o}
  end

  @spec set(t, String.t, Mal.t) :: t
  def set(env, s, val) do
    %{env | data: Map.put(env.data, s, val)}
  end

  @spec find(t, String.t) :: t | nil
  def find(env, s) do
    case {Map.has_key?(env.data, s), env.outer} do
      {true, _} -> env
      {false, nil} -> nil
      {false, _} -> find(env.outer, s)
    end
  end

  @spec get(t, String.t) :: Mal.t
  def get(env, s) do
    case find(env, s) do
      nil -> raise MalException, "#{s} not found in environment"
      found_env -> found_env.data[s]
    end
  end
end
