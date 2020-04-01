defmodule Core.Atom do
  @moduledoc """
  Provide atom functionality for the core environment.

  Atoms are held on the agent in a map with integer keys on the Agent. They
  are represented as {:atom, pid, non_neg_integer} tuples.

  """

  use Agent

  @doc """
  Setup the agent for our Atoms. Must be called once at the beginning of program execution
  """
  @spec initialize :: pid
  def initialize do
    {:ok, agent} = Agent.start_link(fn -> %{} end, name: __MODULE__)
    agent
  end

  @doc """
  Implements the mal atom function which creates a new atom.
  """
  @spec mal_atom(Mal.t(), pid) :: Mal.Atom.t()
  def mal_atom(x, agent) do
    :ok = Agent.update(agent, &Map.put(&1, map_size(&1), x))
    key = Agent.get(agent, &(map_size(&1) - 1))
    %Mal.Atom{agent: agent, key: key}
  end

  @doc """
  Implements the mal atom? function which tests if the value is an atom
  """
  @spec mal_atom?(Mal.t()) :: boolean
  def mal_atom?(%Mal.Atom{}), do: true
  def mal_atom?(_), do: false

  @doc """
  Implements the mal deref function which returns the value held by the atom
  """
  @spec mal_deref(Mal.Atom.t()) :: Mal.t()
  def mal_deref(%Mal.Atom{agent: agent, key: key}) do
    Agent.get(agent, &Map.get(&1, key))
  end

  @doc """
  Implements the mal reset function which mutates the value held by the atom
  """
  @spec mal_reset!(Mal.Atom.t(), Mal.t()) :: Mal.t()
  def mal_reset!(%Mal.Atom{agent: agent, key: key}, new_val) do
    Agent.update(agent, &Map.replace!(&1, key, new_val))
    new_val
  end

  @doc """
  Implements the mal swap! function which mutates the value held by the atom using
  the supplied function.
  """
  @spec mal_swap!(Mal.arguments()) :: Mal.t()
  def mal_swap!([%Mal.Atom{agent: agent, key: key} | [%Mal.Function{closure: f} | other_args]]) do
    old_value = Agent.get(agent, &Map.get(&1, key))
    new_value = f.([old_value | other_args])
    Agent.update(agent, &Map.replace!(&1, key, new_value))
    new_value
  end
end
