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

  @spec mal_atom(Mal.t(), pid) :: {:atom, pid, non_neg_integer()}
  def mal_atom(x, agent) do
    :ok = Agent.update(agent, &Map.put(&1, map_size(&1), x))
    atom_key = Agent.get(agent, &(map_size(&1) - 1))
    {:atom, agent, atom_key}
  end

  @spec mal_atom?(Mal.t()) :: boolean
  def mal_atom?({:atom, _, _}), do: true
  def mal_atom?(_), do: false

  @spec mal_deref(Mal.t()) :: Mal.t()
  def mal_deref({:atom, agent, key}) do
    Agent.get(agent, &Map.get(&1, key))
  end

  def mal_deref(_), do: raise(MalException, "deref called on non-atom")

  @spec mal_reset!(Mal.t(), Mal.t()) :: Mal.t()
  def mal_reset!({:atom, agent, key}, val) do
    Agent.update(agent, &Map.replace!(&1, key, val))
    val
  end

  def mal_reset!(_, _), do: raise(MalException, "reset! called on non-atom")

  @spec mal_swap!([Mal.t()]) :: Mal.t()
  def mal_swap!([{:atom, agent, key} | [%Mal.Function{closure: f} | other_args]]) do
    old_value = Agent.get(agent, &Map.get(&1, key))
    new_value = f.([old_value | other_args])
    Agent.update(agent, &Map.replace!(&1, key, new_value))
    new_value
  end

  def mal_swap!(_), do: raise(MalException, "Bad arguments for swap!")
end
