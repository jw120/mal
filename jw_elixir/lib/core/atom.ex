defmodule Core.Atom do
  @moduledoc """
  Provide atom functionality for the core environment.

  Atoms are held on the agent in a map with integer keys on the Agent. They
  are represented as {:atom, pid, non_neg_integer} tuples.

  """

  import Core.Helpers
  use Agent

  @doc """
  Add functions for this module to the environment
  """
  def add(env) do
    {:ok, agent_pid} = Agent.start_link(fn -> %{} end, name: __MODULE__)

    wrap1(env, "atom", fn
      x ->
        :ok = Agent.update(agent_pid, &Map.put(&1, map_size(&1), x))
        key = Agent.get(agent_pid, &(map_size(&1) - 1))
        %Mal.Atom{agent: agent_pid, key: key}
    end)

    wrap1(env, "atom?", fn
      %Mal.Atom{} -> true
      _ -> false
    end)

    wrap1(env, "deref", &mal_deref/1)

    wrap2(env, "reset!", fn
      %Mal.Atom{agent: agent, key: key}, new_val ->
        Agent.update(agent, &Map.replace!(&1, key, new_val))
        new_val
    end)

    wrapN(env, "swap!", fn
      [%Mal.Atom{agent: agent, key: key} | [%Mal.Function{closure: f} | other_args]] ->
        old_value = Agent.get(agent, &Map.get(&1, key))
        new_value = f.([old_value | other_args])
        Agent.update(agent, &Map.replace!(&1, key, new_value))
        new_value
    end)
  end

  @doc """
  Implements the mal deref function which returns the value held by the atom
  """
  @spec mal_deref(Mal.Atom.t()) :: Mal.t()
  def mal_deref(%Mal.Atom{agent: agent, key: key}) do
    Agent.get(agent, &Map.get(&1, key))
  end
end
