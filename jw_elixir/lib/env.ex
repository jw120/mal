defmodule Env do

  @moduledoc """
  Implement mutable environment using ETS

  Environment modelled as a set-type ETS table with string keys
  and mal-type values. A special :__outer__ key is used to hold the
  id of the outer table (or nil)
  """

  @outer_key :__outer__

  @type t :: :ets.tid()

  @doc """
  Create a new environment with the given outer key (or nil)
  """
  @spec new(t | nil) :: t
  def new(o \\ nil) do
    table_id = :ets.new(:mal_env, [])
    :ets.insert(table_id, {@outer_key, o})
    table_id
  end

  @doc """
  Mutate the environment to include the given symbol and value
  """
  @spec set!(t, String.t, Mal.t) :: t
  def set!(env, s, val) do
    :ets.insert(env, {s, val})
    env
  end

  @doc """
  Return the first environment in the given environment's chain that has
  a value for the given symnol. Returns nil if not found.
  """
  @spec find(t, String.t) :: t | nil
  def find(env, s) do
    if :ets.member(env, s) do
      env
    else
      case :ets.lookup(env, @outer_key) do
        [{@outer_key, nil}] -> nil
        [{@outer_key, outer}] -> find(outer, s)
      end
    end
  end

  @doc """
  Return the value for the given symbol in the given environment.
  """
  @spec get(t, String.t) :: Mal.t
  def get(env, s) do
    case find(env, s) do
      nil -> raise MalException, "#{s} not found in environment"
      found_env ->
        [{^s, val}] = :ets.lookup(found_env, s)
        val
    end
  end

  @doc """
  Bind a an alternating list of symbols and values in the given environment. Later values
  can refer to earlier symbols in the list. Used to implement let*
  """
  @spec bind!(t, [Mal.t]) :: t
  def bind!(env, [{:symbol, s} | [val | rest]]) do
    eval_val = Eval.eval(val, env)
    set!(env, s, eval_val)
    bind!(env, rest)
  end
  def bind!(env, []), do: env
  def bind!(_, _), do: raise MalException, "Bad binding list for let"

end

