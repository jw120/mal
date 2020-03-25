defmodule Env do

  @moduledoc """
  Implement mutable environment using ETS

  Environment modelled as a set-type ETS table with string keys
  and mal-type values. A special :__outer__ key is used to hold the
  id of the outer table (or nil)
  """

  @outer_key :__outer__

  @type t :: :ets.tid()

  @spec new(t | nil) :: t
  def new(o \\ nil) do
    table_id = :ets.new(:mal_env, [])
    :ets.insert(table_id, {@outer_key, o})
    table_id
  end

  @spec set!(t, String.t, Mal.t) :: t
  def set!(env, s, val) do
    :ets.insert(env, {s, val})
    env
  end

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

  @spec get(t, String.t) :: Mal.t
  def get(env, s) do
    case find(env, s) do
      nil -> raise MalException, "#{s} not found in environment"
      found_env ->
        [{^s, val}] = :ets.lookup(found_env, s)
        val
    end
  end
end
