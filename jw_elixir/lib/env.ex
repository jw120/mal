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
  @spec set!(t, String.t(), Mal.t()) :: t
  def set!(env, s, val) do
    :ets.insert(env, {s, val})
    env
  end

  @doc """
  Return the first environment in the given environment's chain that has
  a value for the given symnol. Returns nil if not found.
  """
  @spec find(t, String.t()) :: t | nil
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
  @spec get(t, String.t()) :: Mal.t()
  def get(env, s) do
    case find(env, s) do
      nil ->
        raise MalException, "#{s} not found in environment"

      found_env ->
        [{^s, val}] = :ets.lookup(found_env, s)
        val
    end
  end

  @doc """
  Bind an alternating list of symbols and expressions in the given environment. Expressions
  are evaluated before binding and can refer to earlier symbols in the list. Used to implement let*
  """
  @spec bind_star!(t, [Mal.t()]) :: t
  def bind_star!(env, [{:symbol, s} | [val | rest]]) do
    eval_val = Eval.eval(val, env)
    set!(env, s, eval_val)
    bind_star!(env, rest)
  end

  def bind_star!(env, []), do: env
  def bind_star!(_, _), do: raise(MalException, "Bad binding list for let*")

  @doc """
  Bind a list of symbols and a list of values in the given environment. Used to
  implement fn*. The & symbol means to bind the next-symbol to a list of all
  remaining bindings
  """
  @spec bind!(t, [Mal.t()], [Mal.t()]) :: t
  def bind!(env, [{:symbol, "&"} | [{:symbol, rest_bind}]], exprs) do
    set!(env, rest_bind, {:list, exprs})
  end

  def bind!(env, [{:symbol, s} | other_syms], [x | other_exprs]) do
    set!(env, s, x)
    bind!(env, other_syms, other_exprs)
  end

  def bind!(env, [], []), do: env
  def bind!(_, _, _), do: raise(MalException, "Bad binding list")
end
