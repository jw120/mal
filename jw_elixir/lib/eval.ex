defmodule Eval do
  @moduledoc """
  Mal evaluation
  """

  @doc """
  Evaludates a mal expression
  """
  @spec eval(Mal.t, Env.t) :: Mal.t
  def eval(ast, env) do
    case ast do
      {:list, []} ->
        ast
      {:list, _} ->
        evaluated_list = eval_ast(ast, env)
        case evaluated_list do
          {:list, [{:function, f} | rest]} ->
            f.(rest)
          _ ->
            raise MalException, "Non-function when evaluating a list: #{evaluated_list}"
        end
      _ ->
        eval_ast(ast, env)
    end
  end

  @spec eval_ast(Mal.t, Env.t) :: Mal.t
  def eval_ast(ast, env) do
    case ast do
      {:symbol, s} ->
        Env.get(env, s)
      {:list, contents} ->
        {:list, Enum.map(contents, &(eval(&1, env)))}
      {:vector, contents} ->
        {:vector, eval_map_values(contents, env)}
      {:map, contents} ->
        {:map, eval_map_values(contents, env)}
      _ ->
        ast
    end
  end

  @spec eval_map_values(map(), Env.t) :: map()
  defp eval_map_values(m, env) do
    Enum.map(m, fn {k, v} -> {k, eval(v, env)} end)
      |> Enum.into(%{})
  end

end


