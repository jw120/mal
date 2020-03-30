defmodule Eval do
  @moduledoc """
  Mal evaluation
  """

  alias Eval.SpecialForm

  @doc """
  Full evaluation of a mal expression (including apply phase with handling of special forms)
  """
  @spec eval(Mal.t(), Env.t()) :: Mal.t()
  def eval(ast, env) do
    case ast do
      [] ->
        ast

      [head | rest] ->
        case SpecialForm.invoke(head, rest, env) do
          {:ok, val} ->
            val

          :not_special ->
            evaluated_list = eval_ast(ast, env)

            case evaluated_list do
              [{:function, f} | rest] ->
                f.(rest)

              _ ->
                raise MalException,
                      "Non-function when evaluating a list: #{inspect(evaluated_list)}"
            end
        end

      _ ->
        eval_ast(ast, env)
    end
  end

  @doc """
  Evaluate a mal expression without considering the content of lists (special forms or
  function application)
  """
  @spec eval_ast(Mal.t(), Env.t()) :: Mal.t()
  def eval_ast(ast, env) do
    case ast do
      {:symbol, s} ->
        Env.get(env, s)

      contents when is_list(contents) ->
        Enum.map(contents, &eval(&1, env))

      {:vector, contents} ->
        {:vector, Seq.eval_map_values(contents, env)}

      {:hash_map, contents} ->
        {:hash_map, Seq.eval_map_values(contents, env)}

      _ ->
        ast
    end
  end
end
