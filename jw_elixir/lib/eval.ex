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
      %Mal.List{contents: []} ->
        ast

      %Mal.List{contents: [head | rest]} ->
        case SpecialForm.invoke(head, rest, env) do
          {:ok, val} ->
            val

          :not_special ->
            %Mal.List{contents: evaluated_list} = eval_ast(ast, env)

            case evaluated_list do
              [%Mal.Function{closure: f} | rest] ->
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

      %Mal.List{contents: contents} ->
        %Mal.List{contents: Enum.map(contents, &eval(&1, env))}

      %Mal.Vector{vector_map: v} ->
        %Mal.Vector{vector_map: Seq.eval_map_values(v, env)}

      %Mal.HashMap{hashmap_map: m} ->
        %Mal.HashMap{hashmap_map: Seq.eval_map_values(m, env)}

      _ ->
        ast
    end
  end
end
