defmodule Eval do
  @moduledoc """
  Mal evaluation
  """

  alias Eval.SpecialForm
  import Mal, only: :macros

  @doc """
  Full evaluation of a mal expression (including apply phase with handling of special forms)
  """
  @spec eval(Mal.t(), Env.t()) :: Mal.t()
  def eval(ast, env) do
    case ast do
      %Mal.List{contents: []} ->
        ast

      %Mal.List{} ->
        case macro_expand(ast, env) do
          %Mal.List{contents: [head | rest]} ->
            case SpecialForm.invoke(head, rest, env) do
              {:special, val} ->
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

          expanded_ast ->
            eval_ast(expanded_ast, env)
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

  @doc """
  Repeatedly macro-expand the given ast
  """
  @spec macro_expand(Mal.t(), Env.t()) :: Mal.t()
  def macro_expand(ast, env) do
    if is_macro_call(ast, env) do
      %Mal.List{contents: [sym(macro_name) | args]} = ast
      %Mal.Function{closure: macro_closure, is_macro: true} = Env.get(env, macro_name)
      macro_expand(macro_closure.(args), env)
    else
      ast
    end
  end

  # Helper function that checks if the ast represents a macro function call
  @spec is_macro_call(Mal.t(), Env.t()) :: boolean
  defp is_macro_call(%Mal.List{contents: [sym(s) | _]}, env) do
    case Env.find(env, s) do
      nil ->
        false

      containing_env ->
        case Env.get(containing_env, s) do
          %Mal.Function{is_macro: is_macro} -> is_macro
          _ -> false
        end
    end
  end

  defp is_macro_call(_, _env), do: false
end
