defmodule Eval do
  @moduledoc """
  Mal evaluation
  """

  import Mal, only: :macros
  alias Eval.SpecialForm

  @doc """
  Full evaluation of a mal expression (including apply phase with handling of special forms)
  """
  @spec eval(Mal.t(), Env.t()) :: Mal.t()
  def eval(ast = %Mal.List{contents: [_ | _]}, env) do
    case macro_expand(ast, env) do
      %Mal.List{contents: [head | rest]} ->
        case SpecialForm.invoke(head, rest, env) do
          {:special, val} -> val
          :not_special -> apply_ast(eval_ast(ast, env))
        end

      expanded_ast ->
        eval_ast(expanded_ast, env)
    end
  end

  def eval(ast, env), do: eval_ast(ast, env)

  # Helper function to apply a evaluated list
  defp apply_ast(%Mal.List{contents: [%Mal.Function{closure: f, name: f_name} | rest]}) do
    f.(rest)
  rescue
    _e in FunctionClauseError ->
      reraise MalException, "Bad arguments for #{f_name}", __STACKTRACE__
  end

  defp apply_ast(args), do: raise(MalException, "Applying a non-function: #{inspect(args)}")

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

  defp is_macro_call(_, _), do: false
end
