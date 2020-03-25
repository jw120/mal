defmodule Eval do
  @moduledoc """
  Mal evaluation
  """

  @doc """
  Full evaluation of a mal expression (including apply phase with handling of special forms)
  """
  @spec eval(Mal.t(), Env.t()) :: Mal.t()
  # credo:disable-for-next-line Credo.Check.Refactor.CyclomaticComplexity
  def eval(ast, env) do
    case ast do
      {:list, []} ->
        ast

      {:list, [{:symbol, "def!"} | rest]} ->
        SpecialForm.def_form(rest, env)

      {:list, [{:symbol, "do"} | rest]} ->
        SpecialForm.do_form(rest, env)

      {:list, [{:symbol, "if"} | rest]} ->
        SpecialForm.if_form(rest, env)

      {:list, [{:symbol, "let*"} | rest]} ->
        SpecialForm.let_form(rest, env)

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

  @doc """
  Evaluate a mal expression without considering the content of lists (special forms or
  function application)
  """
  @spec eval_ast(Mal.t(), Env.t()) :: Mal.t()
  def eval_ast(ast, env) do
    case ast do
      {:symbol, s} ->
        Env.get(env, s)

      {:list, contents} ->
        {:list, Enum.map(contents, &eval(&1, env))}

      {:vector, contents} ->
        {:vector, Seq.eval_map_values(contents, env)}

      {:hash_map, contents} ->
        {:hash_map, Seq.eval_map_values(contents, env)}

      _ ->
        ast
    end
  end
end
