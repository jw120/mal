defmodule Eval do
  @moduledoc """
  Mal evaluation
  """

  @doc """
  Full evaluation of a mal expression (including apply phase with handling of special forms)
  """
  @spec eval(Mal.t(), Env.t()) :: Mal.t()
  def eval(ast, env) do
    case ast do
      {:list, []} ->
        ast

      {:list, [{:symbol, "def!"} | rest]} ->
        def_special_form(rest, env)

      {:list, [{:symbol, "let*"} | rest]} ->
        let_special_form(rest, env)

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

  @doc """
  Handle the def! special form
  """
  @spec def_special_form([Mal.t()], Env.t()) :: Mal.t()
  def def_special_form([{:symbol, s}, val], env) do
    eval_val = eval(val, env)
    Env.set!(env, s, eval_val)
    eval_val
  end

  def def_special_form(_, _), do: raise(MalException, "Bad arguments to def!")

  @doc """
  Handle the let* special form
  """
  @spec let_special_form([Mal.t()], Env.t()) :: Mal.t()
  def let_special_form([{:list, bindings}, val], env) do
    let_env = Env.new(env)
    Env.bind!(let_env, bindings)
    eval(val, let_env)
  end

  def let_special_form([{:vector, vec_bindings}, val], env) do
    let_special_form([{:list, Seq.vector_to_list(vec_bindings)}, val], env)
  end

  def let_special_form(_, _), do: raise(MalException, "Bad arguments to def!")
end
