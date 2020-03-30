defmodule Eval.SpecialForm do
  @moduledoc """
  Functions to handle special forms during evaluation
  """

  @doc """
  If the given head expression is a symbol matching a special form then call
  the given special form and return true. Otherwise false
  """
  @spec invoke(Mal.t(), [Mal.t()], Env.t()) :: {:ok, Mal.t()} | :not_special
  def invoke({:symbol, "def!"}, args, env), do: {:ok, def_form(args, env)}
  def invoke({:symbol, "do"}, args, env), do: {:ok, do_form(args, env)}
  def invoke({:symbol, "fn*"}, args, env), do: {:ok, fn_form(args, env)}
  def invoke({:symbol, "if"}, args, env), do: {:ok, if_form(args, env)}
  def invoke({:symbol, "let*"}, args, env), do: {:ok, let_form(args, env)}
  def invoke({:symbol, "quote"}, args, env), do: {:ok, quote_form(args, env)}
  def invoke({:symbol, "quasiquote"}, args, env), do: {:ok, quasiquote_form(args, env)}
  def invoke(_, _, _), do: :not_special

  @doc """
  Handle the def! special form
  """
  @spec def_form([Mal.t()], Env.t()) :: Mal.t()
  def def_form([{:symbol, s}, val], env) do
    eval_val = Eval.eval(val, env)
    Env.set!(env, s, eval_val)
    eval_val
  end

  def def_form(_, _), do: raise(MalException, "Bad arguments to def!")

  @doc """
  Handle the do special form
  """
  @spec do_form([Mal.t()], Env.t()) :: Mal.t()
  def do_form([], _), do: raise(MalException, "No arguments to do")

  def do_form(exprs, env) do
    exprs
    |> Enum.reduce(nil, fn x, _ -> Eval.eval(x, env) end)
  end

  @doc """
  Handle the fn* special form
  """
  @spec fn_form([Mal.t()], Env.t()) :: Mal.t()
  def fn_form([{:list, binds}, val], env) do
    closure = fn args ->
      closure_env = env |> Env.new() |> Env.bind!(binds, args)
      Eval.eval(val, closure_env)
    end

    {:function, closure}
  end

  def fn_form([{:vector, vector_binds}, val], env) do
    binds = Seq.vector_to_list(vector_binds)
    fn_form([{:list, binds}, val], env)
  end

  def fn_form(_, _), do: raise(MalException, "Bad arguments to fn*")

  @doc """
  Handle the if special form
  """
  @spec if_form([Mal.t()], Env.t()) :: Mal.t()
  def if_form([condition | [if_val | rest]], env) do
    else_val =
      case rest do
        [val] -> val
        [] -> {nil}
        _ -> raise(MalException, "Too many arguments for if")
      end

    case Eval.eval(condition, env) do
      {:boolean, false} -> Eval.eval(else_val, env)
      {nil} -> Eval.eval(else_val, env)
      _ -> Eval.eval(if_val, env)
    end
  end

  def if_form(_, _), do: raise(MalException, "Bad arguments to if")

  @doc """
  Handle the let* special form
  """
  @spec let_form([Mal.t()], Env.t()) :: Mal.t()
  def let_form([{:list, bindings}, val], env) do
    let_env = Env.new(env)
    Env.bind_star!(let_env, bindings)
    Eval.eval(val, let_env)
  end

  def let_form([{:vector, vec_bindings}, val], env) do
    let_form([{:list, Seq.vector_to_list(vec_bindings)}, val], env)
  end

  def let_form(_, _), do: raise(MalException, "Bad arguments to def!")

  @doc """
  Handle the quote special form
  """
  @spec quote_form([Mal.t()], Env.t()) :: Mal.t()
  def quote_form([val], _env), do: val
  def quote_form(_, _), do: raise(MalException, "Bad arguments to quote")

  @doc """
  Handle the quasiquote special form
  """
  @spec quasiquote_form([Mal.t()], Env.t()) :: Mal.t()
  def quasiquote_form([val], env) do
    Eval.eval(quasiquote(val), env)
  end

  def quasiquote_form(_, _), do: raise(MalException, "Bad arguments to quasiquote")

  # Helper function to implement quasiquote
  @spec quasiquote(Mal.t()) :: {:list, [Mal.t()]}
  defp quasiquote({:vector, v}), do: quasiquote({:list, Seq.vector_to_list(v)})
  defp quasiquote({:list, [head | rest]}), do: quasiquote_pair([head | rest])
  defp quasiquote(ast), do: {:list, [{:symbol, "quote"}, ast]}

  # Helper function to quasi
  @spec quasiquote_pair(nonempty_list(Mal.t())) :: Mal.t()
  defp quasiquote_pair([{:symbol, "unquote"}, val]) do
    val
  end

  defp quasiquote_pair([{:list, [{:symbol, "splice-unquote"}, val]} | rest]) do
    {:list, [{:symbol, "concat"}, val, quasiquote({:list, rest})]}
  end

  defp quasiquote_pair([head | rest]) do
    {:list, [{:symbol, "cons"}, quasiquote(head), quasiquote({:list, rest})]}
  end
end
