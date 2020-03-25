defmodule SpecialForm do
  @moduledoc """
  Functions to handle special forms during evaluation
  """
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
end
