defmodule Eval.SpecialForm do
  @moduledoc """
  Functions to handle special forms during evaluation
  """

  import Mal, only: :macros

  @doc """
  If the given head expression is a symbol matching a special form then call
  the given special form and return the result with :special. Otherwise :not_special
  """
  @spec invoke(Mal.t(), Mal.arguments(), Env.t()) :: {:special, Mal.t()} | :not_special
  def invoke(sym("def!"), args, env), do: {:special, def_form(args, env)}
  def invoke(sym("defmacro!"), args, env), do: {:special, defmacro_form(args, env)}
  def invoke(sym("do"), args, env), do: {:special, do_form(args, env)}
  def invoke(sym("fn*"), args, env), do: {:special, fn_form(args, env)}
  def invoke(sym("if"), args, env), do: {:special, if_form(args, env)}
  def invoke(sym("let*"), args, env), do: {:special, let_form(args, env)}
  def invoke(sym("macroexpand"), args, env), do: {:special, macroexpand_form(args, env)}
  def invoke(sym("quote"), args, env), do: {:special, quote_form(args, env)}
  def invoke(sym("quasiquote"), args, env), do: {:special, quasiquote_form(args, env)}
  def invoke(sym("try*"), args, env), do: {:special, try_form(args, env)}
  def invoke(_, _, _), do: :not_special

  @doc """
  Handle the def! special form
  """
  @spec def_form(Mal.arguments(), Env.t()) :: Mal.t()
  def def_form([sym(s), val], env) do
    eval_val = Eval.eval(val, env)
    Env.set!(env, s, eval_val)
    eval_val
  end

  @doc """
  Handle the defmacro! special form
  """
  @spec defmacro_form(Mal.arguments(), Env.t()) :: Mal.t()
  def defmacro_form([sym(s), val], env) do
    macro_val =
      case Eval.eval(val, env) do
        f = %Mal.Function{} -> %{f | is_macro: true}
        _ -> raise(MalException, "Need a function for defmacro!")
      end

    Env.set!(env, s, macro_val)
    macro_val
  end

  @doc """
  Handle the do special form
  """
  @spec do_form(Mal.arguments(), Env.t()) :: Mal.t()
  def do_form([], _), do: raise(MalException, "No arguments to do")

  def do_form(exprs, env) do
    exprs
    |> Enum.reduce(nil, fn x, _acc -> Eval.eval(x, env) end)
  end

  @doc """
  Handle the fn* special form
  """
  @spec fn_form(Mal.arguments(), Env.t()) :: Mal.t()
  def fn_form([%Mal.List{contents: binds}, val], env) do
    %Mal.Function{
      is_macro: false,
      name: "fn*",
      closure: fn args ->
        closure_env = env |> Env.new() |> Env.bind!(binds, args)
        Eval.eval(val, closure_env)
      end
    }
  end

  def fn_form([%Mal.Vector{vector_map: vector_binds}, val], env) do
    binds = Seq.vector_map_to_list(vector_binds)
    fn_form([%Mal.List{contents: binds}, val], env)
  end

  @doc """
  Handle the if special form
  """
  @spec if_form(Mal.arguments(), Env.t()) :: Mal.t()
  def if_form([condition | [then_val | rest]], env) do
    else_val =
      case rest do
        [val] -> val
        [] -> nil
        _ -> raise(MalException, "Too many arguments for if")
      end

    if Eval.eval(condition, env) in [false, nil] do
      Eval.eval(else_val, env)
    else
      Eval.eval(then_val, env)
    end
  end

  @doc """
  Handle the let* special form
  """
  @spec let_form(Mal.arguments(), Env.t()) :: Mal.t()
  def let_form([%Mal.List{contents: bindings}, val], env) do
    let_env = Env.new(env)
    Env.bind_star!(let_env, bindings)
    Eval.eval(val, let_env)
  end

  def let_form([%Mal.Vector{vector_map: vec_bindings}, val], env) do
    let_form([%Mal.List{contents: Seq.vector_map_to_list(vec_bindings)}, val], env)
  end

  @doc """
  Handle the macroexpand special form
  """
  @spec macroexpand_form(Mal.arguments(), Env.t()) :: Mal.t()
  def macroexpand_form([ast], env) do
    Eval.macro_expand(ast, env)
  end

  @doc """
  Handle the quote special form
  """
  @spec quote_form(Mal.arguments(), Env.t()) :: Mal.t()
  def quote_form([val], _env), do: val

  @doc """
  Handle the quasiquote special form
  """
  @spec quasiquote_form(Mal.arguments(), Env.t()) :: Mal.t()
  def quasiquote_form([val], env) do
    Eval.eval(quasiquote(val), env)
  end

  @doc """
  Handle the quasiquote try* form
  """
  @spec try_form(Mal.arguments(), Env.t()) :: Mal.t()
  def try_form([a, %Mal.List{contents: [sym("catch*"), sym(b), c]}], env) do
    try do
      #      IO.puts("try eval1")
      Eval.eval(a, env)
    rescue
      e in MalException ->
        #        IO.puts("try rescue1 binding #{b}")
        catch_env = Env.new(env)
        Env.set!(catch_env, b, e.val)
        Eval.eval(c, catch_env)
    end
  end

  def try_form([a], env) do
    Eval.eval(a, env)
  end

  # Helper function to implement quasiquote
  @spec quasiquote(Mal.t()) :: Mal.t()
  defp quasiquote(%Mal.Vector{vector_map: v}) do
    quasiquote(%Mal.List{contents: Seq.vector_map_to_list(v)})
  end

  defp quasiquote(%Mal.List{contents: [sym("unquote"), val]}) do
    val
  end

  defp quasiquote(%Mal.List{contents: [%Mal.List{contents: [sym("splice-unquote"), val]} | rest]}) do
    %Mal.List{contents: qq_rest_contents} = quasiquote(%Mal.List{contents: rest})

    %Mal.List{
      contents: [
        sym("concat"),
        val,
        %Mal.List{contents: qq_rest_contents}
      ]
    }
  end

  defp quasiquote(%Mal.List{contents: [head | rest]}) do
    %Mal.List{contents: qq_rest_contents} = quasiquote(%Mal.List{contents: rest})

    %Mal.List{
      contents: [
        sym("cons"),
        quasiquote(head),
        %Mal.List{contents: qq_rest_contents}
      ]
    }
  end

  defp quasiquote(ast), do: %Mal.List{contents: [sym("quote"), ast]}
end
