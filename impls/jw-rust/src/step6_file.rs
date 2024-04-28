#![allow(non_snake_case)]

use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::collections::HashMap;

use jw_rust_mal::core::get_builtins;
use jw_rust_mal::env::*;
use jw_rust_mal::printer::pr_str;
use jw_rust_mal::reader::{read_str, ReadError};
use jw_rust_mal::types::*;

static RUSTYLINE_HISTORY_FILE: &str = ".jw-rust-mal-history";
static RUSTYLINE_PROMPT: &str = "user> ";

fn READ(s: &str) -> Option<Result<Mal, ReadError>> {
    read_str(s)
}

fn EVAL(mut ast: Mal, mut env: Env) -> Result<Mal, String> {
    // Looping to implement TCO
    loop {
        // Evaluate a list applying functions and special forms
        if let Mal::Seq(true, xs, _meta) = ast.clone() {
            match xs.as_slice() {
                // Empty list just returns itself
                [] => return Ok(ast.clone()),

                // do special form - evaluate all but last element and loop with last element
                [Mal::Symbol(n), tail @ ..] if n == "do" => {
                    if tail.is_empty() {
                        return mk_err("No arguments to do");
                    } else {
                        for x in tail[..xs.len() - 1].iter() {
                            EVAL(x.clone(), env.clone())?;
                        }
                        ast = xs[xs.len() - 1].clone();
                    }
                }
                // if special form - continue with appropriate branch
                [Mal::Symbol(n), cond, t, f] if n == "if" => {
                    ast = if is_falsy(&EVAL(cond.clone(), env.clone())?) {
                        f.clone()
                    } else {
                        t.clone()
                    };
                }

                // if special form with missing else-branch - continue with appropriate branch
                [Mal::Symbol(n), cond, t] if n == "if" => {
                    ast = if is_falsy(&EVAL(cond.clone(), env.clone())?) {
                        Mal::Nil
                    } else {
                        t.clone()
                    };
                }

                // def! special form - add to environment and done
                [Mal::Symbol(n), Mal::Symbol(s), x] if n == "def!" => {
                    let y = EVAL(x.clone(), env.clone())?;
                    env_set(&env, s, y.clone());
                    return Ok(y);
                }

                // let* special form -- update environment and continue
                [Mal::Symbol(n), Mal::Seq(_, xs, _), z] if n == "let*" => {
                    env = env_new(Some(&env));
                    let mut xs_iter = xs.iter();
                    loop {
                        match xs_iter.next() {
                            None => break,
                            Some(Mal::Symbol(s)) => {
                                if let Some(value) = xs_iter.next() {
                                    let value_eval = EVAL(value.clone(), env.clone())?;
                                    env_set(&env, s, value_eval.clone());
                                } else {
                                    return mk_err("Bad value in set list");
                                }
                            }
                            Some(_non_symbol) => return mk_err("Bad symbol in set list"),
                        }
                    }
                    ast = z.clone();
                }

                // fn* special form - return closure
                [Mal::Symbol(n), Mal::Seq(_, params, _), body] if n == "fn*" => {
                    return Ok(into_mal_closure(body.clone(), params, &env))
                }

                // eval as if it was a special form - continue with given ast and root environment
                [Mal::Symbol(n), eval_ast] if n == "eval" => {
                    // We have to EVAL the argument as this is really a function call not a special form
                    let eval_ast_evaluated = EVAL(eval_ast.clone(), env.clone())?;
                    // Then we continue with the argument
                    ast = eval_ast_evaluated.clone();
                    env = env_outermost(&env).clone();
                }

                // Non-special form
                _ => {
                    if let Mal::Seq(true, ys, _) = eval_ast(&ast, &env)? {
                        match ys.as_slice() {
                            // Builtin function, apply and done
                            [Mal::Function(f, _), tail @ ..] => return f(tail),

                            // Mal closure - move to new environment and continue with closure body
                            [Mal::Closure {
                                ast: closure_ast,
                                params,
                                env: closure_env,
                                meta: _,
                            }, tail @ ..] => {
                                env = env_new_binds(Some(closure_env), params, tail)?;
                                ast = (**closure_ast).clone();
                            }

                            // This should't happen
                            _ => return mk_err("Applying non-function"),
                        }
                    } else {
                        return mk_err("No longer a list!");
                    }
                }
            }
        } else {
            // Non-list
            return eval_ast(&ast, &env);
        }
    }
}

fn eval_ast(ast: &Mal, env: &Env) -> Result<Mal, String> {
    match ast {
        Mal::Symbol(s) => env_get(env, s),
        Mal::Seq(is_list, xs, _) => {
            let mut ys = Vec::new();
            for x in xs.iter() {
                ys.push(EVAL(x.clone(), env.clone())?);
            }
            Ok(into_mal_seq(*is_list, ys))
        }
        Mal::HashMap(m, _) => {
            let mut n = HashMap::new();
            for (k, v) in m.iter() {
                n.insert(k.clone(), EVAL(v.clone(), env.clone())?);
            }
            Ok(into_mal_hashmap(n))
        }
        other_value => Ok(other_value.clone()),
    }
}

fn PRINT(x: &Mal) {
    println!("{}", pr_str(x, true));
}

// read, evaluate, print (quiet suppress non-error output for use with start-up code)
fn rep(s: &str, env: &Env, quiet: bool) {
    match READ(s) {
        None => {}
        Some(Ok(x)) => match EVAL(x, env.clone()) {
            Ok(value) => {
                if !quiet {
                    PRINT(&value)
                }
            }
            Err(msg) => println!("Evaluation error: {}", msg),
        },
        Some(Err(ReadError::Internal(msg))) => println!("Internal error: {}", msg),
        Some(Err(ReadError::Parse(msg))) => println!("Parse error: {}", msg),
    }
}

fn main() -> Result<(), ReadlineError> {
    let mut rl = DefaultEditor::new()?;
    if rl.load_history(RUSTYLINE_HISTORY_FILE).is_err() {
        println!("No previous history.");
    }

    let repl_env: Env = env_new(None);
    let (builtins_rust, builtins_mal) = get_builtins();
    for (name, value) in builtins_rust {
        env_set(&repl_env, name, value);
    }
    for code in builtins_mal {
        rep(code, &repl_env, true)
    }

    loop {
        match rl.readline(RUSTYLINE_PROMPT) {
            Ok(line) => {
                let line = line.trim_end_matches("\n\r");
                if !line.is_empty() {
                    rl.add_history_entry(line)?;
                    rep(line, &repl_env, false);
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("Interrupted");
                break;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                println!("Error reading line: {:?}", err);
            }
        }
    }
    rl.save_history(RUSTYLINE_HISTORY_FILE)?;
    Ok(())
}
