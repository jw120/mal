#![allow(non_snake_case)]

use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::collections::HashMap;

use jw_rust_mal::core;
use jw_rust_mal::env;
use jw_rust_mal::printer;
use jw_rust_mal::reader;
use jw_rust_mal::types::{
    err, into_mal_closure, into_mal_hashmap, into_mal_seq, is_falsy, Env, Mal, MalError, MalResult,
};

static RUSTYLINE_HISTORY_FILE: &str = ".jw-rust-mal-history";
static RUSTYLINE_PROMPT: &str = "user> ";

fn READ(s: &str) -> MalResult {
    reader::read_str(s)
}

fn EVAL(mut ast: Mal, mut env: Env) -> MalResult {
    // Looping to implement TCO
    loop {
        // Evaluate a list applying functions and special forms
        if let Mal::Seq(true, xs, _meta) = ast.clone() {
            match xs.as_slice() {
                // Empty list just returns itself
                [] => return Ok(ast),

                // do special form - evaluate all but last element and loop with last element
                [Mal::Symbol(n), tail @ ..] if n == "do" => {
                    if tail.is_empty() {
                        return err("No arguments to do");
                    }
                    for x in &tail[..xs.len() - 1] {
                        EVAL(x.clone(), env.clone())?;
                    }
                    ast = xs[xs.len() - 1].clone();
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
                    env::set(&env, s, y.clone());
                    return Ok(y);
                }

                // let* special form -- update environment and continue
                [Mal::Symbol(n), Mal::Seq(_, xs, _), z] if n == "let*" => {
                    env = env::new(Some(&env));
                    let mut xs_iter = xs.iter();
                    loop {
                        match xs_iter.next() {
                            None => break,
                            Some(Mal::Symbol(s)) => {
                                if let Some(value) = xs_iter.next() {
                                    let value_eval = EVAL(value.clone(), env.clone())?;
                                    env::set(&env, s, value_eval.clone());
                                } else {
                                    return err("Bad value in set list");
                                }
                            }
                            Some(_non_symbol) => return err("Bad symbol in set list"),
                        }
                    }
                    ast = z.clone();
                }

                // fn* special form - return closure
                [Mal::Symbol(n), Mal::Seq(_, params, _), body] if n == "fn*" => {
                    return Ok(into_mal_closure(EVAL, body.clone(), params, &env))
                }

                // Non-special form
                _ => {
                    if let Mal::Seq(true, ys, _) = eval_ast(&ast, &env)? {
                        match ys.as_slice() {
                            // Builtin function, apply and done
                            [Mal::Function(f, _), tail @ ..] => return f(tail),

                            // Mal closure - move to new environment and continue with closure body
                            [Mal::Closure {
                                eval: _,
                                ast: closure_ast,
                                params,
                                env: closure_env,
                                is_macro: _,
                                meta: _,
                            }, tail @ ..] => {
                                env = env::new_binds(Some(closure_env), params, tail)?;
                                ast = (**closure_ast).clone();
                            }

                            // This should't happen
                            _ => return err("Applying non-function"),
                        }
                    } else {
                        return err("No longer a list!");
                    }
                }
            }
        } else {
            // Non-list
            return eval_ast(&ast, &env);
        }
    }
}

fn eval_ast(ast: &Mal, env: &Env) -> MalResult {
    match ast {
        Mal::Symbol(s) => env::get(env, s),
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
    println!("{}", printer::pr_str(x, true));
}

// read, evaluate, print (quiet suppress non-error output for use with start-up code)
fn rep(s: &str, env: &Env, quiet: bool) {
    match READ(s) {
        Ok(Mal::Nil) => {}
        Ok(x) => match EVAL(x, env.clone()) {
            Ok(value) => {
                if !quiet {
                    PRINT(&value);
                }
            }
            Err(MalError::Msg(msg)) => println!("Evaluation error: {msg}"),
            Err(MalError::Exception(e)) => {
                println!("Uncaught exception {}", printer::pr_str(&e, true));
            }
        },
        Err(MalError::Msg(msg)) => println!("Read error: {msg}"),
        Err(MalError::Exception(e)) => println!(
            "Unexpected exception during read {}",
            printer::pr_str(&e, true)
        ),
    }
}

fn main() -> Result<(), ReadlineError> {
    let mut rl = DefaultEditor::new()?;
    if rl.load_history(RUSTYLINE_HISTORY_FILE).is_err() {
        println!("No previous history.");
    }

    let repl_env: Env = env::new(None);
    let (builtins_rust, builtins_mal) = core::get_builtins();
    for (name, value) in builtins_rust {
        env::set(&repl_env, name, value);
    }
    for code in builtins_mal {
        rep(code, &repl_env, true);
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
                println!("Error reading line: {err:?}");
            }
        }
    }
    rl.save_history(RUSTYLINE_HISTORY_FILE)?;
    Ok(())
}
