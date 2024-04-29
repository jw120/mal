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

fn READ(s: &str) -> Result<Mal, ReadError> {
    read_str(s)
}

fn EVAL(ast: &Mal, env: &Env) -> MalResult {
    if let Mal::Seq(true, xs, _meta) = ast {
        match xs.as_slice() {
            [] => Ok(ast.clone()),
            [Mal::Symbol(n), tail @ ..] if n == "do" => apply_do(env, tail),
            [Mal::Symbol(n), cond, t, f] if n == "if" => apply_if(env, cond, t, f),
            [Mal::Symbol(n), cond, t] if n == "if" => apply_if(env, cond, t, &Mal::Nil),
            [Mal::Symbol(n), Mal::Symbol(s), x] if n == "def!" => apply_def(env, s, x),
            [Mal::Symbol(n), Mal::Seq(_, xs, _), z] if n == "let*" => apply_let(env, xs, z),
            [Mal::Symbol(n), Mal::Seq(_, params, _), body] if n == "fn*" => {
                Ok(into_mal_closure(body.clone(), params, env))
            }
            _ => {
                if let Mal::Seq(true, ys, _) = eval_ast(ast, env)? {
                    match ys.as_slice() {
                        [Mal::Function(f, _), tail @ ..] => Ok(f(tail)?),
                        [Mal::Closure {
                            ast: closure_ast,
                            params,
                            env: closure_env,
                            meta: _,
                        }, tail @ ..] => {
                            let new_env = env_new_binds(Some(closure_env), params, tail)?;
                            EVAL(closure_ast, &new_env)
                        }
                        // This should't happen
                        _ => mk_err("Applying non-function"),
                    }
                } else {
                    mk_err("No longer a list!")
                }
            }
        }
    } else {
        // Non-list
        eval_ast(ast, env)
    }
}

fn apply_do(env: &Env, xs: &[Mal]) -> MalResult {
    let mut last = Mal::Nil;
    for x in xs {
        last = EVAL(x, env)?;
    }
    Ok(last)
}

fn apply_def(env: &Env, s: &str, x: &Mal) -> MalResult {
    let y = EVAL(x, env)?;
    env_set(env, s, y.clone());
    Ok(y)
}

fn apply_if(env: &Env, cond: &Mal, t: &Mal, f: &Mal) -> MalResult {
    EVAL(if is_falsy(&EVAL(cond, env)?) { f } else { t }, env)
}

fn apply_let(env: &Env, xs: &[Mal], z: &Mal) -> MalResult {
    let new_env = env_new(Some(env));
    let mut xs_iter = xs.iter();
    loop {
        match xs_iter.next() {
            None => return EVAL(z, &new_env),
            Some(Mal::Symbol(s)) => {
                if let Some(value) = xs_iter.next() {
                    let value_eval = EVAL(value, &new_env)?;
                    env_set(&new_env, s, value_eval.clone());
                } else {
                    return mk_err("Bad value in set list");
                }
            }
            Some(_non_symbol) => return mk_err("Bad symbol in set list"),
        }
    }
}

fn eval_ast(ast: &Mal, env: &Env) -> MalResult {
    match ast {
        Mal::Symbol(s) => env_get(env, s),
        Mal::Seq(is_list, xs, _) => {
            let mut ys = Vec::new();
            for x in xs.iter() {
                ys.push(EVAL(x, env)?);
            }
            Ok(into_mal_seq(*is_list, ys))
        }
        Mal::HashMap(m, _) => {
            let mut n = HashMap::new();
            for (k, v) in m.iter() {
                n.insert(k.clone(), EVAL(v, env)?);
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
        Ok(Mal::Nil) => {}
        Ok(x) => match EVAL(&x, env) {
            Ok(value) => {
                if !quiet {
                    PRINT(&value)
                }
            }
            Err(msg) => println!("Evaluation error: {}", msg),
        },
        Err(ReadError::Internal(msg)) => println!("Internal error: {}", msg),
        Err(ReadError::Parse(msg)) => println!("Parse error: {}", msg),
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
