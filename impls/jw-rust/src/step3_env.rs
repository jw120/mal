#![allow(non_snake_case)]

use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::collections::HashMap;

use jw_rust_mal::env;
use jw_rust_mal::printer;
use jw_rust_mal::reader;
use jw_rust_mal::types::*;

static RUSTYLINE_HISTORY_FILE: &str = ".jw-rust-mal-history";
static RUSTYLINE_PROMPT: &str = "user> ";

fn READ(s: &str) -> MalResult {
    reader::read_str(s)
}

fn EVAL(ast: &Mal, env: &Env) -> MalResult {
    if let Mal::Seq(true, xs, _) = ast {
        match xs.as_slice() {
            [] => Ok(ast.clone()),
            [Mal::Symbol(n), Mal::Symbol(s), x] if n == "def!" => apply_def(env, s, x),
            [Mal::Symbol(n), Mal::Seq(_, xs, _), z] if n == "let*" => apply_let_star(env, xs, z),
            _ => {
                if let Mal::Seq(true, ys, _) = eval_ast(ast, env)? {
                    match ys.as_slice() {
                        [Mal::Function(f, _), tail @ ..] => Ok(f(tail)?),
                        [Mal::Closure {
                            eval: _,
                            ast: closure_ast,
                            params,
                            env: closure_env,
                            meta: _,
                        }, tail @ ..] => {
                            let new_env = env::new_binds(Some(closure_env), params, tail)?;
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

fn apply_def(env: &Env, s: &str, x: &Mal) -> MalResult {
    let y = EVAL(x, env)?;
    env::set(env, s, y.clone());
    Ok(y)
}

fn apply_let_star(env: &Env, xs: &[Mal], z: &Mal) -> MalResult {
    let new_env = env::new(Some(env));
    let mut xs_iter = xs.iter();
    loop {
        match xs_iter.next() {
            None => return EVAL(z, &new_env),
            Some(Mal::Symbol(s)) => {
                if let Some(value) = xs_iter.next() {
                    let value_eval = EVAL(value, &new_env)?;
                    env::set(&new_env, s, value_eval.clone());
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
        Mal::Symbol(s) => env::get(env, s),
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
    println!("{}", printer::pr_str(x, true));
}

fn rep(s: &str, env: &Env) {
    match READ(s) {
        Ok(Mal::Nil) => {}
        Ok(x) => match EVAL(&x, env) {
            Ok(value) => PRINT(&value),
            Err(msg) => println!("Evaluation error: {}", msg),
        },
        Err(msg) => println!("{}", msg),
    }
}

fn main() -> Result<(), ReadlineError> {
    let mut rl = DefaultEditor::new()?;
    if rl.load_history(RUSTYLINE_HISTORY_FILE).is_err() {
        println!("No previous history.");
    }

    // Mini environment
    let repl_env: Env = env::new(None);
    env::set(&repl_env, "+", into_mal_fn(add));
    env::set(&repl_env, "-", into_mal_fn(sub));
    env::set(&repl_env, "*", into_mal_fn(mul));
    env::set(&repl_env, "/", into_mal_fn(div));
    loop {
        match rl.readline(RUSTYLINE_PROMPT) {
            Ok(line) => {
                let line = line.trim_end_matches("\n\r");
                if !line.is_empty() {
                    rl.add_history_entry(line)?;
                    rep(line, &repl_env);
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

fn add(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Int(x), Mal::Int(y)] => Ok(Mal::Int(x + y)),
        _ => mk_err("Bad arguments for +"),
    }
}

fn sub(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Int(x), Mal::Int(y)] => Ok(Mal::Int(x - y)),
        _ => mk_err("Bad arguments for +"),
    }
}

fn mul(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Int(x), Mal::Int(y)] => Ok(Mal::Int(x * y)),
        _ => mk_err("Bad arguments for +"),
    }
}

fn div(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Int(_), Mal::Int(0)] => mk_err("Division by zero"),
        [Mal::Int(x), Mal::Int(y)] => Ok(Mal::Int(x / y)),
        _ => mk_err("Bad arguments for +"),
    }
}
