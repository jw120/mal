#![allow(non_snake_case)]

use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::collections::HashMap;
use std::rc::Rc;

use jw_rust_mal::core::get_ns;
use jw_rust_mal::env::{env_get, env_new, env_new_binds, env_set, Env};
use jw_rust_mal::printer::pr_str;
use jw_rust_mal::reader::{read_str, ReadError};
use jw_rust_mal::types::{into_mal_list, mk_err, Mal};

static RUSTYLINE_HISTORY_FILE: &str = ".jw-rust-mal-history";
static RUSTYLINE_PROMPT: &str = "user> ";

fn READ(s: &str) -> Option<Result<Mal, ReadError>> {
    read_str(s)
}

fn EVAL(ast: &Mal, env: &Env) -> Result<Mal, String> {
    if let Mal::List(xs, _meta) = ast {
        match xs.as_slice() {
            [] => Ok(ast.clone()),
            [Mal::Symbol(n), tail @ ..] if n == "do" => apply_do(env, tail),
            [Mal::Symbol(n), cond, t, f] if n == "if" => apply_if(env, cond, t, f),
            [Mal::Symbol(n), cond, t] if n == "if" => apply_if(env, cond, t, &Mal::Nil),
            [Mal::Symbol(n), Mal::Symbol(s), x] if n == "def!" => apply_def(env, s, x),
            [Mal::Symbol(n), Mal::List(xs, _), z] if n == "let*" => apply_let(env, xs, z),
            [Mal::Symbol(n), Mal::Vector(xs, _), z] if n == "let*" => apply_let(env, xs, z),
            [Mal::Symbol(n), Mal::List(params, _), body] if n == "fn*" => {
                apply_fn(env.clone(), params.to_vec(), body.clone())
            }
            _ => {
                if let Mal::List(ys, _) = eval_ast(ast, env)? {
                    match ys.as_slice() {
                        [Mal::Function(f, _), tail @ ..] => Ok(f(tail.to_vec())?),
                        [_non_function, _tail @ ..] => mk_err("Applying non-function"),
                        [] => mk_err("List disappeared!"),
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

fn apply_do(env: &Env, xs: &[Mal]) -> Result<Mal, String> {
    let mut last = Mal::Nil;
    for x in xs {
        last = EVAL(x, env)?;
    }
    Ok(last)
}

fn apply_def(env: &Env, s: &str, x: &Mal) -> Result<Mal, String> {
    let y = EVAL(x, env)?;
    env_set(env, s, y.clone());
    Ok(y)
}

fn apply_if(env: &Env, cond: &Mal, t: &Mal, f: &Mal) -> Result<Mal, String> {
    match EVAL(cond, env)? {
        Mal::Nil | Mal::False => EVAL(f, env),
        _ => EVAL(t, env),
    }
}

fn apply_fn(env: Env, params: Vec<Mal>, body: Mal) -> Result<Mal, String> {
    let closure = move |values: Vec<Mal>| {
        let new_env = env_new_binds(Some(&env), &params, &values)?;
        EVAL(&body, &new_env)
    };
    Ok(Mal::Function(Rc::new(closure), Rc::new(Mal::Nil)))
}

fn apply_let(env: &Env, xs: &[Mal], z: &Mal) -> Result<Mal, String> {
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

fn eval_ast(ast: &Mal, env: &Env) -> Result<Mal, String> {
    match ast {
        Mal::Symbol(s) => env_get(env, s),
        Mal::List(xs, _) => {
            let mut ys = Vec::new();
            for x in xs.iter() {
                ys.push(EVAL(x, env)?);
            }
            Ok(into_mal_list(ys))
            // Ok(Mal::List(Rc::new(ys), Rc::new(Mal::Nil)))
        }
        Mal::Vector(xs, _) => {
            let mut ys = Vec::new();
            for x in xs.iter() {
                ys.push(EVAL(x, env)?);
            }
            Ok(Mal::Vector(Rc::new(ys), Rc::new(Mal::Nil)))
        }
        Mal::HashMap(m, _) => {
            let mut n = HashMap::new();
            for (k, v) in m.iter() {
                n.insert(k.clone(), EVAL(v, env)?);
            }
            Ok(Mal::HashMap(Rc::new(n), Rc::new(Mal::Nil)))
        }
        other_value => Ok(other_value.clone()),
    }
}

fn PRINT(x: &Mal) {
    println!("{}", pr_str(x, true));
}

fn rep(s: &str, env: &Env) {
    match READ(s) {
        None => {}
        Some(Ok(x)) => match EVAL(&x, env) {
            Ok(value) => PRINT(&value),
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
    for (name, value) in get_ns() {
        env_set(&repl_env, name, value);
    }

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
