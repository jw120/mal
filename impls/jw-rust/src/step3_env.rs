#![allow(non_snake_case)]

use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::collections::HashMap;
use std::rc::Rc;

use jw_rust_mal::env::{env_get, env_new, env_set, Env};
use jw_rust_mal::printer::pr_str;
use jw_rust_mal::reader::{read_str, ReadError};
use jw_rust_mal::types::{into_mal_fn, into_mal_hashmap, into_mal_seq, mk_err, Mal};

static RUSTYLINE_HISTORY_FILE: &str = ".jw-rust-mal-history";
static RUSTYLINE_PROMPT: &str = "user> ";

fn READ(s: &str) -> Option<Result<Mal, ReadError>> {
    read_str(s)
}

fn EVAL(ast: &Mal, env: &Env) -> Result<Mal, String> {
    if let Mal::Seq(true, xs, _) = ast {
        match xs.as_slice() {
            [] => Ok(ast.clone()),
            [Mal::Symbol(n), Mal::Symbol(s), x] if n == "def!" => apply_def(env, s, x),
            [Mal::Symbol(n), Mal::Seq(_, xs, _), z] if n == "let*" => apply_let_star(env, xs, z),
            _ => {
                if let Mal::Seq(true, ys, _) = eval_ast(ast, env)? {
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

fn apply_def(env: &Env, s: &str, x: &Mal) -> Result<Mal, String> {
    let y = EVAL(x, env)?;
    env_set(env, s, y.clone());
    Ok(y)
}

fn apply_let_star(env: &Env, xs: &[Mal], z: &Mal) -> Result<Mal, String> {
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

    // Mini environment
    let repl_env: Env = env_new(None);
    env_set(&repl_env, "+", into_mal_fn(Rc::new(add)));
    env_set(&repl_env, "-", into_mal_fn(Rc::new(sub)));
    env_set(&repl_env, "*", into_mal_fn(Rc::new(mul)));
    env_set(&repl_env, "/", into_mal_fn(Rc::new(div)));
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

fn add(args: Vec<Mal>) -> Result<Mal, String> {
    match args.as_slice() {
        [Mal::Int(x), Mal::Int(y)] => Ok(Mal::Int(x + y)),
        _ => mk_err("Bad arguments for +"),
    }
}

fn sub(args: Vec<Mal>) -> Result<Mal, String> {
    match args.as_slice() {
        [Mal::Int(x), Mal::Int(y)] => Ok(Mal::Int(x - y)),
        _ => mk_err("Bad arguments for +"),
    }
}

fn mul(args: Vec<Mal>) -> Result<Mal, String> {
    match args.as_slice() {
        [Mal::Int(x), Mal::Int(y)] => Ok(Mal::Int(x * y)),
        _ => mk_err("Bad arguments for +"),
    }
}

fn div(args: Vec<Mal>) -> Result<Mal, String> {
    match args.as_slice() {
        [Mal::Int(_), Mal::Int(0)] => mk_err("Division by zero"),
        [Mal::Int(x), Mal::Int(y)] => Ok(Mal::Int(x / y)),
        _ => mk_err("Bad arguments for +"),
    }
}
