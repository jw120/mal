#![allow(non_snake_case)]

use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::collections::HashMap;
use std::rc::Rc;

use jw_rust_mal::env::{env_get, env_new, env_new_binds, env_set, Env};
use jw_rust_mal::printer::pr_str;
use jw_rust_mal::reader::{read_str, ReadError};
use jw_rust_mal::types::Mal;

static RUSTYLINE_HISTORY_FILE: &str = ".jw-rust-mal-history";
static RUSTYLINE_PROMPT: &str = "user> ";

fn READ(s: &str) -> Option<Result<Mal, ReadError>> {
    read_str(s)
}

fn EVAL(ast: &Mal, env: Env) -> Result<Mal, String> {
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
                apply_fn(env, params.to_vec(), body.clone())
            }
            _ => {
                if let Mal::List(ys, _) = eval_ast(ast, env)? {
                    match ys.as_slice() {
                        [Mal::Function(f, _), tail @ ..] => Ok(f(tail.to_vec())?),
                        [_non_function, _tail @ ..] => Err("Applying non-function".to_string()),
                        [] => Err("List disappeared!".to_string()),
                    }
                } else {
                    Err("No longer a list!".to_string())
                }
            }
        }
    } else {
        // Non-list
        eval_ast(ast, env)
    }
}

fn apply_do(env: Env, xs: &[Mal]) -> Result<Mal, String> {
    let mut last = Mal::Nil;
    for x in xs {
        last = eval_ast(x, env.clone())?;
    }
    Ok(last)
}

fn apply_def(env: Env, s: &str, x: &Mal) -> Result<Mal, String> {
    let y = EVAL(x, env.clone())?;
    env_set(&env, s, y.clone());
    Ok(y)
}

fn apply_if(env: Env, cond: &Mal, t: &Mal, f: &Mal) -> Result<Mal, String> {
    match EVAL(cond, env.clone())? {
        Mal::Nil | Mal::False => EVAL(f, env.clone()),
        _ => EVAL(t, env),
    }
}

fn apply_fn(env: Env, params: Vec<Mal>, body: Mal) -> Result<Mal, String> {
    let closure = move |values: Vec<Mal>| {
        let new_env = env_new_binds(Some(env.clone()), &params, &values)?;
        EVAL(&body, new_env)
    };
    Ok(Mal::Function(Rc::new(closure), Rc::new(Mal::Nil)))
}

fn apply_let(env: Env, xs: &[Mal], z: &Mal) -> Result<Mal, String> {
    let new_env = env_new(Some(env.clone()));
    let mut xs_iter = xs.iter();
    loop {
        match xs_iter.next() {
            None => return EVAL(z, new_env),
            Some(Mal::Symbol(s)) => {
                if let Some(value) = xs_iter.next() {
                    let value_eval = EVAL(value, new_env.clone())?;
                    env_set(&new_env, s, value_eval.clone());
                } else {
                    return Err("Bad value in set list".to_string());
                }
            }
            Some(_non_symbol) => return Err("Bad symbol in set list".to_string()),
        }
    }
}

fn eval_ast(ast: &Mal, env: Env) -> Result<Mal, String> {
    match ast {
        Mal::Symbol(s) => env_get(env, s),
        Mal::List(xs, _) => {
            let mut ys = Vec::new();
            for x in xs.iter() {
                ys.push(EVAL(x, env.clone())?);
            }
            Ok(Mal::List(Rc::new(ys), Rc::new(Mal::Nil)))
        }
        Mal::Vector(xs, _) => {
            let mut ys = Vec::new();
            for x in xs.iter() {
                ys.push(EVAL(x, env.clone())?);
            }
            Ok(Mal::Vector(Rc::new(ys), Rc::new(Mal::Nil)))
        }
        Mal::HashMap(m, _) => {
            let mut n = HashMap::new();
            for (k, v) in m.iter() {
                n.insert(k.clone(), EVAL(v, env.clone())?);
            }
            Ok(Mal::HashMap(Rc::new(n), Rc::new(Mal::Nil)))
        }
        other_value => Ok(other_value.clone()),
    }
}

fn PRINT(x: &Mal) {
    println!("{}", pr_str(x, true));
}

fn rep(s: &str, env: Env) {
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

    // Mini (read-only) environment
    let repl_env: Env = env_new(None);
    env_set(
        &repl_env,
        "+",
        Mal::Function(Rc::new(add), Rc::new(Mal::Nil)),
    );
    // env_set(repl_env.clone(), "-", Mal::Function(Rc::new(|xs| sub(xs))));
    // env_set(repl_env.clone(), "*", Mal::Function(Rc::new(|xs| mul(xs))));
    // env_set(repl_env.clone(), "/", Mal::Function(Rc::new(|xs| div(xs))));

    loop {
        match rl.readline(RUSTYLINE_PROMPT) {
            Ok(line) => {
                let line = line.trim_end_matches("\n\r");
                if !line.is_empty() {
                    rl.add_history_entry(line)?;
                    rep(line, repl_env.clone());
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
        _ => Err("Bad arguments for +".to_string()),
    }
}

// fn sub(args: &[Mal]) -> Result<Mal, String> {
//     match args {
//         [Mal::Int(x), Mal::Int(y)] => Ok(Mal::Int(x - y)),
//         _ => Err("Bad arguments for +".to_string()),
//     }
// }

// fn mul(args: &[Mal]) -> Result<Mal, String> {
//     match args {
//         [Mal::Int(x), Mal::Int(y)] => Ok(Mal::Int(x * y)),
//         _ => Err("Bad arguments for +".to_string()),
//     }
// }

// fn div(args: &[Mal]) -> Result<Mal, String> {
//     match args {
//         [Mal::Int(_), Mal::Int(0)] => Err("Division by zero".to_string()),
//         [Mal::Int(x), Mal::Int(y)] => Ok(Mal::Int(x / y)),
//         _ => Err("Bad arguments for +".to_string()),
//     }
// }
