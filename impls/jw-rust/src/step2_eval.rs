#![allow(non_snake_case)]

use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::collections::HashMap;

use jw_rust_mal::printer::pr_str;
use jw_rust_mal::reader::{read_str, ReadError};
use jw_rust_mal::types::Mal;

static RUSTYLINE_HISTORY_FILE: &str = ".jw-rust-mal-history";
static RUSTYLINE_PROMPT: &str = "user> ";

type Env = HashMap<String, Mal>;

fn READ(s: &str) -> Option<Result<Mal, ReadError>> {
    read_str(s)
}

fn EVAL(ast: &Mal, env: &Env) -> Result<Mal, String> {
    match ast {
        Mal::List(xs) => {
            if xs.is_empty() {
                Ok(ast.clone())
            } else {
                match eval_ast(ast, env)? {
                    Mal::List(ys) => match ys.as_slice() {
                        [Mal::Function(f), tail @ ..] => Ok(f(tail)?),
                        [_non_function, _tail @ ..] => Err("Applying non-function".to_string()),
                        [] => Err("List disappeared!".to_string()),
                    },
                    _non_list => Err("No longer a list!".to_string()),
                }
            }
        }
        non_list => eval_ast(non_list, env),
    }
}

fn PRINT(x: &Mal) {
    println!("{}", pr_str(&x, true));
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

fn eval_ast(ast: &Mal, env: &Env) -> Result<Mal, String> {
    match ast {
        Mal::Symbol(s) => match env.get(s) {
            Some(value) => Ok(value.clone()),
            None => Err(format!("Can't find value for symbol '{}'", s).to_string()),
        },
        Mal::List(xs) => {
            let mut ys = Vec::new();
            for x in xs {
                ys.push(EVAL(&x, env)?);
            }
            Ok(Mal::List(ys))
        }
        Mal::Vector(xs) => {
            let mut ys = Vec::new();
            for x in xs {
                ys.push(EVAL(&x, env)?);
            }
            Ok(Mal::Vector(ys))
        }
        Mal::HashMap(m) => {
            let mut n = HashMap::new();
            for (k, v) in m {
                n.insert(k.clone(), EVAL(v, env)?);
            }
            Ok(Mal::HashMap(n))
        }
        other_value => Ok(other_value.clone()),
    }
}

fn main() -> Result<(), ReadlineError> {
    let mut rl = DefaultEditor::new()?;
    if rl.load_history(RUSTYLINE_HISTORY_FILE).is_err() {
        println!("No previous history.");
    }

    // Mini (read-only) environment
    let repl_env: Env = HashMap::from([
        ("+".to_string(), Mal::Function(add)),
        ("-".to_string(), Mal::Function(sub)),
        ("*".to_string(), Mal::Function(mul)),
        ("/".to_string(), Mal::Function(div)),
    ]);

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

fn add(args: &[Mal]) -> Result<Mal, String> {
    match args {
        [Mal::Int(x), Mal::Int(y)] => Ok(Mal::Int(x + y)),
        _ => Err("Bad arguments for +".to_string()),
    }
}

fn sub(args: &[Mal]) -> Result<Mal, String> {
    match args {
        [Mal::Int(x), Mal::Int(y)] => Ok(Mal::Int(x - y)),
        _ => Err("Bad arguments for +".to_string()),
    }
}

fn mul(args: &[Mal]) -> Result<Mal, String> {
    match args {
        [Mal::Int(x), Mal::Int(y)] => Ok(Mal::Int(x * y)),
        _ => Err("Bad arguments for +".to_string()),
    }
}

fn div(args: &[Mal]) -> Result<Mal, String> {
    match args {
        [Mal::Int(_), Mal::Int(0)] => Err("Division by zero".to_string()),
        [Mal::Int(x), Mal::Int(y)] => Ok(Mal::Int(x / y)),
        _ => Err("Bad arguments for +".to_string()),
    }
}
