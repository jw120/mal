#![allow(non_snake_case)]

use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::collections::HashMap;

use jw_rust_mal::printer;
use jw_rust_mal::reader;
use jw_rust_mal::types::{
    err, into_mal_fn, into_mal_hashmap, into_mal_seq, Mal, MalError, MalResult,
};

static RUSTYLINE_HISTORY_FILE: &str = ".jw-rust-mal-history";
static RUSTYLINE_PROMPT: &str = "user> ";

type Env = HashMap<String, Mal>;

fn READ(s: &str) -> MalResult {
    reader::read_str(s)
}

fn EVAL(ast: &Mal, env: &Env) -> MalResult {
    if let Mal::Seq(true, xs, _) = ast {
        if xs.is_empty() {
            Ok(ast.clone())
        } else {
            match eval_ast(ast, env)? {
                Mal::Seq(true, ys, _) => match ys.as_slice() {
                    [Mal::Function(f, _), tail @ ..] => Ok(f(tail)?),
                    [_non_function, _tail @ ..] => err("Applying non-function"),
                    [] => err("List disappeared!"),
                },
                _non_list => err("No longer a list!"),
            }
        }
    } else {
        // Non-list
        eval_ast(ast, env)
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

fn eval_ast(ast: &Mal, env: &Env) -> MalResult {
    match ast {
        Mal::Symbol(s) => env.get(s).map_or_else(
            || err("Can't find value for symbol"),
            |value| Ok(value.clone()),
        ),
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

fn main() -> Result<(), ReadlineError> {
    let mut rl = DefaultEditor::new()?;
    if rl.load_history(RUSTYLINE_HISTORY_FILE).is_err() {
        println!("No previous history.");
    }

    // Mini environment
    let repl_env: Env = HashMap::from([
        ("+".to_string(), into_mal_fn(add)),
        ("-".to_string(), into_mal_fn(sub)),
        ("*".to_string(), into_mal_fn(mul)),
        ("/".to_string(), into_mal_fn(div)),
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
                println!("Error reading line: {err:?}");
            }
        }
    }
    rl.save_history(RUSTYLINE_HISTORY_FILE)?;
    Ok(())
}

fn add(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Int(x), Mal::Int(y)] => Ok(Mal::Int(x + y)),
        _ => err("Bad arguments for +"),
    }
}

fn sub(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Int(x), Mal::Int(y)] => Ok(Mal::Int(x - y)),
        _ => err("Bad arguments for +"),
    }
}

fn mul(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Int(x), Mal::Int(y)] => Ok(Mal::Int(x * y)),
        _ => err("Bad arguments for +"),
    }
}

fn div(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Int(_), Mal::Int(0)] => err("Division by zero"),
        [Mal::Int(x), Mal::Int(y)] => Ok(Mal::Int(x / y)),
        _ => err("Bad arguments for +"),
    }
}
