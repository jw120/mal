#![allow(non_snake_case)]

use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use jw_rust_mal::printer;
use jw_rust_mal::reader;
use jw_rust_mal::types::{Mal, MalError, MalResult};

static RUSTYLINE_HISTORY_FILE: &str = ".jw-rust-mal-history";
static RUSTYLINE_PROMPT: &str = "user> ";

fn READ(s: &str) -> MalResult {
    reader::read_str(s)
}

const fn EVAL(x: &Mal) -> &Mal {
    x
}

fn PRINT(x: &Mal) {
    println!("{}", printer::pr_str(x, true));
}

fn rep(s: &str) {
    match READ(s) {
        Ok(x) => PRINT(EVAL(&x)),
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
    loop {
        match rl.readline(RUSTYLINE_PROMPT) {
            Ok(line) => {
                let line = line.trim_end_matches("\n\r");
                if !line.is_empty() {
                    rl.add_history_entry(line)?;
                    rep(line);
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
