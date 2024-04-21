#![allow(non_snake_case)]

use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};

mod reader;

static RUSTYLINE_HISTORY_FILE: &str = ".jw-rust-mal-history";
static RUSTYLINE_PROMPT: &str = "user> ";

fn READ(s: &str) -> String {
    let tokens = reader::tokenize(&s);
    format!("{:?}", tokens)
}

fn EVAL(s: String) -> String {
    s
}

fn PRINT(s: String) {
    println!("{}", s);
}

fn rep(s: &str) {
    PRINT(EVAL(READ(s)))
}

fn main() -> Result<()> {
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
                println!("Error reading line: {:?}", err);
            }
        }
    }
    rl.save_history(RUSTYLINE_HISTORY_FILE)?;
    Ok(())
}
