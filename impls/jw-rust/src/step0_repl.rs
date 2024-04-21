#![allow(non_snake_case)]

use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};

static RUSTYLINE_HISTORY_FILE: &str = ".jw-rust-mal-history";
static RUSTYLINE_PROMPT: &str = "user> ";

fn READ(s: &str) -> &str {
    s
}

fn EVAL(s: &str) -> &str {
    s
}

fn PRINT(s: &str) {
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
                let trimmed_line = line.trim();
                if !trimmed_line.is_empty() {
                    rl.add_history_entry(trimmed_line)?;
                    rep(trimmed_line);
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
