use crate::types::{Mal, MalKey};

pub fn pr_str(x: &Mal, print_readably: bool) -> String {
    match x {
        Mal::Int(i) => i.to_string(),
        Mal::List(xs) => format!("({})", seq(xs, print_readably)),
        Mal::Vector(xs) => format!("[{}]", seq(xs, print_readably)),
        Mal::HashMap(m) => {
            let mut xs = Vec::new();
            for (k, v) in m {
                match k {
                    MalKey::String(s) => xs.push(Mal::String(s.to_string())),
                    MalKey::Keyword(s) => xs.push(Mal::Keyword(s.to_string())),
                };
                xs.push(v.clone());
            }
            format!("{{{}}}", seq(&xs, print_readably))
        }
        Mal::String(s) => {
            if print_readably {
                pr_readably(s)
            } else {
                format!("\"{}\"", s)
            }
        }
        Mal::Keyword(s) => format!(":{}", s),
        Mal::Symbol(s) => s.to_string(),
        Mal::Nil => "nil".to_string(),
        Mal::True => "true".to_string(),
        Mal::False => "false".to_string(),
        Mal::Function(_) => "<fn>".to_string(),
    }
}

// Helper function to format a sequence (without delimiters)
fn seq(xs: &[Mal], print_readably: bool) -> String {
    let ys: Vec<String> = xs.iter().map(|y| pr_str(y, print_readably)).collect();
    ys.join(" ")
}

// Helper function to print a string readably
fn pr_readably(s: &str) -> String {
    let mut t = "\"".to_string();
    for c in s.chars() {
        match c {
            '\n' => {
                t.push('\\');
                t.push('n');
            }
            '\\' => {
                t.push('\\');
                t.push('\\');
            }
            '"' => {
                t.push('\\');
                t.push('\"');
            }
            _ => t.push(c),
        }
    }
    t.push('\"');
    t
}
