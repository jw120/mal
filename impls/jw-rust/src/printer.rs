use crate::types::Mal;

pub fn pr_str(x: &Mal, print_readably: bool) -> String {
    match x {
        Mal::Int(i) => i.to_string(),
        Mal::List(xs) => {
            let ys: Vec<String> = xs.iter().map(|y| pr_str(y, print_readably)).collect();
            format!("({})", ys.join(" "))
        }
        Mal::String(s) => {
            if print_readably {
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
            } else {
                format!("\"{}\"", s)
            }
        }
        Mal::Symbol(s) => s.to_string(),
        Mal::Nil => "nil".to_string(),
        Mal::True => "true".to_string(),
        Mal::False => "false".to_string(),
    }
}
