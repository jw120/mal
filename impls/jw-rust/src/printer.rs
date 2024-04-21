use crate::types::Mal;

pub fn pr_str(x: &Mal) -> String {
    match x {
        Mal::Int(i) => i.to_string(),
        Mal::List(xs) => {
            let ys: Vec<String> = xs.iter().map(pr_str).collect();
            let mut y = ys.join(" ");
            y.insert(0, '(');
            y.push(')');
            y
        }
        Mal::Symbol(s) => s.to_string(),
        Mal::Nil => "nil".to_string(),
        Mal::True => "true".to_string(),
        Mal::False => "false".to_string(),
    }
}
