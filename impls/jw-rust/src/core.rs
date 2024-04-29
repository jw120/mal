// Definitions of built-in functions
// Used from step 4 onwards

use std::fs;
use std::ops;

use crate::printer::pr_str;
use crate::reader;
use crate::types::*;

// Built-in definitions (rust code and mal code)
pub fn get_builtins() -> (Vec<(&'static str, Mal)>, Vec<&'static str>) {
    (
        vec![
            ("list", into_mal_fn(list)),
            ("list?", into_mal_fn(is_list)),
            ("empty?", into_mal_fn(is_empty)),
            ("count", into_mal_fn(count)),
            ("+", into_mal_fn(add)),
            ("-", into_mal_fn(sub)),
            ("*", into_mal_fn(mul)),
            ("/", into_mal_fn(div)),
            ("=", into_mal_fn(eq)),
            ("<", into_mal_fn(lt)),
            ("<=", into_mal_fn(le)),
            (">", into_mal_fn(gt)),
            (">=", into_mal_fn(ge)),
            ("pr-str", into_mal_fn(pr_dash_str)),
            ("str", into_mal_fn(str)),
            ("prn", into_mal_fn(prn)),
            ("println", into_mal_fn(println)),
            ("atom", into_mal_fn(atom)),
            ("atom?", into_mal_fn(is_atom)),
            ("deref", into_mal_fn(deref)),
            ("reset!", into_mal_fn(reset)),
            ("swap!", into_mal_fn(swap)),
            ("read-string", into_mal_fn(read_string)),
            ("slurp", into_mal_fn(slurp)),
        ],
        vec![
            "(def! not (fn* (a) (if a false true)))",
            "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\\nnil)\")))))",
        ],
    )
}

fn list(args: &[Mal]) -> MalResult {
    Ok(into_mal_seq(true, args.to_vec()))
}

fn is_list(args: &[Mal]) -> MalResult {
    match args {
        [] => mk_err("No argument for list?"),
        [Mal::Seq(true, _, _), ..] => Ok(Mal::Bool(true)),
        _ => Ok(Mal::Bool(false)),
    }
}

fn is_empty(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Seq(_, xs, _), ..] => Ok(Mal::Bool(xs.is_empty())),
        _ => mk_err("empty? needs a list"),
    }
}

fn count(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Seq(_, xs, _), ..] => Ok(Mal::Int(xs.len() as i64)),
        [Mal::Nil, ..] => Ok(Mal::Int(0)),
        _ => mk_err("count needs a list"),
    }
}

fn add(args: &[Mal]) -> MalResult {
    do_iii(ops::Add::add, "+", args)
}

fn sub(args: &[Mal]) -> MalResult {
    do_iii(ops::Sub::sub, "-", args)
}

fn mul(args: &[Mal]) -> MalResult {
    do_iii(ops::Mul::mul, "*", args)
}

fn div(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Int(_), Mal::Int(0)] => Err("Division by zero".to_string()),
        [Mal::Int(x), Mal::Int(y)] => Ok(Mal::Int(x / y)),
        _ => Err("Bad arguments for /".to_string()),
    }
}

fn lt(args: &[Mal]) -> MalResult {
    do_iib(|a, b| a < b, "<", args)
}

fn le(args: &[Mal]) -> MalResult {
    do_iib(|a, b| a <= b, "<=", args)
}

fn gt(args: &[Mal]) -> MalResult {
    do_iib(|a, b| a > b, ">", args)
}

fn ge(args: &[Mal]) -> MalResult {
    do_iib(|a, b| a >= b, ">=", args)
}

fn eq(args: &[Mal]) -> MalResult {
    match args {
        [x, y] => Ok(Mal::Bool(x == y)),
        _ => Err("Bad arguments for =".to_string()),
    }
}

// pr-str function
fn pr_dash_str(args: &[Mal]) -> MalResult {
    let arg_strings: Vec<String> = args.iter().map(|x| pr_str(x, true)).collect();
    Ok(Mal::String(arg_strings.join(" ")))
}

fn str(args: &[Mal]) -> MalResult {
    let arg_strings: Vec<String> = args.iter().map(|x| pr_str(x, false)).collect();
    Ok(Mal::String(arg_strings.join("")))
}

fn prn(args: &[Mal]) -> MalResult {
    let arg_strings: Vec<String> = args.iter().map(|x| pr_str(x, true)).collect();
    println!("{}", arg_strings.join(" "));
    Ok(Mal::Nil)
}

fn println(args: &[Mal]) -> MalResult {
    let arg_strings: Vec<String> = args.iter().map(|x| pr_str(x, false)).collect();
    println!("{}", arg_strings.join(" "));
    Ok(Mal::Nil)
}

// Atom functions

fn atom(args: &[Mal]) -> MalResult {
    if let [x] = args {
        Ok(into_mal_atom(x.clone()))
    } else {
        mk_err("atom takes one value")
    }
}

fn is_atom(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Atom(_)] => Ok(Mal::Bool(true)),
        [_] => Ok(Mal::Bool(false)),
        _ => mk_err("atom? takes on value"),
    }
}

fn deref(args: &[Mal]) -> MalResult {
    if let [Mal::Atom(a)] = args {
        Ok(((*a).borrow()).clone())
    } else {
        mk_err("NYI")
    }
}

fn reset(args: &[Mal]) -> MalResult {
    if let [Mal::Atom(a), value] = args {
        *((*a).borrow_mut()) = value.clone();
        Ok(value.clone())
    } else {
        mk_err("reset takes an atom and a mal value")
    }
}

fn swap(args: &[Mal]) -> MalResult {
    if let [_x] = args {
        Ok(Mal::Nil)
    } else {
        mk_err("NYI")
    }
}

// Other functions

fn read_string(args: &[Mal]) -> MalResult {
    if let [Mal::String(s)] = args {
        match reader::read_str(s) {
            Ok(x) => Ok(x),
            Err(reader::ReadError::Internal(msg)) => Err(msg),
            Err(reader::ReadError::Parse(msg)) => Err(msg),
        }
    } else {
        mk_err("read-string needs a string")
    }
}

fn slurp(args: &[Mal]) -> MalResult {
    if let [Mal::String(path)] = args {
        match fs::read_to_string(path) {
            Ok(s) => Ok(Mal::String(s)),
            Err(e) => Err(e.to_string()),
        }
    } else {
        mk_err("slurp takes a filename")
    }
}

// Helper functions

// (int, int) -> int
fn do_iii(op: fn(i64, i64) -> i64, name: &str, args: &[Mal]) -> MalResult {
    match args {
        [Mal::Int(x), Mal::Int(y)] => Ok(Mal::Int(op(*x, *y))),
        _ => {
            let msg = format!("Bad arguments for {}", name);
            mk_err(&msg)
        }
    }
}

// (int, int) -> bool
fn do_iib(op: fn(i64, i64) -> bool, name: &str, args: &[Mal]) -> MalResult {
    match args {
        [Mal::Int(x), Mal::Int(y)] => Ok(Mal::Bool(op(*x, *y))),
        _ => {
            let msg = format!("Bad arguments for {}", name);
            mk_err(&msg)
        }
    }
}
