// Definitions of built-in functions
// Used from step 4 onwards

use std::ops;

use crate::printer::pr_str;
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
        ],
        vec!["(def! not (fn* (a) (if a false true)))"],
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

// fn not(args: &[Mal]) -> MalResult {
//     match args {
//         [x] => Ok(Mal::Bool(is_falsy(x))),
//         _ => Err("Bad arguments for not".to_string()),
//     }
// }

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
