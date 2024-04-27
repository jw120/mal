use std::ops;
use std::rc::Rc;

use crate::printer::pr_str;
use crate::types::{into_mal_bool, into_mal_fn, into_mal_list, is_falsy, mk_err, Mal};

pub fn get_ns() -> Vec<(&'static str, Mal)> {
    vec![
        ("list", into_mal_fn(Rc::new(list))),
        ("list?", into_mal_fn(Rc::new(is_list))),
        ("empty?", into_mal_fn(Rc::new(is_empty))),
        ("count", into_mal_fn(Rc::new(count))),
        ("+", into_mal_fn(Rc::new(add))),
        ("-", into_mal_fn(Rc::new(sub))),
        ("*", into_mal_fn(Rc::new(mul))),
        ("/", into_mal_fn(Rc::new(div))),
        ("=", into_mal_fn(Rc::new(eq))),
        ("<", into_mal_fn(Rc::new(lt))),
        ("<=", into_mal_fn(Rc::new(le))),
        (">", into_mal_fn(Rc::new(gt))),
        (">=", into_mal_fn(Rc::new(ge))),
        ("not", into_mal_fn(Rc::new(not))),
        ("pr-str", into_mal_fn(Rc::new(pr_dash_str))),
        ("str", into_mal_fn(Rc::new(str))),
        ("prn", into_mal_fn(Rc::new(prn))),
        ("println", into_mal_fn(Rc::new(println))),
    ]
}

fn list(args: Vec<Mal>) -> Result<Mal, String> {
    Ok(into_mal_list(args))
}

fn is_list(args: Vec<Mal>) -> Result<Mal, String> {
    match args.as_slice() {
        [] => mk_err("No argument for list?"),
        [Mal::List(_, _), ..] => Ok(into_mal_bool(true)),
        _ => Ok(into_mal_bool(false)),
    }
}

fn is_empty(args: Vec<Mal>) -> Result<Mal, String> {
    match args.as_slice() {
        [Mal::List(xs, _), ..] => Ok(into_mal_bool(xs.is_empty())),
        [Mal::Vector(xs, _), ..] => Ok(into_mal_bool(xs.is_empty())),
        _ => mk_err("empty? needs a list"),
    }
}

fn count(args: Vec<Mal>) -> Result<Mal, String> {
    match args.as_slice() {
        [Mal::List(xs, _), ..] => Ok(Mal::Int(xs.len() as i64)),
        [Mal::Vector(xs, _), ..] => Ok(Mal::Int(xs.len() as i64)),
        [Mal::Nil, ..] => Ok(Mal::Int(0)),
        _ => mk_err("count needs a list"),
    }
}

fn add(args: Vec<Mal>) -> Result<Mal, String> {
    do_iii(ops::Add::add, "+", args)
}

fn sub(args: Vec<Mal>) -> Result<Mal, String> {
    do_iii(ops::Sub::sub, "-", args)
}

fn mul(args: Vec<Mal>) -> Result<Mal, String> {
    do_iii(ops::Mul::mul, "*", args)
}

fn div(args: Vec<Mal>) -> Result<Mal, String> {
    match args.as_slice() {
        [Mal::Int(_), Mal::Int(0)] => Err("Division by zero".to_string()),
        [Mal::Int(x), Mal::Int(y)] => Ok(Mal::Int(x / y)),
        _ => Err("Bad arguments for /".to_string()),
    }
}

fn lt(args: Vec<Mal>) -> Result<Mal, String> {
    do_iib(|a, b| a < b, "<", args)
}

fn le(args: Vec<Mal>) -> Result<Mal, String> {
    do_iib(|a, b| a <= b, "<=", args)
}

fn gt(args: Vec<Mal>) -> Result<Mal, String> {
    do_iib(|a, b| a > b, ">", args)
}

fn ge(args: Vec<Mal>) -> Result<Mal, String> {
    do_iib(|a, b| a >= b, ">=", args)
}

fn eq(args: Vec<Mal>) -> Result<Mal, String> {
    match args.as_slice() {
        [x, y] => Ok(into_mal_bool(x == y)),
        _ => Err("Bad arguments for =".to_string()),
    }
}

fn not(args: Vec<Mal>) -> Result<Mal, String> {
    match args.as_slice() {
        [x] => Ok(into_mal_bool(is_falsy(x))),
        _ => Err("Bad arguments for not".to_string()),
    }
}

// pr-str function
fn pr_dash_str(args: Vec<Mal>) -> Result<Mal, String> {
    let arg_strings: Vec<String> = args.iter().map(|x| pr_str(x, true)).collect();
    Ok(Mal::String(arg_strings.join(" ")))
}

fn str(args: Vec<Mal>) -> Result<Mal, String> {
    let arg_strings: Vec<String> = args.iter().map(|x| pr_str(x, false)).collect();
    Ok(Mal::String(arg_strings.join("")))
}

fn prn(args: Vec<Mal>) -> Result<Mal, String> {
    let arg_strings: Vec<String> = args.iter().map(|x| pr_str(x, true)).collect();
    println!("{}", arg_strings.join(" "));
    Ok(Mal::Nil)
}

fn println(args: Vec<Mal>) -> Result<Mal, String> {
    let arg_strings: Vec<String> = args.iter().map(|x| pr_str(x, false)).collect();
    println!("{}", arg_strings.join(" "));
    Ok(Mal::Nil)
}

// Helper functions

// (int, int) -> int
fn do_iii(op: fn(i64, i64) -> i64, name: &str, args: Vec<Mal>) -> Result<Mal, String> {
    match args.as_slice() {
        [Mal::Int(x), Mal::Int(y)] => Ok(Mal::Int(op(*x, *y))),
        _ => {
            let msg = format!("Bad arguments for {}", name);
            mk_err(&msg)
        }
    }
}

// (int, int) -> bool
fn do_iib(op: fn(i64, i64) -> bool, name: &str, args: Vec<Mal>) -> Result<Mal, String> {
    match args.as_slice() {
        [Mal::Int(x), Mal::Int(y)] => Ok(into_mal_bool(op(*x, *y))),
        _ => {
            let msg = format!("Bad arguments for {}", name);
            mk_err(&msg)
        }
    }
}
