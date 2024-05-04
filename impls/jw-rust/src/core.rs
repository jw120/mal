// Definitions of built-in functions
// Used from step 4 onwards

use std::fs;
use std::ops;

use crate::env;
use crate::printer;
use crate::reader;
use crate::types::{err, into_mal_atom, into_mal_fn, into_mal_seq, Mal, MalResult};

// Built-in definitions (rust code and mal code)
pub fn get_builtins() -> (Vec<(&'static str, Mal)>, Vec<&'static str>) {
    (
        vec![
            ("list", into_mal_fn(list)),
            ("cons", into_mal_fn(cons)),
            ("list?", into_mal_fn(is_list)),
            ("empty?", into_mal_fn(is_empty)),
            ("count", into_mal_fn(count)),
            ("cons", into_mal_fn(cons)),
            ("concat", into_mal_fn(concat)),
            ("vec", into_mal_fn(vec)),
            ("nth", into_mal_fn(nth)),
            ("first", into_mal_fn(first)),
            ("rest", into_mal_fn(rest)),
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
            "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))"
        ],
    )
}

// List functions

#[allow(clippy::unnecessary_wraps)] // Clippy does not think this should return a Result
fn list(args: &[Mal]) -> MalResult {
    Ok(into_mal_seq(true, args.to_vec()))
}

fn is_list(args: &[Mal]) -> MalResult {
    match args {
        [] => err("No argument for list?"),
        [Mal::Seq(true, _, _), ..] => Ok(Mal::Bool(true)),
        _ => Ok(Mal::Bool(false)),
    }
}

fn is_empty(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Seq(_, xs, _), ..] => Ok(Mal::Bool(xs.is_empty())),
        _ => err("empty? needs a list"),
    }
}

fn count(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Seq(_, xs, _), ..] => {
            i64::try_from(xs.len()).map_or(err("Integer overflow"), |i| Ok(Mal::Int(i)))
        }
        [Mal::Nil, ..] => Ok(Mal::Int(0)),
        _ => err("count needs a list"),
    }
}

fn cons(args: &[Mal]) -> MalResult {
    match args {
        [value, Mal::Seq(_is_list, xs, _meta)] => {
            let mut ys: Vec<Mal> = Vec::new();
            ys.push(value.clone());
            ys.extend_from_slice(xs);
            Ok(into_mal_seq(true, ys))
        }
        _ => err("cons takes a value and a sequence"),
    }
}

fn concat(args: &[Mal]) -> MalResult {
    let mut zs: Vec<Mal> = Vec::new();
    for x in args {
        if let Mal::Seq(_is_list, xs, _meta) = x {
            zs.extend_from_slice(xs);
        } else {
            return err("concat takes zero or more sequences");
        }
    }
    Ok(into_mal_seq(true, zs))
}

fn vec(args: &[Mal]) -> MalResult {
    match args {
        [arg] => match arg {
            Mal::Seq(false, _, _) => Ok(arg.clone()),
            Mal::Seq(true, xs, _) => Ok(into_mal_seq(false, xs.to_vec())),
            _ => err("vec takes a sequence"),
        },
        _ => err("vec takes a sequence"),
    }
}

fn nth(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Seq(_is_list, xs, _meta), Mal::Int(i)] => match xs.get(*i as usize) {
            Some(x) => Ok(x.clone()),
            None => err("Invalid index in nth"),
        },
        _ => err("nth takes a sequence and an index"),
    }
}

fn first(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Seq(_is_list, xs, _meta)] => Ok(match xs.get(0) {
            Some(x) => x.clone(),
            None => Mal::Nil,
        }),
        [Mal::Nil] => Ok(Mal::Nil),
        _ => err("first takes a sequence or nil"),
    }
}

fn rest(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Seq(_is_list, xs, _meta)] if xs.len() > 0 => Ok(into_mal_seq(true, xs[1..].to_vec())),
        [Mal::Seq(_, _, _)] | [Mal::Nil] => Ok(into_mal_seq(true, Vec::new())),
        _ => err("rest takes a sequence or nil"),
    }
}

// Arithmetic and logic functions

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

// Print functions

#[allow(clippy::unnecessary_wraps)] // Clippy does not think this should return a Result
fn pr_dash_str(args: &[Mal]) -> MalResult {
    let arg_strings: Vec<String> = args.iter().map(|x| printer::pr_str(x, true)).collect();
    Ok(Mal::String(arg_strings.join(" ")))
}

#[allow(clippy::unnecessary_wraps)] // Clippy does not think this should return a Result
fn str(args: &[Mal]) -> MalResult {
    let arg_strings: Vec<String> = args.iter().map(|x| printer::pr_str(x, false)).collect();
    Ok(Mal::String(arg_strings.join("")))
}

#[allow(clippy::unnecessary_wraps)] // Clippy does not think this should return a Result
fn prn(args: &[Mal]) -> MalResult {
    let arg_strings: Vec<String> = args.iter().map(|x| printer::pr_str(x, true)).collect();
    println!("{}", arg_strings.join(" "));
    Ok(Mal::Nil)
}

#[allow(clippy::unnecessary_wraps)] // Clippy does not think this should return a Result
fn println(args: &[Mal]) -> MalResult {
    let arg_strings: Vec<String> = args.iter().map(|x| printer::pr_str(x, false)).collect();
    println!("{}", arg_strings.join(" "));
    Ok(Mal::Nil)
}

// Atom functions

fn atom(args: &[Mal]) -> MalResult {
    if let [x] = args {
        Ok(into_mal_atom(x.clone()))
    } else {
        err("atom takes one value")
    }
}

fn is_atom(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Atom(_)] => Ok(Mal::Bool(true)),
        [_] => Ok(Mal::Bool(false)),
        _ => err("atom? takes on value"),
    }
}

fn deref(args: &[Mal]) -> MalResult {
    if let [Mal::Atom(a)] = args {
        Ok(((*a).borrow()).clone())
    } else {
        err("NYI")
    }
}

fn reset(args: &[Mal]) -> MalResult {
    if let [Mal::Atom(a), value] = args {
        *((*a).borrow_mut()) = value.clone();
        Ok(value.clone())
    } else {
        err("reset takes an atom and a mal value")
    }
}

fn swap(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Atom(a), Mal::Function(f, _), args @ ..] => {
            let mut full_args: Vec<Mal> = Vec::new();
            full_args.push((*a).borrow().clone());
            for x in args {
                full_args.push(x.clone());
            }
            let new_value = f(&full_args)?;
            *((*a).borrow_mut()) = new_value.clone();
            Ok(new_value)
        }
        [Mal::Atom(a), Mal::Closure {
            eval,
            ast,
            params,
            env,
            is_macro: _,
            meta: _,
        }, args @ ..] => {
            let mut full_args: Vec<Mal> = Vec::new();
            full_args.push((*a).borrow().clone());
            for x in args {
                full_args.push(x.clone());
            }
            let new_env = env::new_binds(Some(env), params, &full_args)?;
            let new_value = eval((**ast).clone(), new_env)?;
            *((*a).borrow_mut()) = new_value.clone();
            Ok(new_value)
        }

        _ => err("swap! takes an atom, a function and optionally additional arguments"),
    }
}

// Other functions

fn read_string(args: &[Mal]) -> MalResult {
    if let [Mal::String(s)] = args {
        reader::read_str(s)
    } else {
        err("read-string needs a string")
    }
}

fn slurp(args: &[Mal]) -> MalResult {
    if let [Mal::String(path)] = args {
        match fs::read_to_string(path) {
            Ok(s) => Ok(Mal::String(s)),
            Err(e) => Err(e.to_string()),
        }
    } else {
        err("slurp takes a filename")
    }
}

// Helper functions

// (int, int) -> int
fn do_iii(op: fn(i64, i64) -> i64, name: &str, args: &[Mal]) -> MalResult {
    if let [Mal::Int(x), Mal::Int(y)] = args {
        Ok(Mal::Int(op(*x, *y)))
    } else {
        let msg = format!("Bad arguments for {name}");
        err(&msg)
    }
}

// (int, int) -> bool
fn do_iib(op: fn(i64, i64) -> bool, name: &str, args: &[Mal]) -> MalResult {
    if let [Mal::Int(x), Mal::Int(y)] = args {
        Ok(Mal::Bool(op(*x, *y)))
    } else {
        let msg = format!("Bad arguments for {name}");
        err(&msg)
    }
}
