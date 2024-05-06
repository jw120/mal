// Definitions of built-in functions
// Used from step 4 onwards

use std::collections::HashMap;
use std::fs;
use std::io;
use std::io::Write; // needed for flush()
use std::ops;
use std::rc::Rc;
use std::time;

use crate::env;
use crate::printer;
use crate::reader;
use crate::types::{
    err, from_key, into_key, into_mal_atom, into_mal_fn, into_mal_hashmap, into_mal_seq, Mal,
    MalError, MalKey, MalResult,
};

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
            ("nil?", into_mal_fn(is_nil)),
            ("true?", into_mal_fn(is_true)),
            ("false?", into_mal_fn(is_false)),
            ("symbol?", into_mal_fn(is_symbol)),
            ("read-string", into_mal_fn(read_string)),
            ("slurp", into_mal_fn(slurp)),
            ("throw", into_mal_fn(throw)),
            ("map", into_mal_fn(map)),
            ("apply", into_mal_fn(apply)),
            ("symbol", into_mal_fn(symbol)),
            ("keyword", into_mal_fn(keyword)),
            ("keyword?", into_mal_fn(is_keyword)),
            ("vector", into_mal_fn(vector)),
            ("vector?", into_mal_fn(is_vector)),
            ("sequential?", into_mal_fn(is_sequential)),
            ("hash-map", into_mal_fn(hash_map)),
            ("map?", into_mal_fn(is_map)),
            ("assoc", into_mal_fn(assoc)),
            ("dissoc", into_mal_fn(dissoc)),
            ("get", into_mal_fn(get)),
            ("contains?", into_mal_fn(contains)),
            ("keys", into_mal_fn(keys)),
            ("vals", into_mal_fn(vals)),
            ("readline", into_mal_fn(readline)),
            ("time-ms", into_mal_fn(time_ms)),
            ("meta", into_mal_fn(meta)),
            ("with-meta", into_mal_fn(with_meta)),
            ("fn?", into_mal_fn(is_fn)),
            ("macro?", into_mal_fn(is_macro)),
            ("string?", into_mal_fn(is_string)),
            ("number?", into_mal_fn(is_number)),
            ("seq", into_mal_fn(seq)),
            ("conj", into_mal_fn(conj)),
        ],
        vec![
            "(def! not (fn* (a) 
                (if a false true)))",
            "(def! load-file (fn* (f) 
                (eval (read-string (str \"(do \" (slurp f) \"\\nnil)\")))))",
            "(defmacro! cond (fn* (& xs) 
                (if (> (count xs) 0) 
                    (list 'if (first xs) (if (> (count xs) 1) 
                                            (nth xs 1) 
                                            (throw \"odd number of forms to cond\")) 
                    (cons 'cond (rest (rest xs)))))))",
            "(def! *host-language* (symbol \"jw-rust\"))",
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
        [Mal::Seq(_is_list, xs, _meta), Mal::Int(i)] => {
            let index =
                usize::try_from(*i).map_err(|_| MalError::Msg("Index out of range".to_string()))?;
            xs.get(index)
                .map_or_else(|| err("Index out of bounds in nth"), |x| Ok(x.clone()))
        }
        _ => err("nth takes a sequence and an index"),
    }
}

fn first(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Seq(_is_list, xs, _meta)] => {
            Ok(xs.first().map_or(Mal::Nil, std::clone::Clone::clone))
        }
        [Mal::Nil] => Ok(Mal::Nil),
        _ => err("first takes a sequence or nil"),
    }
}

fn rest(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Seq(_is_list, xs, _meta)] if xs.len() > 0 => Ok(into_mal_seq(true, xs[1..].to_vec())),
        [Mal::Seq(_, _, _) | Mal::Nil] => Ok(into_mal_seq(true, Vec::new())),
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
        [Mal::Int(_), Mal::Int(0)] => err("Division by zero"),
        [Mal::Int(x), Mal::Int(y)] => Ok(Mal::Int(x / y)),
        _ => err("Bad arguments for /"),
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
        _ => err("Bad arguments for ="),
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

// Type predicate functions

fn is_nil(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Nil] => Ok(Mal::Bool(true)),
        [_] => Ok(Mal::Bool(false)),
        _ => err("nil? takes one argument"),
    }
}

fn is_true(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Bool(true)] => Ok(Mal::Bool(true)),
        [_] => Ok(Mal::Bool(false)),
        _ => err("true? takes one argument"),
    }
}

fn is_false(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Bool(false)] => Ok(Mal::Bool(true)),
        [_] => Ok(Mal::Bool(false)),
        _ => err("false? takes one argument"),
    }
}

fn is_symbol(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Symbol(_)] => Ok(Mal::Bool(true)),
        [_] => Ok(Mal::Bool(false)),
        _ => err("symbol? takes one argument"),
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
            Err(e) => err(&e.to_string()),
        }
    } else {
        err("slurp takes a filename")
    }
}

fn throw(args: &[Mal]) -> MalResult {
    if let [e] = args {
        Err(MalError::Exception(e.clone()))
    } else {
        err("throw takes one argument")
    }
}

fn map(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Closure {
            eval,
            ast,
            params,
            env,
            is_macro: false,
            meta: _,
        }, Mal::Seq(_is_list, xs, _meta)] => {
            let mut v: Vec<Mal> = Vec::new();
            for x in xs.as_ref() {
                let args: Vec<Mal> = vec![x.clone()];
                let map_env = env::new_binds(Some(env), params, &args)?;
                v.push(eval(ast.as_ref().clone(), map_env)?);
            }
            Ok(into_mal_seq(true, v))
        }
        [Mal::Function(f, _), Mal::Seq(_is_list, xs, _)] => {
            let mut v: Vec<Mal> = Vec::new();
            for x in xs.as_ref() {
                let args: Vec<Mal> = vec![x.clone()];
                v.push(f(&args)?);
            }
            Ok(into_mal_seq(true, v))
        }
        _ => err("map takes a function or closure and a sequence"),
    }
}

fn apply(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Function(f, _meta), rest @ ..] => {
            let ys = copy_args(rest);
            f(&ys)
        }
        [Mal::Closure {
            eval,
            ast,
            params,
            env,
            is_macro: false,
            meta: _,
        }, rest @ ..] => {
            let ys = copy_args(rest);
            let apply_env = env::new_binds(Some(env), params, &ys)?;
            eval(ast.as_ref().clone(), apply_env)
        }
        _ => err("apply requires a function or closure"),
    }
}

fn readline(args: &[Mal]) -> MalResult {
    if let [Mal::String(s)] = args {
        let mut input = String::new();
        print!("{s}");
        io::stdout().flush().expect("Unable to flush stdout");
        match io::stdin().read_line(&mut input) {
            Ok(_bytes_read) => {
                if input.ends_with('\n') {
                    input.pop();
                    if input.ends_with('\r') {
                        input.pop();
                    }
                }
                Ok(Mal::String(input))
            }
            Err(e) => {
                let msg = format!("readline error ({e})");
                err(&msg)
            }
        }
    } else {
        err("readline takes a string")
    }
}

// Step 9 functions

// symbol: takes a string and returns a new symbol with the string as its name.

fn symbol(args: &[Mal]) -> MalResult {
    match args {
        [Mal::String(s)] => Ok(Mal::Symbol(s.to_string())),
        _ => err("symbol takes a string"),
    }
}

// keyword: takes a string and returns a keyword with the same name
// This function should also detect if the argument is already a keyword and just return it.
fn keyword(args: &[Mal]) -> MalResult {
    match args {
        [Mal::String(s) | Mal::Keyword(s)] => Ok(Mal::Keyword(s.to_string())),
        _ => err("keyword takes a string or a keyword"),
    }
}

// keyword?: takes a single argument and returns true (mal true value) if the argument is a keyword,
// otherwise returns false (mal false value).

fn is_keyword(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Keyword(_)] => Ok(Mal::Bool(true)),
        [_] => Ok(Mal::Bool(false)),
        _ => err("keyword? takes one argument"),
    }
}

// vector: takes a variable number of arguments and returns a vector containing those arguments.

#[allow(clippy::unnecessary_wraps)] // Clippy does not think this should return a Result
fn vector(args: &[Mal]) -> MalResult {
    Ok(into_mal_seq(false, args.to_vec()))
}

// vector?: takes a single argument and returns true (mal true value) if the argument is a vector
// otherwise returns false (mal false value).

fn is_vector(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Seq(false, _, _)] => Ok(Mal::Bool(true)),
        [_] => Ok(Mal::Bool(false)),
        _ => err("vector? takes one argument"),
    }
}

// sequential?: takes a single argument and returns true (mal true value) if it is a list or a vector, otherwise returns false (mal false value).

fn is_sequential(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Seq(_, _, _)] => Ok(Mal::Bool(true)),
        [_] => Ok(Mal::Bool(false)),
        _ => err("sequential? takes one argument"),
    }
}

// hash-map: takes a variable but even number of arguments and returns a new mal hash-map value with keys from the odd arguments and values from the even arguments respectively. This is basically the functional form of the {} reader literal syntax.

fn hash_map(args: &[Mal]) -> MalResult {
    let mut args_iter = args.iter();
    let mut m: HashMap<MalKey, Mal> = HashMap::new();
    loop {
        match (args_iter.next(), args_iter.next()) {
            (Some(key), Some(v)) => match into_key(key) {
                Some(k) => m.insert(k, v.clone()),
                None => return err("Bad key in hash-map"),
            },
            (Some(_), None) => {
                return err("hash-map takes an even number of arguments");
            }
            (None, Some(_)) => return err("internal error!"),
            (None, None) => {
                return Ok(into_mal_hashmap(m));
            }
        };
    }
}

// map?: takes a single argument and returns true (mal true value) if the argument is a hash-map, otherwise returns false (mal false value).
fn is_map(args: &[Mal]) -> MalResult {
    match args {
        [Mal::HashMap(_, _)] => Ok(Mal::Bool(true)),
        [_] => Ok(Mal::Bool(false)),
        _ => err("map? takes one argument"),
    }
}

// assoc: takes a hash-map as the first argument and the remaining arguments are odd/even key/value pairs to "associate" (merge) into the hash-map. Note that the original hash-map is unchanged (remember, mal values are immutable), and a new hash-map containing the old hash-maps key/values plus the merged key/value arguments is returned.

fn assoc(args: &[Mal]) -> MalResult {
    if let [Mal::HashMap(m, _meta), rest @ ..] = args {
        let mut rest_iter = rest.iter();
        let mut n: HashMap<MalKey, Mal> = HashMap::new();
        n.clone_from(m);
        loop {
            match (rest_iter.next(), rest_iter.next()) {
                (Some(key), Some(v)) => match into_key(key) {
                    Some(k) => n.insert(k, v.clone()),
                    None => return err("Bad key in assoc"),
                },
                (Some(_), None) => {
                    return err("assoc must have an even number of key, value arguments");
                }
                (None, Some(_)) => return err("internal error!"),
                (None, None) => {
                    return Ok(into_mal_hashmap(n));
                }
            };
        }
    } else {
        err("assoc takes a hash-map and an even number of arguments for the new keys and values")
    }
}

// dissoc: takes a hash-map and a list of keys to remove from the hash-map. Again,
// note that the original hash-map is unchanged and a new hash-map with the keys removed is returned.
// Key arguments that do not exist in the hash-map are ignored.

fn dissoc(args: &[Mal]) -> MalResult {
    if let [Mal::HashMap(m, _meta), keys @ ..] = args {
        let mut n: HashMap<MalKey, Mal> = HashMap::new();
        n.clone_from(m);
        for key in keys {
            if let Some(k) = into_key(key) {
                n.remove(&k);
            }
        }
        Ok(into_mal_hashmap(n))
    } else {
        err("dissoc takes a hash-map and keys")
    }
}

// get: takes a hash-map and a key and returns the value of looking up that key in the hash-map. If the key is not
// found in the hash-map then nil is returned.
fn get(args: &[Mal]) -> MalResult {
    let lookup = match args {
        [Mal::HashMap(m, _meta), key] => into_key(key).and_then(|k| m.get(&k)),
        [Mal::Nil, _] => None,
        _ => return err("get takes a hash-map (or nil) and a key"),
    };

    Ok(lookup.map_or(Mal::Nil, std::clone::Clone::clone))
}

// contains?: takes a hash-map and a key and returns true (mal true value) if the key exists
// in the hash-map and false (mal false value) otherwise.

fn contains(args: &[Mal]) -> MalResult {
    if let [Mal::HashMap(m, _meta), key] = args {
        Ok(Mal::Bool(
            into_key(key).map_or(false, |k| m.contains_key(&k)),
        ))
    } else {
        err("contains? takes a hash-map and a key value")
    }
}

// keys: takes a hash-map and returns a list (mal list value) of all the keys in the hash-map.

fn keys(args: &[Mal]) -> MalResult {
    if let [Mal::HashMap(m, _meta)] = args {
        let ys: Vec<Mal> = m.keys().map(from_key).collect();
        Ok(into_mal_seq(true, ys))
    } else {
        err("keys takes a hash-map")
    }
}

// vals: takes a hash-map and returns a list (mal list value) of all the values in the hash-map.

fn vals(args: &[Mal]) -> MalResult {
    if let [Mal::HashMap(m, _meta)] = args {
        let ys: Vec<Mal> = m.values().cloned().collect();
        Ok(into_mal_seq(true, ys))
    } else {
        err("vals takes a hash-map")
    }
}

// Step A functions

// meta: this takes a single mal function/list/vector/hash-map argument and returns the value of the metadata attribute.

fn meta(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Function(_f, meta)] => Ok(meta.as_ref().clone()),
        [Mal::Closure {
            eval: _,
            params: _,
            ast: _,
            env: _,
            is_macro: _,
            meta,
        }] => Ok(meta.as_ref().clone()),
        [Mal::Seq(_, _, meta)] => Ok(meta.as_ref().clone()),
        [Mal::HashMap(_, meta)] => Ok(meta.as_ref().clone()),
        _ => err("meta takes a function, sequence or hash-map"),
    }
}

// with-meta: this function takes two arguments. The first argument is a mal function/list/vector/hash-map and
// the second argument is another mal value/type to set as metadata. A copy of the mal function is returned
// that has its meta attribute set to the second argument. Note that it is important that the environment and
// macro attribute of mal function are retained when it is copied.

fn with_meta(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Function(f, _old_meta), new_meta] => Ok(Mal::Function(*f, Rc::new(new_meta.clone()))),
        [Mal::Closure {
            eval,
            params,
            ast,
            env,
            is_macro,
            meta: _,
        }, new_meta] => Ok(Mal::Closure {
            eval: *eval,
            params: params.clone(),
            ast: ast.clone(),
            env: env.clone(),
            is_macro: *is_macro,
            meta: Rc::new(new_meta.clone()),
        }),
        [Mal::Seq(is_list, xs, _old_meta), new_meta] => {
            Ok(Mal::Seq(*is_list, xs.clone(), Rc::new(new_meta.clone())))
        }
        [Mal::HashMap(m, _old_meta), new_meta] => {
            Ok(Mal::HashMap(m.clone(), Rc::new(new_meta.clone())))
        }
        _ => err("meta takes a function, sequence or hash-map and a new meta value"),
    }
}

// time-ms: takes no arguments and returns the number of milliseconds since epoch (00:00:00 UTC January 1,
// 1970), or, if not possible, since another point in time (time-ms is usually used relatively to measure time durations).

fn time_ms(args: &[Mal]) -> MalResult {
    if !args.is_empty() {
        return err("time-ms takes no arguments");
    }

    match time::SystemTime::now().duration_since(time::UNIX_EPOCH) {
        Ok(d) => match i64::try_from(d.as_millis()) {
            Ok(i) => Ok(Mal::Int(i)),
            Err(e) => {
                let msg = format!("Error in time-ms conversion ({e})");
                err(&msg)
            }
        },
        Err(e) => {
            let msg = format!("Error in time-ms ({e})");
            err(&msg)
        }
    }
}

// conj: takes a collection and one or more elements as arguments and returns a new collection which
// includes the original collection and the new elements. If the collection is a list, a new list is
// returned with the elements inserted at the start of the given list in opposite order; if the collection
// is a vector, a new vector is returned with the elements added to the end of the given vector.

fn conj(args: &[Mal]) -> MalResult {
    if let [Mal::Seq(is_list, xs, _meta), rest @ ..] = args {
        if !rest.is_empty() {
            return Ok(into_mal_seq(
                *is_list,
                if *is_list {
                    let mut ys: Vec<Mal> = rest.iter().cloned().rev().collect();
                    ys.extend_from_slice(xs);
                    ys
                } else {
                    let mut ys: Vec<Mal> = xs.to_vec();
                    ys.extend_from_slice(rest);
                    ys
                },
            ));
        }
    }
    err("conj takes a sequence and one or more elements")
}

// string?: returns true if the parameter is a string.

fn is_string(args: &[Mal]) -> MalResult {
    match args {
        [Mal::String(_)] => Ok(Mal::Bool(true)),
        [_] => Ok(Mal::Bool(false)),
        _ => err("string? takes one argument"),
    }
}

// number?: returns true if the parameter is a number.

fn is_number(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Int(_)] => Ok(Mal::Bool(true)),
        [_] => Ok(Mal::Bool(false)),
        _ => err("number? takes one argument"),
    }
}

// fn?: returns true if the parameter is a function (internal or user-defined).

fn is_fn(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Function(_, _)
        | Mal::Closure {
            eval: _,
            params: _,
            env: _,
            is_macro: false,
            ast: _,
            meta: _,
        }] => Ok(Mal::Bool(true)),
        [_] => Ok(Mal::Bool(false)),
        _ => err("fn? takes one argument"),
    }
}

// macro?: returns true if the parameter is a macro.

fn is_macro(args: &[Mal]) -> MalResult {
    match args {
        [Mal::Closure {
            eval: _,
            params: _,
            env: _,
            is_macro: true,
            ast: _,
            meta: _,
        }] => Ok(Mal::Bool(true)),
        [_] => Ok(Mal::Bool(false)),
        _ => err("macro? takes one argument"),
    }
}

// seq: takes a list, vector, string, or nil. If an empty list, empty vector, or empty string ("") is
// passed in then nil is returned. Otherwise, a list is returned unchanged, a vector is converted into
// a list, and a string is converted to a list that containing the original string split into single character strings.

fn seq(args: &[Mal]) -> MalResult {
    if let [arg] = args {
        match arg {
            Mal::Seq(true, xs, _meta) => {
                if xs.is_empty() {
                    Ok(Mal::Nil)
                } else {
                    Ok(arg.clone())
                }
            }
            Mal::Seq(false, xs, _meta) => {
                if xs.is_empty() {
                    Ok(Mal::Nil)
                } else {
                    Ok(into_mal_seq(true, xs.to_vec()))
                }
            }
            Mal::String(s) => {
                if s.is_empty() {
                    Ok(Mal::Nil)
                } else {
                    let ys: Vec<Mal> = s.chars().map(|c| Mal::String(c.to_string())).collect();
                    Ok(into_mal_seq(true, ys))
                }
            }
            Mal::Nil => Ok(Mal::Nil),

            _ => err("seq takes a list, vector, string or nil"),
        }
    } else {
        err("seq takes one argument")
    }
}

// fn nyi(_args: &[Mal]) -> MalResult {
//     err("NYI")
// }

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

// copy arguments to a new vector, with sequences replaced by their contents
fn copy_args(xs: &[Mal]) -> Vec<Mal> {
    let mut ys: Vec<Mal> = Vec::new();
    for x in xs {
        match x {
            Mal::Seq(_is_list, zs, _meta) => ys.extend_from_slice(zs.as_ref()),
            _ => ys.push(x.clone()),
        }
    }
    ys
}
