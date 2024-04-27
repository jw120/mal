// Environment data type and manipulation functions
// Used in step 3 onwards

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::types::{into_mal_seq, mk_err, Mal};

// Type borrowed from mal's supplied rust impl
// Use RefCell so we can mutate the environment

pub struct EnvStruct {
    data: RefCell<HashMap<String, Mal>>,
    outer: Option<Env>,
}

pub type Env = Rc<EnvStruct>;

pub fn env_set(env: &Env, key: &str, value: Mal) {
    env.data.borrow_mut().insert(key.to_string(), value);
}

pub fn env_find(env: &Env, key: &str) -> Option<Env> {
    if env.data.borrow().contains_key(key) {
        return Some(Rc::clone(env));
    }
    match &env.outer {
        Some(o) => env_find(o, key),
        None => None,
    }
}

pub fn env_get(env: &Env, key: &String) -> Result<Mal, String> {
    match env_find(env, key) {
        Some(e) => Ok(e.data.borrow()[key].clone()),
        None => Err(format!("{} not found.", key).to_string()),
    }
}

pub fn env_new(outer: Option<&Env>) -> Env {
    Rc::new(EnvStruct {
        data: RefCell::new(HashMap::new()),
        outer: outer.cloned(),
    })
}

pub fn env_new_binds(outer: Option<&Env>, binds: &[Mal], exprs: &[Mal]) -> Result<Env, String> {
    let env = env_new(outer);
    let mut binds_iter = binds.iter();
    let mut exprs_iter = exprs.iter();
    loop {
        match binds_iter.next() {
            Some(Mal::Symbol(s)) if s == "&" => {
                if let Some(Mal::Symbol(s_var)) = binds_iter.next() {
                    let mut xs: Vec<Mal> = Vec::new();
                    for x in exprs_iter {
                        xs.push(x.clone());
                    }
                    return match binds_iter.next() {
                        None => {
                            env_set(&env, s_var, into_mal_seq(true, xs));
                            Ok(env)
                        }
                        _ => mk_err("Extra argument in variadic bind"),
                    };
                } else {
                    return mk_err("No symbol for variadic bind");
                }
            }
            Some(Mal::Symbol(s)) => match exprs_iter.next() {
                Some(expr) => env_set(&env, s, expr.clone()),
                None => return mk_err("Missing expression in variadic bind"),
            },
            Some(_non_symbol) => return mk_err("Non-symbol in variadic bins"),
            None => match exprs_iter.next() {
                Some(_) => return mk_err("Extra expression in variadic bind"),
                None => return Ok(env),
            },
        }
    }
}
