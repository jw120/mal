// Environment data type and manipulation functions
// Used in step 3 onwards

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::types::{err, into_mal_seq, Env, EnvStruct, Mal, MalResult};

// Type borrowed from mal's supplied rust impl
// Use RefCell so we can mutate the environment

pub fn set(env: &Env, key: &str, value: Mal) {
    env.data.borrow_mut().insert(key.to_string(), value);
}

pub fn find(env: &Env, key: &str) -> Option<Env> {
    if env.data.borrow().contains_key(key) {
        return Some(Rc::clone(env));
    }
    env.outer.as_ref().and_then(|o| find(o, key))
    // match &env.outer {
    //     Some(o) => find(o, key),
    //     None => None,
    // }
}

pub fn get(env: &Env, key: &String) -> MalResult {
    find(env, key).map_or_else(
        || Err(format!("{key} not found.")),
        |e| Ok(e.data.borrow()[key].clone()),
    )
}

#[must_use]
pub fn new(outer: Option<&Env>) -> Env {
    Rc::new(EnvStruct {
        data: RefCell::new(HashMap::new()),
        outer: outer.cloned(),
    })
}

pub fn outermost(env: &Env) -> Env {
    let mut e = env.clone();
    while let Some(outer) = &e.outer {
        e = outer.clone();
    }
    e
}

pub fn new_binds(outer: Option<&Env>, binds: &[Mal], exprs: &[Mal]) -> Result<Env, String> {
    let env = new(outer);
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
                            set(&env, s_var, into_mal_seq(true, xs));
                            Ok(env)
                        }
                        _ => err("Extra argument in variadic bind"),
                    };
                }
                return err("No symbol for variadic bind");
            }
            Some(Mal::Symbol(s)) => match exprs_iter.next() {
                Some(expr) => set(&env, s, expr.clone()),
                None => return err("Missing expression in variadic bind"),
            },
            Some(_non_symbol) => return err("Non-symbol in variadic bins"),
            None => match exprs_iter.next() {
                Some(_) => return err("Extra expression in variadic bind"),
                None => return Ok(env),
            },
        }
    }
}
