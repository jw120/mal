use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::types::Mal;

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

pub fn env_set_list(_env: &Env, _xs: &[Mal]) {
    unimplemented!()
}

pub fn env_find(env: &Env, key: &str) -> Option<Env> {
    if env.data.borrow().contains_key(key) {
        return Some(Rc::clone(env));
    }
    match &env.outer {
        Some(o) => env_find(&o, key),
        None => None,
    }
}

pub fn env_get(env: &Env, key: &String) -> Result<Mal, String> {
    match env_find(env, key) {
        Some(e) => Ok(e.data.borrow()[key].clone()),
        None => Err(format!("{} not found.", key).to_string()),
    }
}

pub fn env_new(outer: Option<Env>) -> Env {
    Rc::new(EnvStruct {
        data: RefCell::new(HashMap::new()),
        outer,
    })
}
