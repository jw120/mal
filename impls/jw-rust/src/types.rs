// Types for mal values
// Used in step 1 onwards

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

// All Mal values are immutable. We use Rc<> to avoid copying on complex types
// Lists, Vectors, Hashmaps and Functions have a second meta argument
// Closure carries its eval function to avoid awkward calling across modules

#[derive(Clone)]
pub enum Mal {
    // Seq is a list of vector (first param is true for a list)
    Seq(bool, Rc<Vec<Mal>>, Rc<Mal>),
    HashMap(Rc<HashMap<MalKey, Mal>>, Rc<Mal>),
    Int(i64),
    Keyword(String),
    String(String),
    Symbol(String),
    Bool(bool),
    Nil,
    Function(fn(&[Mal]) -> MalResult, Rc<Mal>), // Rust function
    Closure {
        eval: fn(Mal, Env) -> MalResult,
        ast: Rc<Mal>,
        params: Vec<Mal>,
        env: Env,
        is_macro: bool,
        meta: Rc<Mal>,
    },
    Atom(Rc<RefCell<Mal>>),
}

// Lists and vectors treated as equal to each other. Functions always compare false.
impl PartialEq for Mal {
    fn eq(&self, other: &Self) -> bool {
        #[allow(clippy::match_same_arms)]
        match (self, other) {
            (Self::Seq(_, xs, _), Self::Seq(_, ys, _)) => xs == ys,
            (Self::HashMap(xs, _), Self::HashMap(ys, _)) => xs == ys,
            (Self::Int(x), Self::Int(y)) => x == y,
            (Self::Keyword(x), Self::Keyword(y)) => x == y,
            (Self::String(x), Self::String(y)) => x == y,
            (Self::Symbol(x), Self::Symbol(y)) => x == y,
            (Self::Bool(x), Self::Bool(y)) => x == y,
            (Self::Nil, Self::Nil) => true,
            (_, _) => false,
        }
    }
}

// Helper constructor functions to reduce boilerplate

#[must_use]
pub fn into_mal_atom(value: Mal) -> Mal {
    Mal::Atom(Rc::new(RefCell::new(value)))
}

pub fn into_mal_closure(
    eval: fn(Mal, Env) -> MalResult,
    ast: Mal,
    params: &[Mal],
    env: &Env,
) -> Mal {
    Mal::Closure {
        eval,
        ast: Rc::new(ast),
        params: params.to_vec(),
        env: env.clone(),
        is_macro: false,
        meta: Rc::new(Mal::Nil),
    }
}

#[must_use]
pub fn into_mal_hashmap(m: HashMap<MalKey, Mal>) -> Mal {
    Mal::HashMap(Rc::new(m), Rc::new(Mal::Nil))
}

pub fn into_mal_fn(f: fn(&[Mal]) -> MalResult) -> Mal {
    Mal::Function(f, Rc::new(Mal::Nil))
}

#[must_use]
pub fn into_mal_seq(is_list: bool, v: Vec<Mal>) -> Mal {
    Mal::Seq(is_list, Rc::new(v), Rc::new(Mal::Nil))
}

#[must_use]
pub fn sym(s: &str) -> Mal {
    Mal::Symbol(s.to_string())
}

#[must_use]
pub const fn is_falsy(x: &Mal) -> bool {
    matches!(x, Mal::Bool(false) | Mal::Nil)
}

pub type MalResult = Result<Mal, String>;

pub fn err<T>(s: &str) -> Result<T, String> {
    Err(s.to_string())
}

// Types which can be used for HashMap keys
#[derive(Clone, Eq, PartialEq, Hash)]
pub enum MalKey {
    Keyword(String),
    String(String),
}

pub struct EnvStruct {
    pub data: RefCell<HashMap<String, Mal>>,
    pub outer: Option<Env>,
}

pub type Env = Rc<EnvStruct>;
