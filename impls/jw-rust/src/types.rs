// Types for mal values
// Used in step 1 onwards

use std::collections::HashMap;
use std::rc::Rc;

// All Mal values are immutable. We use Rc<> to avoid copying on complex types
// Lists, Vectors, Hashmaps and Functions have a second meta argument

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
    Function(Rc<dyn Fn(Vec<Mal>) -> Result<Mal, String>>, Rc<Mal>),
}

// Lists and vectors treated as equal to each other. Functions always compare false.
impl PartialEq for Mal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Mal::Seq(_, xs, _), Mal::Seq(_, ys, _)) => xs == ys,
            (Mal::HashMap(xs, _), Mal::HashMap(ys, _)) => xs == ys,
            (Mal::Int(x), Mal::Int(y)) => x == y,
            (Mal::Keyword(x), Mal::Keyword(y)) => x == y,
            (Mal::String(x), Mal::String(y)) => x == y,
            (Mal::Symbol(x), Mal::Symbol(y)) => x == y,
            (Mal::Bool(x), Mal::Bool(y)) => x == y,
            (Mal::Nil, Mal::Nil) => true,
            (_, _) => false,
        }
    }
}

pub fn mk_err<T>(s: &str) -> Result<T, String> {
    Err(s.to_string())
}

pub fn into_mal_seq(is_list: bool, v: Vec<Mal>) -> Mal {
    Mal::Seq(is_list, Rc::new(v), Rc::new(Mal::Nil))
}

pub fn into_mal_fn(f: Rc<dyn Fn(Vec<Mal>) -> Result<Mal, String>>) -> Mal {
    Mal::Function(f, Rc::new(Mal::Nil))
}

pub fn is_falsy(x: &Mal) -> bool {
    matches!(x, Mal::Bool(false) | Mal::Nil)
}

// Types which can be used for HashMap keys
#[derive(Clone, Eq, PartialEq, Hash)]
pub enum MalKey {
    Keyword(String),
    String(String),
}
