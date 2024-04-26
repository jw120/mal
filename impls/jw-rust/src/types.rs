use std::collections::HashMap;
use std::rc::Rc;

// All Mal values are immutable. We use Rc<> to avoid copying on complex types
// Lists, Vectors, Hashmaps and Functions have a second meta argument

#[derive(Clone)]
pub enum Mal {
    List(Rc<Vec<Mal>>, Rc<Mal>),
    Vector(Rc<Vec<Mal>>, Rc<Mal>),
    HashMap(Rc<HashMap<MalKey, Mal>>, Rc<Mal>),
    Int(i64),
    Keyword(String),
    String(String),
    Symbol(String),
    Nil,
    True,
    False,
    Function(Rc<dyn Fn(Vec<Mal>) -> Result<Mal, String>>, Rc<Mal>),
}

impl PartialEq for Mal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Mal::List(xs, _), Mal::List(ys, _)) => xs == ys,
            (Mal::Vector(xs, _), Mal::Vector(ys, _)) => xs == ys,
            (Mal::List(xs, _), Mal::Vector(ys, _)) => xs == ys,
            (Mal::Vector(xs, _), Mal::List(ys, _)) => xs == ys,
            (Mal::HashMap(xs, _), Mal::HashMap(ys, _)) => xs == ys,
            (Mal::Int(x), Mal::Int(y)) => x == y,
            (Mal::Keyword(x), Mal::Keyword(y)) => x == y,
            (Mal::String(x), Mal::String(y)) => x == y,
            (Mal::Symbol(x), Mal::Symbol(y)) => x == y,
            (Mal::Nil, Mal::Nil) => true,
            (Mal::True, Mal::True) => true,
            (Mal::False, Mal::False) => true,
            (_, _) => false,
        }
    }
}

pub fn mk_err<T>(s: &str) -> Result<T, String> {
    Err(s.to_string())
}

pub fn into_mal_bool(b: bool) -> Mal {
    if b {
        Mal::True
    } else {
        Mal::False
    }
}

pub fn into_mal_list(v: Vec<Mal>) -> Mal {
    Mal::List(Rc::new(v), Rc::new(Mal::Nil))
}

pub fn into_mal_fn(f: Rc<dyn Fn(Vec<Mal>) -> Result<Mal, String>>) -> Mal {
    Mal::Function(f, Rc::new(Mal::Nil))
}

pub fn is_falsy(x: &Mal) -> bool {
    match x {
        Mal::False => true,
        Mal::Nil => true,
        _ => false,
    }
}

// Types which can be used for HashMap keys
#[derive(Clone, Eq, PartialEq, Hash)]
pub enum MalKey {
    Keyword(String),
    String(String),
}
