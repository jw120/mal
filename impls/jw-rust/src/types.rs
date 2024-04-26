use std::collections::HashMap;
use std::rc::Rc;

// type MalFunction = fn(&[Mal]) -> Result<Mal, String>;

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
            (_, _) => false,
        }
    }
}

// Types which can be used for HashMap keys
#[derive(Clone, Eq, PartialEq, Hash)]
pub enum MalKey {
    Keyword(String),
    String(String),
}
