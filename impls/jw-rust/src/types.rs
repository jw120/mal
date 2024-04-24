use std::collections::HashMap;
use std::rc::Rc;

type MalFunction = fn(&[Mal]) -> Result<Mal, String>;

// All Mal values are immutable. We use Rc<> to avoid copying on complex types

#[derive(Clone, Eq, PartialEq)]
pub enum Mal {
    List(Rc<Vec<Mal>>),
    Vector(Rc<Vec<Mal>>),
    HashMap(Rc<HashMap<MalKey, Mal>>),
    Int(i64),
    Keyword(String),
    String(String),
    Symbol(String),
    Nil,
    True,
    False,
    Function(MalFunction),
}

// Types which can be used for HashMap keys
#[derive(Clone, Eq, PartialEq, Hash)]
pub enum MalKey {
    Keyword(String),
    String(String),
}
