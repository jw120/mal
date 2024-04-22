use std::collections::HashMap;

#[derive(Clone, Eq, PartialEq)]
pub enum Mal {
    List(Vec<Mal>),
    Vector(Vec<Mal>),
    HashMap(HashMap<MalKey, Mal>),
    Int(i64),
    Keyword(String),
    String(String),
    Symbol(String),
    Nil,
    True,
    False,
}

// Types which can be used for HashMap keys
#[derive(Clone, Eq, PartialEq, Hash)]
pub enum MalKey {
    Keyword(String),
    String(String),
}
