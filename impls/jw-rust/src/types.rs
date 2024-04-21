pub enum Mal {
    List(Vec<Mal>),
    Int(i64),
    Symbol(String),
    Nil,
    True,
    False,
}
