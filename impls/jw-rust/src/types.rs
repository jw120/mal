pub enum Mal {
    List(Vec<Mal>),
    Int(i64),
    String(String),
    Symbol(String),
    Nil,
    True,
    False,
}
