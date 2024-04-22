use crate::types::Mal;

pub fn plus(args: &[Mal]) -> Result<Mal, String> {
    match args {
        [Mal::Int(x), Mal::Int(y)] => Ok(Mal::Int(x + y)),
        _ => Err("Bad arguments for +".to_string()),
    }
}
