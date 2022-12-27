use crate::SpanWith;

pub fn analyze(source: SpanWith) -> Result<(), nom::error::Error<SpanWith>> {
    let result = crate::typechecker::typecheck(source);
    //println!("semantic analysis...");
    result
}