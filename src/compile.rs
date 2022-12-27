use crate::SpanWith;

pub fn compile_source(source: SpanWith) -> Result<(), nom::error::Error<SpanWith>> {
    let result = crate::semant::analyze(source);
    //println!("compiling...");
    result
}