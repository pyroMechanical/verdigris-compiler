use nom_locate::LocatedSpan;
use nom_supreme::error::ErrorTree;

pub fn compile_source(source: &str) -> Result<(), ErrorTree<LocatedSpan<&str>>> {
    let result = crate::semant::analyze(source);
    //println!("compiling...");
    result
}