use nom_locate::LocatedSpan;
use nom_supreme::error::ErrorTree;

pub fn analyze(source: &str) -> Result<(), ErrorTree<LocatedSpan<&str>>> {
    let result = crate::typechecker::typecheck(source);
    //println!("semantic analysis...");
    result
}