pub fn analyze(source: &str) -> Result<(), ()> {
    crate::typechecker::typecheck(source);
    Ok(())
}
