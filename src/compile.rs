pub fn compile_source(source: &str) -> Result<(), ()> {
    crate::semant::analyze(source)
}
