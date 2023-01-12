
pub fn compile_source(source: &str) -> Result<(), ()> {
  let result = crate::semant::analyze(source);
  //println!("compiling...");
  result
}