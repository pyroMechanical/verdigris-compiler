
pub fn analyze(source: &str) -> Result<(), ()> {
  let result = crate::typechecker::typecheck(source);
  //println!("semantic analysis...");
  result
}