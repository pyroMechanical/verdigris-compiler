use std::io::{self, BufRead, Write};

mod lexer;
mod parser;
mod typechecker;
mod semant;
mod compile;

fn repl() {
  println!("To execute your code, type '-eval' on a new line after the end of your block.");
  print!("> ");
  let _ = io::stdout().flush();

  loop {
    let mut source = String::new();
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
      match line {
        Ok(source_line) => {
          if source_line.as_str().eq("-eval") {
            let result = compile::compile_source(source.as_str());
            match result
            {
              Ok(_) => {println!("Compile Success!");}
              Err(e) => ()//{println!("{:?}", e);}
            }
            source.clear();
          }
          else {
            if !source.is_empty() {source.push('\n');}
            source.push_str(source_line.as_str());
          }
        }
        Err(e) => {
          eprintln!("{}", e);
        }
      }
      print!("> ");
      let _ = io::stdout().flush();
    }
  }
}

fn main()
{
  repl()
}